type error =
  [ `Unterminated_quoted_string
  | `Expected_quote
  | `Invalid_escape_sequence
  | `No_colon_in_line of string
  | `Invalid_array_syntax
  | `Array_length_mismatch
  | `Invalid_number_format ]

type line_info = { content : string; indent : int }

let get_indent line =
  let rec count i =
    if i >= String.length line then i
    else match line.[i] with ' ' -> count (i + 1) | _ -> i
  in
  count 0

let parse_lines input =
  String.split_on_char '\n' input
  |> List.filter_map (fun line ->
      if String.trim line = "" then None
      else
        let indent = get_indent line in
        let content = String.sub line indent (String.length line - indent) in
        Some { content; indent })

let unescape_char = function
  | 'n' -> '\n'
  | 't' -> '\t'
  | 'r' -> '\r'
  | '\\' -> '\\'
  | '"' -> '"'
  | c -> c

let parse_quoted_string s pos =
  let buf = Buffer.create 16 in
  let rec loop i =
    if i >= String.length s then Error `Unterminated_quoted_string
    else
      match s.[i] with
      | '"' -> Ok (Buffer.contents buf, i + 1)
      | '\\' when i + 1 < String.length s ->
          Buffer.add_char buf (unescape_char s.[i + 1]);
          loop (i + 2)
      | c ->
          Buffer.add_char buf c;
          loop (i + 1)
  in
  if pos < String.length s && s.[pos] = '"' then loop (pos + 1)
  else Error `Expected_quote

let parse_primitive_value s =
  let s = String.trim s in
  if s = "true" then `Bool true
  else if s = "false" then `Bool false
  else if s = "null" then `Null
  else if s = "" then `String ""
  else
    try
      if String.contains s '.' || String.contains s 'e' || String.contains s 'E'
      then `Float (float_of_string s)
      else `Int (int_of_string s)
    with _ -> `String s

let split_by_comma s =
  let rec loop acc buf i in_quotes escaped =
    if i >= String.length s then
      if Buffer.length buf > 0 then List.rev (Buffer.contents buf :: acc)
      else List.rev acc
    else
      match s.[i] with
      | '\\' when not escaped ->
          Buffer.add_char buf s.[i];
          loop acc buf (i + 1) in_quotes true
      | '"' when not escaped ->
          Buffer.add_char buf s.[i];
          loop acc buf (i + 1) (not in_quotes) false
      | ',' when (not in_quotes) && not escaped ->
          loop
            (Buffer.contents buf :: acc)
            (Buffer.create 16) (i + 1) false false
      | c ->
          Buffer.add_char buf c;
          loop acc buf (i + 1) in_quotes false
  in
  if String.trim s = "" then []
  else loop [] (Buffer.create 16) 0 false false |> List.map String.trim

let parse_value_from_string s =
  let s = String.trim s in
  if String.length s > 0 && s.[0] = '"' then
    match parse_quoted_string s 0 with
    | Ok (str, _) -> Ok (`String str)
    | Error e -> Error e
  else Ok (parse_primitive_value s)

let rec parse_structure lines indent_level =
  match lines with
  | [] -> Ok (`Assoc [], [])
  | line :: _ when line.indent < indent_level -> Ok (`Assoc [], lines)
  | line :: _ when line.indent > indent_level -> Ok (`Assoc [], lines)
  | line :: rest -> (
      let colon_idx = String.index_opt line.content ':' in
      match colon_idx with
      | None -> Error (`No_colon_in_line line.content)
      | Some idx -> (
          let before_colon = String.sub line.content 0 idx |> String.trim in
          let after_colon =
            if idx + 1 < String.length line.content then
              String.sub line.content (idx + 1)
                (String.length line.content - idx - 1)
              |> String.trim
            else ""
          in

          let key, is_array, _array_len, array_keys =
            if String.contains before_colon '[' then
              let bracket_start = String.index before_colon '[' in
              let key_part = String.sub before_colon 0 bracket_start in
              let bracket_part =
                String.sub before_colon bracket_start
                  (String.length before_colon - bracket_start)
              in

              if String.contains bracket_part '{' then
                let brace_idx = String.index bracket_part '{' in
                let len_part = String.sub bracket_part 1 (brace_idx - 1) in
                let keys_part =
                  let end_idx = String.index bracket_part '}' in
                  String.sub bracket_part (brace_idx + 1)
                    (end_idx - brace_idx - 1)
                in
                let len_str = String.trim len_part in
                let len_str =
                  if String.length len_str > 0 && len_str.[0] = '#' then
                    String.sub len_str 1 (String.length len_str - 1)
                    |> String.trim
                  else len_str
                in
                let len = try int_of_string len_str with _ -> 0 in
                let keys =
                  split_by_comma keys_part
                  |> List.map (fun k ->
                      if String.length k > 0 && k.[0] = '"' then
                        match parse_quoted_string k 0 with
                        | Ok (s, _) -> s
                        | Error _ -> k
                      else k)
                in
                (key_part, true, len, Some keys)
              else
                let len_str =
                  String.sub bracket_part 1 (String.length bracket_part - 2)
                  |> String.trim
                in
                let len_str =
                  if String.length len_str > 0 && len_str.[0] = '#' then
                    String.sub len_str 1 (String.length len_str - 1)
                    |> String.trim
                  else len_str
                in
                let len = try int_of_string len_str with _ -> 0 in
                (key_part, true, len, None)
            else (before_colon, false, 0, None)
          in

          let key =
            if String.length key > 0 && key.[0] = '"' then
              match parse_quoted_string key 0 with
              | Ok (s, _) -> s
              | Error _ -> key
            else key
          in

          if is_array then
            if after_colon = "" then
              match array_keys with
              | Some keys -> (
                  match parse_tabular_rows rest (indent_level + 2) keys with
                  | Ok (rows, remaining) ->
                      parse_remaining_fields key (`List rows) rest remaining
                        indent_level
                  | Error e -> Error e)
              | None -> (
                  match rest with
                  | next :: _
                    when next.indent > line.indent
                         && String.starts_with ~prefix:"- " next.content -> (
                      match parse_list_items rest (indent_level + 2) with
                      | Ok (items, remaining) ->
                          parse_remaining_fields key (`List items) rest
                            remaining indent_level
                      | Error e -> Error e)
                  | _ ->
                      parse_remaining_fields key (`List []) rest rest
                        indent_level)
            else
              let items = split_by_comma after_colon in
              match parse_primitives items with
              | Ok parsed ->
                  parse_remaining_fields key (`List parsed) rest rest
                    indent_level
              | Error e -> Error e
          else if after_colon = "" then
            match rest with
            | next :: _ when next.indent > line.indent -> (
                match parse_structure rest (indent_level + 2) with
                | Ok (value, remaining) ->
                    parse_remaining_fields key value rest remaining indent_level
                | Error e -> Error e)
            | _ -> parse_remaining_fields key (`Assoc []) rest rest indent_level
          else
            match parse_value_from_string after_colon with
            | Ok value ->
                parse_remaining_fields key value rest rest indent_level
            | Error e -> Error e))

and parse_remaining_fields first_key first_value _original_rest remaining
    indent_level =
  match parse_structure remaining indent_level with
  | Ok (`Assoc more_fields, final_remaining) ->
      Ok (`Assoc ((first_key, first_value) :: more_fields), final_remaining)
  | Ok (_, final_remaining) ->
      Ok (`Assoc [ (first_key, first_value) ], final_remaining)
  | Error e -> Error e

and parse_tabular_rows lines expected_indent keys =
  let rec loop acc remaining =
    match remaining with
    | [] -> Ok (List.rev acc, [])
    | line :: _ when line.indent < expected_indent ->
        Ok (List.rev acc, remaining)
    | line :: rest when line.indent = expected_indent -> (
        let values = split_by_comma line.content in
        match parse_primitives values with
        | Ok parsed ->
            let obj = List.combine keys parsed |> fun pairs -> `Assoc pairs in
            loop (obj :: acc) rest
        | Error e -> Error e)
    | _ :: rest -> loop acc rest
  in
  loop [] lines

and parse_list_items lines expected_indent =
  let rec collect_item_lines remaining acc =
    match remaining with
    | [] -> (List.rev acc, [])
    | line :: _ when line.indent < expected_indent -> (List.rev acc, remaining)
    | line :: _
      when line.indent = expected_indent
           && String.starts_with ~prefix:"- " line.content ->
        (List.rev acc, remaining)
    | line :: rest -> collect_item_lines rest (line :: acc)
  in

  let rec loop acc remaining =
    match remaining with
    | [] -> Ok (List.rev acc, [])
    | line :: _ when line.indent < expected_indent ->
        Ok (List.rev acc, remaining)
    | line :: rest
      when line.indent = expected_indent
           && String.starts_with ~prefix:"- " line.content -> (
        let item_content =
          String.sub line.content 2 (String.length line.content - 2)
          |> String.trim
        in
        let item_lines, rest_after = collect_item_lines rest [] in

        if item_content = "" && item_lines <> [] then
          match parse_structure item_lines (expected_indent + 2) with
          | Ok (`Assoc fields, _) -> loop (`Assoc fields :: acc) rest_after
          | Ok (value, _) -> loop (value :: acc) rest_after
          | Error e -> Error e
        else if String.length item_content > 0 && item_content.[0] = '[' then
          let bracket_end = try String.index item_content ']' with Not_found -> -1 in
          if bracket_end > 0 && bracket_end + 1 < String.length item_content && item_content.[bracket_end + 1] = ':' then
            let len_str = String.sub item_content 1 (bracket_end - 1) |> String.trim in
            let len_str = if String.length len_str > 0 && len_str.[0] = '#' then
              String.sub len_str 1 (String.length len_str - 1) |> String.trim
            else len_str in
            let _len = try int_of_string len_str with _ -> 0 in
            let after_colon = String.sub item_content (bracket_end + 2) (String.length item_content - bracket_end - 2) |> String.trim in
            let items = split_by_comma after_colon in
            (match parse_primitives items with
            | Ok parsed -> loop (`List parsed :: acc) rest_after
            | Error e -> Error e)
          else
            (match parse_value_from_string item_content with
            | Ok value -> loop (value :: acc) rest_after
            | Error e -> Error e)
        else if String.contains item_content ':' || item_lines <> [] then
          let all_item_lines =
            if item_content <> "" then
              { content = item_content; indent = expected_indent + 2 }
              :: item_lines
            else item_lines
          in
          match parse_structure all_item_lines (expected_indent + 2) with
          | Ok (`Assoc fields, _) -> loop (`Assoc fields :: acc) rest_after
          | Ok (value, _) -> loop (value :: acc) rest_after
          | Error e -> Error e
        else
          match parse_value_from_string item_content with
          | Ok value -> loop (value :: acc) rest_after
          | Error e -> Error e)
    | _ :: rest -> loop acc rest
  in
  loop [] lines

and parse_primitives items =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | s :: rest -> (
        match parse_value_from_string s with
        | Ok v -> loop (v :: acc) rest
        | Error e -> Error e)
  in
  loop [] items

let parse input =
  let input = String.trim input in
  if input = "" then Ok (`Assoc [])
  else if String.length input > 0 && input.[0] = '[' then
    try
      let bracket_end = String.index input ']' in
      let colon_idx = String.index_from input bracket_end ':' in
      let rest_of_first_line =
        if colon_idx + 1 < String.length input then
          let rest = String.sub input (colon_idx + 1) (String.length input - colon_idx - 1) in
          let newline_idx = try String.index rest '\n' with Not_found -> String.length rest in
          String.sub rest 0 newline_idx |> String.trim
        else ""
      in

      if rest_of_first_line = "" && String.contains input '\n' then
        let after_first_line_idx = String.index input '\n' + 1 in
        let rest_input = String.sub input after_first_line_idx (String.length input - after_first_line_idx) in
        let item_lines = parse_lines rest_input in
        if item_lines <> [] && List.exists (fun line -> String.starts_with ~prefix:"- " line.content) item_lines then
          (match parse_list_items item_lines 2 with
          | Ok (items, _) -> Ok (`List items)
          | Error e -> Error e)
        else Ok (`List [])
      else if rest_of_first_line = "" then Ok (`List [])
      else if not (String.contains rest_of_first_line '\n') then
        let items = split_by_comma rest_of_first_line in
        match parse_primitives items with
        | Ok parsed -> Ok (`List parsed)
        | Error e -> Error e
      else
        let lines = parse_lines input in
        match parse_structure lines 0 with
        | Ok (value, _) -> Ok value
        | Error e -> Error e
    with Not_found -> Error `Invalid_array_syntax
  else if (not (String.contains input ':')) && not (String.contains input '\n')
  then parse_value_from_string input
  else
    let lines = parse_lines input in
    match parse_structure lines 0 with
    | Ok (value, _) -> Ok value
    | Error e -> Error e
