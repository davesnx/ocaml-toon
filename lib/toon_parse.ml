type error =
  [ `Unterminated_quoted_string
  | `Expected_quote
  | `Invalid_escape_sequence
  | `No_colon_in_line of string
  | `Invalid_array_syntax
  | `Array_length_mismatch
  | `Invalid_number_format ]

let error_to_string : error -> string = function
  | `Unterminated_quoted_string -> "Unterminated quoted string"
  | `Expected_quote -> "Expected quote"
  | `Invalid_escape_sequence -> "Invalid escape sequence"
  | `No_colon_in_line line -> "No colon in line: " ^ line
  | `Invalid_array_syntax -> "Invalid array syntax"
  | `Array_length_mismatch -> "Array length mismatch"
  | `Invalid_number_format -> "Invalid number format"

type location = { content : string; indent : int }

let get_indent line =
  let rec count i =
    if i >= String.length line then
      i
    else
      match line.[i] with ' ' -> count (i + 1) | _ -> i
  in
  count 0

let parse_line line =
  if String.trim line = "" then
    None
  else
    let indent = get_indent line in
    let content = String.sub line indent (String.length line - indent) in
    Some { content; indent }

let parse_lines input =
  input |> String.split_on_char '\n' |> List.filter_map parse_line

let unescape_char = function
  | 'n' -> '\n'
  | 't' -> '\t'
  | 'r' -> '\r'
  | '\\' -> '\\'
  | '"' -> '"'
  | c -> c

let parse_quoted_string str pos =
  let buf = Buffer.create 16 in
  let rec loop i =
    if i >= String.length str then
      Error `Unterminated_quoted_string
    else
      match str.[i] with
      | '"' -> Ok (Buffer.contents buf, i + 1)
      | '\\' when i + 1 < String.length str ->
          Buffer.add_char buf (unescape_char str.[i + 1]);
          loop (i + 2)
      | c ->
          Buffer.add_char buf c;
          loop (i + 1)
  in
  if pos < String.length str && str.[pos] = '"' then
    loop (pos + 1)
  else
    Error `Expected_quote

let parse_number str =
  try Some (`Int (int_of_string str))
  with Failure _ -> (
    try Some (`Float (float_of_string str)) with Failure _ -> None)

let parse_primitive (str : string) =
  match String.trim str with
  | "true" -> `Bool true
  | "false" -> `Bool false
  | "null" -> `Null
  | _ -> ( match parse_number str with Some num -> num | None -> `String str)

let split_by_comma str =
  let rec loop acc buf i ~in_quotes ~escaped =
    if i >= String.length str then
      if Buffer.length buf > 0 then
        List.rev (Buffer.contents buf :: acc)
      else
        List.rev acc
    else
      match str.[i] with
      | '\\' when not escaped ->
          Buffer.add_char buf str.[i];
          loop acc buf (i + 1) ~in_quotes ~escaped:true
      | '"' when not escaped ->
          Buffer.add_char buf str.[i];
          loop acc buf (i + 1) ~in_quotes:(not in_quotes) ~escaped:false
      | ',' when (not in_quotes) && not escaped ->
          let sub = Buffer.create 16 in
          loop
            (Buffer.contents buf :: acc)
            sub (i + 1) ~in_quotes:false ~escaped:false
      | c ->
          Buffer.add_char buf c;
          loop acc buf (i + 1) ~in_quotes ~escaped:false
  in
  if String.trim str = "" then
    []
  else
    let buf = Buffer.create 16 in
    loop [] buf 0 ~in_quotes:false ~escaped:false |> List.map String.trim

let parse_value str =
  let str = String.trim str in
  if String.length str > 0 && str.[0] = '"' then
    match parse_quoted_string str 0 with
    | Ok (result_str, _) -> Ok (`String result_str)
    | Error e -> Error e
  else
    Ok (parse_primitive str)

let strip_length_marker str =
  let str = String.trim str in
  if String.length str > 0 && str.[0] = '#' then
    String.sub str 1 (String.length str - 1) |> String.trim
  else
    str

let parse_unquoted_key key =
  if String.length key > 0 && key.[0] = '"' then
    match parse_quoted_string key 0 with Ok (str, _) -> str | Error _ -> key
  else
    key

let parse_array_keys keys_part =
  split_by_comma keys_part
  |> List.map (fun k ->
      if String.length k > 0 && k.[0] = '"' then
        match parse_quoted_string k 0 with Ok (str, _) -> str | Error _ -> k
      else
        k)

let parse_header before_colon =
  if not (String.contains before_colon '[') then
    (before_colon, false, 0, None)
  else
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
        String.sub bracket_part (brace_idx + 1) (end_idx - brace_idx - 1)
      in
      let len_str = strip_length_marker len_part in
      let len = try int_of_string len_str with _ -> 0 in
      let keys = parse_array_keys keys_part in
      (key_part, true, len, Some keys)
    else
      let len_str =
        String.sub bracket_part 1 (String.length bracket_part - 2)
        |> strip_length_marker
      in
      let len = try int_of_string len_str with _ -> 0 in
      (key_part, true, len, None)

let rec parse_array_value key array_keys after_colon rest line indent_level =
  if after_colon = "" then
    match array_keys with
    | Some keys -> (
        match
          parse_tabular_rows rest ~expected_indent:(indent_level + 2) keys
        with
        | Ok (rows, remaining) ->
            parse_remaining_fields key (`List rows) rest remaining indent_level
        | Error e -> Error e)
    | None -> (
        match rest with
        | next :: _
          when next.indent > line.indent
               && String.starts_with ~prefix:"- " next.content -> (
            match parse_list_items rest ~expected_indent:(indent_level + 2) with
            | Ok (items, remaining) ->
                parse_remaining_fields key (`List items) rest remaining
                  indent_level
            | Error e -> Error e)
        | _ -> parse_remaining_fields key (`List []) rest rest indent_level)
  else
    let items = split_by_comma after_colon in
    match parse_primitives items with
    | Ok parsed ->
        parse_remaining_fields key (`List parsed) rest rest indent_level
    | Error e -> Error e

and parse_object_or_primitive key after_colon rest indent_level =
  if after_colon = "" then
    match rest with
    | next :: _ when next.indent > indent_level -> (
        match parse_structure rest (indent_level + 2) with
        | Ok (value, remaining) ->
            parse_remaining_fields key value rest remaining indent_level
        | Error e -> Error e)
    | _ -> parse_remaining_fields key (`Assoc []) rest rest indent_level
  else
    match parse_value after_colon with
    | Ok value -> parse_remaining_fields key value rest rest indent_level
    | Error e -> Error e

and parse_structure lines indent_level =
  match lines with
  | [] -> Ok (`Assoc [], [])
  | line :: _ when line.indent < indent_level -> Ok (`Assoc [], lines)
  | line :: _ when line.indent > indent_level -> Ok (`Assoc [], lines)
  | line :: rest -> (
      let colon_idx = String.index_opt line.content ':' in
      match colon_idx with
      | None -> Error (`No_colon_in_line line.content)
      | Some idx ->
          let before_colon = String.sub line.content 0 idx |> String.trim in
          let after_colon =
            if idx + 1 < String.length line.content then
              String.sub line.content (idx + 1)
                (String.length line.content - idx - 1)
              |> String.trim
            else
              ""
          in

          let raw_key, is_array, _array_len, array_keys =
            parse_header before_colon
          in
          let key = parse_unquoted_key raw_key in

          if is_array then
            parse_array_value key array_keys after_colon rest line indent_level
          else
            parse_object_or_primitive key after_colon rest indent_level)

and parse_remaining_fields first_key first_value _original_rest remaining
    indent_level =
  match parse_structure remaining indent_level with
  | Ok (`Assoc more_fields, final_remaining) ->
      Ok (`Assoc ((first_key, first_value) :: more_fields), final_remaining)
  | Ok (_, final_remaining) ->
      Ok (`Assoc [ (first_key, first_value) ], final_remaining)
  | Error e -> Error e

and parse_tabular_rows lines ~expected_indent keys =
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

and collect_item_lines ~expected_indent remaining acc =
  match remaining with
  | [] -> (List.rev acc, [])
  | line :: _ when line.indent < expected_indent -> (List.rev acc, remaining)
  | line :: _
    when line.indent = expected_indent
         && String.starts_with ~prefix:"- " line.content ->
      (List.rev acc, remaining)
  | line :: rest -> collect_item_lines ~expected_indent rest (line :: acc)

and parse_inline_array_item (item_content : string) =
  let bracket_end = try String.index item_content ']' with Not_found -> -1 in
  if
    bracket_end > 0
    && bracket_end + 1 < String.length item_content
    && item_content.[bracket_end + 1] = ':'
  then
    let after_colon =
      String.trim
        (String.sub item_content (bracket_end + 2)
           (String.length item_content - bracket_end - 2))
    in
    let items = split_by_comma after_colon in
    match parse_primitives items with
    | Ok parsed -> Ok (`List parsed)
    | Error e -> Error e
  else
    parse_value item_content

and parse_structured_item item_content item_lines ~expected_indent =
  let all_item_lines =
    match item_content with
    | "" -> item_lines
    | _ ->
        { content = item_content; indent = expected_indent + 2 } :: item_lines
  in
  match parse_structure all_item_lines (expected_indent + 2) with
  | Ok (value, _) -> Ok value
  | Error e -> Error e

and parse_list_item content lines ~expected_indent =
  match (content, lines) with
  | "", _ :: _ -> parse_structured_item content lines ~expected_indent
  | str, _ when String.length str > 0 && str.[0] = '[' ->
      parse_inline_array_item content
  | str, _ when String.contains str ':' || lines <> [] ->
      parse_structured_item content lines ~expected_indent
  | _ -> parse_value content

and parse_list_items lines ~expected_indent =
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
        let item_lines, rest_after =
          collect_item_lines ~expected_indent rest []
        in
        match parse_list_item item_content item_lines ~expected_indent with
        | Ok value -> loop (value :: acc) rest_after
        | Error e -> Error e)
    | _ :: rest -> loop acc rest
  in
  loop [] lines

and parse_primitives items =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | str :: rest -> (
        match parse_value str with
        | Ok v -> loop (v :: acc) rest
        | Error e -> Error e)
  in
  loop [] items

let extract_first_line_after_colon input colon_idx =
  if colon_idx + 1 < String.length input then
    let rest =
      String.sub input (colon_idx + 1) (String.length input - colon_idx - 1)
    in
    let newline_idx =
      try String.index rest '\n' with Not_found -> String.length rest
    in
    String.sub rest 0 newline_idx |> String.trim
  else
    ""

let parse_array_with_list_items input =
  let after_first_line_idx = String.index input '\n' + 1 in
  let rest_input =
    String.sub input after_first_line_idx
      (String.length input - after_first_line_idx)
  in
  let item_lines = parse_lines rest_input in
  match item_lines with
  | _ :: _
    when List.exists
           (fun line -> String.starts_with ~prefix:"- " line.content)
           item_lines -> (
      match parse_list_items item_lines ~expected_indent:2 with
      | Ok (items, _) -> Ok (`List items)
      | Error e -> Error e)
  | _ -> Ok (`List [])

let parse_array (input : string) : (Yojson.Basic.t, error) result =
  try
    let bracket_end = String.index input ']' in
    let colon_idx = String.index_from input bracket_end ':' in
    let rest_of_first_line = extract_first_line_after_colon input colon_idx in
    match rest_of_first_line with
    | "" when String.contains input '\n' -> parse_array_with_list_items input
    | "" -> Ok (`List [])
    | _ -> (
        let lines = parse_lines input in
        match parse_structure lines 0 with
        | Ok (value, _) -> Ok value
        | Error e -> Error e)
  with Not_found -> Error `Invalid_array_syntax

let parse (input : string) : (Yojson.Basic.t, error) result =
  match input with
  | "" -> Ok (`Assoc [])
  | input when String.length input > 0 && input.[0] = '[' -> parse_array input
  | input
    when (not (String.contains input ':')) && not (String.contains input '\n')
    ->
      parse_value input
  | _ -> (
      let lines = parse_lines input in
      match parse_structure lines 0 with
      | Ok (value, _) -> Ok value
      | Error e -> Error e)
