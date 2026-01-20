type delimiter = Comma | Tab | Pipe

let delimiter_char = function Comma -> ',' | Tab -> '\t' | Pipe -> '|'
let delimiter_str = function Comma -> "," | Tab -> "\t" | Pipe -> "|"

let delimiter_header_marker = function
  | Comma -> ""
  | Tab -> "\t"
  | Pipe -> "|"

let needs_quoting_for_key ~delim s =
  let d = delimiter_char delim in
  if s = "" then
    true
  else if String.contains s ':' || String.contains s d then
    true
  else if String.contains s ' ' then
    true
  else if
    String.contains s '\n' || String.contains s '\t' || String.contains s '\r'
  then
    true
  else if String.contains s '"' || String.contains s '\\' then
    true
  else if String.contains s '|' then
    true
  else if
    s.[0] = '-'
    && String.length s > 1
    && not (Char.code s.[1] >= 48 && Char.code s.[1] <= 57)
  then
    true
  else if s.[0] = '[' || s.[0] = '{' then
    true
  else
    try
      ignore (Float.of_string s);
      true
    with Failure _ -> false

let needs_quoting ~delim s =
  let d = delimiter_char delim in
  if s = "" then
    true
  else if s = "true" || s = "false" || s = "null" then
    true
  else if String.contains s ':' || String.contains s d then
    true
  else if
    String.contains s '\n' || String.contains s '\t' || String.contains s '\r'
  then
    true
  else if String.contains s '"' || String.contains s '\\' then
    true
  else if String.contains s '|' then
    true
  else if s = "-" || (String.length s > 0 && s.[0] = '-') then
    true
  else if s.[0] = '[' || s.[0] = '{' then
    true
  else if String.starts_with ~prefix:" " s || String.ends_with ~suffix:" " s
  then
    true
  else
    try
      ignore (Float.of_string s);
      true
    with Failure _ -> false

let escape_string s =
  let buf = Buffer.create (String.length s * 2) in
  String.iter
    (fun c ->
      match c with
      | '\n' -> Buffer.add_string buf "\\n"
      | '\t' -> Buffer.add_string buf "\\t"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\\' -> Buffer.add_string buf "\\\\"
      | '"' -> Buffer.add_string buf "\\\""
      | _ -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let print_quoted_string ~delim s =
  if needs_quoting ~delim s then
    "\"" ^ escape_string s ^ "\""
  else
    s

let quote_key ~delim s =
  if needs_quoting_for_key ~delim s then
    "\"" ^ escape_string s ^ "\""
  else
    s

let rec all_same_keys = function
  | [] | [ _ ] -> true
  | `Assoc a1 :: `Assoc a2 :: rest ->
      let keys1 = List.map fst a1 |> List.sort String.compare in
      let keys2 = List.map fst a2 |> List.sort String.compare in
      keys1 = keys2 && all_same_keys (`Assoc a2 :: rest)
  | _ -> false

let rec all_primitives = function
  | [] -> true
  | (`String _ | `Int _ | `Float _ | `Bool _ | `Null) :: rest ->
      all_primitives rest
  | _ -> false

let has_nested_values items =
  List.exists
    (function
      | `Assoc fields ->
          List.exists
            (function _, (`Assoc _ | `List _) -> true | _ -> false)
            fields
      | _ -> false)
    items

let print_primitive ~delim = function
  | `String s -> print_quoted_string ~delim s
  | `Int i -> string_of_int i
  | `Float f ->
      if f = 0.0 && 1.0 /. f < 0.0 then
        "0"
      else
        let s = string_of_float f in
        if String.ends_with ~suffix:"." s then
          s ^ "0"
        else
          s
  | `Bool true -> "true"
  | `Bool false -> "false"
  | `Null -> "null"
  | `Assoc _ | `List _ -> ""

let print_inline_array ~delim buf (items : Yojson.Basic.t list) =
  let d = delimiter_str delim in
  List.iteri
    (fun i item ->
      if i > 0 then Buffer.add_string buf d;
      Buffer.add_string buf (print_primitive ~delim item))
    items

let print_tabular_header ~delim buf prefix key len keys =
  let d = delimiter_str delim in
  let header_keys =
    List.mapi
      (fun i k ->
        (if i > 0 then
           d
         else
           "")
        ^ quote_key ~delim k)
      keys
    |> String.concat ""
  in
  let marker = delimiter_header_marker delim in
  Buffer.add_string buf
    (prefix ^ key ^ "[" ^ string_of_int len ^ marker ^ "]{" ^ header_keys ^ "}:")

let print_tabular_rows ~delim buf indent keys (items : Yojson.Basic.t list) =
  let d = delimiter_str delim in
  List.iter
    (fun item ->
      match item with
      | `Assoc obj ->
          Buffer.add_char buf '\n';
          Buffer.add_string buf (String.make indent ' ');
          List.iteri
            (fun i k ->
              if i > 0 then Buffer.add_string buf d;
              Buffer.add_string buf (List.assoc k obj |> print_primitive ~delim))
            keys
      | _ -> ())
    items

let rec print_object ~delim buf indent (json : Yojson.Basic.t) =
  let prefix = String.make indent ' ' in
  match json with
  | `Assoc [] -> ()
  | `Assoc fields ->
      List.iteri
        (fun i (key, value) ->
          if i > 0 then Buffer.add_char buf '\n';
          print_field ~delim buf prefix indent key value)
        fields
  | _ -> Buffer.add_string buf (print_primitive ~delim json)

and print_field ~delim buf prefix indent key (value : Yojson.Basic.t) =
  let key_str = quote_key ~delim key in
  match value with
  | `Assoc [] -> Buffer.add_string buf (prefix ^ key_str ^ ":")
  | `Assoc _ ->
      Buffer.add_string buf (prefix ^ key_str ^ ":\n");
      print_object ~delim buf (indent + 2) value
  | `List [] ->
      let marker = delimiter_header_marker delim in
      Buffer.add_string buf (prefix ^ key_str ^ "[0" ^ marker ^ "]:")
  | `List _ -> print_array_field ~delim buf prefix indent key_str value
  | _ ->
      Buffer.add_string buf (prefix ^ key_str ^ ": ");
      Buffer.add_string buf (print_primitive ~delim value)

and print_array_field ~delim buf prefix indent key items =
  let marker = delimiter_header_marker delim in
  match items with
  | `List items when all_primitives items ->
      Buffer.add_string buf
        (prefix ^ key ^ "[" ^ string_of_int (List.length items) ^ marker
       ^ "]: ");
      print_inline_array ~delim buf items
  | `List items when all_same_keys items && not (has_nested_values items) -> (
      match items with
      | `Assoc first :: _ ->
          let keys = List.map fst first in
          print_tabular_header ~delim buf prefix key (List.length items) keys;
          print_tabular_rows ~delim buf (indent + 2) keys items
      | _ -> ())
  | `List items -> print_list_format ~delim buf prefix indent key items
  | _ -> ()

and print_list_format ~delim buf prefix indent key items =
  let len = List.length items in
  let marker = delimiter_header_marker delim in
  Buffer.add_string buf
    (prefix ^ key ^ "[" ^ string_of_int len ^ marker ^ "]:");
  List.iter (print_list_item ~delim buf (indent + 2)) items

and print_list_item ~delim buf indent item =
  Buffer.add_char buf '\n';
  Buffer.add_string buf (String.make indent ' ');
  Buffer.add_string buf "- ";
  match item with
  | `Assoc fields -> print_list_object_fields ~delim buf indent fields
  | `List subitems when all_primitives subitems ->
      let marker = delimiter_header_marker delim in
      Buffer.add_string buf
        ("[" ^ string_of_int (List.length subitems) ^ marker ^ "]: ");
      print_inline_array ~delim buf subitems
  | _ -> Buffer.add_string buf (print_primitive ~delim item)

and print_list_object_fields ~delim buf indent fields =
  List.iteri
    (fun i (k, v) ->
      if i > 0 then (
        Buffer.add_char buf '\n';
        Buffer.add_string buf (String.make (indent + 2) ' ')
      );
      let key_str = quote_key ~delim k in
      match v with
      | `Assoc _ ->
          Buffer.add_string buf (key_str ^ ":\n");
          print_object ~delim buf (indent + 4) v
      | `List [] ->
          let marker = delimiter_header_marker delim in
          Buffer.add_string buf (key_str ^ "[0" ^ marker ^ "]:")
      | `List _ ->
          print_array_field ~delim buf
            (String.make (indent + 2) ' ')
            (indent + 2) key_str v
      | _ ->
          Buffer.add_string buf (key_str ^ ": ");
          Buffer.add_string buf (print_primitive ~delim v))
    fields

let print_root_array ~delim buf items =
  let len = List.length items in
  let marker = delimiter_header_marker delim in
  if items = [] then
    Buffer.add_string buf ("[0" ^ marker ^ "]:")
  else if all_primitives items then (
    Buffer.add_string buf ("[" ^ string_of_int len ^ marker ^ "]: ");
    print_inline_array ~delim buf items
  ) else if all_same_keys items && not (has_nested_values items) then
    match items with
    | `Assoc first :: _ ->
        let keys = List.map fst first in
        print_tabular_header ~delim buf "" "" len keys;
        print_tabular_rows ~delim buf 2 keys items
    | _ -> ()
  else
    print_list_format ~delim buf "" 0 "" items

let encode ?(delimiter = Comma) json =
  let buf = Buffer.create 256 in
  (match json with
  | `Assoc _ -> print_object ~delim:delimiter buf 0 json
  | `List items -> print_root_array ~delim:delimiter buf items
  | _ -> Buffer.add_string buf (print_primitive ~delim:delimiter json));
  Buffer.contents buf
