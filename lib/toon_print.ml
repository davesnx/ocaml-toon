let needs_quoting_for_key s =
  if s = "" then
    true
  else if String.contains s ':' || String.contains s ',' then
    true
  else if String.contains s ' ' then
    true
  else if
    String.contains s '\n' || String.contains s '\t' || String.contains s '\r'
  then
    true
  else if String.contains s '"' || String.contains s '\\' then
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
      ignore (float_of_string s);
      true
    with Failure _ -> false

let needs_quoting s =
  if s = "" then
    true
  else if s = "true" || s = "false" || s = "null" then
    true
  else if String.contains s ':' || String.contains s ',' then
    true
  else if
    String.contains s '\n' || String.contains s '\t' || String.contains s '\r'
  then
    true
  else if String.contains s '"' || String.contains s '\\' then
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
      ignore (float_of_string s);
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

let print_quoted_string s =
  if needs_quoting s then
    "\"" ^ escape_string s ^ "\""
  else
    s

let quote_key s =
  if needs_quoting_for_key s then
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

let print_primitive = function
  | `String s -> print_quoted_string s
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

let print_inline_array buf items =
  List.iteri
    (fun i item ->
      if i > 0 then Buffer.add_char buf ',';
      Buffer.add_string buf (print_primitive item))
    items

let print_tabular_header buf prefix key len keys =
  let header_keys =
    List.mapi
      (fun i k ->
        (if i > 0 then
           ","
         else
           "")
        ^ quote_key k)
      keys
    |> String.concat ""
  in
  Buffer.add_string buf
    (prefix ^ key ^ "[" ^ string_of_int len ^ "]{" ^ header_keys ^ "}:")

let print_tabular_rows buf indent keys items =
  List.iter
    (fun item ->
      match item with
      | `Assoc obj ->
          Buffer.add_char buf '\n';
          Buffer.add_string buf (String.make indent ' ');
          List.iteri
            (fun i k ->
              if i > 0 then Buffer.add_char buf ',';
              Buffer.add_string buf (List.assoc k obj |> print_primitive))
            keys
      | _ -> ())
    items

let rec print_object buf indent json =
  let prefix = String.make indent ' ' in
  match json with
  | `Assoc [] -> ()
  | `Assoc fields ->
      List.iteri
        (fun i (key, value) ->
          if i > 0 then Buffer.add_char buf '\n';
          print_field buf prefix indent key value)
        fields
  | _ -> Buffer.add_string buf (print_primitive json)

and print_field buf prefix indent key value =
  let key_str = quote_key key in
  match value with
  | `Assoc [] -> Buffer.add_string buf (prefix ^ key_str ^ ":")
  | `Assoc _ ->
      Buffer.add_string buf (prefix ^ key_str ^ ":\n");
      print_object buf (indent + 2) value
  | `List [] -> Buffer.add_string buf (prefix ^ key_str ^ "[0]:")
  | `List _ -> print_array_field buf prefix indent key_str value
  | _ ->
      Buffer.add_string buf (prefix ^ key_str ^ ": ");
      Buffer.add_string buf (print_primitive value)

and print_array_field buf prefix indent key items =
  match items with
  | `List items when all_primitives items ->
      Buffer.add_string buf
        (prefix ^ key ^ "[" ^ string_of_int (List.length items) ^ "]: ");
      print_inline_array buf items
  | `List items when all_same_keys items && not (has_nested_values items) -> (
      match items with
      | `Assoc first :: _ ->
          let keys = List.map fst first in
          print_tabular_header buf prefix key (List.length items) keys;
          print_tabular_rows buf (indent + 2) keys items
      | _ -> ())
  | `List items -> print_list_format buf prefix indent key items
  | _ -> ()

and print_list_format buf prefix indent key items =
  let len = List.length items in
  Buffer.add_string buf (prefix ^ key ^ "[" ^ string_of_int len ^ "]:");
  List.iter (print_list_item buf (indent + 2)) items

and print_list_item buf indent item =
  Buffer.add_char buf '\n';
  Buffer.add_string buf (String.make indent ' ');
  Buffer.add_string buf "- ";
  match item with
  | `Assoc fields -> print_list_object_fields buf indent fields
  | `List subitems when all_primitives subitems ->
      Buffer.add_string buf ("[" ^ string_of_int (List.length subitems) ^ "]: ");
      print_inline_array buf subitems
  | _ -> Buffer.add_string buf (print_primitive item)

and print_list_object_fields buf indent fields =
  List.iteri
    (fun i (k, v) ->
      if i > 0 then (
        Buffer.add_char buf '\n';
        Buffer.add_string buf (String.make (indent + 2) ' ')
      );
      let key_str = quote_key k in
      match v with
      | `Assoc _ ->
          Buffer.add_string buf (key_str ^ ":\n");
          print_object buf (indent + 4) v
      | `List [] -> Buffer.add_string buf (key_str ^ "[0]:")
      | `List _ ->
          print_array_field buf
            (String.make (indent + 2) ' ')
            (indent + 2) key_str v
      | _ ->
          Buffer.add_string buf (key_str ^ ": ");
          Buffer.add_string buf (print_primitive v))
    fields

let print_root_array buf items =
  let len = List.length items in
  if items = [] then
    Buffer.add_string buf "[0]:"
  else if all_primitives items then (
    Buffer.add_string buf ("[" ^ string_of_int len ^ "]: ");
    print_inline_array buf items
  ) else if all_same_keys items && not (has_nested_values items) then
    match items with
    | `Assoc first :: _ ->
        let keys = List.map fst first in
        print_tabular_header buf "" "" len keys;
        print_tabular_rows buf 2 keys items
    | _ -> ()
  else
    print_list_format buf "" 0 "" items

let print json =
  let buf = Buffer.create 256 in
  (match json with
  | `Assoc _ -> print_object buf 0 json
  | `List items -> print_root_array buf items
  | _ -> Buffer.add_string buf (print_primitive json));
  Buffer.contents buf
