type delimiter = Comma | Tab | Pipe

let delim_char = function Comma -> ',' | Tab -> '\t' | Pipe -> '|'
let delim_marker = function Comma -> "" | Tab -> "\t" | Pipe -> "|"

(* §7: quoting and escaping *)

let is_control c = Char.code c < 0x20

(* /^[+-]?[0-9]+(?:\.[0-9]+)?(?:e[+-]?[0-9]+)?$/i, ASCII digits only (§7.2) *)
let is_numeric_like s =
  let n = String.length s in
  let i = ref 0 in
  let is_digit c = c >= '0' && c <= '9' in
  let digits () =
    let start = !i in
    while !i < n && is_digit s.[!i] do
      incr i
    done;
    !i > start
  in
  if !i < n && (s.[!i] = '+' || s.[!i] = '-') then incr i;
  digits ()
  && (if !i < n && s.[!i] = '.' then (
        incr i;
        digits ()
      ) else
        true)
  && (if !i < n && (s.[!i] = 'e' || s.[!i] = 'E') then (
        incr i;
        if !i < n && (s.[!i] = '+' || s.[!i] = '-') then incr i;
        digits ()
      ) else
        true)
  && !i = n

let needs_quote ~delim s =
  let n = String.length s in
  let dc = delim_char delim in
  n = 0 || s = "true" || s = "false" || s = "null" || is_numeric_like s
  || String.exists
       (fun c ->
         c = ':' || c = '"' || c = '\\' || c = '[' || c = ']' || c = '{'
         || c = '}' || c = dc || is_control c)
       s
  || s.[0] = ' '
  || s.[0] = '\t'
  || s.[n - 1] = ' '
  || s.[n - 1] = '\t'
  || s.[0] = '-'
  || s.[0] = '#'

let escape_string s =
  let buf = Buffer.create (String.length s + 8) in
  String.iter
    (fun c ->
      match c with
      | '\\' -> Buffer.add_string buf "\\\\"
      | '"' -> Buffer.add_string buf "\\\""
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c when is_control c ->
          Buffer.add_string buf (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

(* §7.3: ^[A-Za-z_][A-Za-z0-9_.]*$ *)
let key_is_bare s =
  let n = String.length s in
  let is_head c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c = '_' in
  let is_tail c = is_head c || (c >= '0' && c <= '9') || c = '.' in
  n > 0
  && is_head s.[0]
  &&
  let rec loop i = i >= n || (is_tail s.[i] && loop (i + 1)) in
  loop 1

let quote_key s =
  if key_is_bare s then
    s
  else
    "\"" ^ escape_string s ^ "\""

let quote_value ~delim s =
  if needs_quote ~delim s then
    "\"" ^ escape_string s ^ "\""
  else
    s

(* §2: number form. A %g string carries an exponent iff it contains 'e'. *)
let expand_exponent s =
  match String.index_opt s 'e' with
  | None -> s
  | Some ei ->
      let neg = s.[0] = '-' in
      let mant_start =
        if neg then
          1
        else
          0
      in
      let mantissa = String.sub s mant_start (ei - mant_start) in
      let exp =
        int_of_string (String.sub s (ei + 1) (String.length s - ei - 1))
      in
      let int_part, frac_part =
        match String.index_opt mantissa '.' with
        | Some di ->
            ( String.sub mantissa 0 di,
              String.sub mantissa (di + 1) (String.length mantissa - di - 1) )
        | None -> (mantissa, "")
      in
      let digits = int_part ^ frac_part in
      let point = String.length int_part + exp in
      let body =
        if point <= 0 then
          "0." ^ String.make (-point) '0' ^ digits
        else if point >= String.length digits then
          digits ^ String.make (point - String.length digits) '0'
        else
          String.sub digits 0 point ^ "."
          ^ String.sub digits point (String.length digits - point)
      in
      (if neg then
         "-"
       else
         "")
      ^ body

let format_float f =
  if f <> f || f = Float.infinity || f = Float.neg_infinity then
    "null"
  else if f = 0.0 then
    "0"
  else if Float.is_integer f && Float.abs f < 1e21 then
    (* %.0f is exact for any finite double *)
    Printf.sprintf "%.0f" f
  else
    let shortest =
      let try_p p =
        let s = Printf.sprintf "%.*g" p f in
        if float_of_string s = f then
          Some s
        else
          None
      in
      match try_p 15 with
      | Some s -> s
      | None -> (
          match try_p 16 with Some s -> s | None -> Printf.sprintf "%.17g" f)
    in
    let af = Float.abs f in
    if af >= 1e-6 && af < 1e21 then
      expand_exponent shortest
    else
      shortest

let print_primitive ~delim = function
  | `String s -> quote_value ~delim s
  | `Int i -> string_of_int i
  | `Float f -> format_float f
  | `Bool b ->
      if b then
        "true"
      else
        "false"
  | `Null -> "null"
  | `Assoc _ | `List _ -> assert false

let is_primitive = function `Assoc _ | `List _ -> false | _ -> true

(* §9.3/§9.5: uniform column detection, shared by tabular arrays, keyed
   tabular objects, and nested field groups. *)

type field_tree = Leaf of string | Group of string * field_tree list

exception Not_uniform

(* Every value is a non-empty object and all objects share the same key set. *)
let same_key_assocs (values : Yojson.Basic.t list) :
    (string * Yojson.Basic.t) list list option =
  match values with
  | [] -> None
  | _ ->
      let extract = function `Assoc (_ :: _ as kvs) -> Some kvs | _ -> None in
      let extracted = List.map extract values in
      if List.exists Option.is_none extracted then
        None
      else
        let kvs_list = List.map Option.get extracted in
        let key_set kvs = List.sort_uniq String.compare (List.map fst kvs) in
        let ks0 = key_set (List.hd kvs_list) in
        if List.for_all (fun kvs -> key_set kvs = ks0) kvs_list then
          Some kvs_list
        else
          None

let classify_column values =
  if List.for_all is_primitive values then
    `Prim
  else
    match same_key_assocs values with Some kvs -> `Nested kvs | None -> `Fail

let rec build_tree (items : (string * Yojson.Basic.t) list list) :
    field_tree list =
  match items with
  | [] -> raise Not_uniform
  | first :: _ ->
      List.map
        (fun key ->
          let values = List.map (fun kvs -> List.assoc key kvs) items in
          match classify_column values with
          | `Prim -> Leaf key
          | `Nested sub -> Group (key, build_tree sub)
          | `Fail -> raise Not_uniform)
        (List.map fst first)

let detect_tabular items =
  match same_key_assocs items with
  | None -> None
  | Some kvs_list -> (
      try Some (build_tree kvs_list) with Not_uniform -> None)

let detect_keyed fields =
  if List.length fields < 2 then
    None
  else
    detect_tabular (List.map snd fields)

let rec fields_text ~delim tree =
  let sep = String.make 1 (delim_char delim) in
  tree
  |> List.map (function
    | Leaf k -> quote_key k
    | Group (k, sub) -> quote_key k ^ "{" ^ fields_text ~delim sub ^ "}")
  |> String.concat sep

let rec leaf_values tree kvs =
  List.concat_map
    (function
      | Leaf k -> [ List.assoc k kvs ]
      | Group (k, sub) -> (
          match List.assoc k kvs with
          | `Assoc kvs2 -> leaf_values sub kvs2
          | _ -> assert false))
    tree

let bracket ~delim n keyed =
  "[" ^ string_of_int n
  ^ (if keyed then
       ":"
     else
       "")
  ^ delim_marker delim ^ "]"

let header_text ~delim key n keyed tree_opt =
  let fields =
    match tree_opt with
    | None -> ""
    | Some tree -> "{" ^ fields_text ~delim tree ^ "}"
  in
  key ^ bracket ~delim n keyed ^ fields ^ ":"

let inline_items ~delim items =
  let sep = String.make 1 (delim_char delim) in
  items |> List.map (print_primitive ~delim) |> String.concat sep

(* Writers. `body_depth` is the depth of a field's own nested content (rows,
   items, sub-fields), which for a first field on a list-item hyphen line is
   two deeper than the hyphen line rather than one (§10). *)

let rec write_field ~delim ~indent_size buf ~line_prefix ~body_depth key value =
  let key_str = quote_key key in
  match value with
  | `Assoc [] -> Buffer.add_string buf (line_prefix ^ key_str ^ ":")
  | `Assoc fields -> (
      match detect_keyed fields with
      | Some tree ->
          Buffer.add_string buf
            (line_prefix
            ^ header_text ~delim key_str (List.length fields) true (Some tree));
          write_entry_rows ~delim ~indent_size buf body_depth tree fields
      | None ->
          Buffer.add_string buf (line_prefix ^ key_str ^ ":");
          write_object_body ~delim ~indent_size buf body_depth fields)
  | `List [] -> Buffer.add_string buf (line_prefix ^ key_str ^ ": []")
  | `List items when List.for_all is_primitive items ->
      Buffer.add_string buf
        (line_prefix
        ^ header_text ~delim key_str (List.length items) false None
        ^ " " ^ inline_items ~delim items)
  | `List items -> (
      match detect_tabular items with
      | Some tree ->
          Buffer.add_string buf
            (line_prefix
            ^ header_text ~delim key_str (List.length items) false (Some tree));
          write_rows ~delim ~indent_size buf body_depth tree items
      | None ->
          Buffer.add_string buf
            (line_prefix
            ^ header_text ~delim key_str (List.length items) false None);
          write_list_items ~delim ~indent_size buf body_depth items)
  | prim ->
      Buffer.add_string buf
        (line_prefix ^ key_str ^ ": " ^ print_primitive ~delim prim)

and write_object_body ~delim ~indent_size buf depth fields =
  let prefix = String.make (depth * indent_size) ' ' in
  List.iter
    (fun (k, v) ->
      Buffer.add_char buf '\n';
      write_field ~delim ~indent_size buf ~line_prefix:prefix
        ~body_depth:(depth + 1) k v)
    fields

and write_rows ~delim ~indent_size buf depth tree items =
  let prefix = String.make (depth * indent_size) ' ' in
  let sep = String.make 1 (delim_char delim) in
  List.iter
    (fun item ->
      match item with
      | `Assoc kvs ->
          Buffer.add_char buf '\n';
          Buffer.add_string buf prefix;
          Buffer.add_string buf
            (leaf_values tree kvs
            |> List.map (print_primitive ~delim)
            |> String.concat sep)
      | _ -> assert false)
    items

and write_entry_rows ~delim ~indent_size buf depth tree fields =
  let prefix = String.make (depth * indent_size) ' ' in
  let sep = String.make 1 (delim_char delim) in
  List.iter
    (fun (entry_key, value) ->
      match value with
      | `Assoc kvs ->
          Buffer.add_char buf '\n';
          Buffer.add_string buf prefix;
          Buffer.add_string buf (quote_key entry_key ^ ": ");
          Buffer.add_string buf
            (leaf_values tree kvs
            |> List.map (print_primitive ~delim)
            |> String.concat sep)
      | _ -> assert false)
    fields

and write_list_items ~delim ~indent_size buf depth items =
  let prefix = String.make (depth * indent_size) ' ' in
  List.iter
    (fun item ->
      Buffer.add_char buf '\n';
      Buffer.add_string buf prefix;
      write_list_item ~delim ~indent_size buf depth item)
    items

and write_list_item ~delim ~indent_size buf depth item =
  match item with
  | `Assoc [] -> Buffer.add_string buf "-"
  | `Assoc ((fk, fv) :: rest) ->
      Buffer.add_string buf "- ";
      write_field ~delim ~indent_size buf ~line_prefix:""
        ~body_depth:(depth + 2) fk fv;
      write_object_body ~delim ~indent_size buf (depth + 1) rest
  | `List [] -> Buffer.add_string buf ("- " ^ bracket ~delim 0 false ^ ":")
  | `List items when List.for_all is_primitive items ->
      Buffer.add_string buf
        ("- "
        ^ header_text ~delim "" (List.length items) false None
        ^ " " ^ inline_items ~delim items)
  | `List items ->
      Buffer.add_string buf
        ("- " ^ header_text ~delim "" (List.length items) false None);
      write_list_items ~delim ~indent_size buf (depth + 1) items
  | prim -> Buffer.add_string buf ("- " ^ print_primitive ~delim prim)

let encode ?(delimiter = Comma) ?(indent_size = 2) (json : Yojson.Basic.t) :
    string =
  let delim = delimiter in
  let buf = Buffer.create 256 in
  (match json with
  | `Assoc [] -> ()
  | `Assoc fields -> (
      match detect_keyed fields with
      | Some tree ->
          Buffer.add_string buf
            (header_text ~delim "" (List.length fields) true (Some tree));
          write_entry_rows ~delim ~indent_size buf 1 tree fields
      | None -> (
          match fields with
          | [] -> ()
          | (k0, v0) :: rest ->
              write_field ~delim ~indent_size buf ~line_prefix:"" ~body_depth:1
                k0 v0;
              write_object_body ~delim ~indent_size buf 0 rest))
  | `List [] -> Buffer.add_string buf "[]"
  | `List items when List.for_all is_primitive items ->
      Buffer.add_string buf
        (header_text ~delim "" (List.length items) false None
        ^ " " ^ inline_items ~delim items)
  | `List items -> (
      match detect_tabular items with
      | Some tree ->
          Buffer.add_string buf
            (header_text ~delim "" (List.length items) false (Some tree));
          write_rows ~delim ~indent_size buf 1 tree items
      | None ->
          Buffer.add_string buf
            (header_text ~delim "" (List.length items) false None);
          write_list_items ~delim ~indent_size buf 1 items)
  | prim -> Buffer.add_string buf (print_primitive ~delim prim));
  Buffer.contents buf
