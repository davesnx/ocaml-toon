(* Decoder for toon-spec 4.1. See SPEC.md sections 4-14 for the rules this
   file implements; comments below only note choices the spec leaves open. *)

type error = { line : int; message : string }

let error_to_string (e : error) =
  if e.line > 0 then
    Printf.sprintf "line %d: %s" e.line e.message
  else
    e.message

exception Decode_error of int * string
exception Header_fail

let err ?(line = 0) msg = raise (Decode_error (line, msg))

type field = Leaf of string | Group of string * field list
type kv = { key : string; value_text : string }

type header = {
  key : string option;
  len : int;
  keyed : bool;
  delim : char;
  fields : field list option;
  rest : string; (* trimmed text after the terminal colon *)
  orig_text : string; (* full line text, for non-strict key-value fallback *)
}

type shape = Header of header | KeyValue of kv | Scalar of string
type ln = { lno : int; indent : int; text : string }
type tok = Blank of int | Ln of ln

(* ---- string/char primitives (pure, no strict/cursor state) ---- *)

let is_digit c = c >= '0' && c <= '9'

let is_number_token s =
  let len = String.length s in
  let exception Fail in
  try
    if len = 0 then raise Fail;
    let i = ref 0 in
    if s.[0] = '-' then i := 1;
    let int_start = !i in
    while !i < len && is_digit s.[!i] do
      incr i
    done;
    let int_len = !i - int_start in
    if int_len = 0 then raise Fail;
    if int_len > 1 && s.[int_start] = '0' then raise Fail;
    if !i < len && s.[!i] = '.' then begin
      incr i;
      let fs = !i in
      while !i < len && is_digit s.[!i] do
        incr i
      done;
      if !i = fs then raise Fail
    end;
    if !i < len && (s.[!i] = 'e' || s.[!i] = 'E') then begin
      incr i;
      if !i < len && (s.[!i] = '+' || s.[!i] = '-') then incr i;
      let es = !i in
      while !i < len && is_digit s.[!i] do
        incr i
      done;
      if !i = es then raise Fail
    end;
    !i = len
  with Fail -> false

(* Position of the first char satisfying [pred] that is not inside a quoted
   span; a bare backslash inside quotes skips the next char (scanning only,
   validity of the escape is checked when the token is actually parsed). *)
let find_unquoted pred s =
  let len = String.length s in
  let rec loop i in_q =
    if i >= len then
      None
    else
      let c = s.[i] in
      if in_q then
        if c = '\\' && i + 1 < len then
          loop (i + 2) true
        else if c = '"' then
          loop (i + 1) false
        else
          loop (i + 1) true
      else if c = '"' then
        loop (i + 1) true
      else if pred c then
        Some i
      else
        loop (i + 1) false
  in
  loop 0 false

let trim_u0020 s =
  let len = String.length s in
  let i = ref 0 in
  while !i < len && s.[!i] = ' ' do
    incr i
  done;
  let j = ref (len - 1) in
  while !j >= !i && s.[!j] = ' ' do
    decr j
  done;
  if !j < !i then
    ""
  else
    String.sub s !i (!j - !i + 1)

(* Splits on [delim_char] outside quotes, trimming U+0020 around each token.
   Empty (or all-space) input is zero cells, not one empty cell (SPEC 9.1/9.5). *)
let split_on_delim delim_char s =
  if trim_u0020 s = "" then
    []
  else begin
    let len = String.length s in
    let parts = ref [] in
    let start = ref 0 in
    let i = ref 0 in
    let in_q = ref false in
    while !i < len do
      let c = s.[!i] in
      if !in_q then
        begin if c = '\\' && !i + 1 < len then
          i := !i + 2
        else begin
          if c = '"' then in_q := false;
          incr i
        end
      end
      else if c = '"' then begin
        in_q := true;
        incr i
      end else if c = delim_char then begin
        parts := String.sub s !start (!i - !start) :: !parts;
        incr i;
        start := !i
      end else
        incr i
    done;
    parts := String.sub s !start (len - !start) :: !parts;
    List.rev_map trim_u0020 !parts
  end

let strip_bom s =
  if String.length s >= 3 && s.[0] = '\xEF' && s.[1] = '\xBB' && s.[2] = '\xBF'
  then
    String.sub s 3 (String.length s - 3)
  else
    s

let hex_val c =
  match c with
  | '0' .. '9' -> Char.code c - Char.code '0'
  | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
  | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
  | _ -> raise Not_found

let utf8_encode buf cp =
  if cp < 0x80 then
    Buffer.add_char buf (Char.chr cp)
  else if cp < 0x800 then begin
    Buffer.add_char buf (Char.chr (0xC0 lor (cp lsr 6)));
    Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
  end else begin
    Buffer.add_char buf (Char.chr (0xE0 lor (cp lsr 12)));
    Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
  end

(* s.[start] = '"'. Returns (unescaped, index right after the closing quote). *)
let parse_quoted_token ~line s start =
  let len = String.length s in
  let buf = Buffer.create 16 in
  let rec loop i =
    if i >= len then
      err ~line "unterminated string"
    else
      match s.[i] with
      | '"' -> (Buffer.contents buf, i + 1)
      | '\\' ->
          if i + 1 >= len then
            err ~line "unterminated string"
          else
            begin match s.[i + 1] with
            | '\\' ->
                Buffer.add_char buf '\\';
                loop (i + 2)
            | '"' ->
                Buffer.add_char buf '"';
                loop (i + 2)
            | 'n' ->
                Buffer.add_char buf '\n';
                loop (i + 2)
            | 'r' ->
                Buffer.add_char buf '\r';
                loop (i + 2)
            | 't' ->
                Buffer.add_char buf '\t';
                loop (i + 2)
            | 'u' ->
                if i + 6 > len then
                  err ~line "truncated unicode escape"
                else begin
                  let cp =
                    try
                      (hex_val s.[i + 2] * 4096)
                      + (hex_val s.[i + 3] * 256)
                      + (hex_val s.[i + 4] * 16)
                      + hex_val s.[i + 5]
                    with Not_found -> err ~line "truncated unicode escape"
                  in
                  if cp >= 0xD800 && cp <= 0xDFFF then
                    err ~line "lone surrogate in unicode escape";
                  utf8_encode buf cp;
                  loop (i + 6)
                end
            | _ -> err ~line "invalid escape sequence"
          end
      | c ->
          Buffer.add_char buf c;
          loop (i + 1)
  in
  loop (start + 1)

let decode_key_token ~line text =
  if String.length text > 0 && text.[0] = '"' then begin
    let s, end_i = parse_quoted_token ~line text 0 in
    if end_i <> String.length text then
      err ~line "characters after closing quote";
    s
  end else
    text

(* Primitive token decoding (SPEC 4, Appendix B.4); same in strict and
   non-strict mode. Int when the token has no fraction/exponent and fits an
   OCaml native int; Float otherwise (including integers too large for a
   native int, which then lose precision going through [float_of_string]). *)
let parse_value_token ~line tok =
  if tok = "" then
    `String ""
  else if tok.[0] = '"' then begin
    let s, end_i = parse_quoted_token ~line tok 0 in
    if end_i <> String.length tok then
      err ~line "characters after closing quote";
    `String s
  end else if tok = "true" then
    `Bool true
  else if tok = "false" then
    `Bool false
  else if tok = "null" then
    `Null
  else if is_number_token tok then
    if
      String.contains tok '.' || String.contains tok 'e'
      || String.contains tok 'E'
    then
      `Float (float_of_string tok)
    else
      match int_of_string_opt tok with
      | Some i -> `Int i
      | None -> `Float (float_of_string tok)
  else
    `String tok

(* ---- header / field-list parsing (pure aside from ~strict) ---- *)

let check_field_dups ~strict ~line fields =
  let names = List.map (function Leaf n -> n | Group (n, _) -> n) fields in
  let tbl = Hashtbl.create 8 in
  let dup =
    List.exists
      (fun n ->
        if Hashtbl.mem tbl n then
          true
        else begin
          Hashtbl.add tbl n ();
          false
        end)
      names
  in
  if dup && strict then err ~line "duplicate field name in field list"

(* s.[brace_pos] = '{'. Brace matching ignores braces inside quoted names;
   an unquoted delimiter other than [delim] anywhere in the field list is a
   header/field-list delimiter mismatch (SPEC 6). *)
let parse_fields_seg ~strict ~line delim s brace_pos =
  let len_s = String.length s in
  let other_delims = List.filter (fun c -> c <> delim) [ ','; '\t'; '|' ] in
  let rec parse_level i =
    let entries = ref [] in
    let i = ref i in
    let continue_loop = ref true in
    while !continue_loop do
      if !i < len_s && s.[!i] = '}' then
        begin if !entries = [] then begin
          incr i;
          continue_loop := false
        end else
          raise Header_fail (* trailing delimiter before '}' *)
      end
      else begin
        let name, after_name =
          if !i < len_s && s.[!i] = '"' then begin
            let unescaped, end_i = parse_quoted_token ~line s !i in
            if
              not
                (end_i < len_s
                && (s.[end_i] = delim || s.[end_i] = '{' || s.[end_i] = '}'))
            then
              raise Header_fail;
            (unescaped, end_i)
          end else begin
            let start = !i in
            let j = ref !i in
            while
              !j < len_s
              && s.[!j] <> delim
              && s.[!j] <> '{'
              && s.[!j] <> '}'
              && not (List.mem s.[!j] other_delims)
            do
              incr j
            done;
            if !j >= len_s then raise Header_fail;
            if List.mem s.[!j] other_delims then raise Header_fail;
            (String.sub s start (!j - start), !j)
          end
        in
        i := after_name;
        let field, after_field =
          if !i < len_s && s.[!i] = '{' then begin
            let sub, after = parse_level (!i + 1) in
            if sub = [] then raise Header_fail;
            check_field_dups ~strict ~line sub;
            (Group (name, sub), after)
          end else
            (Leaf name, !i)
        in
        entries := field :: !entries;
        i := after_field;
        if !i < len_s && s.[!i] = delim then
          incr i
        else if !i < len_s && s.[!i] = '}' then begin
          incr i;
          continue_loop := false
        end else
          raise Header_fail
      end
    done;
    (List.rev !entries, !i)
  in
  let entries, after = parse_level (brace_pos + 1) in
  if entries = [] then raise Header_fail;
  check_field_dups ~strict ~line entries;
  (entries, after)

(* s.[bracket_pos] = '['. Raises Header_fail on any malformed header syntax
   (SPEC 6); duplicate field names are checked but never raise Header_fail
   (that particular defect is a strict-only, non-fallback-eligible error). *)
let parse_header_at ~strict ~line s bracket_pos =
  let len_s = String.length s in
  let key =
    if bracket_pos = 0 then
      None
    else begin
      let kt = String.sub s 0 bracket_pos in
      if kt = "" then
        None
      else if kt.[0] = '"' then begin
        let unescaped, end_i = parse_quoted_token ~line kt 0 in
        if end_i = String.length kt then
          Some unescaped
        else
          raise Header_fail
      end else begin
        let last = kt.[String.length kt - 1] in
        if last = ' ' || last = '\t' then
          raise Header_fail
        else
          Some kt
      end
    end
  in
  let i = ref (bracket_pos + 1) in
  if !i >= len_s then raise Header_fail;
  let digit_start = !i in
  if s.[!i] = '0' then
    incr i
  else if s.[!i] >= '1' && s.[!i] <= '9' then begin
    incr i;
    while !i < len_s && is_digit s.[!i] do
      incr i
    done
  end else
    raise Header_fail;
  let digit_len = !i - digit_start in
  if digit_len > 1 && s.[digit_start] = '0' then raise Header_fail;
  let length_val =
    match int_of_string_opt (String.sub s digit_start digit_len) with
    | Some v -> v
    | None -> raise Header_fail
  in
  let keyed = !i < len_s && s.[!i] = ':' in
  if keyed then incr i;
  let delim =
    if !i < len_s && s.[!i] = '\t' then begin
      incr i;
      '\t'
    end else if !i < len_s && s.[!i] = '|' then begin
      incr i;
      '|'
    end else
      ','
  in
  if !i >= len_s || s.[!i] <> ']' then raise Header_fail;
  incr i;
  let fields =
    if !i < len_s && s.[!i] = '{' then begin
      let flds, after = parse_fields_seg ~strict ~line delim s !i in
      i := after;
      Some flds
    end else
      None
  in
  if keyed && fields = None then raise Header_fail;
  if !i >= len_s || s.[!i] <> ':' then raise Header_fail;
  incr i;
  let rest = trim_u0020 (String.sub s !i (len_s - !i)) in
  { key; len = length_val; keyed; delim; fields; rest; orig_text = s }

let try_parse_header_keyless ~strict ~line content =
  try Some (parse_header_at ~strict ~line content 0) with Header_fail -> None

let build_kv ~line s colon_pos =
  let key_text = trim_u0020 (String.sub s 0 colon_pos) in
  let key = decode_key_token ~line key_text in
  let value_text =
    trim_u0020 (String.sub s (colon_pos + 1) (String.length s - colon_pos - 1))
  in
  { key; value_text }

(* Line classification (SPEC 5.2/6): a '[' before any unquoted ':' attempts a
   header; a malformed header attempt is a strict error or a non-strict
   fall-through to a fresh key-value split of the original text. *)
let classify ~strict ~line s =
  match find_unquoted (fun c -> c = ':' || c = '[') s with
  | None -> Scalar s
  | Some pos when s.[pos] = ':' -> KeyValue (build_kv ~line s pos)
  | Some bracket_pos -> (
      try Header (parse_header_at ~strict ~line s bracket_pos)
      with Header_fail ->
        if strict then
          err ~line "malformed header"
        else
          begin match find_unquoted (fun c -> c = ':') s with
          | Some cp -> KeyValue (build_kv ~line s cp)
          | None -> Scalar s
        end)

(* Duplicate sibling keys (SPEC 14.3): strict errors; non-strict keeps the
   first key's position and the last occurrence's value. *)
let resolve_pairs ~strict ~line pairs =
  let seen = Hashtbl.create 8 in
  let has_dup =
    List.exists
      (fun (k, _) ->
        if Hashtbl.mem seen k then
          true
        else begin
          Hashtbl.add seen k ();
          false
        end)
      pairs
  in
  if not has_dup then
    pairs
  else if strict then
    err ~line "duplicate key"
  else begin
    let last = Hashtbl.create 8 in
    List.iter (fun (k, v) -> Hashtbl.replace last k v) pairs;
    let seen2 = Hashtbl.create 8 in
    List.filter_map
      (fun (k, _) ->
        if Hashtbl.mem seen2 k then
          None
        else begin
          Hashtbl.add seen2 k ();
          Some (k, Hashtbl.find last k)
        end)
      pairs
  end

let check_count ~strict ~line n actual =
  if strict && n <> actual then err ~line "count mismatch"

let is_list_item_shape text =
  text = "-" || (String.length text >= 2 && text.[0] = '-' && text.[1] = ' ')

(* ---- pre-pass: BOM/CRLF/trailing-space/comment stripping, depth ---- *)

(* Non-strict tabs in indentation: each tab counts as indentSize spaces
   toward the floor-depth computation (documented leniency, SPEC 12). *)
let tokenize ~indent_size ~strict input =
  let raw_lines = String.split_on_char '\n' input in
  List.concat_map
    (fun (idx, raw) ->
      let lno = idx + 1 in
      let line =
        let len = String.length raw in
        if len > 0 && raw.[len - 1] = '\r' then
          String.sub raw 0 (len - 1)
        else
          raw
      in
      let line =
        let len = String.length line in
        let j = ref len in
        while !j > 0 && line.[!j - 1] = ' ' do
          decr j
        done;
        String.sub line 0 !j
      in
      let is_blank = String.for_all (fun c -> c = ' ' || c = '\t') line in
      if is_blank then
        [ Blank lno ]
      else begin
        let len = String.length line in
        let lead_chars = ref 0 in
        let space_equiv = ref 0 in
        let has_tab = ref false in
        while
          !lead_chars < len
          && (line.[!lead_chars] = ' ' || line.[!lead_chars] = '\t')
        do
          begin if line.[!lead_chars] = '\t' then begin
            has_tab := true;
            space_equiv := !space_equiv + indent_size
          end else
            incr space_equiv
          end;
          incr lead_chars
        done;
        let lead = !lead_chars in
        let content = String.sub line lead (len - lead) in
        if content <> "" && content.[0] = '#' && not !has_tab then
          []
        else begin
          if strict && !has_tab then err ~line:lno "tab used in indentation";
          if strict && lead mod indent_size <> 0 then
            err ~line:lno "indentation not a multiple of indent size";
          let depth =
            if strict then
              lead / indent_size
            else
              !space_equiv / indent_size
          in
          [ Ln { lno; indent = depth; text = content } ]
        end
      end)
    (List.mapi (fun i l -> (i, l)) raw_lines)
  |> Array.of_list

(* ---- recursive descent over the token array ---- *)

let decode ?(indent_size = 2) ?(strict = true) (input : string) :
    (Yojson.Basic.t, error) result =
  try
    let input = strip_bom input in
    if strict && not (String.is_valid_utf_8 input) then
      err "invalid UTF-8 input";
    let toks = tokenize ~indent_size ~strict input in
    let n = Array.length toks in
    let peek pos =
      if pos >= 0 && pos < n then
        Some toks.(pos)
      else
        None
    in
    let skip_blanks pos =
      let p = ref pos in
      while match peek !p with Some (Blank _) -> true | _ -> false do
        incr p
      done;
      !p
    in
    let count_lines () =
      Array.fold_left
        (fun acc t -> match t with Ln _ -> acc + 1 | Blank _ -> acc)
        0 toks
    in
    let find_first_line () =
      let rec go i =
        if i >= n then
          None
        else
          match toks.(i) with Ln l -> Some (i, l) | Blank _ -> go (i + 1)
      in
      go 0
    in
    (* Blanks strictly between two siblings of a header's span (array items,
       tabular rows, keyed entries, and any object fields nested inside a
       list item) are a strict error (SPEC 12); blanks before the first
       sibling, or trailing after the scope's real content, are always fine. *)
    let advance_past_blanks ~in_span ~first ~depth pos =
      let rec go pos =
        match peek pos with
        | Some (Blank bl) ->
            let target = skip_blanks pos in
            let continues =
              match peek target with
              | Some (Ln l) -> l.indent >= depth
              | _ -> false
            in
            if continues then
              if (not first) && in_span && strict then
                err ~line:bl "blank line inside header span"
              else
                go target
            else
              pos
        | _ -> pos
      in
      go pos
    in
    let rec parse_object_body ~in_span ~depth ~seed pos =
      let rec loop acc first pos =
        let pos = advance_past_blanks ~in_span ~first ~depth pos in
        match peek pos with
        | None -> (acc, pos)
        | Some (Blank _) -> (acc, pos)
        | Some (Ln l) ->
            if l.indent < depth then
              (acc, pos)
            else
              (* A scalar-shaped line is invalid here regardless of depth (SPEC
               5.2, any mode); only a non-scalar line is eligible for the
               strict-error/non-strict-skip over-indentation leniency. *)
              begin match classify ~strict ~line:l.lno l.text with
              | Scalar _ -> err ~line:l.lno "scalar line not valid here"
              | shape ->
                  if l.indent > depth then
                    if strict then
                      err ~line:l.lno "over-indented line"
                    else
                      loop acc first (pos + 1)
                  else begin
                    let key, value, pos' =
                      field_from_classification ~in_span ~depth ~line_no:l.lno
                        shape (pos + 1)
                    in
                    loop ((key, value) :: acc) false pos'
                  end
            end
      in
      let acc, pos' = loop (List.rev seed) (seed = []) pos in
      (resolve_pairs ~strict ~line:0 (List.rev acc), pos')
    and field_from_classification ~in_span ~depth ~line_no shape rest_pos =
      match shape with
      | Scalar _ -> err ~line:line_no "scalar line not valid here"
      | KeyValue kv ->
          let v, pos' = value_of_kv ~in_span ~depth kv rest_pos line_no in
          (kv.key, v, pos')
      | Header h -> (
          match h.key with
          | Some key ->
              let v, pos' = value_of_header ~depth h rest_pos line_no in
              (key, v, pos')
          | None ->
              if strict then
                err ~line:line_no "keyless header not valid in field position"
              else
                begin match find_unquoted (fun c -> c = ':') h.orig_text with
                | Some cp ->
                    let kv = build_kv ~line:line_no h.orig_text cp in
                    let v, pos' =
                      value_of_kv ~in_span ~depth kv rest_pos line_no
                    in
                    (kv.key, v, pos')
                | None -> err ~line:line_no "invalid line"
              end)
    and value_of_kv ~in_span ~depth kv rest_pos line_no =
      if kv.value_text = "" then begin
        let pairs, pos' =
          parse_object_body ~in_span ~depth:(depth + 1) ~seed:[] rest_pos
        in
        (`Assoc pairs, pos')
      end else if kv.value_text = "[]" then
        (`List [], rest_pos)
      else
        (parse_value_token ~line:line_no kv.value_text, rest_pos)
    and decode_plain_array ~content_depth h rest_pos line_no =
      if trim_u0020 h.rest <> "" then begin
        let cells = split_on_delim h.delim h.rest in
        check_count ~strict ~line:line_no h.len (List.length cells);
        (`List (List.map (parse_value_token ~line:line_no) cells), rest_pos)
      end else if h.len = 0 then
        (`List [], rest_pos)
      else begin
        let items, pos' = parse_list_items ~depth:content_depth rest_pos in
        check_count ~strict ~line:line_no h.len (List.length items);
        (`List items, pos')
      end
    and value_of_header ~depth h rest_pos line_no =
      match h.fields with
      | None -> decode_plain_array ~content_depth:(depth + 1) h rest_pos line_no
      | Some fields ->
          if trim_u0020 h.rest <> "" then
            err ~line:line_no "content after fields-bearing header colon";
          if h.keyed then begin
            let pairs, pos' =
              parse_keyed_entries ~depth:(depth + 1) ~delim:h.delim ~fields
                rest_pos
            in
            check_count ~strict ~line:line_no h.len (List.length pairs);
            (`Assoc pairs, pos')
          end else begin
            let rows, pos' =
              parse_tabular_rows ~depth:(depth + 1) ~delim:h.delim ~fields
                rest_pos
            in
            check_count ~strict ~line:line_no h.len (List.length rows);
            (`List rows, pos')
          end
    and count_leaves fields =
      List.fold_left
        (fun acc f ->
          acc + match f with Leaf _ -> 1 | Group (_, sub) -> count_leaves sub)
        0 fields
    and assign_cells ~line fields cells =
      let remaining = ref cells in
      let take () =
        match !remaining with
        | x :: rest ->
            remaining := rest;
            Some x
        | [] -> None
      in
      let rec walk fields =
        List.filter_map
          (fun f ->
            match f with
            | Leaf name -> (
                match take () with
                | Some tok -> Some (name, parse_value_token ~line tok)
                | None -> None)
            | Group (name, sub) ->
                let sub_pairs = walk sub in
                Some (name, `Assoc (resolve_pairs ~strict ~line sub_pairs)))
          fields
      in
      walk fields
    and parse_tabular_rows ~depth ~delim ~fields pos =
      let leaf_count = count_leaves fields in
      let rec loop acc first pos =
        let pos = advance_past_blanks ~in_span:true ~first ~depth pos in
        match peek pos with
        | None | Some (Blank _) -> (List.rev acc, pos)
        | Some (Ln l) ->
            if l.indent < depth then
              (List.rev acc, pos)
            else if l.indent > depth then
              (List.rev acc, pos)
            else begin
              (* row-vs-key-value disambiguation, SPEC 9.3 *)
                let delim_pos = find_unquoted (fun c -> c = delim) l.text in
                let colon_pos = find_unquoted (fun c -> c = ':') l.text in
                let is_row =
                  match colon_pos with
                  | None -> true
                  | Some cp -> (
                      match delim_pos with Some dp -> dp < cp | None -> false)
                in
                if not is_row then
                  (List.rev acc, pos)
                else begin
                  let cells = split_on_delim delim l.text in
                  if strict && List.length cells <> leaf_count then
                    err ~line:l.lno "row width mismatch";
                  let pairs = assign_cells ~line:l.lno fields cells in
                  loop
                    (`Assoc (resolve_pairs ~strict ~line:l.lno pairs) :: acc)
                    false (pos + 1)
                end
            end
      in
      loop [] true pos
    and parse_keyed_entries ~depth ~delim ~fields pos =
      let leaf_count = count_leaves fields in
      let rec loop acc first pos =
        let pos = advance_past_blanks ~in_span:true ~first ~depth pos in
        match peek pos with
        | None | Some (Blank _) -> (List.rev acc, pos)
        | Some (Ln l) ->
            if l.indent < depth then
              (List.rev acc, pos)
            else if l.indent > depth then
              (List.rev acc, pos)
            else
              begin match find_unquoted (fun c -> c = ':') l.text with
              | None ->
                  if strict then
                    err ~line:l.lno "entry row missing colon"
                  else
                    loop acc false (pos + 1)
              | Some cp ->
                  let key_text = trim_u0020 (String.sub l.text 0 cp) in
                  let key = decode_key_token ~line:l.lno key_text in
                  let cells_text =
                    String.sub l.text (cp + 1) (String.length l.text - cp - 1)
                  in
                  let cells = split_on_delim delim cells_text in
                  if strict && List.length cells <> leaf_count then
                    err ~line:l.lno "entry row width mismatch";
                  let pairs = assign_cells ~line:l.lno fields cells in
                  let value =
                    `Assoc (resolve_pairs ~strict ~line:l.lno pairs)
                  in
                  loop ((key, value) :: acc) false (pos + 1)
            end
      in
      let pairs, pos' = loop [] true pos in
      (resolve_pairs ~strict ~line:0 pairs, pos')
    and parse_list_items ~depth pos =
      let rec loop acc first pos =
        let pos = advance_past_blanks ~in_span:true ~first ~depth pos in
        match peek pos with
        | None | Some (Blank _) -> (List.rev acc, pos)
        | Some (Ln l) ->
            if l.indent < depth then
              (List.rev acc, pos)
            else if l.indent > depth then
              (List.rev acc, pos)
            else if is_list_item_shape l.text then begin
              let value, pos' = parse_one_list_item ~hyphen_depth:depth pos l in
              loop (value :: acc) false pos'
            end else
              (List.rev acc, pos)
      in
      loop [] true pos
    and parse_one_list_item ~hyphen_depth idx l =
      let rest_pos = idx + 1 in
      if l.text = "-" then
        (`Assoc [], rest_pos)
      else begin
        let content =
          trim_u0020 (String.sub l.text 2 (String.length l.text - 2))
        in
        if content = "[]" then
          (`List [], rest_pos)
        else if String.length content > 0 && content.[0] = '[' then
          begin match try_parse_header_keyless ~strict ~line:l.lno content with
          | None -> err ~line:l.lno "malformed inner array header"
          | Some h ->
              if h.keyed then
                err ~line:l.lno "keyless keyed header not valid as list item"
              else if h.fields <> None then
                err ~line:l.lno
                  "keyless fields-bearing header not valid as list item"
              else
                decode_plain_array ~content_depth:(hyphen_depth + 1) h rest_pos
                  l.lno
        end
        else
          begin match classify ~strict ~line:l.lno content with
          | Scalar s -> (parse_value_token ~line:l.lno s, rest_pos)
          | shape ->
              let key, value, pos2 =
                field_from_classification ~in_span:true
                  ~depth:(hyphen_depth + 1) ~line_no:l.lno shape rest_pos
              in
              let pairs, pos' =
                parse_object_body ~in_span:true ~depth:(hyphen_depth + 1)
                  ~seed:[ (key, value) ]
                  pos2
              in
              (`Assoc pairs, pos')
        end
      end
    and check_trailing_root pos =
      let rec go i =
        if i >= n then
          ()
        else
          match toks.(i) with
          | Blank _ -> go (i + 1)
          | Ln l ->
              if strict then
                err ~line:l.lno "trailing content after root"
              else
                ()
      in
      go pos
    and parse_root () =
      match find_first_line () with
      | None -> `Assoc []
      | Some (idx, l) ->
          if l.indent = 0 && l.text = "[]" then begin
            check_trailing_root (idx + 1);
            `List []
          end else if l.indent = 0 then
            begin match classify ~strict ~line:l.lno l.text with
            | Header h when h.key = None ->
                let value, pos' = value_of_header ~depth:0 h (idx + 1) l.lno in
                check_trailing_root pos';
                value
            | Scalar s when count_lines () = 1 ->
                parse_value_token ~line:l.lno s
            | shape ->
                let key, value, pos' =
                  field_from_classification ~in_span:false ~depth:0
                    ~line_no:l.lno shape (idx + 1)
                in
                let pairs, _ =
                  parse_object_body ~in_span:false ~depth:0
                    ~seed:[ (key, value) ]
                    pos'
                in
                `Assoc pairs
          end
          else begin
            let pairs, _ =
              parse_object_body ~in_span:false ~depth:0 ~seed:[] idx
            in
            `Assoc pairs
          end
    in
    Ok (parse_root ())
  with Decode_error (line, message) -> Error { line; message }
