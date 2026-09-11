(* Runner for the vendored toon-spec conformance fixtures (see fixtures/README.md). *)

let rec to_basic (j : Yojson.Safe.t) : Yojson.Basic.t =
  match j with
  | `Null -> `Null
  | `Bool b -> `Bool b
  | `Int i -> `Int i
  | `Intlit s -> `Float (float_of_string s)
  | `Float f -> `Float f
  | `String s -> `String s
  | `Assoc fields -> `Assoc (List.map (fun (k, v) -> (k, to_basic v)) fields)
  | `List items -> `List (List.map to_basic items)

let json_num : Yojson.Basic.t -> float = function
  | `Int i -> float_of_int i
  | `Float f -> f
  | _ -> invalid_arg "json_num"

let rec json_equal (a : Yojson.Basic.t) (b : Yojson.Basic.t) : bool =
  match (a, b) with
  | `Null, `Null -> true
  | `Bool a, `Bool b -> a = b
  | `String a, `String b -> a = b
  | (`Int _ | `Float _), (`Int _ | `Float _) -> json_num a = json_num b
  | `List a, `List b -> (
      try List.for_all2 json_equal a b with Invalid_argument _ -> false)
  | `Assoc a, `Assoc b -> (
      try
        List.for_all2 (fun (k1, v1) (k2, v2) -> k1 = k2 && json_equal v1 v2) a b
      with Invalid_argument _ -> false)
  | _ -> false

let field fields name = List.assoc_opt name fields

let string_field fields name =
  match field fields name with
  | Some (`String s) -> s
  | _ -> failwith (name ^ " field missing or not a string")

let bool_field fields name default =
  match field fields name with
  | Some (`Bool b) -> b
  | None -> default
  | _ -> failwith (name ^ " field not a bool")

let option_fields fields =
  match field fields "options" with Some (`Assoc l) -> l | _ -> []

let delimiter_of_string = function
  | "," -> Toon.Comma
  | "\t" -> Toon.Tab
  | "|" -> Toon.Pipe
  | s -> failwith ("unknown delimiter: " ^ s)

let opt_delimiter opts =
  match field opts "delimiter" with
  | Some (`String s) -> Some (delimiter_of_string s)
  | _ -> None

let opt_indent_size opts =
  match field opts "indentSize" with Some (`Int i) -> Some i | _ -> None

let opt_strict opts =
  match field opts "strict" with Some (`Bool b) -> Some b | _ -> None

(* (file, index, reason) triples for fixtures the runner cannot express. Empty for now. *)
let skipped_fixtures : (string * int * string) list = []

let skip_reason file idx =
  List.find_map
    (fun (f, i, reason) ->
      if f = file && i = idx then
        Some reason
      else
        None)
    skipped_fixtures

let encode_test file idx (fields : (string * Yojson.Safe.t) list) =
  let name = string_field fields "name" in
  let input = to_basic (Option.get (field fields "input")) in
  let expected = string_field fields "expected" in
  let opts = option_fields fields in
  let delimiter = opt_delimiter opts in
  let indent_size = opt_indent_size opts in
  Testo.create
    ~category:[ "fixtures"; "encode"; Filename.remove_extension file ]
    ?skipped:(skip_reason file idx)
    (Printf.sprintf "#%d %s" idx name)
    (fun () ->
      let actual = Toon.encode ?delimiter ?indent_size input in
      Alcotest.(check string) name expected actual)

let decode_test file idx (fields : (string * Yojson.Safe.t) list) =
  let name = string_field fields "name" in
  let input = string_field fields "input" in
  let should_error = bool_field fields "shouldError" false in
  let opts = option_fields fields in
  let indent_size = opt_indent_size opts in
  let strict = opt_strict opts in
  Testo.create
    ~category:[ "fixtures"; "decode"; Filename.remove_extension file ]
    ?skipped:(skip_reason file idx)
    (Printf.sprintf "#%d %s" idx name)
    (fun () ->
      match Toon.decode ?indent_size ?strict input with
      | Ok v ->
          if should_error then
            Alcotest.failf "expected an error, got: %s"
              (Yojson.Basic.pretty_to_string v)
          else
            let expected = to_basic (Option.get (field fields "expected")) in
            if not (json_equal expected v) then
              Alcotest.failf "mismatch@.expected: %s@.actual: %s"
                (Yojson.Basic.pretty_to_string expected)
                (Yojson.Basic.pretty_to_string v)
      | Error err ->
          if not should_error then Alcotest.fail (Toon.error_to_string err))

let tests_of_file make dir file =
  match Yojson.Safe.from_file (Filename.concat dir file) with
  | `Assoc fields -> (
      match field fields "tests" with
      | Some (`List tests) ->
          List.mapi
            (fun idx t ->
              match t with
              | `Assoc t_fields -> make file idx t_fields
              | _ ->
                  failwith
                    (Printf.sprintf "%s: test #%d is not an object" file idx))
            tests
      | _ -> failwith (file ^ ": missing tests array"))
  | _ -> failwith (file ^ ": fixture root is not an object")

let fixture_files dir =
  Sys.readdir dir |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f ".json")
  |> List.sort compare

let test () =
  let encode_dir = "fixtures/encode" in
  let decode_dir = "fixtures/decode" in
  let encode_tests =
    fixture_files encode_dir
    |> List.concat_map (tests_of_file encode_test encode_dir)
  in
  let decode_tests =
    fixture_files decode_dir
    |> List.concat_map (tests_of_file decode_test decode_dir)
  in
  encode_tests @ decode_tests
