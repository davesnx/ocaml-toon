let check_json msg expected actual =
  Alcotest.(check (testable Yojson.Basic.pp Yojson.Basic.equal))
    msg expected actual

let parse input =
  match Toon.parse input with
  | Ok json -> json
  | Error _err -> Alcotest.fail "Parse error"

let test () =
  [
    Testo.create "parses safe unquoted strings" (fun () ->
        check_json "hello" (`String "hello") (parse "hello");
        check_json "Ada_99" (`String "Ada_99") (parse "Ada_99"));
    Testo.create "parses numbers and booleans" (fun () ->
        check_json "42" (`Int 42) (parse "42");
        check_json "true" (`Bool true) (parse "true");
        check_json "false" (`Bool false) (parse "false");
        check_json "null" `Null (parse "null"));
    Testo.create "parses simple objects" (fun () ->
        let toon = "id: 123\nname: Ada" in
        let expected : Yojson.Basic.t = `Assoc [ ("id", `Int 123); ("name", `String "Ada") ] in
        check_json "simple object" expected (parse toon));
    Testo.create "parses primitive arrays" (fun () ->
        let toon = "tags[3]: reading,gaming,coding" in
        let expected : Yojson.Basic.t =
          `Assoc
            [
              ( "tags",
                `List [ `String "reading"; `String "gaming"; `String "coding" ]
              );
            ]
        in
        check_json "primitive array" expected (parse toon));
  ]
