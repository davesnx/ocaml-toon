let check_json msg expected actual =
  Alcotest.check
    (Alcotest.testable Yojson.Basic.pp Yojson.Basic.equal)
    msg expected actual

let check_roundtrip msg json =
  let encoded = Toon.encode json in
  match Toon.decode encoded with
  | Ok decoded -> check_json msg json decoded
  | Error err ->
      let msg_str = Toon.error_to_string err in
      Alcotest.fail msg_str

let test () =
  [
    Testo.create "arrays of arrays (primitives)" (fun () ->
        let json : Yojson.Basic.t =
          `Assoc
            [
              ( "pairs",
                `List [ `List [ `Int 1; `Int 2 ]; `List [ `Int 3; `Int 4 ] ] );
            ]
        in
        check_roundtrip "arrays of arrays" json);
    Testo.create "root arrays of arrays" (fun () ->
        let json : Yojson.Basic.t =
          `List [ `List [ `Int 1; `Int 2 ]; `List [] ]
        in
        check_roundtrip "root arrays of arrays" json);
    Testo.create "-0 normalization" (fun () ->
        let json : Yojson.Basic.t = `Assoc [ ("zero", `Float (-0.0)) ] in
        let encoded = Toon.encode json in
        Alcotest.(check string) "zero" "zero: 0" encoded);
    Testo.create "hyphen quoting" (fun () ->
        let json : Yojson.Basic.t = `Assoc [ ("value", `String "-") ] in
        let encoded = Toon.encode json in
        Alcotest.(check bool) "quotes hyphen" true (String.contains encoded '"'));
    Testo.create "strings starting with hyphen" (fun () ->
        let json : Yojson.Basic.t = `Assoc [ ("value", `String "-test") ] in
        let encoded = Toon.encode json in
        Alcotest.(check bool) "quotes -test" true (String.contains encoded '"'));
  ]
