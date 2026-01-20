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
    Testo.create "array length mismatch - inline array too few" (fun () ->
        let result = Toon.decode "items[3]: a,b" in
        match result with
        | Error `Array_length_mismatch -> ()
        | Ok _ -> Alcotest.fail "Expected Array_length_mismatch error"
        | Error e -> Alcotest.fail (Toon.error_to_string e));
    Testo.create "array length mismatch - inline array too many" (fun () ->
        let result = Toon.decode "items[2]: a,b,c,d" in
        match result with
        | Error `Array_length_mismatch -> ()
        | Ok _ -> Alcotest.fail "Expected Array_length_mismatch error"
        | Error e -> Alcotest.fail (Toon.error_to_string e));
    Testo.create "array length mismatch - tabular too few rows" (fun () ->
        let result = Toon.decode "items[3]{id,name}:\n  1,a\n  2,b" in
        match result with
        | Error `Array_length_mismatch -> ()
        | Ok _ -> Alcotest.fail "Expected Array_length_mismatch error"
        | Error e -> Alcotest.fail (Toon.error_to_string e));
    Testo.create "array length mismatch - tabular too many rows" (fun () ->
        let result = Toon.decode "items[1]{id,name}:\n  1,a\n  2,b" in
        match result with
        | Error `Array_length_mismatch -> ()
        | Ok _ -> Alcotest.fail "Expected Array_length_mismatch error"
        | Error e -> Alcotest.fail (Toon.error_to_string e));
    Testo.create "array length mismatch - list format too few" (fun () ->
        let result = Toon.decode "items[3]:\n  - a\n  - b" in
        match result with
        | Error `Array_length_mismatch -> ()
        | Ok _ -> Alcotest.fail "Expected Array_length_mismatch error"
        | Error e -> Alcotest.fail (Toon.error_to_string e));
    Testo.create "array length mismatch - root array" (fun () ->
        let result = Toon.decode "[3]: a,b" in
        match result with
        | Error `Array_length_mismatch -> ()
        | Ok _ -> Alcotest.fail "Expected Array_length_mismatch error"
        | Error e -> Alcotest.fail (Toon.error_to_string e));
    Testo.create "array length mismatch - empty declared non-empty" (fun () ->
        let result = Toon.decode "items[2]:" in
        match result with
        | Error `Array_length_mismatch -> ()
        | Ok _ -> Alcotest.fail "Expected Array_length_mismatch error"
        | Error e -> Alcotest.fail (Toon.error_to_string e));
    Testo.create "array length valid - exact match" (fun () ->
        let result = Toon.decode "items[3]: a,b,c" in
        match result with
        | Ok _ -> ()
        | Error e -> Alcotest.fail (Toon.error_to_string e));
    Testo.create "CRLF normalization - simple object" (fun () ->
        let result = Toon.decode "id: 1\r\nname: Ada" in
        let expected : Yojson.Basic.t =
          `Assoc [ ("id", `Int 1); ("name", `String "Ada") ]
        in
        match result with
        | Ok json -> check_json "crlf object" expected json
        | Error e -> Alcotest.fail (Toon.error_to_string e));
    Testo.create "CRLF normalization - nested object" (fun () ->
        let result = Toon.decode "user:\r\n  id: 1\r\n  name: Ada" in
        let expected : Yojson.Basic.t =
          `Assoc
            [ ("user", `Assoc [ ("id", `Int 1); ("name", `String "Ada") ]) ]
        in
        match result with
        | Ok json -> check_json "crlf nested" expected json
        | Error e -> Alcotest.fail (Toon.error_to_string e));
    Testo.create "CRLF normalization - array" (fun () ->
        let result = Toon.decode "items[2]:\r\n  - a\r\n  - b" in
        let expected : Yojson.Basic.t =
          `Assoc [ ("items", `List [ `String "a"; `String "b" ]) ]
        in
        match result with
        | Ok json -> check_json "crlf array" expected json
        | Error e -> Alcotest.fail (Toon.error_to_string e));
    Testo.create "lone CR normalization" (fun () ->
        let result = Toon.decode "id: 1\rname: Ada" in
        let expected : Yojson.Basic.t =
          `Assoc [ ("id", `Int 1); ("name", `String "Ada") ]
        in
        match result with
        | Ok json -> check_json "lone cr" expected json
        | Error e -> Alcotest.fail (Toon.error_to_string e));
    Testo.create "scientific notation - small positive" (fun () ->
        check_roundtrip "1e-6" (`Float 1e-6));
    Testo.create "scientific notation - large positive" (fun () ->
        check_roundtrip "3.14e10" (`Float 3.14e10));
    Testo.create "scientific notation - negative exponent" (fun () ->
        check_roundtrip "2.5e-8" (`Float 2.5e-8));
    Testo.create "scientific notation - positive exponent" (fun () ->
        check_roundtrip "1.23e+5" (`Float 1.23e+5));
    Testo.create "scientific notation - uppercase E" (fun () ->
        let encoded = Toon.encode (`Float 1e10) in
        let result = Toon.decode encoded in
        match result with
        | Ok (`Float f) ->
            Alcotest.(check (float 0.01)) "1e10" 1e10 f
        | Ok _ -> Alcotest.fail "Expected Float"
        | Error e -> Alcotest.fail (Toon.error_to_string e));
    Testo.create "scientific notation in object" (fun () ->
        check_roundtrip "sci in obj"
          (`Assoc [ ("tiny", `Float 1e-10); ("huge", `Float 9.99e20) ]));
    Testo.create "scientific notation in array" (fun () ->
        check_roundtrip "sci in array"
          (`Assoc [ ("values", `List [ `Float 1e-3; `Float 2e6; `Float 3e-9 ]) ]));
    Testo.create "pipe delimiter - decode inline array" (fun () ->
        let result = Toon.decode "items[3|]: a|b|c" in
        let expected : Yojson.Basic.t =
          `Assoc [ ("items", `List [ `String "a"; `String "b"; `String "c" ]) ]
        in
        match result with
        | Ok json -> check_json "pipe inline" expected json
        | Error e -> Alcotest.fail (Toon.error_to_string e));
    Testo.create "pipe delimiter - decode tabular" (fun () ->
        let result = Toon.decode "items[2|]{id|name}:\n  1|Ada\n  2|Bob" in
        let expected : Yojson.Basic.t =
          `Assoc
            [
              ( "items",
                `List
                  [
                    `Assoc [ ("id", `Int 1); ("name", `String "Ada") ];
                    `Assoc [ ("id", `Int 2); ("name", `String "Bob") ];
                  ] );
            ]
        in
        match result with
        | Ok json -> check_json "pipe tabular" expected json
        | Error e -> Alcotest.fail (Toon.error_to_string e));
    Testo.create "tab delimiter - decode inline array" (fun () ->
        let result = Toon.decode "items[3\t]: a\tb\tc" in
        let expected : Yojson.Basic.t =
          `Assoc [ ("items", `List [ `String "a"; `String "b"; `String "c" ]) ]
        in
        match result with
        | Ok json -> check_json "tab inline" expected json
        | Error e -> Alcotest.fail (Toon.error_to_string e));
    Testo.create "tab delimiter - decode tabular" (fun () ->
        let result = Toon.decode "items[2\t]{id\tname}:\n  1\tAda\n  2\tBob" in
        let expected : Yojson.Basic.t =
          `Assoc
            [
              ( "items",
                `List
                  [
                    `Assoc [ ("id", `Int 1); ("name", `String "Ada") ];
                    `Assoc [ ("id", `Int 2); ("name", `String "Bob") ];
                  ] );
            ]
        in
        match result with
        | Ok json -> check_json "tab tabular" expected json
        | Error e -> Alcotest.fail (Toon.error_to_string e));
    Testo.create "pipe delimiter - with comma in values" (fun () ->
        let result = Toon.decode "items[2|]: a,b|c,d" in
        let expected : Yojson.Basic.t =
          `Assoc [ ("items", `List [ `String "a,b"; `String "c,d" ]) ]
        in
        match result with
        | Ok json -> check_json "pipe with commas" expected json
        | Error e -> Alcotest.fail (Toon.error_to_string e));
    Testo.create "pipe delimiter - root array" (fun () ->
        let result = Toon.decode "[3|]: x|y|z" in
        let expected : Yojson.Basic.t =
          `List [ `String "x"; `String "y"; `String "z" ]
        in
        match result with
        | Ok json -> check_json "pipe root" expected json
        | Error e -> Alcotest.fail (Toon.error_to_string e));
    Testo.create "pipe delimiter - encode inline array" (fun () ->
        let json : Yojson.Basic.t =
          `Assoc [ ("items", `List [ `String "a"; `String "b"; `String "c" ]) ]
        in
        let encoded = Toon.encode ~delimiter:Toon.Pipe json in
        Alcotest.(check string) "pipe encoded" "items[3|]: a|b|c" encoded);
    Testo.create "pipe delimiter - encode tabular" (fun () ->
        let json : Yojson.Basic.t =
          `Assoc
            [
              ( "items",
                `List
                  [
                    `Assoc [ ("id", `Int 1); ("name", `String "Ada") ];
                    `Assoc [ ("id", `Int 2); ("name", `String "Bob") ];
                  ] );
            ]
        in
        let encoded = Toon.encode ~delimiter:Toon.Pipe json in
        Alcotest.(check string)
          "pipe tabular" "items[2|]{id|name}:\n  1|Ada\n  2|Bob" encoded);
    Testo.create "pipe delimiter - roundtrip" (fun () ->
        let json : Yojson.Basic.t =
          `Assoc
            [
              ( "items",
                `List
                  [
                    `Assoc [ ("id", `Int 1); ("name", `String "Ada") ];
                    `Assoc [ ("id", `Int 2); ("name", `String "Bob") ];
                  ] );
            ]
        in
        let encoded = Toon.encode ~delimiter:Toon.Pipe json in
        let decoded = Toon.decode encoded in
        match decoded with
        | Ok result -> check_json "pipe roundtrip" json result
        | Error e -> Alcotest.fail (Toon.error_to_string e));
    Testo.create "tab delimiter - encode inline array" (fun () ->
        let json : Yojson.Basic.t =
          `Assoc [ ("items", `List [ `String "a"; `String "b"; `String "c" ]) ]
        in
        let encoded = Toon.encode ~delimiter:Toon.Tab json in
        Alcotest.(check string) "tab encoded" "items[3\t]: a\tb\tc" encoded);
    Testo.create "tab delimiter - roundtrip" (fun () ->
        let json : Yojson.Basic.t =
          `Assoc
            [
              ( "items",
                `List
                  [
                    `Assoc [ ("id", `Int 1); ("name", `String "Ada") ];
                    `Assoc [ ("id", `Int 2); ("name", `String "Bob") ];
                  ] );
            ]
        in
        let encoded = Toon.encode ~delimiter:Toon.Tab json in
        let decoded = Toon.decode encoded in
        match decoded with
        | Ok result -> check_json "tab roundtrip" json result
        | Error e -> Alcotest.fail (Toon.error_to_string e));
    Testo.create "pipe delimiter - preserves commas in values" (fun () ->
        let json : Yojson.Basic.t =
          `Assoc [ ("items", `List [ `String "a,b"; `String "c,d" ]) ]
        in
        let encoded = Toon.encode ~delimiter:Toon.Pipe json in
        let decoded = Toon.decode encoded in
        match decoded with
        | Ok result -> check_json "pipe preserves commas" json result
        | Error e -> Alcotest.fail (Toon.error_to_string e));
  ]
