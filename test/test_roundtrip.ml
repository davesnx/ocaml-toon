let check_json msg expected actual =
  Alcotest.(check (testable Yojson.Basic.pp Yojson.Basic.equal))
    msg expected actual

let check_roundtrip msg json =
  let encoded = Toon.encode json in
  match Toon.decode encoded with
  | Ok decoded -> check_json msg json decoded
  | Error err -> Alcotest.fail (Toon.error_to_string err)

module Primitives = struct
  let test () =
    [
      Testo.create "roundtrip null" (fun () -> check_roundtrip "null" `Null);
      Testo.create "roundtrip true" (fun () ->
          check_roundtrip "true" (`Bool true));
      Testo.create "roundtrip false" (fun () ->
          check_roundtrip "false" (`Bool false));
      Testo.create "roundtrip positive integer" (fun () ->
          check_roundtrip "42" (`Int 42));
      Testo.create "roundtrip negative integer" (fun () ->
          check_roundtrip "-42" (`Int (-42)));
      Testo.create "roundtrip zero" (fun () -> check_roundtrip "0" (`Int 0));
      Testo.create "roundtrip float" (fun () ->
          check_roundtrip "3.14" (`Float 3.14));
      Testo.create "roundtrip negative float" (fun () ->
          check_roundtrip "-2.5" (`Float (-2.5)));
      Testo.create "roundtrip string" (fun () ->
          check_roundtrip "hello" (`String "hello"));
      Testo.create "roundtrip empty string" (fun () ->
          check_roundtrip "empty" (`String ""));
    ]
end

module String_quoting = struct
  let test () =
    [
      Testo.create "roundtrip unicode" (fun () ->
          let json : Yojson.Basic.t =
            `Assoc
              [
                ("emoji", `String "😀🎉🦀");
                ("chinese", `String "你好世界");
                ("arabic", `String "مرحبا");
                ("mixed", `String "Hello 世界 🌍");
              ]
          in
          check_roundtrip "unicode" json);
    ]
end

module Objects = struct
  let test () =
    [
      Testo.create "roundtrip simple object" (fun () ->
          check_roundtrip "simple" (`Assoc [ ("key", `String "value") ]));
      Testo.create "roundtrip multiple fields" (fun () ->
          check_roundtrip "multi"
            (`Assoc [ ("a", `Int 1); ("b", `Int 2); ("c", `Int 3) ]));
      Testo.create "roundtrip nested object" (fun () ->
          check_roundtrip "nested"
            (`Assoc [ ("nested", `Assoc [ ("key", `String "value") ]) ]));
      Testo.create "roundtrip deeply nested - 3 levels" (fun () ->
          let json : Yojson.Basic.t =
            `Assoc
              [
                ( "level1",
                  `Assoc
                    [
                      ( "level2",
                        `Assoc
                          [ ("level3", `Assoc [ ("value", `String "deep") ]) ]
                      );
                    ] );
              ]
          in
          check_roundtrip "nested" json);
    ]
end

module Arrays = struct
  let test () =
    [
      Testo.create "roundtrip array of numbers" (fun () ->
          check_roundtrip "nums"
            (`Assoc [ ("array", `List [ `Int 1; `Int 2; `Int 3 ]) ]));
      Testo.create "roundtrip primitive arrays" (fun () ->
          let json : Yojson.Basic.t =
            `Assoc
              [
                ( "tags",
                  `List
                    [ `String "reading"; `String "gaming"; `String "coding" ] );
              ]
          in
          check_roundtrip "tags" json);
      Testo.create "roundtrip large primitive array" (fun () ->
          let numbers = List.init 1000 (fun i -> `Int i) in
          let json : Yojson.Basic.t = `Assoc [ ("numbers", `List numbers) ] in
          check_roundtrip "1000 numbers" json);
      Testo.create "roundtrip mixed array" (fun () ->
          check_roundtrip "mixed"
            (`Assoc
               [
                 ( "mixed",
                   `List
                     [ `Int 1; `String "two"; `Bool true; `Null; `Float 3.14 ]
                 );
               ]));
      Testo.create "roundtrip empty array" (fun () ->
          check_roundtrip "empty array" (`Assoc [ ("array", `List []) ]));
      Testo.create "roundtrip empty object" (fun () ->
          check_roundtrip "empty obj" (`Assoc [ ("empty_object", `Assoc []) ]));
    ]
end

module Tabular_arrays = struct
  let test () =
    [
      Testo.create "roundtrip tabular arrays - users" (fun () ->
          let json : Yojson.Basic.t =
            `Assoc
              [
                ( "users",
                  `List
                    [
                      `Assoc [ ("id", `Int 1); ("name", `String "Alice") ];
                      `Assoc [ ("id", `Int 2); ("name", `String "Bob") ];
                    ] );
              ]
          in
          check_roundtrip "users" json);
      Testo.create "roundtrip tabular arrays - products" (fun () ->
          let json : Yojson.Basic.t =
            `Assoc
              [
                ( "products",
                  `List
                    [
                      `Assoc
                        [
                          ("sku", `String "A1");
                          ("name", `String "Widget");
                          ("price", `Float 9.99);
                          ("stock", `Int 100);
                        ];
                      `Assoc
                        [
                          ("sku", `String "B2");
                          ("name", `String "Gadget");
                          ("price", `Float 19.99);
                          ("stock", `Int 50);
                        ];
                    ] );
              ]
          in
          check_roundtrip "products" json);
      Testo.create "roundtrip tabular arrays - single item" (fun () ->
          let json : Yojson.Basic.t =
            `Assoc
              [
                ( "items",
                  `List
                    [ `Assoc [ ("a", `Int 1); ("b", `Int 2); ("c", `Int 3) ] ]
                );
              ]
          in
          check_roundtrip "single item" json);
      Testo.create "roundtrip large tabular array" (fun () ->
          let records =
            List.init 500 (fun i ->
                `Assoc
                  [
                    ("id", `Int i);
                    ("name", `String (Printf.sprintf "user_%d" i));
                    ("value", `Int (i * 2));
                  ])
          in
          let json : Yojson.Basic.t = `Assoc [ ("records", `List records) ] in
          check_roundtrip "500 records" json);
      Testo.create "roundtrip array of objects" (fun () ->
          check_roundtrip "users"
            (`Assoc
               [
                 ( "users",
                   `List
                     [
                       `Assoc [ ("id", `Int 1); ("name", `String "Alice") ];
                       `Assoc [ ("id", `Int 2); ("name", `String "Bob") ];
                     ] );
               ]));
    ]
end

let test () =
  Primitives.test () @ String_quoting.test () @ Objects.test () @ Arrays.test ()
  @ Tabular_arrays.test ()
