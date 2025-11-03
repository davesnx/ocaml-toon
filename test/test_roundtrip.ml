let check_json msg expected actual =
  Alcotest.(check (testable Yojson.Basic.pp Yojson.Basic.equal))
    msg expected actual

let check_string msg expected actual =
  Alcotest.(check string) msg expected actual

let check_roundtrip msg json =
  let encoded = Toon.encode json in
  match Toon.decode encoded with
  | Ok decoded -> check_json msg json decoded
  | Error `Unterminated_quoted_string ->
      Alcotest.fail "Unterminated quoted string"
  | Error `Expected_quote -> Alcotest.fail "Expected quote"
  | Error `Invalid_escape_sequence -> Alcotest.fail "Invalid escape sequence"
  | Error (`No_colon_in_line line) -> Alcotest.fail ("No colon in line: " ^ line)
  | Error `Invalid_array_syntax -> Alcotest.fail "Invalid array syntax"
  | Error `Array_length_mismatch -> Alcotest.fail "Array length mismatch"
  | Error `Invalid_number_format -> Alcotest.fail "Invalid number format"

module Primitives = struct
  let test () =
    [
      Testo.create "encode null" (fun () ->
          check_string "null" "null" (Toon.encode `Null));
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
      Testo.create "encode empty string with quotes" (fun () ->
          check_string "empty" {|""|} (Toon.encode (`String "")));
    ]
end

module String_quoting = struct
  let test () =
    [
      Testo.create "quotes strings that look like booleans" (fun () ->
          check_string "quoted true" {|"true"|} (Toon.encode (`String "true"));
          check_string "quoted false" {|"false"|} (Toon.encode (`String "false"));
          check_string "quoted null" {|"null"|} (Toon.encode (`String "null")));
      Testo.create "quotes strings that look like numbers" (fun () ->
          check_string "quoted 42" {|"42"|} (Toon.encode (`String "42"));
          check_string "quoted -3.14" {|"-3.14"|} (Toon.encode (`String "-3.14"));
          check_string "quoted 1e-6" {|"1e-6"|} (Toon.encode (`String "1e-6"));
          check_string "quoted 05" {|"05"|} (Toon.encode (`String "05")));
      Testo.create "escapes control characters" (fun () ->
          check_string "newline" {|"line1\nline2"|}
            (Toon.encode (`String "line1\nline2"));
          check_string "tab" {|"tab\there"|} (Toon.encode (`String "tab\there"));
          check_string "return" {|"return\rcarriage"|}
            (Toon.encode (`String "return\rcarriage"));
          check_string "backslash" {|"C:\\Users\\path"|}
            (Toon.encode (`String {|C:\Users\path|})));
      Testo.create "quotes strings with structural characters" (fun () ->
          check_string "array-like" {|"[3]: x,y"|}
            (Toon.encode (`String "[3]: x,y"));
          check_string "list item" {|"- item"|} (Toon.encode (`String "- item"));
          check_string "brackets" {|"[test]"|} (Toon.encode (`String "[test]"));
          check_string "braces" {|"{key}"|} (Toon.encode (`String "{key}")));
      Testo.create "handles unicode and emoji" (fun () ->
          check_string "café" "café" (Toon.encode (`String "café"));
          check_string "chinese" "你好" (Toon.encode (`String "你好"));
          check_string "emoji" "🚀" (Toon.encode (`String "🚀"));
          check_string "mixed" "hello 👋 world"
            (Toon.encode (`String "hello 👋 world")));
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
      Testo.create "preserves key order in objects" (fun () ->
          let obj : Yojson.Basic.t =
            `Assoc
              [
                ("id", `Int 123); ("name", `String "Ada"); ("active", `Bool true);
              ]
          in
          check_string "key order" "id: 123\nname: Ada\nactive: true"
            (Toon.encode obj));
      Testo.create "encodes null values in objects" (fun () ->
          let obj : Yojson.Basic.t =
            `Assoc [ ("id", `Int 123); ("value", `Null) ]
          in
          check_string "null value" "id: 123\nvalue: null" (Toon.encode obj));
      Testo.create "encodes empty objects as empty string" (fun () ->
          check_string "empty object" "" (Toon.encode (`Assoc [])));
      Testo.create "roundtrip simple object" (fun () ->
          check_roundtrip "simple" (`Assoc [ ("key", `String "value") ]));
      Testo.create "roundtrip multiple fields" (fun () ->
          check_roundtrip "multi"
            (`Assoc [ ("a", `Int 1); ("b", `Int 2); ("c", `Int 3) ]));
      Testo.create "quotes keys with special characters" (fun () ->
          check_string "colon" {|"order:id": 7|}
            (Toon.encode (`Assoc [ ("order:id", `Int 7) ]));
          check_string "brackets" {|"[index]": 5|}
            (Toon.encode (`Assoc [ ("[index]", `Int 5) ]));
          check_string "braces" {|"{key}": 5|}
            (Toon.encode (`Assoc [ ("{key}", `Int 5) ]));
          check_string "comma" {|"a,b": 1|}
            (Toon.encode (`Assoc [ ("a,b", `Int 1) ])));
      Testo.create "quotes keys with spaces or hyphens" (fun () ->
          check_string "spaces" {|"full name": Ada|}
            (Toon.encode (`Assoc [ ("full name", `String "Ada") ]));
          check_string "hyphen" {|"-lead": 1|}
            (Toon.encode (`Assoc [ ("-lead", `Int 1) ]));
          check_string "padded" {|" a ": 1|}
            (Toon.encode (`Assoc [ (" a ", `Int 1) ])));
      Testo.create "quotes numeric keys" (fun () ->
          check_string "numeric" {|"123": x|}
            (Toon.encode (`Assoc [ ("123", `String "x") ])));
      Testo.create "quotes empty string key" (fun () ->
          check_string "empty key" {|"": 1|}
            (Toon.encode (`Assoc [ ("", `Int 1) ])));
      Testo.create "quotes string values with special characters" (fun () ->
          check_string "colon" {|note: "a:b"|}
            (Toon.encode (`Assoc [ ("note", `String "a:b") ]));
          check_string "comma" {|note: "a,b"|}
            (Toon.encode (`Assoc [ ("note", `String "a,b") ]));
          check_string "newline" {|text: "line1\nline2"|}
            (Toon.encode (`Assoc [ ("text", `String "line1\nline2") ]));
          check_string "quotes" {|text: "say \"hello\""|}
            (Toon.encode (`Assoc [ ("text", `String {|say "hello"|}) ])));
      Testo.create "quotes string values with leading/trailing spaces"
        (fun () ->
          check_string "padded" {|text: " padded "|}
            (Toon.encode (`Assoc [ ("text", `String " padded ") ]));
          check_string "space" {|text: " "|}
            (Toon.encode (`Assoc [ ("text", `String " ") ])));
      Testo.create "encodes deeply nested objects" (fun () ->
          let obj : Yojson.Basic.t =
            `Assoc [ ("a", `Assoc [ ("b", `Assoc [ ("c", `String "deep") ]) ]) ]
          in
          check_string "nested" "a:\n  b:\n    c: deep" (Toon.encode obj));
      Testo.create "encodes empty nested object" (fun () ->
          check_string "empty nested" "user:"
            (Toon.encode (`Assoc [ ("user", `Assoc []) ])));
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
      Testo.create "encodes string arrays inline" (fun () ->
          let obj : Yojson.Basic.t =
            `Assoc [ ("tags", `List [ `String "reading"; `String "gaming" ]) ]
          in
          check_string "tags" "tags[2]: reading,gaming" (Toon.encode obj));
      Testo.create "encodes number arrays inline" (fun () ->
          let obj : Yojson.Basic.t =
            `Assoc [ ("nums", `List [ `Int 1; `Int 2; `Int 3 ]) ]
          in
          check_string "nums" "nums[3]: 1,2,3" (Toon.encode obj));
      Testo.create "encodes mixed primitive arrays inline" (fun () ->
          let obj : Yojson.Basic.t =
            `Assoc
              [
                ("data", `List [ `String "x"; `String "y"; `Bool true; `Int 10 ]);
              ]
          in
          check_string "mixed" "data[4]: x,y,true,10" (Toon.encode obj));
      Testo.create "encodes empty arrays" (fun () ->
          let obj : Yojson.Basic.t = `Assoc [ ("items", `List []) ] in
          check_string "empty" "items[0]:" (Toon.encode obj));
      Testo.create "handles empty string in arrays" (fun () ->
          let obj : Yojson.Basic.t =
            `Assoc [ ("items", `List [ `String "" ]) ]
          in
          check_string "one empty" {|items[1]: ""|} (Toon.encode obj);
          let obj2 : Yojson.Basic.t =
            `Assoc [ ("items", `List [ `String "a"; `String ""; `String "b" ]) ]
          in
          check_string "with empties" {|items[3]: a,"",b|} (Toon.encode obj2));
      Testo.create "handles whitespace-only strings in arrays" (fun () ->
          let obj : Yojson.Basic.t =
            `Assoc [ ("items", `List [ `String " "; `String " " ]) ]
          in
          check_string "spaces" {|items[2]: " "," "|} (Toon.encode obj));
      Testo.create "quotes array strings with special characters" (fun () ->
          let obj : Yojson.Basic.t =
            `Assoc
              [ ("items", `List [ `String "a"; `String "b,c"; `String "d:e" ]) ]
          in
          check_string "special" {|items[3]: a,"b,c","d:e"|} (Toon.encode obj));
      Testo.create "quotes strings that look like booleans/numbers in arrays"
        (fun () ->
          let obj : Yojson.Basic.t =
            `Assoc
              [
                ( "items",
                  `List
                    [
                      `String "x"; `String "true"; `String "42"; `String "-3.14";
                    ] );
              ]
          in
          check_string "ambiguous" {|items[4]: x,"true","42","-3.14"|}
            (Toon.encode obj));
      Testo.create "quotes strings with structural meanings in arrays"
        (fun () ->
          let obj : Yojson.Basic.t =
            `Assoc
              [
                ( "items",
                  `List [ `String "[5]"; `String "- item"; `String "{key}" ] );
              ]
          in
          check_string "structural" {|items[3]: "[5]","- item","{key}"|}
            (Toon.encode obj));
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
      Testo.create "encodes arrays of primitives at root level" (fun () ->
          let arr : Yojson.Basic.t =
            `List
              [ `String "x"; `String "y"; `String "true"; `Bool true; `Int 10 ]
          in
          check_string "root" {|[5]: x,y,"true",true,10|} (Toon.encode arr));
      Testo.create "encodes empty arrays at root level" (fun () ->
          check_string "empty root" "[0]:" (Toon.encode (`List [])));
      Testo.create "roundtrip empty array" (fun () ->
          check_roundtrip "empty array" (`Assoc [ ("array", `List []) ]));
      Testo.create "roundtrip empty object" (fun () ->
          check_roundtrip "empty obj" (`Assoc [ ("empty_object", `Assoc []) ]));
    ]
end

module Tabular_arrays = struct
  let test () =
    [
      Testo.create "encodes arrays of similar objects in tabular format"
        (fun () ->
          let obj : Yojson.Basic.t =
            `Assoc
              [
                ( "items",
                  `List
                    [
                      `Assoc
                        [
                          ("sku", `String "A1");
                          ("qty", `Int 2);
                          ("price", `Float 9.99);
                        ];
                      `Assoc
                        [
                          ("sku", `String "B2");
                          ("qty", `Int 1);
                          ("price", `Float 14.5);
                        ];
                    ] );
              ]
          in
          check_string "tabular"
            "items[2]{sku,qty,price}:\n  A1,2,9.99\n  B2,1,14.5"
            (Toon.encode obj));
      Testo.create "handles null values in tabular format" (fun () ->
          let obj : Yojson.Basic.t =
            `Assoc
              [
                ( "items",
                  `List
                    [
                      `Assoc [ ("id", `Int 1); ("value", `Null) ];
                      `Assoc [ ("id", `Int 2); ("value", `String "test") ];
                    ] );
              ]
          in
          check_string "nulls" "items[2]{id,value}:\n  1,null\n  2,test"
            (Toon.encode obj));
      Testo.create "quotes strings containing delimiters in tabular rows"
        (fun () ->
          let obj : Yojson.Basic.t =
            `Assoc
              [
                ( "items",
                  `List
                    [
                      `Assoc
                        [
                          ("sku", `String "A,1");
                          ("desc", `String "cool");
                          ("qty", `Int 2);
                        ];
                      `Assoc
                        [
                          ("sku", `String "B2");
                          ("desc", `String "wip: test");
                          ("qty", `Int 1);
                        ];
                    ] );
              ]
          in
          check_string "delimiters"
            {|items[2]{sku,desc,qty}:
  "A,1",cool,2
  B2,"wip: test",1|}
            (Toon.encode obj));
      Testo.create "handles tabular arrays with keys needing quotes" (fun () ->
          let obj : Yojson.Basic.t =
            `Assoc
              [
                ( "items",
                  `List
                    [
                      `Assoc
                        [ ("order:id", `Int 1); ("full name", `String "Ada") ];
                      `Assoc
                        [ ("order:id", `Int 2); ("full name", `String "Bob") ];
                    ] );
              ]
          in
          check_string "quoted keys"
            {|items[2]{"order:id","full name"}:
  1,Ada
  2,Bob|}
            (Toon.encode obj));
      Testo.create "uses field order from first object for tabular headers"
        (fun () ->
          let obj : Yojson.Basic.t =
            `Assoc
              [
                ( "items",
                  `List
                    [
                      `Assoc [ ("a", `Int 1); ("b", `Int 2); ("c", `Int 3) ];
                      `Assoc [ ("c", `Int 30); ("b", `Int 20); ("a", `Int 10) ];
                    ] );
              ]
          in
          check_string "field order" "items[2]{a,b,c}:\n  1,2,3\n  10,20,30"
            (Toon.encode obj));
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

module Complex_structures = struct
  let test () =
    [
      Testo.create "encodes objects with mixed arrays and nested objects"
        (fun () ->
          let obj : Yojson.Basic.t =
            `Assoc
              [
                ( "user",
                  `Assoc
                    [
                      ("id", `Int 123);
                      ("name", `String "Ada");
                      ("tags", `List [ `String "reading"; `String "gaming" ]);
                      ("active", `Bool true);
                      ("prefs", `List []);
                    ] );
              ]
          in
          check_string "complex"
            {|user:
  id: 123
  name: Ada
  tags[2]: reading,gaming
  active: true
  prefs[0]:|}
            (Toon.encode obj));
    ]
end

let test () =
  Primitives.test () @ String_quoting.test () @ Objects.test () @ Arrays.test ()
  @ Tabular_arrays.test () @ Complex_structures.test ()
