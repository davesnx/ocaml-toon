let check_string msg expected actual =
  Alcotest.(check string) msg expected actual

let test () =
  [
    Testo.create "prints safe strings" (fun () ->
        check_string "hello" "hello" (Toon.print (`String "hello"));
        check_string "Ada_99" "Ada_99" (Toon.print (`String "Ada_99")));
    Testo.create "prints numbers and booleans" (fun () ->
        check_string "42" "42" (Toon.print (`Int 42));
        check_string "true" "true" (Toon.print (`Bool true));
        check_string "false" "false" (Toon.print (`Bool false));
        check_string "null" "null" (Toon.print `Null));
    Testo.create "prints simple objects" (fun () ->
        let obj = `Assoc [ ("id", `Int 123); ("name", `String "Ada") ] in
        check_string "simple object" "id: 123\nname: Ada" (Toon.print obj));
    Testo.create "prints primitive arrays" (fun () ->
        let obj =
          `Assoc
            [
              ( "tags",
                `List [ `String "reading"; `String "gaming"; `String "coding" ]
              );
            ]
        in
        check_string "primitive array" "tags[3]: reading,gaming,coding"
          (Toon.print obj));
    Testo.create "produces no trailing spaces at end of lines" (fun () ->
        let obj =
          `Assoc
            [
              ("user", `Assoc [ ("id", `Int 123); ("name", `String "Ada") ]);
              ("items", `List [ `String "a"; `String "b" ]);
            ]
        in
        let result = Toon.print obj in
        let lines = String.split_on_char '\n' result in
        List.iter
          (fun line ->
            Alcotest.(check bool)
              "no trailing space" true
              (not (String.ends_with ~suffix:" " line)))
          lines);
    Testo.create "produces no trailing newline at end of output" (fun () ->
        let obj = `Assoc [ ("id", `Int 123) ] in
        let result = Toon.print obj in
        Alcotest.(check bool)
          "no trailing newline" true
          (not (String.ends_with ~suffix:"\n" result)));
  ]
