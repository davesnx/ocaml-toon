let test_deep_nesting () =
  Printf.printf "Testing deep nesting (10,000 levels)...\n%!";

  let rec build_deep n acc =
    if n = 0 then
      acc
    else
      build_deep (n - 1) (`Assoc [ ("level", acc) ])
  in

  let deep = build_deep 10000 (`String "bottom") in

  let start = Unix.gettimeofday () in
  let toon = Toon.print deep in
  let print_time = Unix.gettimeofday () -. start in

  Printf.printf "  Print: %.4fs (%d bytes)\n%!" print_time (String.length toon);

  let start = Unix.gettimeofday () in
  match Toon.parse toon with
  | Ok parsed ->
      let parse_time = Unix.gettimeofday () -. start in
      Printf.printf "  Parse: %.4fs\n%!" parse_time;

      if deep = parsed then
        Printf.printf "  ✓ Deep nesting roundtrip successful\n\n%!"
      else
        Printf.printf "  ✗ Deep nesting roundtrip FAILED\n\n%!"
  | Error err ->
      let msg =
        match err with
        | `Unterminated_quoted_string -> "Unterminated quoted string"
        | `Expected_quote -> "Expected quote"
        | `Invalid_escape_sequence -> "Invalid escape sequence"
        | `No_colon_in_line line -> "No colon in line: " ^ line
        | `Invalid_array_syntax -> "Invalid array syntax"
        | `Array_length_mismatch -> "Array length mismatch"
        | `Invalid_number_format -> "Invalid number format"
      in
      Printf.printf "  ✗ Parse error: %s\n\n%!" msg

let test_wide_arrays () =
  Printf.printf "Testing wide arrays (100,000 elements)...\n%!";

  let large_array = List.init 100000 (fun i -> `Int i) in
  let json = `Assoc [ ("numbers", `List large_array) ] in

  let start = Unix.gettimeofday () in
  let toon = Toon.print json in
  let print_time = Unix.gettimeofday () -. start in

  Printf.printf "  Print: %.4fs (%d bytes)\n%!" print_time (String.length toon);

  let start = Unix.gettimeofday () in
  match Toon.parse toon with
  | Ok parsed ->
      let parse_time = Unix.gettimeofday () -. start in
      Printf.printf "  Parse: %.4fs\n%!" parse_time;

      if json = parsed then
        Printf.printf "  ✓ Wide array roundtrip successful\n\n%!"
      else
        Printf.printf "  ✗ Wide array roundtrip FAILED\n\n%!"
  | Error err ->
      let msg =
        match err with
        | `Unterminated_quoted_string -> "Unterminated quoted string"
        | `Expected_quote -> "Expected quote"
        | `Invalid_escape_sequence -> "Invalid escape sequence"
        | `No_colon_in_line line -> "No colon in line: " ^ line
        | `Invalid_array_syntax -> "Invalid array syntax"
        | `Array_length_mismatch -> "Array length mismatch"
        | `Invalid_number_format -> "Invalid number format"
      in
      Printf.printf "  ✗ Parse error: %s\n\n%!" msg

let test_tabular_arrays () =
  Printf.printf "Testing large tabular arrays (50,000 rows)...\n%!";

  let records =
    List.init 50000 (fun i ->
        `Assoc
          [
            ("id", `Int i);
            ("name", `String (Printf.sprintf "user_%d" i));
            ("score", `Float (float_of_int i *. 1.5));
            ("active", `Bool (i mod 2 = 0));
          ])
  in
  let json = `Assoc [ ("records", `List records) ] in

  let start = Unix.gettimeofday () in
  let toon = Toon.print json in
  let print_time = Unix.gettimeofday () -. start in

  Printf.printf "  Print: %.4fs (%d bytes)\n%!" print_time (String.length toon);

  let start = Unix.gettimeofday () in
  match Toon.parse toon with
  | Ok parsed ->
      let parse_time = Unix.gettimeofday () -. start in
      Printf.printf "  Parse: %.4fs\n%!" parse_time;

      if json = parsed then
        Printf.printf "  ✓ Tabular array roundtrip successful\n\n%!"
      else
        Printf.printf "  ✗ Tabular array roundtrip FAILED\n\n%!"
  | Error err ->
      let msg =
        match err with
        | `Unterminated_quoted_string -> "Unterminated quoted string"
        | `Expected_quote -> "Expected quote"
        | `Invalid_escape_sequence -> "Invalid escape sequence"
        | `No_colon_in_line line -> "No colon in line: " ^ line
        | `Invalid_array_syntax -> "Invalid array syntax"
        | `Array_length_mismatch -> "Array length mismatch"
        | `Invalid_number_format -> "Invalid number format"
      in
      Printf.printf "  ✗ Parse error: %s\n\n%!" msg

let test_mixed_structure () =
  Printf.printf "Testing mixed deep & wide structure...\n%!";

  let nested =
    List.init 100 (fun i ->
        `Assoc
          [
            ("id", `Int i);
            ( "data",
              `Assoc
                [
                  ( "values",
                    `List (List.init 1000 (fun j -> `Int ((i * 1000) + j))) );
                  ( "meta",
                    `Assoc
                      [
                        ("created", `String "2025-01-01");
                        ("tags", `List [ `String "test"; `String "benchmark" ]);
                      ] );
                ] );
          ])
  in
  let json = `Assoc [ ("items", `List nested) ] in

  let start = Unix.gettimeofday () in
  let toon = Toon.print json in
  let print_time = Unix.gettimeofday () -. start in

  Printf.printf "  Print: %.4fs (%d bytes)\n%!" print_time (String.length toon);

  let start = Unix.gettimeofday () in
  match Toon.parse toon with
  | Ok parsed ->
      let parse_time = Unix.gettimeofday () -. start in
      Printf.printf "  Parse: %.4fs\n%!" parse_time;

      if json = parsed then
        Printf.printf "  ✓ Mixed structure roundtrip successful\n\n%!"
      else
        Printf.printf "  ✗ Mixed structure roundtrip FAILED\n\n%!"
  | Error err ->
      let msg =
        match err with
        | `Unterminated_quoted_string -> "Unterminated quoted string"
        | `Expected_quote -> "Expected quote"
        | `Invalid_escape_sequence -> "Invalid escape sequence"
        | `No_colon_in_line line -> "No colon in line: " ^ line
        | `Invalid_array_syntax -> "Invalid array syntax"
        | `Array_length_mismatch -> "Array length mismatch"
        | `Invalid_number_format -> "Invalid number format"
      in
      Printf.printf "  ✗ Parse error: %s\n\n%!" msg

let () =
  Printf.printf "\n=== TOON Benchmark ===\n\n%!";
  test_deep_nesting ();
  test_wide_arrays ();
  test_tabular_arrays ();
  test_mixed_structure ()
