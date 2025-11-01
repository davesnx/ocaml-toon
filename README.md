# TOON for OCaml

**Token-Oriented Object Notation** is a compact, human-readable format designed for passing structured data to Large Language Models with significantly reduced token usage.

This is an OCaml port of the [TOON library](https://github.com/johannschopplich/toon) originally written in TypeScript.

TOON excels at **uniform complex objects** – multiple fields per row, same structure across items. It borrows YAML's indentation-based structure for nested objects and CSV's tabular format for uniform data rows, then optimizes both for token efficiency in LLM contexts.

## Why TOON?

AI is becoming cheaper and more accessible, but larger context windows allow for larger data inputs as well. **LLM tokens still cost money** – and standard JSON is verbose and token-expensive:

```json
{
  "users": [
    { "id": 1, "name": "Alice", "role": "admin" },
    { "id": 2, "name": "Bob", "role": "user" }
  ]
}
```

TOON conveys the same information with **fewer tokens**:

```
users[2]{id,name,role}:
  1,Alice,admin
  2,Bob,user
```

## Features

- 💸 **Token-efficient:** typically 30–60% fewer tokens than JSON
- 🤿 **LLM-friendly guardrails:** explicit lengths and field lists help models validate output
- 🍱 **Minimal syntax:** removes redundant punctuation (braces, brackets, most quotes)
- 📐 **Indentation-based structure:** replaces braces with whitespace for better readability
- 🧺 **Tabular arrays:** declare keys once, then stream rows without repetition

> NOTE: `ocaml-toon` currently works exclusively with [yojson](https://opam.ocaml.org/packages/yojson/). Support for other JSON libraries or custom adapters could be added in the future, please open an issue.

## Installation

Currently not published to opam repository yet, it needs pinning

### opam

```bash
opam pin add toon.dev "https://github.com/davesnx/ocaml-toon.git"
```

### dune preview

Pin the package in your `dune-project` and run `dune pkg lock` and `dune build`

```lisp
(pin
  (url "git+https://github.com/davesnx/ocaml-toon.git")
  (package (name toon)))

(package
  (name your-package)
  (depends
    toon))
```

## Quick Start

```ocaml
open Yojson.Basic.Util

let data = `Assoc [
  ("user", `Assoc [
    ("id", `Int 123);
    ("name", `String "Ada");
    ("tags", `List [`String "reading"; `String "gaming"]);
    ("active", `Bool true);
    ("preferences", `List [])
  ])
]

let () = print_endline (Toon.print data)
(* Output: *)
(*
user:
  id: 123
  name: Ada
  tags[2]: reading,gaming
  active: true
  preferences[0]:
*)
```

You can also decode TOON back to JSON values:

```ocaml
let toon = {|user:
  id: 123
  name: Ada
  tags[2]: reading,gaming
  active: true
  preferences[0]:|}

let () =
  match Toon.parse toon with
  | Ok (value: Yojson.Basic.t) ->
      Printf.printf "%s\n" (Yojson.Basic.to_string value)
  | Error err ->
      Printf.eprintf "Parse error: %s\n" (Toon.error_to_string err)
```

## API

### `Toon.parse : string -> (Yojson.Basic.t, Toon.error) result`

Parses a TOON-formatted string into a Yojson value. Returns `Ok value` on success or `Error err` on parse failure.

```ocaml
type error =
  [ `Unterminated_quoted_string
  | `Expected_quote
  | `Invalid_escape_sequence
  | `No_colon_in_line of string
  | `Invalid_array_syntax
  | `Array_length_mismatch
  | `Invalid_number_format ]
```

```ocaml
match Toon.parse "tags[3]: a,b,c" with
| Ok json -> Printf.printf "%s\n" (Yojson.Basic.to_string json)
| Error err -> Printf.eprintf "Error: %s\n" (Toon.error_to_string err)
```

### `Toon.print : Yojson.Basic.t -> string`

Converts a Yojson value to TOON format. Returns a TOON-formatted string with no trailing newline or spaces.


```ocaml
Toon.print (`Assoc [("id", `Int 1); ("name", `String "Ada")])
(* => "id: 1\nname: Ada" *)
```

### `Toon.pp : Format.formatter -> Yojson.Basic.t -> unit`

Pretty-print TOON format using OCaml's Format module.

```ocaml
let data = `Assoc [("id", `Int 123); ("name", `String "Ada")]
let s = Format.asprintf "%a" Toon.pp data
```

### `Toon.error_to_string : error -> string`

Convert a parse error to a human-readable string.

```ocaml
match Toon.parse "invalid[" with
| Ok _ -> ()
| Error err ->
    Printf.eprintf "Parse failed: %s\n" (Toon.error_to_string err)
```

## Canonical Formatting Rules

TOON formatting is deterministic and minimal:

- **Indentation**: 2 spaces per nesting level.
- **Lines**:
  - `key: value` for primitives (single space after colon).
  - `key:` for nested/empty objects (no trailing space on that line).
- **Arrays**:
  - Delimiter encoding: Comma delimiters are implicit in array headers (e.g., `tags[3]:`, `items[2]{id,name}:`). Tab and pipe delimiters are explicitly shown in array headers (e.g., `tags[3|]:`, `items[2	]{id	name}:`).
  - Primitive arrays inline: `key[N]: v1,v2` (comma) or `key[N<delim>]: v1<delim>v2` (tab/pipe).
  - Tabular arrays: `key[N]{f1,f2}: …` (comma) or `key[N<delim>]{f1<delim>f2}: …` (tab/pipe).
  - List items: two spaces, hyphen, space (`"  - …"`).
- **Whitespace invariants**:
  - No trailing spaces at end of any line.
  - No trailing newline at end of output.

## Format Overview

### Objects

Simple objects with primitive values:

```ocaml
Toon.print (`Assoc [
  ("id", `Int 123);
  ("name", `String "Ada");
  ("active", `Bool true)
])
```

```
id: 123
name: Ada
active: true
```

Nested objects:

```ocaml
Toon.print (`Assoc [
  ("user", `Assoc [
    ("id", `Int 123);
    ("name", `String "Ada")
  ])
])
```

```
user:
  id: 123
  name: Ada
```

### Arrays

> **Tip:** TOON includes the array length in brackets (e.g., `items[3]`). When using comma delimiters (default), the delimiter is implicit. When using tab or pipe delimiters, the delimiter is explicitly shown in the header (e.g., `tags[2|]` or `[2	]`). This encoding helps LLMs identify the delimiter and track the number of elements, reducing errors when generating or validating structured output.

#### Primitive Arrays (Inline)

```ocaml
Toon.print (`Assoc [
  ("tags", `List [`String "admin"; `String "ops"; `String "dev"])
])
```

```
tags[3]: admin,ops,dev
```

#### Arrays of Objects (Tabular)

When all objects share the same primitive fields, TOON uses an efficient **tabular format**:

```ocaml
Toon.print (`Assoc [
  ("items", `List [
    `Assoc [
      ("sku", `String "A1");
      ("qty", `Int 2);
      ("price", `Float 9.99)
    ];
    `Assoc [
      ("sku", `String "B2");
      ("qty", `Int 1);
      ("price", `Float 14.5)
    ]
  ])
])
```

```
items[2]{sku,qty,price}:
  A1,2,9.99
  B2,1,14.5
```

## Development

After checking out the repo, ensure you have `dune preview` installed (https://preview.dune.build) and install dependencies:

```bash
make install # install dependencies

make build # build the project
make test # run the tests
make test-watch # run the tests in watch
make bench # run some benchmarks
make utop # run a repl with the lib loadede
```

## Contributing

1. Fork it (<https://github.com/davesnx/ocaml-toon/fork>)
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create a new Pull Request

## License

The project is available as open source under the terms of the [MIT License](LICENSE).

## Credits

This is an OCaml port of the original [TOON library](https://github.com/johannschopplich/toon) by [Johann Schopplich](https://github.com/johannschopplich).
