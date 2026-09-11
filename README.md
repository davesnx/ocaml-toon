# TOON for OCaml

**Token-Oriented Object Notation** is a compact, human-readable format designed for passing structured data to Large Language Models with significantly reduced token usage.

This is an OCaml port of the [TOON library](https://github.com/toon-format/toon) originally written in TypeScript. It targets [TOON spec version 4.1](https://github.com/toon-format/spec).

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

##### with opam

```bash
opam pin add toon.dev "https://github.com/davesnx/ocaml-toon.git"
```

##### or with dune preview

add the pin in your `dune-project` and run `dune pkg lock` and `dune build`

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
let data = Yojson.Basic.from_string {|
{
  "user": {
    "id": 123,
    "name": "Ada",
    "tags": ["reading", "gaming"],
    "active": true,
    "preferences": []
  }
}
|}

let () =
  print_endline (Toon.encode data)
(*
user:
  id: 123
  name: Ada
  tags[2]: reading,gaming
  active: true
  preferences: []
*)
```

You can also decode TOON back to a JSON value:

```ocaml
let toon = {|
user:
  id: 123
  name: Ada
  tags[2]: reading,gaming
  active: true
  preferences: []
|}

let () =
  match Toon.decode toon with
  | Ok (value : Yojson.Basic.t) ->
      Printf.printf "%s\n" (Yojson.Basic.to_string value)
  | Error error ->
      Printf.eprintf "Decode error: %s\n" (Toon.error_to_string error)
```

## API

```ocaml
type delimiter = Comma | Tab | Pipe
type error = { line : int; message : string }

val error_to_string : error -> string
val encode : ?delimiter:delimiter -> ?indent_size:int -> Yojson.Basic.t -> string
val decode : ?indent_size:int -> ?strict:bool -> string -> (Yojson.Basic.t, error) result
val pp : Format.formatter -> Yojson.Basic.t -> unit
```

- `delimiter` sets the document delimiter. Default: `Comma`.
- `indent_size` sets the number of spaces per indent level, for `encode` and `decode` alike. Default: `2`.
- `strict` turns on the checks from spec section 14: array count and width mismatches, duplicate keys, bad indentation, and malformed headers. Default: `true`. With `strict:false`, a duplicate key keeps its last value, and a count or width mismatch no longer stops the decode.
- `error.line` is the 1-based line of the problem, or `0` when the error is not tied to one line.

### `Toon.encode`

```ocaml
Toon.encode (`Assoc [ ("id", `Int 123); ("name", `String "Ada"); ("active", `Bool true) ])
(* => "id: 123\nname: Ada\nactive: true" *)

(* Pipe delimiter, for values that contain a comma *)
Toon.encode ~delimiter:Toon.Pipe
  (`Assoc [ ("tags", `List [ `String "reading"; `String "gaming"; `String "coding" ]) ])
(* => "tags[3|]: reading|gaming|coding" *)
```

Numbers follow spec section 2: an integral float encodes without a decimal point; a number keeps its plain decimal form for magnitudes from 0 up to 1e21 and down to 1e-6; outside that range the encoder may use exponent form; `-0.0` encodes as `0`; `NaN` and the infinities encode as `null`.

### `Toon.decode`

```ocaml
match Toon.decode "tags[3]: reading,gaming,coding" with
| Ok json -> Printf.printf "%s\n" (Yojson.Basic.to_string json)
| Error error -> Printf.eprintf "Error: %s\n" (Toon.error_to_string error)
(* => {"tags":["reading","gaming","coding"]} *)

match Toon.decode "tags[3]: a,b" with
| Ok _ -> ()
| Error { line; message } -> Printf.eprintf "line %d: %s\n" line message
(* declared length 3, only 2 values given: a strict-mode error *)
```

A numeric token with no fraction or exponent that fits an OCaml 63-bit `int` decodes as `Int`; every other numeric token decodes as `Float` (spec section 4).

### `Toon.pp`

Pretty-print TOON with OCaml's `Format` module.

```ocaml
let data = `Assoc [ ("id", `Int 123); ("name", `String "Ada") ]
let s = Format.asprintf "%a" Toon.pp data
```

## Canonical Formatting Rules

TOON output is deterministic:

- Indentation: `indent_size` spaces per level (default 2). No tabs.
- `key: value` for a primitive field, one space after the colon.
- `key:` for a nested or empty object, no space after the colon.
- `key: []` for an empty array. A root empty array is `[]` on its own line. The old `key[0]:` form still decodes, but the encoder no longer writes it.
- A non-empty array is inline (`key[N]: v1,v2`) or tabular (`key[N]{f1,f2}:` then one row per line).
- Comma is implicit in headers; tab and pipe are shown (`key[N\t]:`, `key[N|]:`), and carry through to field lists and rows.
- List items: two spaces, a hyphen, a space (`  - `).
- No trailing spaces on any line, and no trailing newline at the end of the output.
- No comment lines in encoder output. A full-line `#` is a decoder-only feature: it is stripped before parsing and never emitted.

## Format Overview

### Objects

```
id: 123
name: Ada
active: true
```

Nested objects indent under their key:

```
user:
  id: 123
  name: Ada
```

### Arrays

Primitive arrays are inline:

```
tags[3]: admin,ops,dev
```

Arrays of objects with the same fields use a tabular header once, then one row per item:

```
items[2]{sku,qty,price}:
  A1,2,9.99
  B2,1,14.5
```

A nested object column, uniform across all items, collapses into a field group in the header so the rows stay flat:

```
orders[2]{id,customer{name,country},total}:
  1,Ada,DK,99
  2,Bob,UK,149
```

Arrays that mix shapes, or objects with different keys, fall back to a list: one item per line, marked `- `. An object item puts its first field on the hyphen line:

```
items[2]:
  - id: 1
    name: First
  - id: 2
    name: Second
    extra: true
```

### Objects of uniform objects

An object whose values are all objects with the same keys encodes as a table too, with the entry key as the row label:

```
users[2:]{age,city}:
  alice: 30,Berlin
  bob: 25,Oslo
```

### Delimiters

Comma is the default and stays out of the header. Tab and pipe are shown in the header, so a reader always knows which one is active:

```
tags[3	]: reading	gaming	coding
tags[3|]: reading|gaming|coding
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

The test suite runs the official conformance fixtures vendored under `test/fixtures`, from the [TOON spec repository](https://github.com/toon-format/spec).

## Contributing

1. Fork it (<https://github.com/davesnx/ocaml-toon/fork>)
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create a new Pull Request

## License

The project is available as open source under the terms of the [MIT License](LICENSE).

## Credits

This is an OCaml port of the TOON format and its original TypeScript library, created by [Johann Schopplich](https://github.com/johannschopplich). The specification and reference implementation now live at [toon-format/spec](https://github.com/toon-format/spec) and [toon-format/toon](https://github.com/toon-format/toon).
