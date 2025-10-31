(lang dune 3.21)

(name toon)

(generate_opam_files true)

(source
 (github davesnx/ocaml-toon))

(authors "David Sancho <dsnxmoreno@gmail.com>")

(maintainers "David Sancho <dsnxmoreno@gmail.com>")

(license MIT)

(documentation https://github.com/davesnx/ocaml-toon)

(package
 (name toon)
 (synopsis "TOON (Token-Oriented Object Notation) parser and printer library")
 (description "OCaml implementation of TOON format - a token-efficient serialization format designed for LLM prompts, providing JSON-like functionality at half the token count through tabular arrays and optimized syntax.")
 (depends
  ocaml
  dune
  yojson
  (testo :with-test)
  (alcotest :with-test))
 (tags
  (serialization tokenization llm yojson json)))
