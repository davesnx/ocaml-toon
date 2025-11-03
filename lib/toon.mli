type error =
  [ `Unterminated_quoted_string
  | `Expected_quote
  | `Invalid_escape_sequence
  | `No_colon_in_line of string
  | `Invalid_array_syntax
  | `Array_length_mismatch
  | `Invalid_number_format ]

val error_to_string : error -> string
(** Convert an error to a string *)

val decode : string -> (Yojson.Basic.t, error) result
(** Decode a TOON format string to a Yojson value *)

val encode : Yojson.Basic.t -> string
(** Encode a Yojson value to TOON format string *)

val pp : Format.formatter -> Yojson.Basic.t -> unit
(** Pretty-print TOON format using OCaml's Format module *)
