type error =
  [ `Unterminated_quoted_string
  | `Expected_quote
  | `Invalid_escape_sequence
  | `No_colon_in_line of string
  | `Invalid_array_syntax
  | `Array_length_mismatch
  | `Invalid_number_format ]

type delimiter = Comma | Tab | Pipe

val error_to_string : error -> string

val decode : string -> (Yojson.Basic.t, error) result

val encode : ?delimiter:delimiter -> Yojson.Basic.t -> string

val pp : Format.formatter -> Yojson.Basic.t -> unit
