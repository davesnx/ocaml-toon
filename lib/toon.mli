(** TOON (Token-Oriented Object Notation) encoder and decoder.

    Targets toon-spec: 4.1. *)

type delimiter = Comma | Tab | Pipe

type error = { line : int; message : string }
(** [line] is 1-based; 0 when not tied to a line. *)

val error_to_string : error -> string

val encode :
  ?delimiter:delimiter -> ?indent_size:int -> Yojson.Basic.t -> string

val decode :
  ?indent_size:int -> ?strict:bool -> string -> (Yojson.Basic.t, error) result

val pp : Format.formatter -> Yojson.Basic.t -> unit
