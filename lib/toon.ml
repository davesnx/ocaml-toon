type error = Toon_parse.error

let parse = Toon_parse.parse
let print = Toon_print.print
let pp fmt (json : Yojson.Basic.t) = Format.fprintf fmt "%s" (print json)
