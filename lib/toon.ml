type error = Toon_decode.error
type delimiter = Toon_encode.delimiter = Comma | Tab | Pipe

let decode = Toon_decode.decode
let error_to_string = Toon_decode.error_to_string
let encode = Toon_encode.encode
let pp fmt (json : Yojson.Basic.t) = Format.fprintf fmt "%s" (encode json)
