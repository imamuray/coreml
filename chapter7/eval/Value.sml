structure Value =
struct
  datatype value =
      INT of int
    | BOOL of bool
    | STRING of string
    | PAIR of value * value
    | CLS of env * string * Syntax.exp
    | REC of env * string * string * Syntax.exp
  withtype env = value SEnv.map
  val emptyEnv = SEnv.empty
  fun valueToString value = case value of
      INT int => Int.toString int 
    | BOOL bool => Bool.toString bool 
    | STRING string => "\"" ^ string ^ "\""
    | PAIR (v1, v2) =>
      "(" ^ valueToString v1 ^ "," ^ valueToString v2 ^ ")"
    | CLS (env, x, exp) => "fn"
    | REC (env, f, x, exp) => "fix"
end
