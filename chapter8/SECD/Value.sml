structure Value =
struct
  datatype value =
      INT of int
    | BOOL of bool
    | STRING of string
    | PAIR of value * value
    | CLS of E * string * Instruction.C
    | REC of E * string * string * Instruction.C
  withtype E = value SEnv.map
  val emptyEnv = SEnv.empty
  fun valueToString value = case value of
      INT int => Int.toString int
    | BOOL bool => Bool.toString bool
    | STRING string => "\"" ^ string ^ "\""
    | PAIR (v1, v2) => "(" ^ valueToString v1 ^ "," ^ valueToString v2 ^ ")"
    | CLS (_, _, _) => "fn"
    | REC (_, _, _, _) => "fix"
end

