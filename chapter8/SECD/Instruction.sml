structure Instruction =
struct
  datatype prim = EQ | ADD | SUB | MUL | DIV
  datatype inst =
      PushI of int
    | PushS of string
    | PushB of bool
    | Acc of string
    | App
    | Pair
    | Proj1
    | Proj2
    | Prim of prim
    | MkCLS of string * inst list
    | MkREC of string * string * inst list
    | If of inst list * inst list
    | Ret
  type C = inst list
  fun codeToString code = Dynamic.format code
  fun instToString inst = Dynamic.format inst
end
