structure Syntax =
struct
  datatype prim = EQ | ADD | SUB | MUL | DIV
  datatype exp =
      EXPID of string
    | INT of int
    | STRING of string
    | TRUE
    | FALSE
    | EXPFN of string * exp
    | EXPAPP of exp * exp
    | EXPPAIR of exp * exp
    | EXPPROJ1 of exp
    | EXPPROJ2 of exp
    | EXPPRIM of prim * exp * exp
    | EXPIF of exp * exp * exp
    | EXPFIX of string * string * exp
  and dec =
      VAL of string * exp
  fun expToString exp =
    case exp of
       EXPID id => id
    | INT int => Int.toString int
    | STRING string => "\"" ^ string ^ "\""
    | TRUE => "true"
    | FALSE => "false"
    | EXPFN (id, exp) => "(fn " ^ id ^ " => " ^ expToString exp ^ ")"
    | EXPAPP (exp1, exp2) => "(" ^ expToString exp1 ^ " " ^ expToString exp2 ^ ")"
    | EXPPAIR (exp1, exp2) => "(" ^ expToString exp1 ^ ", " ^ expToString exp2 ^ ")"
    | EXPPROJ1 exp => "#1 " ^ expToString exp
    | EXPPROJ2 exp => "#2 " ^ expToString exp
    | EXPPRIM (p, exp1, exp2) =>
      let
        val prim = case p of
           EQ => "eq"
         | ADD => "add"
         | SUB => "sub"
         | MUL => "mul"
         | DIV => "div"
      in
        "prim(" ^ prim ^ ", " ^ expToString exp1 ^ ", " ^ expToString exp2  ^ ")"
      end
    | EXPIF (exp1, exp2, exp3) =>
      "if " ^ expToString exp1 ^ " then " ^ expToString exp2 ^ " else " ^ expToString exp3
    | EXPFIX (f, x, exp) =>
      "(fix " ^ f ^ " (" ^ x ^ ") => " ^ expToString exp ^ ")"
  fun decToString dec =
    case dec of
       VAL (id, exp) => "val " ^ id ^ " = " ^ expToString exp
end
