structure Eval =
struct
  structure S = Syntax
  structure V = Value
  exception RuntimeError
  fun evalExp env exp = case exp of
      S.INT int => V.INT int
    | S.STRING string => V.STRING string
    | S.TRUE => V.BOOL true
    | S.FALSE => V.BOOL false
    | S.EXPID string =>
      (case SEnv.find(env, string) of
         SOME value => value
       | NONE => raise RuntimeError)
    | _ => raise RuntimeError
  fun eval env (S.VAL (id, exp)) =
    let
      val value = evalExp env exp
    in
      print ("Evaluated to:\n"
             ^ "val " ^ id ^ " = " ^ V.valueToString value ^ "\n");
      SEnv.insert(env, id, value)
    end
end