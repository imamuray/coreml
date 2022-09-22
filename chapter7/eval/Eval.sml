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
    | S.EXPFN (string, exp) => V.CLS(env, string, exp)
    | S.EXPPAIR (exp1, exp2) =>
      let
        val value1 = evalExp env exp1
        val value2 = evalExp env exp2
      in
        V.PAIR(value1, value2)
      end
    | S.EXPAPP (exp1, exp2) =>
      let
        val value1 = evalExp env exp1
        val value2 = evalExp env exp2
      in
        case value1 of
            V.CLS(env1, x, exp1) =>
              evalExp (SEnv.insert(env1, x, value2)) exp1
          | V.REC(env1, f, x, exp1) =>
              evalExp (SEnv.insert(SEnv.insert(env1, f, value1), x, value2)) exp1
          | _ => raise RuntimeError
      end
    | S.EXPPROJ1 exp =>
      let
        val value = evalExp env exp
      in
        case value of
            V.PAIR(value1, value2) => value1
          | _ => raise RuntimeError 
      end
    | S.EXPPROJ2 exp =>
      let
        val value = evalExp env exp
      in
        case value of
            V.PAIR(value1, value2) => value2
          | _ => raise RuntimeError 
      end
    | S.EXPIF (exp1, exp2, exp3) =>
      let
        val value = evalExp env exp1
      in
        case value of
            V.BOOL true => evalExp env exp2
          | V.BOOL false => evalExp env exp3
          | _ => raise RuntimeError
      end
    | S.EXPPRIM (prim, exp1, exp2) =>
      let
        val value1 = evalExp env exp1
        val value2 = evalExp env exp2
        val arg = case (value1, value2) of
            (V.INT int1, V.INT int2) => (int1, int2)
          | _ => raise RuntimeError
      in
        case prim of
            S.ADD => V.INT (op + arg)
          | S.SUB => V.INT (op - arg)
          | S.MUL => V.INT (op * arg)
          | S.DIV => V.INT (op div arg)
          | S.EQ => V.BOOL (op = arg)
      end
    | S.EXPPRIM1 (prim, exp) =>
      let
        val value = evalExp env exp
        val arg = case value of
            V.STRING string => string
          | _ => raise RuntimeError
      in
        case prim of
            S.PRINT =>
              print (arg ^ "\n");
              V.INT (size arg)
      end
    | S.EXPFIX (string1, string2, exp) => V.REC(env, string1, string2, exp)
  fun eval env (S.VAL (id, exp)) =
    let
      val value = evalExp env exp
    in
      print ("Evaluated to:\n"
             ^ "val " ^ id ^ " = " ^ V.valueToString value ^ "\n");
      SEnv.insert(env, id, value)
    end
end