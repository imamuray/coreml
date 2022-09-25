structure Comp =
struct
  structure S = Syntax
  structure I = Instruction
  exception NotImplemented
  fun comp exp codeList = case exp of
      S.INT int => I.PushI int :: codeList
    | S.STRING string => I.PushS string :: codeList
    | S.TRUE => I.PushB true :: codeList
    | S.FALSE => I.PushB false :: codeList
    | S.EXPID string => I.Acc string :: codeList
    | S.EXPFN (x, e) => I.MkCLS(x, comp e [I.Ret]) :: codeList
    | S.EXPAPP (e1, e2) => comp e1 (comp e2 (I.App :: codeList))
    | S.EXPPAIR (e1, e2) => comp e1 (comp e2 (I.Pair :: codeList))
    | S.EXPPROJ1 e => comp e (I.Proj1 :: codeList)
    | S.EXPPROJ2 e => comp e (I.Proj2 :: codeList)
    | S.EXPPRIM (prim, e1, e2) =>
      let
        val primInst = case prim of
            S.ADD => I.ADD
          | S.SUB => I.SUB
          | S.MUL => I.MUL
          | S.DIV => I.DIV
          | S.EQ => I.EQ
      in
        comp e1 (comp e2 (I.Prim primInst :: codeList))
      end
    | S.EXPFIX (f, x, e) => I.MkREC(f, x, comp e [I.Ret]) :: codeList
    | S.EXPIF (e1, e2, e3) => comp e1 (I.If(comp e2 nil, comp e3 nil) :: codeList)
    | _ => raise NotImplemented
  fun compile (S.VAL (id, exp)) =
    let
      val code = comp exp nil
    in
      print ("Compiled to:\n"
             ^ "  " ^ I.codeToString code ^ "\n");
      (id, code)
    end
end
