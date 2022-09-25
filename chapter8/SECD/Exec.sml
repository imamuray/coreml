structure Exec =
struct
  structure I = Instruction
  structure V = Value
  exception RuntimeError
  fun exec (v::S, _, nil, nil) = v
    | exec (S, E, I.Acc string :: C, D) =
      (case SEnv.find(E, string) of
          SOME v => exec (v::S, E, C, D)
        | NONE => raise RuntimeError)
    | exec (S, E, I.PushI int :: C, D) = exec (V.INT int :: S, E, C, D)
    | exec (S, E, I.PushS string :: C, D) = exec (V.STRING string :: S, E, C, D)
    | exec (S, E, I.PushB bool :: C, D) = exec (V.BOOL bool :: S, E, C, D)
    | exec (S, E, I.MkCLS(x, C0) :: C, D) = exec (V.CLS(E, x, C0) :: S, E, C, D)
    | exec (S, E, I.MkREC(f, x, C0) :: C, D) = exec (V.REC(E, f, x, C0) :: S, E, C, D)
    | exec (v :: V.CLS(E0, x, C0) :: S, E, I.App :: C, D) =
        exec (S, SEnv.insert(E0, x, v), C0, (C, E) :: D)
    | exec (v1 :: (v2 as V.REC(E0, f, x, C0)) :: S, E, I.App :: C, D) =
        exec (S, SEnv.insert((SEnv.insert(E0, f, v2)), x, v1), C0, (C, E) :: D)
    | exec (v1 :: v2 :: S, E, I.Pair :: C, D) = exec (V.PAIR(v2, v1) :: S, E, C, D)
    | exec (V.PAIR(v1, v2) :: S, E, I.Proj1 :: C, D) = exec (v1 :: S, E, C, D)
    | exec (V.PAIR(v1, v2) :: S, E, I.Proj2 :: C, D) = exec (v2 :: S, E, C, D)
    | exec (V.INT i1 :: V.INT i2 :: S, E, I.Prim p :: C, D) =
      (case p of
          I.ADD => exec (V.INT(i2 + i1) :: S, E, C, D)
        | I.SUB => exec (V.INT(i2 - i1) :: S, E, C, D)
        | I.MUL => exec (V.INT(i2 * i1) :: S, E, C, D)
        | I.DIV => exec (V.INT(i2 div i1) :: S, E, C, D)
        | I.EQ => exec (V.BOOL(i2 = i1) :: S, E, C, D))
    | exec (V.BOOL true :: S, E, I.If(C1, C2) :: C, D) = exec (S, E, C1 @ C, D)
    | exec (V.BOOL false :: S, E, I.If(C1, C2) :: C, D) = exec (S, E, C2 @ C, D)
    | exec (v :: S, _, I.Ret :: C, (C0, E0) :: D) = exec (v :: S, E0, C0, D)
    | exec _ = raise RuntimeError
  fun run env (id, code) =
    let
      val value = exec (nil, env, code, nil)
      val newEnv = SEnv.insert(env, id, value)
    in
      print ("Execution result:\n"
             ^ "  val " ^ id ^ " = " ^ V.valueToString value ^ "\n");
      newEnv
    end
end
