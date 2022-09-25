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
    | exec _ = raise RuntimeError
  fun run env (id, code) =
    let
      val value = exec (nil, env, code, nil)
      val newEnv = SEnv.insert(env, id, value)
    in
      print ("Execution result:\n"
             ^ "val " ^ id ^ " = " ^ V.valueToString value ^ "\n");
      newEnv
    end
end
