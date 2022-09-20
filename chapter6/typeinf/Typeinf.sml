structure Typeinf =
struct
  exception TypeError
  (* absynが型判定を持てばその主要な型判定を返す, そうでなければエラーとする *)
  (* absyn: Abstract Syntax Tree *)
  fun PTS absyn = case absyn of
      Syntax.INT int => (TypeUtils.emptyTyEnv, Type.INTty)
    | Syntax.STRING string => (TypeUtils.emptyTyEnv, Type.STRINGty)
    | Syntax.TRUE => (TypeUtils.emptyTyEnv, Type.BOOLty)
    | Syntax.FALSE => (TypeUtils.emptyTyEnv, Type.BOOLty)
    | Syntax.EXPID string =>
      let
        val newty = Type.newTy()
      in
        (TypeUtils.singletonTyEnv(string, newty), newty)
      end
    | Syntax.EXPPAIR (exp1, exp2) =>
      let
        val (tyEnv1, ty1) = PTS exp1
        val (tyEnv2, ty2) = PTS exp2
        val tyEquations = TypeUtils.matches (tyEnv1, tyEnv2)
        val subst = UnifyTy.unify tyEquations
        val tEnv3 = TypeUtils.unionTyEnv (TypeUtils.substTyEnv subst tyEnv1,
                                          TypeUtils.substTyEnv subst tyEnv2)
      in
        (tEnv3, TypeUtils.substTy subst (Type.PAIRty(ty1, ty2)))
      end
    | Syntax.EXPAPP (exp1, exp2) =>
      let
        val (tyEnv1, ty1) = PTS exp1
        val (tyEnv2, ty2) = PTS exp2
        val tyEquations = TypeUtils.matches (tyEnv1, tyEnv2)
        val newTy = Type.newTy()
        val subst = UnifyTy.unify ((Type.FUNty(ty2, newTy), ty1)::tyEquations)
        val tEnv3 = TypeUtils.unionTyEnv (TypeUtils.substTyEnv subst tyEnv1,
                                          TypeUtils.substTyEnv subst tyEnv2)
      in
        (tEnv3, TypeUtils.substTy subst newTy)
      end
    | Syntax.EXPFN (string, exp) =>
      let
        val (tyEnv, ty) = PTS exp
      in
        case TypeUtils.findTyEnv(tyEnv, string) of
            SOME domty => (TypeUtils.removeTyEnv(tyEnv, string), Type.FUNty(domty, ty))
          | NONE => (tyEnv, Type.FUNty(Type.newTy(), ty))
      end
    (* TODO ほかのSyntaxの処理 *)
    | _ => raise TypeError
  fun typeinf dec =
    let
      val exp = case dec of
          Syntax.VAL (id, exp) => exp
        | Syntax.FUN (f, x, exp) => exp
      val (tyEnv, ty) = PTS exp
      val _ = print ("Inferred Typing:\n"
                    ^ TypeUtils.tyEnvToString tyEnv
                    ^ " |- " ^ Syntax.expToString exp
                    ^ " : " ^ Type.tyToString ty
                    ^ "\n")
    in
      ()
    end
    handle UnifyTy.UnifyTy => raise TypeError
end
