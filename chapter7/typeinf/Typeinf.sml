structure Typeinf =
struct
  exception TypeError
  infixr ++
  fun s1 ++ s2 = TypeUtils.composeSubst s1 s2
  fun W gamma exp = case exp of
      Syntax.INT int => (TypeUtils.emptyTyEnv, Type.INTty)
    | Syntax.STRING string => (TypeUtils.emptyTyEnv, Type.STRINGty)
    | Syntax.TRUE => (TypeUtils.emptyTyEnv, Type.BOOLty)
    | Syntax.FALSE => (TypeUtils.emptyTyEnv, Type.BOOLty)
    | Syntax.EXPID string =>
      (case SEnv.find(gamma, string) of
          SOME ty => (TypeUtils.emptySubst, TypeUtils.freshInst ty)
        | NONE => raise TypeError)
    | Syntax.EXPPAIR (exp1, exp2) =>
      let
        val (subst1, ty1) = W gamma exp1
        val (subst2, ty2) = W (TypeUtils.substTyEnv subst1 gamma) exp2
      in
        (subst2 ++ subst1, Type.PAIRty(TypeUtils.substTy subst2 ty1, ty2))
      end
    | Syntax.EXPAPP (exp1, exp2) =>
      let
        val (subst1, ty1) = W gamma exp1
        val (subst2, ty2) = W (TypeUtils.substTyEnv subst1 gamma) exp2
        val newTy = Type.newTy()
        val subst3 = UnifyTy.unify [(Type.FUNty(ty2, newTy), TypeUtils.substTy subst2 ty1)]
      in
        (subst3 ++ subst2 ++ subst1, TypeUtils.substTy subst3 newTy)
      end
    | Syntax.EXPFN (string, exp) =>
      let
        val newTy = Type.newTy()
        val newGamma = SEnv.insert(gamma, string, newTy)
        val (subst, ty) = W newGamma exp
      in
        (subst, Type.FUNty(TypeUtils.substTy subst newTy, ty))
      end
    | Syntax.EXPPROJ1 (exp) =>
      let
        val (subst1, ty) = W gamma exp
        val (newTy1, newTy2) = (Type.newTy(), Type.newTy())
        val subst2 = UnifyTy.unify [(ty, Type.PAIRty(newTy1, newTy2))]
      in
        (subst2 ++ subst1, TypeUtils.substTy subst2 newTy1)
      end
    | Syntax.EXPPROJ2 (exp) =>
      let
        val (subst1, ty) = W gamma exp
        val (newTy1, newTy2) = (Type.newTy(), Type.newTy())
        val subst2 = UnifyTy.unify [(ty, Type.PAIRty(newTy1, newTy2))]
      in
        (subst2 ++ subst1, TypeUtils.substTy subst2 newTy2)
      end
    | Syntax.EXPIF (exp1, exp2, exp3) =>
      let
        val (subst1, ty1) = W gamma exp1
        val (subst2, ty2) = W (TypeUtils.substTyEnv subst1 gamma) exp2
        val (subst3, ty3) = W (TypeUtils.substTyEnv (subst2 ++ subst1) gamma) exp3
        val subst4 = UnifyTy.unify [(TypeUtils.substTy (subst3 ++ subst2) ty1, Type.BOOLty),
                                    (TypeUtils.substTy subst3 ty2, ty3)]
      in
        (subst4 ++ subst3 ++ subst2 ++ subst1, TypeUtils.substTy subst4 ty3)
      end
    | Syntax.EXPPRIM (prim, exp1, exp2) =>
      let
        val primTy = case prim of
            Syntax.EQ => Type.BOOLty
          | _ =>Type.INTty 
        val (subst1, ty1) = W gamma exp1
        val (subst2, ty2) = W (TypeUtils.substTyEnv subst1 gamma) exp2
        val subst3 = UnifyTy.unify [(TypeUtils.substTy subst2 ty1, Type.INTty),
                                    (ty2, Type.INTty)]
      in
        (subst3 ++ subst2 ++ subst1, primTy)
      end
  fun typeinf gamma dec =
    case dec of
        Syntax.VAL (id, exp) =>
          let
            val (subst, ty) = W gamma exp
            val tids = SSet.listItems (UnifyTy.FTV ty)
            val newTy = if null tids then ty else Type.POLYty (tids, ty)
            val _ = print ("Inferred Typing:\n"
                          ^ "val " ^ id ^ " : "
                          ^ Type.tyToString newTy ^ "\n")
          in
            SEnv.insert(gamma, id, newTy)
          end
      (* 第6章の dec は VAL のみ対応, FUN は第7章で実装 *)
      | _ => raise TypeError
    handle UnifyTy.UnifyTy => raise TypeError
end
