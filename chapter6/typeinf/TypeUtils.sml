structure TypeUtils =
struct
  (* 型代入 *)
  type subst = Type.ty SEnv.map
  val emptySubst = SEnv.empty
  fun substTy subst ty = case ty of
      Type.INTty => ty
    | Type.STRINGty => ty
    | Type.BOOLty => ty
    | Type.TYVARty string =>
      (case SEnv.find (subst, string) of
          NONE => ty
        | SOME ty => ty)
    | Type.FUNty (ty1, ty2) =>
      Type.FUNty (substTy subst ty1, substTy subst ty2)
    | Type.PAIRty (ty1, ty2) =>
      Type.PAIRty (substTy subst ty1, substTy subst ty2)
  fun composeSubst subst1 subst2 =
    SEnv.unionWith
      (fn (ty1, ty2) => ty1)
      (SEnv.map (substTy subst1) subst2, subst1)
  (* 型環境 *)
  type tyEnv = Type.ty SEnv.map
  val findTyEnv = SEnv.find
  fun substTyEnv subst tyEnv =
    SEnv.map (substTy subst) tyEnv
  val emptyTyEnv = SEnv.empty
  fun singletonTyEnv (tyID, ty) = SEnv.singleton (tyID, ty)
  (* tyEnv1, tyEnv2 に共通する変数の型の組みを返す *)
  fun matches (tyEnv1, tyEnv2) =
    SEnv.listItems (SEnv.intersectWith (fn x => x) (tyEnv1, tyEnv2))
  fun unionTyEnv (tyEnv1, tyEnv2) =
    SEnv.unionWith #1 (tyEnv1, tyEnv2)
  fun removeTyEnv (tyEnv, string) = #1 (SEnv.remove (tyEnv, string))
  fun tyEnvToString tyEnv =
    let
      val stringTyList = SEnv.listItemsi tyEnv
    in
      "{" ^ (String.concatWith " , "
              (map (fn (id, ty) => id ^ ":" ^ Type.tyToString ty) stringTyList))
      ^ "}"
    end
end
