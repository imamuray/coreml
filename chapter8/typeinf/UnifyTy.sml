structure UnifyTy =
struct
  exception UnifyTy
  fun FTV ty =
    let
      fun scan ty set = case ty of
          Type.TYVARty string => SSet.add (set, string)
        | Type.FUNty (domTy, ranTy) => scan ranTy (scan domTy set)
        | Type.PAIRty (fstTy, sndTy) => scan sndTy (scan fstTy set)
        (* TODO ほかのTypeの処理 *)
        | _ => set
    in
      scan ty SSet.empty
    end
  fun occurs (Type.TYVARty string, ty) = SSet.member(FTV ty, string)
    | occurs _ = false
  fun rewrite (nil, S) = S
    | rewrite ((ty1, ty2)::E, S) =
        if ty1 = ty2 then rewrite (E, S) else
        case (ty1, ty2) of
            (Type.TYVARty tv, _) =>
              if occurs (ty1, ty2) then raise UnifyTy else
              let
                val S1 = SEnv.singleton(tv, ty2)
              in
                rewrite (map (fn (ty1, ty2) => (TypeUtils.substTy S1 ty1, TypeUtils.substTy S1 ty2))
                              E, TypeUtils.composeSubst S1 S)
              end
          | (_, Type.TYVARty tv) => rewrite ((ty2, ty1)::E, S)
          | (Type.FUNty (ty11, ty12), Type.FUNty (ty21, ty22)) =>
            rewrite ((ty11, ty21)::(ty12, ty22)::E, S)
          | (Type.PAIRty (ty11, ty12), Type.PAIRty (ty21, ty22)) =>
            rewrite ((ty11, ty21)::(ty12, ty22)::E, S)
          | _ => raise UnifyTy
  fun unify E = rewrite (E, SEnv.empty)
end
