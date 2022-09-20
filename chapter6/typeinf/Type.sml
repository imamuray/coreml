structure Type =
struct
  (* ref: 参照型 *)
  (* !r: 'a ref -> 'a: rの参照する値を返す *)
  (* r := x: 'a ref * 'a -> unit: rの参照する値をxに書き換える *)
  local
    val nextTyId = ref 0
    fun newTyId () = (!nextTyId before nextTyId := !nextTyId + 1)
    (* 中値演算 before *)
    (* val x = e1 before e2; は *)
    (* val x = let val a = e1; val _ = e2 in a end; と等価 *)
  in
    fun initSeed () = nextTyId := 0
    fun newTyIdName () =
      let
        fun tyIdName tid =
          let
            fun numeral n =
              if n < 26
              then [ord #"a" + n]
              else
                let
                  val (msb, rest) = (n mod 26, (n div 26) - 1)
                in
                  (ord #"a" + msb) :: (numeral rest)
                end
          in
            (implode(map chr (rev (numeral tid))))
          end
      in
        tyIdName (newTyId())
      end
  end
  datatype ty =
      INTty
    | STRINGty
    | BOOLty
    | TYVARty of string  (* 型変数 *)
    | FUNty of ty * ty
    | PAIRty of ty * ty
  (* 呼ばれるごとに新しい型変数を返す関数, t fresh に相当 *)
  fun newTy () = TYVARty (newTyIdName())
  fun tyToString ty = case ty of
      INTty => "int"
    | STRINGty => "string"
    | BOOLty => "bool"
    | TYVARty string => "'" ^ string
    | FUNty (ty1, ty2) =>
      "(" ^ tyToString ty1 ^ " -> " ^ tyToString ty2 ^ ")"
    | PAIRty (ty1, ty2) =>
      "(" ^ tyToString ty1 ^ " * " ^ tyToString ty2 ^ ")"
end
