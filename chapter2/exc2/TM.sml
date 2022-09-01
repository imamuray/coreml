structure TM =
struct
  datatype D = R | L
  datatype S = B | I | O
  (* MoveLC はキャリーあり左移動 *)
  datatype Q = Init | MoveLC | MoveL | MoveR | H
  type delta = ((Q * S) * (Q * S * D)) list
  type program = Q * delta
  type tape = S list * S * S list
  (* 2進数の左側の空白から始まり, *)
  (* 1を加えて2進数の左側の空白で泊まるプログラム *)
  val P = (Init,
    [
      ((Init, B), (MoveR, B, R)),
      ((MoveR, I), (MoveR, I, R)),
      ((MoveR, O), (MoveR, O, R)),
      ((MoveR, B), (MoveLC, B, L)),
      ((MoveL, I), (MoveL, I, L)),
      ((MoveL, O), (MoveL, O, L)),
      ((MoveL, B), (H, O, L)),
      ((MoveLC, I), (MoveLC, O, L)),
      ((MoveLC, O), (MoveL, I, L)),
      ((MoveLC, B), (H, I, L))
    ]
  )
end
