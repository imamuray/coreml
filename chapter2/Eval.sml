structure Eval =
struct
  open TM
  fun Hd nil = B
    | Hd (h :: _) = h

  fun Tl nil = nil
    | Tl (_ :: tl) = tl
  
  fun Cons (B, nil) = nil
    | Cons (h, t) = h :: t

  fun moveL (tapeL, h, tapeR) =
      (Tl tapeL, Hd tapeL, Cons (h, tapeR))

  fun moveR (tapeL, h, tapeR) =
      (Cons (h, tapeL), Hd tapeR, Tl tapeR)

  fun move L tape = moveL tape
    | move R tape = moveR tape
  
  fun exec delta (currentQ, tape as (tapeL, headS, tapeR)) =
    case List.find (fn (x, _) => x = (currentQ, headS)) delta of
      NONE => tape
    | SOME (_, (nextQ, updateS, direction)) =>
      exec delta (nextQ, move direction (tapeL, updateS, tapeR))
  
  fun eval (state, delta) tape = exec delta (state, tape)
end
