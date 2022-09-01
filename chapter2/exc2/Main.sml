open TM
val testcases = [
  (nil, B, [O]),
  (nil, B, [I]),
  (nil, B, [I, O]),
  (nil, B, [I, I]),
  (nil, B, [I, O, O]),
  (nil, B, [I, O, I]),
  (nil, B, [I, I, O]),
  (nil, B, [I, I, I])
];
fun results nil = nil
  | results (testcase :: tl) =
    let
      val r = Eval.eval P testcase
      val _ = Dynamic.pp {Test = testcase, result = r}
    in
      results tl
    end
val _ = results testcases;
