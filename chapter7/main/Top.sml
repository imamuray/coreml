structure Top =
struct
  exception NotImplemented
  (* 6.4のPTSを使用した処理 *)
  fun readAndPrintLoop_PTS stream =
    let
      val (dec, stream) = Parser.doParse stream
      val _ = Typeinf.typeinf_PTS dec
    in
      readAndPrintLoop_PTS stream
    end
  (* 6.5のWを使用した処理 *)
  fun readAndPrintLoop gamma stream =
    let
      val (dec, stream) = Parser.doParse stream
      val newGamma = Typeinf.typeinf gamma dec
    in
      readAndPrintLoop newGamma stream
    end 
  fun top file =
    let
      val inStream = case file of
         "" => TextIO.stdIn
       | _ => TextIO.openIn file
      val stream = Parser.makeStream inStream
      val gamma = TypeUtils.emptyTyEnv
    in
      (* readAndPrintLoop_PTS stream *)
      readAndPrintLoop gamma stream
      handle Parser.EOF => ()
           | Parser.ParseError => print "Syntax error\n"
           | Typeinf.TypeError => print "Type error\n";
      case file of
         "" => ()
       | _ => TextIO.closeIn inStream
    end
end