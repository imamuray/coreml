structure Top =
struct
  fun readAndPrintLoop inStream =
    let
      val _ = ReadString.skipSpaces inStream
      val s = ReadString.readString inStream
      val _ = print (s ^ "\n")
    in
      readAndPrintLoop inStream
    end
  fun top file =
    let
      val inStream = if file = "" then TextIO.stdIn else TextIO.openIn file
    in
      readAndPrintLoop inStream;
      TextIO.closeIn inStream
    end
    handle ReadString.EOF => ()
end
