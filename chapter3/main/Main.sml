val _ = case CommandLine.arguments () of
   h::_ => Top.top h
 | nil => Top.readAndPrintLoop TextIO.stdIn
