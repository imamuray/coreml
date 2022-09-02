structure Token =
struct
  datatype token =
      EOF
    | ANDALSO
    | AND
    | AS
    | CASE
    | DO
    | END
    | EXCEPTION
    | FN
    | FUN
    | HANDLE
    | IF
    | IN
    | INFIX
    | INFIXR
    | NONFIX
    | LET
    | LOCAL
    | OF
    | OP
    | OPEN
    | ORELSE
    | RAISE
    | REC
    | THEN
    | USE
    | VAL
    | WHILE
    | ARROW
    | COLON
    | COMMA
    | DOT
    | DOTS
    | EQ
    | LBRACKET
    | RBRACKET
    | SEMICOLON
    | UNDERBAR
    | VERTICALBAR
    | ID of string
    | STRING of string
    | REAL of string
    | INT of string
    | SPECIAL of string
  fun toString token =
    case token of
       EOF => "EOF"
     | ANDALSO => "ANDALSO"
     | AND => "AND"
     | AS => "AS" 
     | CASE => "CASE"
     | DO => "DO"
     | END => "END"
     | EXCEPTION => "EXCEPTION"
     | FN => "FN"
     | FUN => "FUN"
     | HANDLE => "HANDLE"
     | IF => "IF"
     | IN => "IN"
     | INFIX => "INFIX"
     | INFIXR => "INFIXR"
     | NONFIX => "NONFIX"
     | LET => "LET"
     | LOCAL => "LOCAL"
     | OF => "OF"
     | OP => "OP"
     | OPEN => "OPEN"
     | ORELSE => "ORELSE"
     | RAISE => "RAISE"
     | REC => "REC"
     | THEN => "THEN"
     | USE => "USE"
     | VAL => "VAL"
     | WHILE => "WHILE"
     | ARROW => "ARROW"
     | COLON => "COLON"
     | COMMA => "COMMA"
     | DOT => "DOT"
     | DOTS => "DOTS"
     | EQ => "EQ"
     | LBRACKET => "LBRACKET"
     | RBRACKET => "RBRACKET"
     | SEMICOLON => "SEMICOLON"
     | UNDERBAR => "UNDERBAR"
     | VERTICALBAR => "VERTICALBAR"
     | ID s => "ID " ^ s
     | STRING s => "STRING " ^ "\"" ^ s ^ "\""
     | REAL s => "REAL " ^ s
     | INT s => "INT " ^ s
     | SPECIAL s => "SPECIAL " ^ s
end
