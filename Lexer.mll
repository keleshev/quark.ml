{
  open Parser

  let parse_identifier = function
    | "package"   -> PACKAGE
    | "class"     -> CLASS
    | "interface" -> INTERFACE
    | "primitive" -> PRIMITIVE
    | "extends"   -> EXTENDS
    | "return"    -> RETURN
    | "macro"     -> MACRO
    | "new"       -> NEW
    | "null"      -> NULL
    | "if"        -> IF
    | "else"      -> ELSE
    | "while"     -> WHILE
    | "var"       -> VAR
    | other       -> ID other
}

let whitespace = ' ' | '\t' | '\n' | '\r'
let identifier = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read = parse
  | whitespace { read lexbuf }
  | "{"  { LBR     }
  | "}"  { RBR     }
  | "["  { LBK     }
  | "]"  { RBK     }
  | "("  { LPR     }
  | ")"  { RPR     }
  | ":"  { COLON   }
  | ","  { COMMA   }
  | ";"  { SEMI    }
  | "="  { EQ      }
  | "+"  { PLUS    }
  | "-"  { MINUS   }
  | "!"  { NOT     }
  | "~"  { TWIDDLE }
  | "."  { DOT     }
  | "*"  { MUL     }
  | "/"  { DIV     }
  | ">=" { GE      }
  | "<=" { LE      }
  | ">"  { GT      }
  | "<"  { LT      }
  | "==" { EQL     }
  | "!=" { NEQ     }
  | "&&" { AND     }
  | "||" { OR      }
  | "@"  { AT      }
  | '"' (_* as string) '"' { STRING string }
  | ['0'-'9']* as number { NUMBER (int_of_string number) }
  | identifier as id { parse_identifier id }
  | eof { EOF }
