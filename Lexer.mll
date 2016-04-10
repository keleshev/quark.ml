{
  open Parser

  let parse_identifier = function
    | "package"   -> PACKAGE
    | "namespace" -> NAMESPACE
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
    | other       -> ID other
}

let whitespace = ' ' | '\t' | '\n' | '\r'
let identifier = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let numeric = ['1'-'9'] ['0'-'9']* | '0' (* no leading zeroes *)
(* TODO leading zeroes inside sections are not allowed either *)
let section = ['a'-'z' 'A'-'Z' '0'-'9' '_']+
let pre_release = section ('.' section)*
let build_metadata = section ('.' section)*
let version =
  numeric '.' numeric '.' numeric ('-' pre_release)? ('+' build_metadata)?

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
  | version as string { VERSION string }
  | '"' (_* as string) '"' { STRING string }
  | ['0'-'9']* as number { NUMBER (int_of_string number) }
  | identifier as id { parse_identifier id }
  | eof { EOF }
