{
  open Parser

  let parse_identifier = function
    | "package"   -> PACKAGE
    | "namespace" -> NAMESPACE
    | "use"       -> USE
    | "include"   -> INCLUDE
    | "import"    -> IMPORT
    | "as"        -> AS
    | "static"    -> STATIC
    | "break"     -> BREAK
    | "continue"  -> CONTINUE
    | "true"      -> TRUE
    | "false"     -> FALSE
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

let url = ['a'-'z']+ "://" (_ # [' ' '\t' ';'])+

let hex_digit = ['a'-'f' 'A'-'F' '0'-'9']
let escape_sequence = '\\' ('\\' | 'n' | 'r' | 't' | 'x' hex_digit hex_digit)

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
  | url as string { URL string }
  | '"' { read_string (Buffer.create 16) lexbuf }
  | ['0'-'9']* as number { NUMBER (int_of_string number) }
  | identifier as id { parse_identifier id }
  | eof { EOF }
  | _ { failwith "lexer error" }

and read_string buffer = parse
  | '"' { STRING (Buffer.contents buffer) }
  | escape_sequence as string
    { Buffer.add_string buffer (Scanf.unescaped string);
      read_string buffer lexbuf }
  | _ # '"' as char
    { Buffer.add_char buffer char;
      read_string buffer lexbuf }
