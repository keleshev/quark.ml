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

let identifier = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let line_comment = "//" (_ # '\n')*

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

(* Digits *)
let nonzero_dec = ['1'-'9']
let dec_digit = ['0'-'9']
let oct_digit = ['0'-'7']
let hex_digit = ['a'-'f' 'A'-'F'] | dec_digit

(* Number Literals *)
let dec_literal = (dec_digit | '_')+
let exponent = ('E' | 'e') ('-' | '+')? dec_literal
let float_suffix = (exponent | '.' dec_literal exponent?)
let number_literal = nonzero_dec (dec_digit | '_')* float_suffix?
                   | '0' (       (dec_digit | '_')* float_suffix?
                         | 'b'   ('1' | '0' | '_')+
                         | 'o'   (oct_digit | '_')+
                         | 'x'   (hex_digit | '_')+
                         )

rule read = parse
  | [' ' '\t' '\r']+ { read lexbuf }
  | '\n' { Lexing.new_line lexbuf; read lexbuf }
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
  | "?"  { CAST    }
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
  | line_comment { read lexbuf }
  | version as string { VERSION string }
  | url as string { URL string }
  | '"' { read_string (Buffer.create 16) lexbuf }
  | number_literal as string { NUMBER string }
  | identifier as id { parse_identifier id }
  | eof { EOF }
  | _ { raise Parser.Error }

and read_string buffer = parse
  | '"' { STRING (Buffer.contents buffer) }
  | escape_sequence as string
    { Buffer.add_string buffer (Scanf.unescaped string);
      read_string buffer lexbuf }
  | _ # '"' as char
    { if char = '\n' then Lexing.new_line lexbuf;
      Buffer.add_char buffer char;
      read_string buffer lexbuf }
