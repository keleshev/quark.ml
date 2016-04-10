open Core_kernel.Std
module JSON = Yojson.Safe

let () =
  let lexbuf = Lexing.from_channel In_channel.stdin in
  let ast =
    try Parser.parse_top_level Lexer.read lexbuf
    with Parser.Error ->
      let position = Lexing.(lexbuf.lex_curr_p) in
      let line = Lexing.(position.pos_lnum)
      and column = Lexing.(position.pos_cnum - position.pos_bol) in
      failwithf "Syntax error at line %d, column %d" line column ()
  in
  let json = Syntax.to_json ast in
  JSON.pretty_to_channel Out_channel.stdout json
