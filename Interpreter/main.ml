let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
  let result = (Parser.start Lexer.next lexbuf) in
    print_endline (string_of_int result)
