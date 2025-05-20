{
  open Parser
  exception Eof
}


rule token = parse
| (('-'?)['0'-'9']+) as lxm
                    { let n = int_of_string lxm in 
	                    INT(n) }
| "true"             { TRUE }
| "false"            { FALSE }
| '('                { LPAREN }
| ')'                { RPAREN }
| ','                { COMMA }
| "let"              { LET }
| "fun"              { FUN }
| "rec"              { REC }
| "in"               { IN }
| "if"               { IF }
| "then"             { THEN }
| "else"             { ELSE }
| "="                { EQ }
| "->"               { RIGHT_ARROW }
| ['\n' ]            { (Lexing.new_line lexbuf) ; (token lexbuf) }
| [' ' '\t']         { token lexbuf }    (* skip blanks *)
| "(*"               { comment lexbuf }  (* Comment until closing *)
| eof | "eof"        { EOF }
| ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']* as lxm { IDENT(lxm) }
| _ as lxm          { raise Parsing.Parse_error }


and comment = parse 
| "*)"    { token lexbuf }
| ['\n']  { Lexing.new_line lexbuf; comment lexbuf } 
| _       {comment lexbuf } 