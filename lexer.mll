{
  open Parser

  exception Lexing_error of string

  let kwds = [
    "while", WHILE;
    "if", IF;
    "else", ELSE;
    "print", PRINT
  ]

  let id_or_kwd =
    let h = Hashtbl.create 4 in
    List.iter (fun (s, t) -> Hashtbl.add h s t) kwds;
    fun x -> try Hashtbl.find h x with Not_found -> IDENT x
}

let digit = ['0' - '9']
let number = digit+

let char = ['a' - 'z']
let ident = char+ (char | '_')*

let space = ' ' | '\t'
let newline = '\n'

rule token = parse
  | newline     { Lexing.new_line lexbuf; token lexbuf }
  | space+      { token lexbuf }
  | ident as s  { id_or_kwd s }
  | number as n { NUM (int_of_string n) }
  | '+'         { BADD }
  | '-'         { BSUB }
  | '*'         { BMUL }
  | '/'         { BDIV }
  | '%'         { BMOD }
  | '{'         { LCURLY }
  | '}'         { RCURLY }
  | '('         { LPAR }
  | ')'         { RPAR }
  | ';'         { SEMICOLON }
  | ":="        { ASSIGN }
  | eof         { EOF }
  | _ as c      { raise (Lexing_error
                    ("Ilegal char: " ^ String.make 1 c)) }
