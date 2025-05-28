
(** {2 Abstract Syntax of the While language} *)

(** {3 Parsed trees}

   This is the output of the parser and the input of the type checker. *)

type location = Lexing.position * Lexing.position

type ident = { loc: location; id: string; }

type op = Badd | Bsub | Bmul | Bdiv | Bmod

type expr =
  | Ecst of int
  | Evar of ident
  | Ebinop of op * expr * expr

type stmt =
  | Sskip
  | Sassign of ident * expr
  | Sif of expr * stmt * stmt
  | Sseq of stmt * stmt
  | Swhile of expr * stmt
  | Sprint of expr

type file = stmt

(** {3 Typed trees}

   This is the output of the type checker and the input of the code
   generation. *)

(** In the typed trees, all the occurrences of the same variable
   point to a single record of the following type. *)
type var = {
  var_name: string;
  mutable var_ofs: int (** position wrt %rbp *)
}

type texpr =
  | TEcst of int
  | TEvar of var
  | TEbinop of op * texpr * texpr

type tstmt =
  | TSskip
  | TSassign of var * texpr
  | TSif of texpr * tstmt * tstmt
  | TSseq of tstmt * tstmt
  | TSwhile of texpr * tstmt
  | TSprint of texpr

type tfile = tstmt
