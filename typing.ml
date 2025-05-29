open Ast
module H = Hashtbl

let debug = ref false

let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

exception Error of Ast.location * string

(* use the following function to signal typing errors, e.g.
      error ~loc "unbound variable %s" id
*)
let error ?(loc=dummy_loc) f =
  Format.kasprintf (fun s -> raise (Error (loc, s))) ("@[" ^^ f ^^ "@]")

type var_ctx = (string, var) H.t

let rec expr (ctx: var_ctx) (e: Ast.expr) =
  match e with
  | Ecst c -> TEcst c
  | Evar x -> begin
      try let tx = H.find ctx x.id in TEvar tx
      with Not_found -> error ~loc:x.loc "Unbound variable %s" x.id
    end
  | Ebinop (op, e1, e2) ->
      TEbinop (op, expr ctx e1, expr ctx e2)

let rec stmt (ctx: var_ctx) (s: Ast.stmt) =
  match s with
  | Sskip -> TSskip
  | Sif (e, s1, s2) ->
      TSif (expr ctx e, stmt ctx s1, stmt ctx s2)
  | Sassign (x, e) ->
      let x = H.find ctx x.id in (* this should not fail *)
      TSassign (x, expr ctx e)
  | Sprint e ->
      TSprint (expr ctx e)
  | Sseq (s1, s2) ->
      TSseq (stmt ctx s1, stmt ctx s2)
  | Swhile (e, s) ->
      TSwhile (expr ctx e, stmt ctx s)

let alloc_var (ctx: var_ctx) x =
  if not (H.mem ctx x.id) then
    H.replace ctx x.id { var_name = x.id; var_ofs = -1; }

let rec alloc_vars (ctx: var_ctx) (s: Ast.stmt) =
  match s with
  | Sif (_, s1, s2) ->
      alloc_vars ctx s1; alloc_vars ctx s2
  | Sassign (x, _) ->
      alloc_var ctx x
  | Sseq (s1, s2) ->
      alloc_vars ctx s1;
      alloc_vars ctx s2
  | Swhile (_, s) ->
      alloc_vars ctx s
  | Sskip | Sprint _ -> ()

let file ?debug:(b=false) (p: Ast.file) : Ast.tfile =
  debug := b;
  let ctx = H.create 16 in
  alloc_vars ctx p;
  stmt ctx p
