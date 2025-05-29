open X86_64
open Ast

let debug = ref false

let inline_text = "
P_print_newline:
  pushq %rbp
  movq %rsp, %rbp
  andq $-16, %rsp
  movq $S_new_line, %rdi
  xorq %rax, %rax
  call printf
  movq %rbp, %rsp
  popq %rbp
  ret
P_print:
  pushq %rbp
  movq %rsp, %rbp
  andq $-16, %rsp
  movq %rdi, %rsi
  movq $S_message_int, %rdi
  xorq %rax, %rax
  call printf
  movq %rbp, %rsp
  popq %rbp
  ret
"

let inline_data = "
S_message_int:
  .string \"%d\"
S_new_line:
  .string \"\\n\"
"

let leave =
  movq (reg rbp) (reg rsp) ++ popq rbp ++ ret

let enter =
  pushq (reg rbp) ++ movq (reg rsp) (reg rbp)

let new_label =
  let c = ref 0 in
  fun () -> incr c; "L_" ^ (string_of_int !c)

type env_alloc = {
  mutable nb_local: int; (* next local *)
  mutable nb_total: int; (* total allocated variables *)
}

let empty_env_alloc () = {
  nb_local = 0;
  nb_total = 0;
}

let alloc_var env v =
  if v.var_ofs = -1 then begin (* first assignment *)
    env.nb_local <- env.nb_local + 1;
    let ofs = -8 * env.nb_local in
    v.var_ofs <- ofs;
    env.nb_total <- env.nb_total + 1
  end

type string_env = (string, string) Hashtbl.t

let string_env : string_env = Hashtbl.create 16

let new_string  =
  let c = ref 0 in
  fun s -> incr c;
    let l = "L_" ^ (string_of_int !c) in
    Hashtbl.add string_env l s;
    l

let rec fold_i f i acc = function
  | [] -> acc
  | x :: r -> fold_i f (i + 1) (f i acc x) r

let rec compile_expr (e: Ast.texpr) =
  match e with
  | TEcst n -> movq (imm n) (reg rdi)
  | TEvar {var_ofs = ofs; _} -> movq (ind ~ofs rbp) (reg rdi)
  | TEbinop (op, e1, e2) ->
      compile_expr e1 ++ pushq (reg rdi) ++
      compile_expr e2 ++ movq (reg rdi) (reg rsi) ++
      popq rdi ++
      begin match op with
       | Badd -> addq (reg rsi) (reg rdi)
       | Bsub -> subq (reg rsi) (reg rdi)
       | Bmul -> imulq (reg rsi) (reg rdi)
       | Bdiv -> movq (reg rdi) (reg rax) ++ cqto ++
                 idivq (reg rsi) ++ movq (reg rax) (reg rdi)
       | Bmod -> movq (reg rdi) (reg rax) ++ cqto ++
                 idivq (reg rsi) ++ movq (reg rdx) (reg rdi)
      end

let rec compile_stmt (s: Ast.tstmt) =
  match s with
  | TSskip -> nop
  | TSassign ({var_ofs = ofs; _}, e) ->
      compile_expr e ++
      movq (reg rdi) (ind ~ofs rbp)
  | TSif (e, s1, s2) ->
      let l_else = new_label () and l_end = new_label () in
      compile_expr e ++ testq (reg rdi) (reg rdi) ++
      jz l_else ++
      compile_stmt s1 ++ jmp l_end ++
      label l_else ++
      compile_stmt s2 ++
      label l_end
  | TSseq (s1, s2) ->
      compile_stmt s1 ++
      compile_stmt s2
  | TSwhile (e, s) ->
      let l_start = new_label () and l_end = new_label () in
      label l_start ++
      compile_expr e ++
      testq (reg rdi) (reg rdi) ++
      jz l_end ++
      compile_stmt s ++
      jmp l_start ++
      label l_end
  | TSprint e ->
      compile_expr e ++
      call "P_print" ++
      call "P_print_newline"

let rec alloc_vars env (s: Ast.tstmt) =
  match s with
  | TSskip | TSprint _ -> ()
  | TSassign (x, _) -> alloc_var env x
  | TSif (_, s1, s2) -> alloc_vars env s1; alloc_vars env s2
  | TSseq (s1, s2) -> alloc_vars env s1; alloc_vars env s2
  | TSwhile (_, s) -> alloc_vars env s

let file ?debug:(b=false) (f: Ast.tfile) : X86_64.program =
  debug := b;
  let env = empty_env_alloc () in
  alloc_vars env f;
  let locals =
    if env.nb_total = 0 then nop
    else subq (imm (8 * env.nb_total)) (reg rsp) in
  let cfile = enter ++ locals ++ compile_stmt f ++
              xorq (reg rax) (reg rax) ++ leave in
  { text = globl "main" ++ label "main" ++ cfile ++
           inline inline_text;
    data = inline inline_data }
