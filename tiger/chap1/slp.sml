type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
| AssignStm of id * exp
| PrintStm of exp list

and exp = IdExp of id
| NumExp of int
| OpExp of exp * binop * exp
| EseqExp of stm * exp

val prog = 
    CompoundStm(AssignStm("a", OpExp(NumExp 5, Plus, NumExp 3)),
        CompoundStm(AssignStm("b",
                EseqExp(PrintStm[IdExp"a", OpExp(IdExp"a", Minus, NumExp 1), IdExp"a"],
                    OpExp(NumExp 10, Times, IdExp"a"))),
            PrintStm[
                IdExp "b",
                OpExp(IdExp"a", Times, IdExp"b"),
                EseqExp(
                    AssignStm("b", OpExp(NumExp ~1, Plus, IdExp"a")),
                    OpExp(IdExp"b", Times, IdExp"b")
                    ),
                IdExp "b"
            ]))

fun maxargs (s: stm) : int =
        let
            fun maxargs_exp (e: exp) =
                    case e of EseqExp (s', _) => maxargs s'
                    | _ => 0
        in
            case s of CompoundStm (s1, s2) => Int.max(maxargs s1, maxargs s2)
            | AssignStm (_, e') => maxargs_exp e'
            | PrintStm exp_li =>
                let
                    val m = List.foldl Int.max 0 (map maxargs_exp exp_li)
                in Int.max(List.length exp_li, m)
                end
        end
;

type table = (id * int) list
fun interp (s: stm) : unit =
        let
            fun lookup (k: id, sym_table: table) : int option =
                    case sym_table of [] => NONE
                    | (k', v)::sym_table_tail => if k' = k then SOME v else lookup(k, sym_table_tail)
            fun eval_binop (x: int, b_op: binop, y: int) : int =
                    case b_op of Plus => x + y
                    | Minus => x - y
                    | Times => x * y
                    | Div => x div y
            fun handle_print (exp_li: exp list, sym_table: table) : (string list * table) =
                    case exp_li of [] => ([], sym_table)
                    | exp_li_head::exp_li_tail =>
                        let val (exp_res, sym_table1) = interp_exp(exp_li_head, sym_table)
                            val exp_res_str = Int.toString exp_res
                            val (exp_tail_res_li, sym_table2) = handle_print(exp_li_tail, sym_table1)
                        in (exp_res_str::exp_tail_res_li, sym_table2) end
            and interp_stm (s': stm, sym_table: table) : table =
                    case s' of CompoundStm (s1, s2) =>
                            let val sym_table' = interp_stm(s1, sym_table)
                            in interp_stm(s2, sym_table') end
                    | AssignStm (k', e') =>
                        let val (v, sym_table') = interp_exp(e', sym_table)
                        in (k', v)::sym_table'
                        end
                    | PrintStm exp_li =>
                        let
                            val (exp_res_li, sym_table') = handle_print(exp_li, sym_table)
                        in
                            print ((String.concatWith " " exp_res_li) ^ "\n");
                            sym_table'
                        end
            and interp_exp (e: exp, sym_table: table) : (int * table) =
                    case e of IdExp k =>
                            let val v = valOf(lookup(k, sym_table))
                            in (v, sym_table) end
                    | NumExp(n) => (n, sym_table)
                    | OpExp(e1, b_op, e2) =>
                        let val (x1, sym_table1) = interp_exp(e1, sym_table)
                            val (x2, sym_table2) = interp_exp(e2, sym_table1)
                            val exp_res = eval_binop(x1, b_op, x2)
                        in (exp_res, sym_table2) end
                    | EseqExp(s', e') =>
                        let val sym_table' = interp_stm(s', sym_table)
                        in interp_exp(e', sym_table') end
        in
            interp_stm(s, []);
            ()
        end
;

maxargs prog;
interp prog;
