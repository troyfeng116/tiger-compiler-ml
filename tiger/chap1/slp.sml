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
            PrintStm[IdExp "b"]))

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

maxargs prog
