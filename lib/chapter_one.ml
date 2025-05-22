type id = string

type binop = Plus | Minus | Times | Div

type stm = 
  | CompoundStm of stm * stm 
  | AssignStm of id * exp
  | PrintStm of exp list
and exp = 
  | IdExp of id
  | NumExp of int
  | OpExp of exp * binop * exp
  | EseqExp of stm * exp

let prog = 
 CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp "a"; OpExp(IdExp"a", Minus, NumExp 1)],
           OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b"]))

let maxargs statement =
  let rec aux1 (m1:int) s =
    let rec aux2 m2 e = 
      match e with
      | IdExp _ -> m2
      | NumExp _ -> m2
      | OpExp(ex1, _, ex2) -> max (aux2 m2 ex1) (aux2 m2 ex2)
      | EseqExp(sm, ex) -> max (aux1 m2 sm) (aux2 m2 ex) in
    match s with
    | CompoundStm(left, right) -> max (aux1 m1 left) (aux1 m1 right)
    | AssignStm(_, exp) -> aux2 m1 exp
    | PrintStm exps -> max (List.length exps) (List.fold_left aux2 m1 exps) in
  aux1 0 statement

let update t i v = (i, v) :: t

let rec lookup t i =
  match t with
  | (id, value)::tail -> if id = i then value else (lookup tail i)
  | [] -> raise (Failure "Variable not defined")

let rec interpStm (s, t) =
  match s with
  | CompoundStm(stm1, stm2) -> let t2 = interpStm (stm1, t) in interpStm (stm2, t2)
and interpExp (e, t) = 
  match e with
  | IdExp i -> (lookup t i, t)
  | NumExp i -> (i, t)

let math exp1 op exp2 t = 
    let (valuel, t2) = interpExp (exp1, t) in
    let (valuer, t3) = interpExp (exp2, t2) in
    match op with
    | Plus -> (valuel + valuer, t3)