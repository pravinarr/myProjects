datatype variable = S of string;
type integer_constant = int;
type boolean_constant = bool;
datatype arithmetic_op = Plus | Minus | Times | Div;
datatype relational_op = Lt | Le | Eq | Ne | Ge | Gt;
datatype bool_op = And | Or | Nand | Nor | Xor;
datatype operator = bo_op of bool_op | rel_op of relational_op | air_op of arithmetic_op;
datatype expression = intc_exp of integer_constant | boolc_exp of boolean_constant | var_exp of variable | binarys of expression*expression*operator;
datatype instruction = skip | assignment of variable*expression | instrl of instruction list| conditional of instruction*instruction*expression | loop of instruction*expression;
datatype types = Boolean_type | Integer_type;
type Declaration = variable * types;
type DecList = Declaration list;
type program = DecList * instruction;

(*
PROGRAM power
{
m Integer;
n Integer;
j Integer;
answer Integer;
m = 2;
n = 10;
IF (n Eq 0) THEN answer = 1
ELSE
{
j=1;
answer = 1;
REPEAT
answer = answer Times m;
j = j Plus 1;
UNTIL (j Gt n)
}
}
*)

(* Set of Variables (m,n,j,answer) *)
val val_m = S("m");
val val_n = S("n");
val val_j = S("j");
val val_answer = S("answer");

(* Declaration of Variable (m,n,j,answer) *)
val dec_m = (val_m,Integer_type):Declaration;
val dec_n = (val_n,Integer_type):Declaration;
val dec_j = (val_j,Integer_type):Declaration;
val dec_answer = (val_answer,Integer_type):Declaration;

(* Integer Constants used for assigning to m,n,j,answer *)
val two = 2:integer_constant;
val ten = 10:integer_constant;
val one = 1:integer_constant;
val zero = 0:integer_constant;

(* Binary Operations *)
val decn_eq_0 = binarys(var_exp(val_n),intc_exp(zero),rel_op(Eq));		(* Representation for (n=0) in the if statement *)
val  decans_times_m = binarys(var_exp(val_answer),var_exp(val_m),air_op(Times));		(* Representation for (answer times m) in the else condition *)
val  decj_plus_1 = binarys(var_exp(val_j),intc_exp(one),air_op(Plus));		(* Representation for (j plus 1) in the else condition *)
val  decj_gt_n = binarys(var_exp(val_j),var_exp(val_n),rel_op(Gt));		(* Representation for (j gt n) in the until condition *)

(* Instruction via assignment (m=2,n=10,j=1,answer=1) *)
val instr_m = assignment(val_m,intc_exp(two));	(* Representation for m=2 *)
val instr_n = assignment(val_n,intc_exp(ten));	(* Representation for n = 10 *)
val instr_j = assignment(val_j,intc_exp(one));	(* Representation for j = 1 *)
val instr_answer = assignment(val_answer,intc_exp(one));	(* Representation for answer=1 *)
val instr_answer1 = assignment(val_answer,decans_times_m);	(* Representation for answer= answer time m *)
val instr_j1 = assignment(val_j,decj_plus_1);	(* Representation for j = j plus 1 *)

(* Instruction List under while *)
val instr_while = instrl([instr_answer1,instr_j1]);	(* Representation for answer = answer Times m; j = j Plus 1; *)

(* Loop while *)
val instr_while_loop = loop(instr_while,decj_gt_n);	(* Representation for the instruction under while loop and checking the condition j gt n *)

(* Instruction List under Else *)
val instr_while = instrl([instr_answer1,instr_while_loop]);	(* Representation for while loop and j=1; answer = 1; *)

(* conditional IF *)
val cond_if = conditional(instr_answer,instr_while,decn_eq_0);	(* Representation for if condition; *)

(* Declaration List *)
val dec_list = ([dec_m,dec_n,dec_j,dec_answer]):DecList;

(* Instruction List under Else *)
val instr_list = instrl([instr_m,instr_n,cond_if]);	(* Representation for m=2 and n=10; *)

(* Program *)
val prog = (dec_list,instr_list):program;
