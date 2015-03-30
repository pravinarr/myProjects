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
val instr_while = instrl([instr_j,instr_answer1,instr_while_loop]);	(* Representation for while loop and j=1; answer = 1; *)

(* conditional IF *)
val cond_if = conditional(instr_answer,instr_while,decn_eq_0);	(* Representation for if condition; *)

(* Declaration List *)
val dec_list = ([dec_m,dec_n,dec_j,dec_answer]):DecList;

(* Instruction List under Else *)
val instr_list = instrl([instr_m,instr_n,cond_if]);	(* Representation for m=2 and n=10; *)

(* Program *)
val prog = (dec_list,instr_list):program;

(* Number of occurances *)
fun no_of_occurances([],v:variable) = 0 | no_of_occurances ( (x:variable,t:types) :: declist_tail, v:variable) =
   let val m = (if x=v then 1 else 0) in
       m + no_of_occurances(declist_tail , v)
     end ;

(* Testing number of occurances *) 
no_of_occurances(dec_list, val_m); 

fun vdec_list([]) = true |   vdec_list( (x:variable,t:types) :: declist_tail) = (no_of_occurances(declist_tail,x) = 0) 
andalso vdec_list(declist_tail);

(* Testing number of occurances *) 
(*Positive testcases*)
vdec_list(dec_list); 

(*Negative testcases*)
val dec_list1 = ([dec_m,dec_m,dec_n,dec_j,dec_answer]):DecList;
vdec_list(dec_list1);

(* DataType for the typevalue*)
datatype typeValue = intt | boolt | indefinite;
type typeMap = (variable * typeValue) list;

(* Converter for typeMap*)
fun typeToTypeValue(Integer_type) = intt |
	typeToTypeValue(Boolean_type) = boolt;

(* Typing function*)
fun typing ([]) = [] | typing ((x:variable,t:types) :: declist_tail) = 
     let
      in (x,typeToTypeValue(t)):: typing(declist_tail)
    end;

(* Testing for typing*)
val temp = typing(dec_list);

(* Environment *)
fun createEnvironment ([]) = (fn(x:variable) => 0) | 
 createEnvironment ((v:variable,t:types)::declist_tail) = 
 	(fn(x:variable) => (if x=v then 1+length(declist_tail) 
 		else createEnvironment(declist_tail)x)) ;

(*CreateEnvironment testcases*)
val my_env = createEnvironment(dec_list);
my_env(val_m);
my_env(val_n);
val my_env1 = createEnvironment(([]):DecList);
my_env1(val_m);
my_env1(val_n);

(* Gettypevalue *)
fun GetTypeValue([],v:variable)= indefinite |
	GetTypeValue( (x:variable,tv:typeValue)::tail,v:variable)=
	 if v=x then tv else GetTypeValue(tail,v);

(*testing gettypevalue*)	 
GetTypeValue(temp,val_m);

(*Expression type*)
fun ExpressionType( intc_exp(x),tm:typeMap) = intt |
    ExpressionType( boolc_exp(x),tm:typeMap) = boolt |
     ExpressionType( var_exp(x),tm:typeMap) = GetTypeValue(tm,x) |
     ExpressionType( binarys(e1,e2,air_op(x)),tm:typeMap) = intt |
	 ExpressionType( binarys(e1,e2,bo_op(x)),tm:typeMap) = boolt |
	  ExpressionType( binarys(e1,e2,rel_op(x)),tm:typeMap) = boolt;  

(* VExpression function *)
fun VExpression (intc_exp(x),tm:typeMap) = true |
    VExpression (boolc_exp(x),tm:typeMap ) = true |
    VExpression (var_exp(x),tm:typeMap ) = (GetTypeValue(tm,x) <> indefinite) |
    VExpression ( binarys(e1,e2,air_op(x)),tm:typeMap) = VExpression(e1,tm) andalso 
     			VExpression(e2,tm) andalso ExpressionType(e1,tm) = intt andalso ExpressionType(e2,tm) = intt |
	VExpression ( binarys(e1,e2,bo_op(x)),tm:typeMap) = VExpression(e1,tm) andalso 
     			VExpression(e2,tm) andalso ExpressionType(e1,tm) = boolt andalso ExpressionType(e2,tm) = boolt |
    VExpression ( binarys(e1,e2,rel_op(x)),tm:typeMap) = VExpression(e1,tm) andalso 
     			VExpression(e2,tm) andalso ExpressionType(e1,tm) = intt andalso ExpressionType(e2,tm) = intt ;

(*Testing VExpression*)
 val t1 = var_exp(S("t1"));
 val t2 = var_exp(S("t2"));
 val t1_dec = (S("t1"),Boolean_type):Declaration;
 val t2_dec = (S("t2"),Boolean_type):Declaration;
val dec_list2 = ([dec_m,dec_n,dec_j,dec_answer,t1_dec,t2_dec]):DecList; 
   (*Positive cases*)
   VExpression(boolc_exp(true),temp);
   VExpression(intc_exp(one),temp);
   VExpression(binarys(var_exp(val_j),var_exp(val_j),air_op(Plus)),temp);
   VExpression(binarys(var_exp(val_j),var_exp(val_j),rel_op(Gt)),temp);
   VExpression(binarys(var_exp(val_j),var_exp(val_j),rel_op(Gt)),temp);
   
   (*Negative Cases*)
   VExpression(binarys(var_exp(val_j),var_exp(val_j),bo_op(And)),temp);
   VExpression(binarys(t1,t2,rel_op(Gt)),typing(dec_list2));
   VExpression(binarys(t1,t2,air_op(Plus)),typing(dec_list2));

fun vInstruction(skip,tm:typeMap) = true |
    vInstruction(assignment(v,e),tm:typeMap) = (GetTypeValue(tm,v)=ExpressionType(e,tm)) andalso (GetTypeValue(tm,v) <> indefinite) 
 											andalso VExpression(e,tm) | 
    vInstruction(instrl([]),tm:typeMap) = true |											
    vInstruction(instrl((head:instruction)::tail),tm:typeMap) = (vInstruction(head,tm) andalso vInstruction(instrl(tail),tm)) |
    vInstruction(conditional(i1:instruction,i2:instruction,e:expression),tm:typeMap) = vInstruction(i1,tm) andalso vInstruction(i2,tm)
      andalso ExpressionType(e,tm)=boolt andalso VExpression(e,tm) |
    vInstruction(loop(i:instruction,e:expression),tm:typeMap) = vInstruction(i,tm) andalso ExpressionType(e,tm)=boolt 
       andalso VExpression(e,tm);  
       
 (*Testing instruction*)
 (*Positive cases*)
 vInstruction(instr_list,temp);  
 
 (*Negative testcases*)
 
 val inb = assignment(val_j,binarys(t1,t2,air_op(Plus)));
 val inb1 = assignment(val_j,binarys(t1,t2,rel_op(Gt)));
 val ij = instrl ([assignment(val_j,binarys(t1,t2,air_op(Plus)))]);
 val t_con = conditional(ij,ij,binarys(t1,t2,rel_op(Gt)));
 vInstruction(inb,typing(dec_list2));
 vInstruction(inb1,typing(dec_list2));
 vInstruction(ij,typing(dec_list2));
 vInstruction(t_con,typing(dec_list2));
 vInstruction(loop(instr_m,binarys(var_exp(val_n),intc_exp(zero),bo_op(And))),temp);
 val e1 = var_exp(S("e1"));
 val e2 = var_exp(S("e2"));
 vInstruction(loop(instr_m, binarys(e1,e2,bo_op(And))),temp); 
 

exception Invalid_DecList;

 fun VProgram (decl:DecList,Body:instruction) = 
    if vdec_list(decl) then vInstruction(Body,typing(decl))
     else raise Invalid_DecList;
     
 (*Testing*)
 (*Positive testcases*)
 val tg = VProgram(dec_list,instr_list);
 val tt =  VProgram(([]):DecList,instrl([]));
 
 (*Negative testcases*)
 val dec_list3 = ([dec_m,dec_m,dec_n,dec_j,dec_answer,t1_dec,t2_dec]):DecList; 
 val tg2 = VProgram(dec_list,t_con);
 val tg1 = VProgram(dec_list3,instr_list);
 
 
 
 
 
 
         