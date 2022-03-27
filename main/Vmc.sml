structure Vmc = 
struct
open FunStack
fun rules (V, M, C, st, L) = 
	let 
		val br = FunStack.top(C)
		val controlStack = FunStack.pop(C)
	in
		if String.compare(br,"TIMES") = EQUAL then timesRule(V, M, controlStack, st, L)

		else if String.compare(br,"PLUS") = EQUAL then plusRule(V, M, controlStack, st, L)
		
		else if String.compare(br,"MINUS") = EQUAL then minusRule(V, M, controlStack, st, L)
		
		else if String.compare(br,"DIV") = EQUAL then divRule(V, M, controlStack, st, L)
		
		else if String.compare(br,"MOD") = EQUAL then modRule(V, M, controlStack, st, L)
		
		else if String.compare(br,"LT") = EQUAL then ltRule(V, M, controlStack, st, L)
		
		else if String.compare(br,"LEQ") = EQUAL then leqRule(V, M, controlStack, st, L)
		
		else if String.compare(br,"EQ") = EQUAL then eqRule(V, M, controlStack, st, L)
		
		else if String.compare(br,"GT") = EQUAL then gtRule(V, M, controlStack, st, L)
		
		else if String.compare(br,"GEQ") = EQUAL then geqRule(V, M, controlStack, st, L)
		
		else if String.compare(br,"NEQ") = EQUAL then neqRule(V, M, controlStack, st, L)
		
		else if String.compare(br,"OR") = EQUAL then orRule(V, M, controlStack, st, L)
		
		else if String.compare(br,"AND") = EQUAL then andRule(V, M, controlStack, st, L)
		
		else if String.compare(br,"NOT") = EQUAL then notRule(V, M, controlStack, st, L)
		
		else if String.compare(br,"NEGATE") = EQUAL then negateRule(V, M, controlStack, st, L)
		
		else if String.compare(br,"TT") = EQUAL then (FunStack.push(1,V), M, controlStack, st, L)
		
		else if String.compare(br,"FF") = EQUAL then (FunStack.push(0,V), M, controlStack, st, L)
		
		else if String.compare(br,"SETT") = EQUAL then setterRule(V, M, controlStack, st, L)
		
		else if String.compare(br,"SET") = EQUAL then setRule(V, M, controlStack, st, L)
		
		else if String.compare(br,"INT") = EQUAL then intRule(V, M, controlStack, st, L)
		
		else if String.compare(br,"SEQ") = EQUAL then (V, M, controlStack, st, L)
		
		else if String.compare(br,"then") = EQUAL then iteRule(V, M, controlStack, st, L)
		
		else if String.compare(br,"else") = EQUAL then iteeRule(V, M, controlStack, st, L)
		
		else if String.compare(br,"ITE") = EQUAL then (V, M, controlStack, st, L)
		
		else if String.compare(br,"do") = EQUAL then doRule(V, M, controlStack, st, L)
    	
		else if(String.compare(br,"WH"))= EQUAL then whileRule(V, M, controlStack, st, L)

		else if(String.compare(br,"WRITE"))= EQUAL then writeRule(V, M, controlStack, st, L)

    	else if(String.compare(br,"READ"))= EQUAL then readRule(V, M, controlStack, st, L)
    	
		else if(String.compare(br,"read"))= EQUAL then readRule2(V, M, controlStack, st, L)

		else varRule(V, M, controlStack, st, br, L)
	end

(* fun postfix fileName =
	let 
		val AST.PROG(programName, AST.BLK(AST.DECSEQ(decSeq), AST.CMDSEQ(cmdSeq))) = parseFile fileName
	in 
		postfixAdd(cmdSeq)
	end

and postfixAdd (x) : string list = (case x of
		[] => [] 
	| 	hd :: tl => astToPostfixCMD(hd) @ postfixAdd(tl) @ ["SEQ"])

and astToPostfixCMD(AST.SET(a, b)) = [a] @ astToPostfix(b) @ ["SET"]
| astToPostfixCMD(AST.ITE(a, AST.CMDSEQ(b), AST.CMDSEQ(c))) = astToPostfix(a) @ postfixAdd(b) @ postfixAdd c @ ["ITE"]
| astToPostfixCMD(AST.WH(a, AST.CMDSEQ(b))) = astToPostfix(a) @ postfixAdd(b) @ ["WH"]
| astToPostfixCMD(AST.READ(a)) = [a] @ ["READ"]
| astToPostfixCMD(AST.WRITE(a)) = astToPostfix(a) @ ["WRITE"]

and astToPostfix(AST.PLUS(a, b)) = astToPostfix(a) @ astToPostfix(b) @ ["PLUS"]
| astToPostfix(AST.MINUS(a, b)) = astToPostfix(a) @ astToPostfix(b) @ ["MINUS"]
| astToPostfix(AST.TIMES(a, b)) = astToPostfix(a) @ astToPostfix(b) @ ["TIMES"]
| astToPostfix(AST.DIV(a, b)) = astToPostfix(a) @ astToPostfix(b) @ ["DIV"]
| astToPostfix(AST.MOD(a, b)) = astToPostfix(a) @ astToPostfix(b) @ ["MOD"]
| astToPostfix(AST.LT(a, b)) = astToPostfix(a) @ astToPostfix(b) @ ["LT"]
| astToPostfix(AST.LEQ(a, b)) = astToPostfix(a) @ astToPostfix(b) @ ["LEQ"]
| astToPostfix(AST.EQ(a, b)) = astToPostfix(a) @ astToPostfix(b) @ ["EQ"]
| astToPostfix(AST.GT(a, b)) = astToPostfix(a) @ astToPostfix(b) @ ["GT"]
| astToPostfix(AST.GEQ(a, b)) = astToPostfix(a) @ astToPostfix(b) @ ["GEQ"]
| astToPostfix(AST.NEQ(a, b)) = astToPostfix(a) @ astToPostfix(b) @ ["NEQ"]
| astToPostfix(AST.NEGATE(a)) = astToPostfix(a) @ ["NEGATE"]
| astToPostfix(AST.OR(a, b)) = astToPostfix(a) @ astToPostfix(b) @ ["OR"]
| astToPostfix(AST.AND(a, b)) = astToPostfix(a) @ astToPostfix(b) @ ["AND"]
| astToPostfix(AST.NOT(a)) = astToPostfix(a) @ ["NOT"]
| astToPostfix(AST.TT) = ["TT"]
| astToPostfix(AST.FF) = ["FF"]
| astToPostfix(AST.VAREXP(a)) = [a]
| astToPostfix(AST.NUM(a)) = [Int.toString a] *)

and timesRule(V,M,C,st,L) = 
	let 
		val topVal=FunStack.top(V)
		val popVal=FunStack.pop(V)
		val newTopVal=FunStack.top(popVal)
		val newPopVal=FunStack.pop(popVal)
		val pra = topVal*newTopVal
	in
		(FunStack.push(pra, newPopVal), M, C, st, L)
	end

and plusRule(V,M,C,st,L) = 
	let 
		val topVal=FunStack.top(V)
		val popVal=FunStack.pop(V)
		val newTopVal=FunStack.top(popVal)
		val newPopVal=FunStack.pop(popVal)
		val pra = topVal+newTopVal

	in
		(FunStack.push(pra, newPopVal), M, C, st, L)
	end

and minusRule(V, M, C, st, L) = 
	let 
		val topVal=FunStack.top(V)
		val popVal=FunStack.pop(V)
		val newTopVal=FunStack.top(popVal)
		val newPopVal=FunStack.pop(popVal)
		val pra = newTopVal-topVal

	in
		(FunStack.push(pra, newPopVal), M, C, st, L)
	end


(*
signature STACK = sig

type 'a Stack
exception EmptyStack
exception Error of string
val create: unit -> 'a Stack
val push:  'a * 'a Stack -> 'a Stack
val pop : 'a Stack -> 'a Stack
val top : 'a Stack -> 'a
val empty: 'a Stack -> bool
val poptop : 'a Stack -> ('a * 'a Stack) option
val nth : 'a Stack * int -> 'a
val drop : 'a Stack * int -> 'a Stack
val depth : 'a Stack -> int
val app : ('a -> unit) -> 'a Stack -> unit
val map : ('a -> 'b) -> 'a Stack -> 'b Stack
val mapPartial : ('a -> 'b option) -> 'a Stack -> 'b Stack
val find : ('a -> bool) -> 'a Stack -> 'a option
val filter : ('a -> bool) -> 'a Stack -> 'a Stack
val foldr : ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
val foldl : ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
val exists : ('a -> bool) -> 'a Stack -> bool
val all : ('a -> bool) -> 'a Stack -> bool
val list2stack : 'a list -> 'a Stack 
val stack2list: 'a Stack -> 'a list 
val toString: ('a -> string) -> 'a Stack -> string

end
*)
and divRule(V, M, C, st, L) = 
	let 
		val topVal=FunStack.top(V)
		val popVal=FunStack.pop(V)
		val newTopVal=FunStack.top(popVal)
		val newPopVal=FunStack.pop(popVal)
		val pra = newTopVal div topVal

	in
		(FunStack.push(pra, newPopVal), M, C, st, L)
	end

and modRule(V, M, C, st, L) = 
	let 
		val topVal=FunStack.top(V)
		val popVal=FunStack.pop(V)
		val newTopVal=FunStack.top(popVal)
		val newPopVal=FunStack.pop(popVal)
		val pra = newTopVal mod topVal

	in
		(FunStack.push(pra,newPopVal), M, C, st, L)
	end

and ltRule(V, M, C, st, L) = 
	let 
		val topVal=FunStack.top(V)
		val popVal=FunStack.pop(V)
		val newTopVal=FunStack.top(popVal)
		val newPopVal=FunStack.pop(popVal)
		val pra = newTopVal < topVal
	in
		if(pra = true) then (FunStack.push(1, newPopVal), M, C, st, L) else (FunStack.push(0, newPopVal), M, C, st, L)
	end

and leqRule(V, M, C, st, L) = 
	let 
		val topVal=FunStack.top(V)
		val popVal=FunStack.pop(V)
		val newTopVal=FunStack.top(popVal)
		val newPopVal=FunStack.pop(popVal)
		val pra = newTopVal <= topVal
	in
		if(pra = true) then (FunStack.push(1, newPopVal), M, C, st, L) else (FunStack.push(0, newPopVal), M, C, st, L)
	end

and eqRule(V, M, C, st, L) = 
	let 
		val topVal=FunStack.top(V)
		val popVal=FunStack.pop(V)
		val newTopVal=FunStack.top(popVal)
		val newPopVal=FunStack.pop(popVal)
		val pra = topVal = newTopVal

	in
		if(pra = true) then (FunStack.push(1, newPopVal), M, C, st, L) else (FunStack.push(0, newPopVal), M, C, st, L)
	end

(*
structure FunStack :> STACK =

struct
type 'a Stack = 'a list
exception EmptyStack
exception Error of string

fun create () = [];

fun push (x : 'a , l : 'a Stack ) = x :: l
 
fun pop (nil) =  raise EmptyStack 
    | pop ((hd :: tl) : 'a Stack) = tl

fun top (l : 'a Stack): 'a = (case l of
           [] => raise EmptyStack
        |  hd :: _ => hd)

fun empty (l : 'a Stack): bool = (case l of
           [] => true
        |   _ => false)

fun poptop(nil) = NONE
    | poptop((hd :: tl) : 'a Stack) = SOME (hd, tl) 

fun nth (l : 'a Stack, x : int) = List.nth(l, x)

fun drop (l : 'a Stack, x : int) = List.drop(l, x);

fun depth (l: 'a Stack) : int = (case l of
            [] => 0       
        |   (hd :: tl) => 1 + depth(tl))

fun app f (l : 'a Stack) = List.app f l;

fun map f (l : 'a Stack) = List.map f l;

fun mapPartial f (l : 'a Stack) = List.mapPartial f l;

fun find f (l : 'a Stack) = List.find f l;

fun filter f (l : 'a Stack) = List.filter f l;

fun foldr f x l = List.foldr f x l; 

fun foldl f x l = List.foldl f x l; 

fun exists f (l : 'a Stack) = List.exists f l;

fun all f (l : 'a Stack) = List.all f l;

fun list2stack (l : 'a list) : 'a Stack = l; 

fun stack2list (l : 'a Stack) : 'a list = l; 

fun toString f ([]) = ""
    | toString f (x::y) = (f x) ^ toString f y;

end
*)

and gtRule(V, M, C, st, L) = 
	let 
		val topVal=FunStack.top(V)
		val popVal=FunStack.pop(V)
		val newTopVal=FunStack.top(popVal)
		val newPopVal=FunStack.pop(popVal)
		val pra = newTopVal > topVal

	in
		if(pra = true) then (FunStack.push(1, newPopVal), M, C, st, L) else (FunStack.push(0, newPopVal), M, C, st, L)
	end

and geqRule(V, M, C, st, L) = 
	let 
		val topVal=FunStack.top(V)
		val popVal=FunStack.pop(V)
		val newTopVal=FunStack.top(popVal)
		val newPopVal=FunStack.pop(popVal)
		val pra = newTopVal >= topVal

	in
		if(pra = true) then (FunStack.push(1, newPopVal), M, C, st, L) else (FunStack.push(0, newPopVal), M, C, st, L)
	end

and neqRule(V,M,C,st,L) = 
	let 
		val topVal=FunStack.top(V)
		val popVal=FunStack.pop(V)
		val newTopVal=FunStack.top(popVal)
		val newPopVal=FunStack.pop(popVal)
		val pra = topVal <> newTopVal

	in
		if(pra = true) then (FunStack.push(1, newPopVal), M, C, st, L) else (FunStack.push(0, newPopVal), M, C, st, L)
	end

and orRule(V, M, C, st, L) = 
	let 
		val topVal=FunStack.top(V)
		val popVal=FunStack.pop(V)
		val newTopVal=FunStack.top(popVal)
		val newPopVal=FunStack.pop(popVal)

	in
		if(topVal=0 andalso newTopVal=0) then (FunStack.push(0,newPopVal),M,C,st, L) else (FunStack.push(1,newPopVal),M,C,st, L)
	end

and andRule(V,M,C,st, L) = 
	let 
		val topVal=FunStack.top(V)
		val popVal=FunStack.pop(V)
		val newTopVal=FunStack.top(popVal)
		val newPopVal=FunStack.pop(popVal)

	in
		if(topVal<>0 andalso newTopVal<>0) then (FunStack.push(1,newPopVal),M,C,st, L) else (FunStack.push(0,newPopVal),M,C,st, L)
	end

and notRule(V,M,C,st, L) = 
	let 
		val topVal=FunStack.top(V)
		val popVal=FunStack.pop(V)

	in
		if(topVal=0) then (FunStack.push(1,popVal),M,C,st, L) else (FunStack.push(0,popVal),M,C,st, L)
	end

and negateRule(V,M,C,st, L) = 
	let 
		val topVal=FunStack.top(V)
		val popVal=FunStack.pop(V)
		val pra= ~topVal

	in
		(FunStack.push(pra,popVal),M,C,st, L)
	end

and setRule(V,M,C,st, L) = 
	let 
		val topVal=FunStack.top(V)
		val popVal=FunStack.pop(V)
		val newTopVal=FunStack.top(popVal)
		val newPopVal=FunStack.pop(popVal)
		val bruh = Array.update(M, newTopVal, topVal)
	in
		(popVal,M,C,st, L)
	end

and setterRule(V,M,C,st, L) = 
	let 
		val topVal = FunStack.top(C)
		val popVal = FunStack.pop(C)
		val pra = lookup(topVal,st,0)

	in
		(FunStack.push(pra,V),M,popVal,st, L)
	end

and lookup(topVal, br::tl, toFind) =
	let 
		val (x,y)=br
	in 
		if(String.compare(x,topVal) = EQUAL) then toFind
		else lookup(topVal,tl,toFind+1)
	end
| lookup(topVal, [], toFind) = toFind

and varRule(V,M,C,st,topVal, L) = 
	let 
		val pra = lookup(topVal,st,0)
		val qra = Array.sub(M,pra)
	in 
		(FunStack.push(qra,V),M,C,st, L)
	end

and intRule(V,M,C,st, L) = 
	let 
		val topVal = FunStack.top(C)
		val SOME ggs = Int.fromString (topVal)
		val popVal = FunStack.pop(C)
	in
		(FunStack.push(ggs,V),M,popVal,st, L)
	end

and iteRule(V,M,C,st, L) = 
	let 
		val tempora = FunStack.top(V)
		val tempV = FunStack.pop(V)
	in
		if( tempora = 0 ) then ite0(tempV,M,C,st, L) else (tempV,M,C,st, L)
	end

and ite0 (V,M,C,st, L) = 
	let
		val br = FunStack.top(C)
		val controlStack = FunStack.pop(C)
	in 
		if String.compare(br, "else") = EQUAL then (V,M,controlStack,st, L)
		else ite0(V,M,controlStack,st, L)
	end

and iteeRule(V,M,C,st, L) = 
	let 
		val br = FunStack.top(C)
		val controlStack = FunStack.pop(C)
	in 
		if String.compare(br,"ITE") = EQUAL then (V,M,controlStack,st, L)
		else iteeRule(V,M,controlStack,st, L)
	end

and toString(V,M,C) = 
	let 
		val temp1 = FunStack.stack2list(V)
		val temp2 = arrayToList M 
		val temp3 = FunStack.stack2list(C)
	in (temp1,temp2,temp3)
	end

and arrayToList arr = Array.foldr (op ::) [] arr

(*NEW ADDITIONS FOR WHILE*)

and doRule(V,M,C,st,L) = 
	let    
		val tempora = FunStack.top(V)
		val tempV = FunStack.pop(V)
	in 
		if( tempora = 0) then doRule0(tempV,M,C,st,L) else (tempV,M,C,st,L)
	end

and doRule0 (V,M,C,st,L) = 
	let
		val br = FunStack.top(C)
		val controlStack = FunStack.pop(C)
	in 
		if String.compare(br,"WH") = EQUAL then (V,M,controlStack,st,L) else doRule0(V,M,controlStack,st,L)
	end

and whileRule(V,M,C,st,L) = 
	let 
		val controlStack = addtostack(C,L)
	in 
		(V,M,controlStack,st,L)
	end

and addtostack(C,hd::tl) =
	let 
		val controlStack = FunStack.push(hd,C)
	in 
		addtostack(controlStack,tl)
	end
| addtostack(C, []) = C


(* Added Read and Write Rules *)

and writeRule(V, M, C, st, L) = 
	let 
		val temp1 = FunStack.top(V)
		val temp2 = FunStack.pop(V)
		val temp3 = Int.toString(temp1)
		val temp4 = print temp3 
		val temp5 = print "\n"
	in 
		(temp2, M, C, st, L)
	end

and readRule(V, M, C, st, L) = 
	let

		val temp1=FunStack.top(V)
		val temp2=FunStack.pop(V)
		val (l,m)= List.nth(st,temp1)
		val temp3 = print "Enter value of "
		val temp4 = print l
		val temp5 = print " : "
		val SOME temp6 = TextIO.inputLine(TextIO.stdIn)
		val SOME temp7 = Int.fromString(temp6)
		val x5 = Array.update(M, temp1, temp7)
	
	in 
		(temp2, M, C, st, L)
	end

and readRule2(V,M,C,st,L) = 
	let 
		val temp1 = FunStack.top(C)
		val temp2 = FunStack.pop(C)
		val pra = lookup(temp1, st, 0)

	in
		(FunStack.push(pra, V), M, temp2, st, L)
	end

end

(*
val whileArray = Array.array(70,"")
open TypeChecker
exception syntaxError
structure whileLrVals = whileLrValsFun(structure Token = LrParser.Token)
structure whileLex = whileLexFun(structure Tokens = whileLrVals.Tokens);
structure whileParser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = whileLrVals.ParserData
     	       structure Lex = whileLex)
     
fun invoke lexstream =
    	     	let fun print_error (s,pos:int,_) =
		    	( TextIO.output(TextIO.stdOut, "Syntax Error:"^Int.toString(pos)^":"^Int.toString(pos)^":"^s) ; raise syntaxError )
		in
		    whileParser.parse(0,lexstream,print_error,())
		end

fun stringToLexer str =
    let val done = ref false
    	val lexer=  whileParser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
	lexer
    end	
		
fun parse (lexer) =
    let val dummyEOF = whileLrVals.Tokens.EOF(0,0)
    	val (result, lexer) = invoke lexer
	val (nextToken, lexer) = whileParser.Stream.get lexer
    in
        if whileParser.sameToken(nextToken, dummyEOF) then (result)
 		else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); result)
		
    end

fun read (infile:string) =
   let 
        val instream = TextIO.openIn infile
	fun loop instream =
	    String.implode(String.explode(TextIO.inputAll instream))

    in
	    loop instream before TextIO.closeIn instream
    end

val parseString = parse o stringToLexer 
val parseFile = parse o stringToLexer o read 

fun symbolTableHelper(AST.DEC(idseq, typ), typeEnv) = case idseq of
		[] => typeEnv
	| 	hd :: tl => (let val new = TypeChecker.envAdd(hd, typ, typeEnv)
	 				in symbolTableHelper(AST.DEC(tl, typ), new) 
	 				end)

fun symbolTableGenerate(x, typeEnv) = case x of 
		[] => typeEnv
	|	(hd :: tl) => ( 
			let val new = symbolTableHelper(hd, typeEnv) 
			in symbolTableGenerate(tl, new) 
			end)

fun typeCheck fileName =
	let
		val AST.PROG(programName, AST.BLK(AST.DECSEQ(decSeq), cmdSeq)) = parseFile fileName
		val tenv = symbolTableGenerate(decSeq, [])
		val SOME x = TypeChecker.checkTypeCMDSEQ(cmdSeq, tenv)
	in 
		if (x = BOOL orelse x = INT) then true else false
		(* cmdSeq *)
		(* tenv *)
	end
*)