open TypeChecker
exception syntaxError
val whileArray = Array.array(70,"")
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

(* New Functions *)

val maxMemSize = 60; 
val M = Array.array (maxMemSize, 0); 

fun addVariables inf = 
	let 
		val AST.PROG(programName, AST.BLK(AST.DECSEQ(decSeq),AST.CMDSEQ(cmdSeq))) = parseFile inf;
	in
		symboltable(decSeq)
	end

and symboltable(hd::tl) = 
	let 
		val x = toTable(hd)
	in
		x @ symboltable(tl)
	end

	| symboltable([])=([])


and toTable(AST.DEC(a, AST.INT)) = toTable1(a)
	| toTable(AST.DEC(a, AST.BOOL)) = toTable2(a)


and toTable1(hd::tl) = 
	let 
		val AST.VAR(x)= hd
	in 
		(x,AST.IntTyp) :: toTable1(tl)
	end
	| toTable1 ([]) = []


and toTable2(hd::tl) = 
	let 
		val AST.VAR(x)=hd
	in 
		(x,AST.BoolTyp) :: toTable1(tl)
	end
	| toTable2 ([]) = []


val tempVar1 = addVariables

fun postfix inf =  
	let 
		val AST.PROG(programName,AST.BLK(AST.DECSEQ(decSeq),AST.CMDSEQ(cmdSeq))) = parseFile inf;
	in
		postfixAdd(cmdSeq)
	end

and postfixAdd(e::st):string list  = 
        let 
          val (ex) = astToPostfixCmdHelper(e)

        in
          ex @ postfixAdd(st) @ ["SEQ"]   
        end
  | postfixAdd([]) = ([])
                         
and astToPostfixCmdHelper(AST.SET(a,b)) = [a] @ astToPost(b) @ ["SET"] 
	| 	astToPostfixCmdHelper(AST.ITE(a,AST.CMDSEQ(b),AST.CMDSEQ(c))) = astToPost(a) @ postfixAdd(b) @ postfixAdd(c) @ ["ITE"]
	|	astToPostfixCmdHelper(AST.WH(a,AST.CMDSEQ(b))) = astToPost(a) @ postfixAdd(b) @ ["WH"]
	|  	astToPostfixCmdHelper(AST.READ(a)) = [a] @ ["READ"]
	|	astToPostfixCmdHelper(AST.WRITE(a)) = astToPost(a) @ ["WRITE"]
and astToPost(AST.PLUS(a,b)) = astToPost(a) @ astToPost(b) @ ["PLUS"] 
	|   astToPost(AST.TIMES(a,b)) = astToPost(a) @ astToPost(b) @ ["TIMES"]  
	|   astToPost(AST.DIV(a,b)) = astToPost(a) @ astToPost(b) @ ["DIV"] 
	|   astToPost(AST.MOD(a,b)) = astToPost(a) @ astToPost(b) @ ["MOD"] 
	|   astToPost(AST.MINUS(a,b)) = astToPost(a) @ astToPost(b) @ ["MINUS"] 
	|   astToPost(AST.LT(a,b)) = astToPost(a) @ astToPost(b) @ ["LT"]
	|   astToPost(AST.LEQ(a,b)) = astToPost(a) @ astToPost(b) @ ["LEQ"]
	|   astToPost(AST.EQ(a,b)) = astToPost(a) @ astToPost(b) @ ["EQ"]
	|   astToPost(AST.GT(a,b)) = astToPost(a) @ astToPost(b) @ ["GT"]
	|   astToPost(AST.GEQ(a,b)) = astToPost(a) @ astToPost(b) @ ["GEQ"]
	|   astToPost(AST.NEQ(a,b)) = astToPost(a) @ astToPost(b) @ ["NEQ"]
	|   astToPost(AST.NEGATE(a)) = astToPost(a) @ ["NEGATE"]
	|   astToPost(AST.OR(a,b)) = astToPost(a) @ astToPost(b) @ ["OR"]
	|   astToPost(AST.AND(a,b)) = astToPost(a) @ astToPost(b) @ ["AND"]
	|   astToPost(AST.NOT(a)) = astToPost(a) @ ["NOT"]
	| 	astToPost(AST.TT) = ["TT"]
	| 	astToPost(AST.FF) = ["FF"]
	|   astToPost(AST.VAREXP(a)) = [a]
	|   astToPost(AST.NUM(a)) = [Int.toString(a)]


fun evaluatePostfix inf =  
	let 
		val AST.PROG(programName,AST.BLK(AST.DECSEQ(decSeq),AST.CMDSEQ(cmdSeq))) = parseFile inf;
	in
		postfixAddHelper(cmdSeq)
	end


and postfixAddHelper(e::st):string list  = 
    let 
    	val (temp) = astToPostfixCmd(e)
    in
      	temp @ postfixAddHelper(st) @ ["SEQ"]   
    end
	| postfixAddHelper([]) = ([])


and astToPostfixCmd(AST.SET(a,b)) = ["SETT"] @ [a] @ astToPostfix(b) @ ["SET"] 
	|	 astToPostfixCmd(AST.ITE(a,AST.CMDSEQ(b),AST.CMDSEQ(c))) = astToPostfix(a) @ ["then"] @ postfixAddHelper(b) @ ["else"] @ postfixAddHelper(c) @ ["ITE"]
	|	 astToPostfixCmd(AST.WH(a,AST.CMDSEQ(b))) = ["wh"] @ astToPostfix(a) @ ["do"] @ postfixAddHelper(b) @ ["WH"]
	|    astToPostfixCmd(AST.READ(a)) = ["read"] @ [a] @ ["READ"]
	|	 astToPostfixCmd(AST.WRITE(a)) =astToPostfix(a) @ ["WRITE"]


and astToPostfix(AST.PLUS(a,b)) = astToPostfix(a) @ astToPostfix(b) @ ["PLUS"] 
	|   astToPostfix(AST.TIMES(a,b)) = astToPostfix(a) @ astToPostfix(b) @ ["TIMES"]  
	|   astToPostfix(AST.DIV(a,b)) = astToPostfix(a) @ astToPostfix(b) @ ["DIV"] 
	|   astToPostfix(AST.MOD(a,b)) = astToPostfix(a) @ astToPostfix(b) @ ["MOD"] 
	|   astToPostfix(AST.MINUS(a,b)) = astToPostfix(a) @ astToPostfix(b) @ ["MINUS"] 
	|   astToPostfix(AST.LT(a,b)) = astToPostfix(a) @ astToPostfix(b) @ ["LT"] 
	|   astToPostfix(AST.LEQ(a,b)) = astToPostfix(a) @ astToPostfix(b) @ ["LEQ"]
	|   astToPostfix(AST.EQ(a,b)) = astToPostfix(a) @ astToPostfix(b) @ ["EQ"]
	|   astToPostfix(AST.GT(a,b)) = astToPostfix(a) @ astToPostfix(b) @ ["GT"]
	|   astToPostfix(AST.GEQ(a,b)) = astToPostfix(a) @ astToPostfix(b) @ ["GEQ"]
	|   astToPostfix(AST.NEQ(a,b)) = astToPostfix(a) @ astToPostfix(b) @ ["NEQ"]
	|   astToPostfix(AST.NEGATE(a)) = astToPostfix(a) @ ["NEGATE"]
	|   astToPostfix(AST.OR(a,b)) = astToPostfix(a) @ astToPostfix(b) @ ["OR"]
	|   astToPostfix(AST.AND(a,b)) = astToPostfix(a) @ astToPostfix(b) @ ["AND"]
	|   astToPostfix(AST.NOT(a)) = astToPostfix(a) @ ["NOT"]
	| 	astToPostfix(AST.TT) = ["TT"]
	| 	astToPostfix(AST.FF) = ["FF"]
	|   astToPostfix(AST.VAREXP(a)) = [a]
	|   astToPostfix(AST.NUM(a)) = ["INT"] @ [Int.toString(a)]



(*NEWWWWWWWWWWWWWWW*)

and whileFunc inf = 
	let 
		val x = evaluatePostfix inf
	in
		whileFunc1 x
	end

and whileFunc1 (hd::tl) = 
  	if String.compare(hd,"wh") = EQUAL then whileFunc2(tl,0) else whileFunc1(tl)
| whileFunc1([]) = 0
and whileFunc2(hd::tl,n) = 
	let 
		val x = Array.update(whileArray,n,hd)
	in
		if(String.compare(hd,"WH") = EQUAL) then n+1 else whileFunc2(tl,n+1)
	end
| whileFunc2([], n) = n
(*NEWWWWWWWWWWWWWWW*)

and execute inf = if (typeCheck (inf) = true) then execute2 inf else raise Fail "TypeChecker Failure." 

and execute2 inf = 
	let 
		val tmp1 = evaluatePostfix inf
		val tmp2 = FunStack.create()
		val tmp3 = FunStack.push(1,tmp2)
		val tmp4 = FunStack.pop(tmp3)
		val tmp5 = FunStack.list2stack(tmp1)
		val tmp6 = FunStack.stack2list(tmp5)
		val tmp7 = initialize(maxMemSize-1)
		val st = addVariables inf
		val temp8 = whileFunc inf
  		val temp9 = arrayToList whileArray
  		val temp10 = List.take(temp9, temp8)
  		val temp11 = List.rev(temp10)
	in
		executehelper(tmp4, tmp6, st, temp11)
	end

and executehelper(V2, hd::tl, st, L) = 
	let 
		val Z = hd::tl
		val C2 = FunStack.list2stack(Z)
		val (V1, M1, C1, st, L) = Vmc.rules(V2, M, C2, st, L)
		val CL1 = FunStack.stack2list(C1)
	in 
		executehelper(V1, CL1, st, L)
	end

| executehelper(V2, [], st, L)=
	
	let 
		val tempVar1 = FunStack.stack2list(V2)
		(* val tempVar2 = arrayToList M  *)
	in 
		(tempVar1, M, [])
	end

and arrayToList arr = Array.foldr (op ::) [] arr

and initialize(n) = 
	let 
		val x= Array.update(M,n,0)
	in 
		if( n = 0 ) then Array.update(M,0,0) else initialize(n-1)
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