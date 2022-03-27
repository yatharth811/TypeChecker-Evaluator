open AST
structure TypeChecker = 
struct

	type typEnvironment = (string * TYP) list
	exception brokenTypes

	fun envAdd (var: VAR, typ: TYP, env: typEnvironment) = 
        let
            val VAR(x) = var
        in
            case (List.find (fn (y, _) => y = x)) env of 
                SOME (x, typ) => raise Fail ("Variable " ^ x ^ " already declared.")
            |   NONE => (x,typ) :: env
        end

    fun envLookup (var: VAR, env: typEnvironment) = 
        let
            val VAR(x) = var
        in
            case (List.find (fn (y, _) => y = x)) env of 
                SOME (x, typ) => typ
            |   NONE => raise Fail ("Variable " ^ x ^ " not declared yet.")
        end
    
    fun t2s (INT) = "int"
    | t2s (BOOL) = "bool"

    (* fun requiredType(TT) = BOOL
    | requiredType(FF) = BOOL
    | requiredType() *)
    (* fun checkIteExp(exp, cmdseq, cmdseq) = true;
    fun checkSetExp() *)
    (* fun checkExp(e, typenv) = 
        case e of 
            ITE(x, cmd1, cmd2) => checkIteExp(x, cmd1, cmd2, typenv)
    fun checkIteExp(e, cmd1, cmd2, typenv) = case checkExp(e, env) of
        BOOL => if (checkExpcm) *)


    fun checkTypeExpressionCMD(cmd, typeEnv) = 
        case cmd of
            SET(x, exp)   => (
                if envLookup(VAR(x), typeEnv) = checkTypeExpression(exp, typeEnv) then envLookup(VAR(x), typeEnv) else raise Fail ("\nVariable Assignment error. \nExpected type for " ^ x ^ ": " ^ t2s(envLookup(VAR(x), typeEnv)) ^ "\nAssigned Type: " ^ t2s(checkTypeExpression(exp, typeEnv)))
            ) 

        |   ITE(exp, cmd1, cmd2)  =>  (case checkTypeExpression(exp, typeEnv) of
                BOOL => ((valOf (checkTypeCMDSEQ(cmd1, typeEnv)));
                        valOf (checkTypeCMDSEQ(cmd2, typeEnv)))
                | _ => raise brokenTypes)

        |   WH(exp, cmd)  => (if checkTypeExpression(exp, typeEnv) = BOOL then valOf (checkTypeCMDSEQ(cmd, typeEnv)) else raise brokenTypes)  
        |   READ(x)  =>  envLookup(VAR(x), typeEnv)
        |   WRITE(exp) => checkTypeExpression(exp, typeEnv) 

    and checkTypeExpression(expression, typeEnv) = 
        case expression of
            NUM(_) => INT

        |   PLUS(exp1, exp2) => (
            if checkTypeExpression(exp1, typeEnv) = INT andalso checkTypeExpression(exp2, typeEnv) = INT then INT else raise Fail ("\nArithmetic Error for PLUS.\nOperator Domain: int * int\nOperand Type: " ^ t2s(checkTypeExpression(exp1, typeEnv)) ^ " * " ^ t2s(checkTypeExpression(exp2, typeEnv)))
        )

        |   MINUS(exp1, exp2) => (
            if checkTypeExpression(exp1, typeEnv) = INT andalso checkTypeExpression(exp2, typeEnv) = INT then INT else raise Fail ("\nArithmetic Error for MINUS.\nOperator Domain: int * int\nOperand Type: " ^ t2s(checkTypeExpression(exp1, typeEnv)) ^ " * " ^ t2s(checkTypeExpression(exp2, typeEnv)))
        )

        |   TIMES(exp1, exp2) => (
            if checkTypeExpression(exp1, typeEnv) = INT andalso checkTypeExpression(exp2, typeEnv) = INT then INT else raise Fail ("\nArithmetic Error for TIMES.\nOperator Domain: int * int\nOperand Type: " ^ t2s(checkTypeExpression(exp1, typeEnv)) ^ " * " ^ t2s(checkTypeExpression(exp2, typeEnv)))
        )
        
        |   DIV(exp1, exp2) => (
            if checkTypeExpression(exp1, typeEnv) = INT andalso checkTypeExpression(exp2, typeEnv) = INT then INT else raise Fail ("\nArithmetic Error for DIV.\nOperator Domain: int * int\nOperand Type: " ^ t2s(checkTypeExpression(exp1, typeEnv)) ^ " * " ^ t2s(checkTypeExpression(exp2, typeEnv)))
        )

        |   MOD(exp1, exp2) => (
            if checkTypeExpression(exp1, typeEnv) = INT andalso checkTypeExpression(exp2, typeEnv) = INT then INT else raise Fail ("\nArithmetic Error for MOD.\nOperator Domain: int * int\nOperand Type: " ^ t2s(checkTypeExpression(exp1, typeEnv)) ^ " * " ^ t2s(checkTypeExpression(exp2, typeEnv)))
        )

        |   LT(exp1, exp2) => (
            if checkTypeExpression(exp1, typeEnv) = checkTypeExpression(exp2, typeEnv) then BOOL else raise Fail ("\nRelational Error for LT.\nOperator Domain: 'a * 'a\nOperand Type: " ^ t2s(checkTypeExpression(exp1, typeEnv)) ^ " * " ^ t2s(checkTypeExpression(exp2, typeEnv)))
        )

        |   LEQ(exp1, exp2) => (
            if checkTypeExpression(exp1, typeEnv) = checkTypeExpression(exp2, typeEnv) then BOOL else raise Fail ("\nRelational Error for LEQ.\nOperator Domain: 'a * 'a\nOperand Type: " ^ t2s(checkTypeExpression(exp1, typeEnv)) ^ " * " ^ t2s(checkTypeExpression(exp2, typeEnv)))
        )

        |   EQ(exp1, exp2) => (
            if checkTypeExpression(exp1, typeEnv) = checkTypeExpression(exp2, typeEnv) then BOOL else raise Fail ("\nRelational Error for EQ.\nOperator Domain: 'a * 'a\nOperand Type: " ^ t2s(checkTypeExpression(exp1, typeEnv)) ^ " * " ^ t2s(checkTypeExpression(exp2, typeEnv)))
        )

        |   GT(exp1, exp2) => (
            if checkTypeExpression(exp1, typeEnv) = checkTypeExpression(exp2, typeEnv) then BOOL else raise Fail ("\nRelational Error for GT.\nOperator Domain: 'a * 'a\nOperand Type: " ^ t2s(checkTypeExpression(exp1, typeEnv)) ^ " * " ^ t2s(checkTypeExpression(exp2, typeEnv)))
        )

        |   GEQ(exp1, exp2) => (
            if checkTypeExpression(exp1, typeEnv) = checkTypeExpression(exp2, typeEnv) then BOOL else raise Fail ("\nRelational Error for GEQ.\nOperator Domain: 'a * 'a\nOperand Type: " ^ t2s(checkTypeExpression(exp1, typeEnv)) ^ " * " ^ t2s(checkTypeExpression(exp2, typeEnv)))
        )

        |   NEQ(exp1, exp2) => (
            if checkTypeExpression(exp1, typeEnv) = checkTypeExpression(exp2, typeEnv) then BOOL else raise Fail ("\nRelational Error for NEQ.\nOperator Domain: 'a * 'a\nOperand Type: " ^ t2s(checkTypeExpression(exp1, typeEnv)) ^ " * " ^ t2s(checkTypeExpression(exp2, typeEnv)))
        )

        |   NEGATE(exp) => (
            if checkTypeExpression(exp, typeEnv) = INT then INT else raise Fail ("\nArithmetic Error for NEGATE.\nOperator Domain: int\nOperand Type: " ^ t2s(checkTypeExpression(exp, typeEnv)))
        )

        |   OR(exp1, exp2) => (
            if checkTypeExpression(exp1, typeEnv) = BOOL andalso checkTypeExpression(exp2, typeEnv) = BOOL then BOOL else raise Fail ("\nLogical Error for OR.\nOperator Domain: bool * bool\nOperand Type: " ^ t2s(checkTypeExpression(exp1, typeEnv)) ^ " * " ^ t2s(checkTypeExpression(exp2, typeEnv)))
        )

        |   AND(exp1, exp2) => (
            if checkTypeExpression(exp1, typeEnv) = BOOL andalso checkTypeExpression(exp2, typeEnv) = BOOL then BOOL else raise Fail ("\nLogical Error for AND.\nOperator Domain: bool * bool\nOperand Type: " ^ t2s(checkTypeExpression(exp1, typeEnv)) ^ " * " ^ t2s(checkTypeExpression(exp2, typeEnv)))
        )

        |   NOT(exp) => (
            if checkTypeExpression(exp, typeEnv) = BOOL then BOOL else raise Fail ("\nLogical Error for NOT.\nOperator Domain: bool\nOperand Type: " ^ t2s(checkTypeExpression(exp, typeEnv)))
        )

        | TT => BOOL
        
        | FF => BOOL
        
        | VAREXP(x) => (
                envLookup(VAR(x), typeEnv)
        ) 

    and checkTypeCMDSEQ(CMDSEQ(x), typeEnv) :TYP option = case x of 
        [] => NONE
    |  cmd :: cmds => (
        let val gg = checkTypeExpressionCMD(cmd, typeEnv)
        in
            checkTypeCMDSEQ(CMDSEQ(cmds), typeEnv); 
            SOME (gg)
        end)
end