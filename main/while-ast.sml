structure AST = struct
    datatype TYP = INT | BOOL
    (* E := (A < B) > (C < D); *)
    datatype VAR = VAR of string
    type IDSEQ = VAR list
    datatype DEC = DEC of IDSEQ * TYP
    type DECWRAP = DEC list
    datatype DECSEQ = DECSEQ of DECWRAP
    datatype CMD = SET of string * EXP
        |  ITE of EXP * CMDSEQ * CMDSEQ
        | WH of EXP * CMDSEQ
        | READ of string
        | WRITE of EXP
    and EXP = NUM of int
        | PLUS of EXP * EXP
        | MINUS of EXP * EXP
        | TIMES of EXP * EXP
        | DIV of EXP * EXP
        | MOD of EXP * EXP
        | LT of EXP * EXP
        | LEQ of EXP * EXP
        | EQ of EXP * EXP
        | GT of EXP * EXP
        | GEQ of EXP * EXP
		| NEQ of EXP * EXP
		| NEGATE of EXP
        | OR of EXP * EXP
        | AND of EXP * EXP
		| NOT of EXP
        | TT
        | FF
        | VAREXP of string 
    and CMDSEQ = CMDSEQ of CMD list
    type CMDWRAP = CMD list
    datatype BLK = BLK of DECSEQ * CMDSEQ
    datatype PROG = PROG of string * BLK
    fun decAdd(dec : DEC, seq : DECWRAP ) = dec :: seq
    fun idAdd(hm: VAR, seq: IDSEQ ) = hm :: seq
    fun cmdAdd(ex : CMD, seq: CMDWRAP) = ex :: seq
    datatype typval = IntTyp 
	       	      | BoolTyp
    
    (* type environment = (string * TYP) list

    fun envAdd (var: VAR, typ: TYP, env: environment) = 
        let
            val VAR(x) = var
        in
            case (List.find (fn (y, _) => y = x)) env of 
                SOME (x, typ) => raise Fail ("Variable " ^ x ^ " already declared.")
            |   NONE => (x,typ) :: env
        end

    fun envLookup (var: VAR, typ: TYP, env: environment) = 
        let
            val VAR(x) = var
        in
            case (List.find (fn (y, _) => y = x)) env of 
                SOME (x, typ) => typ
            |   NONE => raise Fail ("Variable " ^ x ^ " not declared yet.")
        end *)

end