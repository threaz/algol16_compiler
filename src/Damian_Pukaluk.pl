% Wersja za 22 punkty
% Autor: Damian Pukaluk
% Używana wersja Prologu: SWI-Prolog version 7.2.3 for x86_64-linux
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

algol16(Source, SextiumBin) :-
    phrase(lexer(Tokens), Source),
    phrase(programParse(AST), Tokens),
    phrase(translateProgram(AST), Result),
    makeLinesOfCommands(Result, ListOfCommands),
    commandsToHexToDecimal(ListOfCommands, SextiumBin).

% przerobiony lekser z programu 'while_parser' z KNO
lexer(Tokens) -->
    white_space,
    (  (
	          "+",       !, { Token = tokPlus }
	       ;  "-",       !, { Token = tokMinus }
	       ;  "*",       !, { Token = tokTimes }
	       ;  "<",       !, { Token = tokLt }
	       ;  ">",       !, { Token = tokGt }
	       ;  "=",       !, { Token = tokEq }
	       ;  ":=",      !, { Token = tokAssgn }
	       ;  ";",       !, { Token = tokSColon }
	       ;  ",",       !, { Token = tokComma }
	       ;  "(",       !, { Token = tokLParen }
	       ;  ")",       !, { Token = tokRParen }
	       ;  digit(D),  !,
            number(D, N),
            { Token = tokNumber(N) }
	       ;  letter(L), !, identifier(L, Kw),
            {  member((Kw, Token), [ (and,   tokAnd),
				                             (begin, tokBegin),
				                             (call,  tokCall),
				                             (div,   tokDiv),
				                             (do,    tokDo),
				                             (done,  tokDone),
				                             (else,  tokElse),
				                             (end,   tokEnd),
				                             (fi,    tokFi),
				                             (if,    tokIf),
				                             (local, tokLocal),
				                             (mod,   tokMod),
				                             (not,   tokNot),
				                             (or,    tokOr),
				                             (procedure, tokProcedure),
				                             (program,   tokProgram),
				                             (read,      tokRead),
				                             (return,    tokReturn),
				                             (then,      tokThen),
				                             (value,     tokValue),
				                             (while,     tokWhile),
				                             (write,     tokWrite)
				                           ]),
               !
               ;  Token = tokId(Kw)
            }
	       ;  [_],
            { Token = tokUnknown }
	      ),
       !,
       
       { Tokens = [Token | TokList] },
       lexer(TokList)
     ;  [],
        { Tokens = [] }
    ).

% pomijamy od razu komentarze
any_sequence -->
    [Char1], !,
    ( { Char1 == 42 }, [Char2], { Char2 == 41 }
     ->  white_space
     ;  any_sequence ).

any_sequence --> [].

white_space -->
    [Char], { code_type(Char, space) }, !, white_space.

white_space -->
    [CH], [CH1], { CH == 40, CH1 == 42 },
    any_sequence.

white_space -->
    [].

digit(N) --> [N], { char_type(N, digit) }.

digit_sequence([H|T]) --> digit(H),
			                    !,
			                    digit_sequence(T).
digit_sequence([]) --> [].

number(D, N) -->
    digit_sequence(Ds),
    { number_chars(N, [D|Ds]) }.

letter(L) --> [L], { char_type(L, alpha) }.

alphanum([H|T]) --> [H], { char_type(H, alnum) },
		                !,
		                alphanum(T).

alphanum([H|T]) --> [H], { H == 0'' },
		                !,
		                alphanum(T).

alphanum([H|T]) --> [H], { H == 0'_ },
		                !,
		                alphanum(T).

alphanum([]) --> [].

identifier(L, Id) -->
    alphanum(As),
    { atom_codes(Id, [L|As]) }.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parser tworzący AST

programParse(program(Name, Block)) -->
    [tokProgram],
    [tokId(Name)],
    blockParse(Block).

blockParse(block(Decls, CompInstr)) -->
    declsParse(Decls),
    [tokBegin],
    compInstr(CompInstr),
    [tokEnd].

declsParse(Decls) -->
    (
	      (
	          declaration(Decls1),
	          declsParse(Decls2),
  	        { append([Decls1], Decls2, Decls ) }, !
	       ;  emptyParse(Decls)
	      )
    ).

emptyParse([]) --> [].

declaration(Declr) -->
    (
	      procedureParse(Declr)
     ;  declarator(Declr)
    ).

declarator(declsVars(Vars)) -->
    [tokLocal],
    variables(Vars).

variables(Vars) -->
    (
	      variable(Var1),
	      [tokComma],
	      variables(Vars1),
	      { Vars = [Var1|Vars1] }, !
     ;  variable(Var),
	      { Vars = [Var] }
    ).

variable(atomVar(Var)) --> [tokId(Var)].

compInstr(Instr) -->
    (
	      instruction(Instr1),
	      [tokSColon],
	      compInstr(Instrs),
	      { Instr = [Instr1|Instrs] }, !
     ;  instruction(Instr1),
	      { Instr = [Instr1] }
    ).

instruction(Instr) --> (
			                     variable(Var),
			                     [tokAssgn],
			                     arithmExpression(Expr),
			                     { Instr = instrAssgn([Var, Expr]) }, !
			                  ;  [tokIf],
			                     logicalExpression(Expr),
			                     [tokThen],
			                     compInstr(CInstr),
			                     [tokFi],
			                     { Instr = instrIf(Expr, CInstr) }, !
			                  ;  [tokIf],
			                     logicalExpression(Expr),
			                     [tokThen],
			                     compInstr(CInstrIf),
			                     [tokElse],
			                     compInstr(CInstrElse),
			                     [tokFi],
			                     { Instr = instrIfElse(Expr, CInstrIf, CInstrElse) }, !
			                  ;  [tokCall],
			                     procedureCallParse(PC),
			                     { Instr = instrCall(PC) }, !
			                  ;  [tokWhile],
			                     logicalExpression(Expr),
			                     [tokDo],
			                     compInstr(CInstr),
			                     [tokDone],
			                     { Instr = instrWhile(Expr, CInstr) }, !
			                  ;  [tokReturn],
			                     arithmExpression(Expr),
			                     { Instr = instrReturn(Expr) }, !
			                  ;  [tokRead],
			                     variable(Var),
			                     { Instr = instrRead([Var]) }, !
			                  ;  [tokWrite],
			                     arithmExpression(Expr),
			                     { Instr = instrWrite([Expr]) }
		                   ).



arithmExpression(Expr) --> summand(T), arithmExpressionR(T, Expr).

arithmExpressionR(Acc, Expr) --> [tokPlus], summand(T), !,
				                         { Tmp = arithmExpr(Acc, sumOp, T)},
				                         arithmExpressionR(Tmp, Expr).
arithmExpressionR(Acc, Expr) --> [tokMinus], summand(T), !,
				                         { Tmp = arithmExpr(Acc, substOp, T)},
				                         arithmExpressionR(Tmp, Expr).
arithmExpressionR(Expr, Expr) --> [].

summand(T) --> factor(F), summandR(F, T).
summandR(Acc, T) --> [tokTimes], factor(F), !,
		                 { Tmp = arithmExpr(Acc, timesOp, F) },
		                 summandR(Tmp, T).

summandR(Acc, T) --> [tokDiv], factor(F), !,
		                 { Tmp = arithmExpr(Acc, divOp, F) },
		                 summandR(Tmp, T).

summandR(Acc, T) --> [tokMod], factor(F), !,
		                 { Tmp = arithmExpr(Acc, modOp, F) },
		                 summandR(Tmp, T).
summandR(T, T) --> [].

factor(F) --> (
		              [tokMinus],
		              simpleExpression(FT), !,
		              { F = minus(FT) }
	             ;  simpleExpression(F), !
	            ).

simpleExpression(SE) -->
    (
	      [tokLParen], arithmExpression(SE), [tokRParen], !
     ;  atomExpression(SE)
    ).

atomExpression(AE) -->
    (
	      procedureCallParse(AE)
     ;	variable(AE), !
     ;  [tokNumber(N)], { AE = atomNumber(N) }
			                      
    ).

logicalExpression(Expr) --> conjunction(C), logicalExpressionR(C, Expr).
logicalExpressionR(Acc, Expr) --> [tokOr], conjunction(C),
				                          { Tmp = [logicalExpr(Acc, orOp, C)] },
				                          logicalExpressionR(Tmp, Expr).
logicalExpressionR(Expr, Expr) --> [].

conjunction(C) --> condition(Cond), conjunctionR(Cond, C).
conjunctionR(Acc, C) --> [tokAnd], condition(Cond),
			                   { Tmp = [logicalExpr(Acc, andOp, Cond)] },
			                   conjunctionR(Tmp, C).
conjunctionR(C, C) --> [].

condition(Cond) --> (
			                  [tokNot], relExpression(Cond1),
			                  { Cond = notCond(Cond1) }, !
		                 ;  relExpression(Cond)
		                ).

relExpression(Expr) --> (
			                      [tokLParen],
			                      logicalExpression(Expr),
			                      [tokRParen], !
			                   ;  arithmExpression(E1),
			                      relOperator(Op),
			                      arithmExpression(E2),
			                      { Expr = [logicalExpr(E1, Op, E2)] }
			                  ).
relOperator(Op) --> (
			                  [tokLt], [tokGt], !,
			                  { Op = neqOp }
		                 ;  [tokLt], [tokEq], !,
			                  { Op = leqOp }
		                 ;  [tokLt], !,  { Op = ltOp }
		                 ;  [tokGt], [tokEq], !,
			                  { Op = geqOp }
		                 ;  [tokGt],  !,  { Op = gtOp }
		                 ;  [tokEq],  !,  { Op = eqOp }

		                ).

procedureParse(procedure(Name, FArgs, Block)) -->
    [tokProcedure],
    [tokId(Name)],
    [tokLParen],
    formalArguments(FArgs),
    [tokRParen],
    blockParse(Block).

formalArguments(formArgs(FArgs)) --> (
					                               formalArgumentsSequence(FArgs), !
				                              ;  emptyParse(FArgs)
				                             ).

formalArgumentsSequence(FArgs) -->
    (
	      formalArgument(Arg),
	      [tokComma],
	      formalArgumentsSequence(ArgS),
	      { FArgs = [Arg|ArgS] }, !
     ;  formalArgument(Arg),
	      { FArgs = [Arg] }
    ).

formalArgument(Arg) --> (
			                      [tokValue],
			                      variable(Var),
			                      { Arg = value(Var) }
			                   ;  variable(Arg)
			                  ).

procedureCallParse(procedureCall(Name, RealArgs)) -->
    [tokId(Name)],
    [tokLParen],
    realArguments(RealArgs),
    [tokRParen].

realArguments(realArgs(RArgs)) --> (
				                               realArgumentsSequence(RArgs), !
				                            ;  emptyParse(RArgs)
				                           ).

realArgumentsSequence(RArgs) -->
    (
	      realArgument(Arg),
	      [tokComma],
	      realArgumentsSequence(ArgS),
	      { RArgs = [Arg|ArgS] }, !
     ;  realArgument(RArgs)
    ).

realArgument(Arg) --> arithmExpression(Arg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% stos i podstawowe operacje na nim

lastStackFrame(65535). %% pod adresem 65535 jest adres kolejnego wolnego miejsca na stosie

loadStackFrame -->
    { lastStackFrame(Addr) },
    ["CONST", Addr, "SWAPA", "LOAD"].

incrStackFrame-->
    { lastStackFrame(Addr) },
    loadStackFrame,
    ["SWAPD", "CONST", 1, "SWAPD", "SUB", "SWAPD", "CONST",
     Addr, "SWAPA", "SWAPD", "STORE"].

decrStackFrame -->
    { lastStackFrame(Addr) },
    loadStackFrame,
    ["SWAPD", "CONST", 1, "ADD", "SWAPD", "CONST",
     Addr, "SWAPA", "SWAPD", "STORE"].

addToStack -->
    ["SWAPD"],
    loadStackFrame,
    ["SWAPA", "SWAPD", "STORE"],
    incrStackFrame.

getFromStack -->
    decrStackFrame,
    loadStackFrame,
    ["SWAPA", "LOAD"].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dodatkowa komórka pamięci i operacje na niej

bufferAddress(65534). % stały adres bufora, po prostu dodatkowy DR

saveToBuffer -->
    { bufferAddress(BAddr) },
    ["SWAPD", "CONST", BAddr, "SWAPA", "SWAPD", "STORE"].

getFromBuffer -->
    { bufferAddress(BAddr) },
    ["CONST", BAddr, "SWAPA", "LOAD"].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% translacja AST na listę komend

operatorCommand(OP) -->
    (
	      { OP == sumOp },   !, ["ADD"]
     ; 	{ OP == substOp }, !, ["SUB"]
     ; 	{ OP == timesOp }, !, ["MUL"]
     ; 	{ OP == divOp },   !, ["DIV"]
     ; 	{ OP == modOp },   !, ["DIV", "CONST", -16, "SWAPD", "SHIFT"]				  
    ).

translateArithmExpr([atomNumber(N)], _) -->
    ["CONST", N], !.

translateArithmExpr([atomVar(Var)], VarList) -->
    { findVarOffset(Var, VarList, Off) },
    loadVariable(Off), !.

translateArithmExpr([minus(atomNumber(N))], _) -->
    ["CONST", N, "SWAPD", "CONST", -1, "MUL"], !.

translateArithmExpr([minus(atomVar(Var))], VarList) -->
    { findVarOffset(Var, VarList, Off) },
    loadVariable(Off),
    ["SWAPD", "CONST", -1, "MUL"], !.

translateArithmExpr([arithmExpr(L, OP, R)], VarList) -->
    translateArithmExpr([L], VarList),
    addToStack,
    translateArithmExpr([R], VarList),
    saveToBuffer,
    getFromStack,
    ["SWAPD"],
    getFromBuffer,
    ["SWAPD"],
    operatorCommand(OP).

translateArithmExpr([minus(arithmExpr(L, OP, R))], VarList) -->
    translateArithmExpr([arithmExpr(L, OP, R)], VarList),
    ["SWAPD", "CONST", -1, "MUL"].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% przestrzeń zmiennych

variableSpaceStart(65533). % początek tej przestrzeni

loadVariable(Nth) -->
    {
	      variableSpaceStart(Start),
	      Addr is Start-Nth
    },
    ["CONST", Addr, "SWAPA", "LOAD"].

readVariables([atomVar(Name)|T], N) -->
    [var(Name, N)],
    { N1 is N+1 },
    readVariables(T, N1), !.
readVariables([], _) --> [].

translateVariables([declsVars(Vars)|Rest], From) -->
    readVariables(Vars, From), !,
    % policz ile otrzymaliśmy zmiennych w tej części deklaracji
    {
	      length(Vars, NVars),
	      N1 is From + NVars
    },
    translateVariables(Rest, N1).
translateVariables([], _) --> [].

findVarOffset(Name, [var(Name, Off)|_], Off) :- !.
findVarOffset(Name, [_|T], Off) :-
    findVarOffset(Name, T, Off).

translateAssgn(instrAssgn(Instr), VarList) -->
    translateAssgnR(Instr, VarList), !.

translateAssgnR([atomVar(Var), AE], VarList) -->
    translateArithmExpr([AE], VarList),
    ["SWAPD"],
    {
	      findVarOffset(Var, VarList, Off),
	      variableSpaceStart(Start),
	      VarAddr is Start - Off
    },
    ["CONST", VarAddr, "SWAPA", "SWAPD", "STORE"].

translateRead(instrRead([atomVar(Var)]), VarList) -->
    {
	      findVarOffset(Var, VarList, Off),
	      variableSpaceStart(Start),
	      Addr is Start - Off
    },
    ["CONST", Addr, "SWAPA", "CONST", 1, "SYSCALL", "STORE"].

translateWrite(instrWrite(AE), VarList) -->
    translateArithmExpr(AE, VarList),
    ["SWAPD", "CONST", 2, "SYSCALL"].


% 1 -> true, 0 -> false
translateLogicalExpr([logicalExpr(L, andOp, R)], VarList) -->
    translateLogicalExpr(L, VarList),
    ["SWAPD"],
    addToStack,
    translateLogicalExpr(R, VarList),
    ["SWAPD"],
    saveToBuffer,
    getFromStack,
    ["SWAPD"],
    getFromBuffer,
    ["MUL", "SWAPD"], !.

translateLogicalExpr([logicalExpr(L, orOp, R)], VarList) -->
    translateLogicalExpr(L, VarList),
    ["SWAPD"],
    addToStack,
    translateLogicalExpr(R, VarList),
    ["SWAPD"],
    saveToBuffer,
    getFromStack,
    ["SWAPD"],
    getFromBuffer,
    ["ADD", "SWAPD"], !.

translateLogicalExpr([logicalExpr(L, OP, R)], VarList) -->
    % sprawdzamy, czy nie ma przepełnienia
    translateArithmExpr([arithmExpr(arithmExpr(L, divOp, atomNumber(2)), substOp,
				                            arithmExpr(R, divOp, atomNumber(2)))], VarList), !,
    ["SWAPD", "CONST", Addr1, "SWAPA", "SWAPD"],
    ["BRANCHZ"],
    ["SWAPD", marker(Addr4), "CONST", Addr2, "SWAPA", "SWAPD"],
    (
	      { OP == ltOp }, !,
	      ["BRANCHN"],
	      ["CONST", 0, "SWAPD", "CONST", Addr3, "JUMP", marker(Addr2)],
	      ["CONST", 1, "SWAPD", "CONST", Addr3, "JUMP"]
     ;  { OP == leqOp }, !,
	      ["BRANCHN", "BRANCHZ"],
	      ["CONST", 0, "SWAPD", "CONST", Addr3, "JUMP", marker(Addr2)],
	      ["CONST", 1, "SWAPD", "CONST", Addr3, "JUMP"]
     ;  { OP == gtOp }, !,
	      ["BRANCHN", "BRANCHZ"],
	      ["CONST", 1, "SWAPD", "CONST", Addr3, "JUMP", marker(Addr2)],
	      ["CONST", 0, "SWAPD", "CONST", Addr3, "JUMP"]
     ;  { OP == geqOp }, !,
	      ["BRANCHN"],
	      ["CONST", 1, "SWAPD", "CONST", Addr3, "JUMP", marker(Addr2)],
	      ["CONST", 0, "SWAPD", "CONST", Addr3, "JUMP"]
     ;  { OP == eqOp }, !,
	      ["BRANCHZ"],
	      ["CONST", 0, "SWAPD", "CONST", Addr3, "JUMP", marker(Addr2)],
	      ["CONST", 1, "SWAPD", "CONST", Addr3, "JUMP"]
     ;  { OP == neqOp }, !,
	      ["BRANCHZ"],
	      ["CONST", 1, "SWAPD", "CONST", Addr3, "JUMP", marker(Addr2)],
	      ["CONST", 0, "SWAPD", "CONST", Addr3, "JUMP"]
    ),
    [marker(Addr1)],
    translateArithmExpr([arithmExpr(L, substOp, R)], VarList),
    ["SWAPD", "CONST", Addr4, "JUMP", marker(Addr3)].


translateLogicalExpr(notCond(LE), VarList) -->
    translateLogicalExpr(LE, VarList),
    ["CONST", 1, "SUB", "SWAPD"], !.

translateIf(instrIf(LE, Instr), VarList) -->
    translateLogicalExpr(LE, VarList),
    ["CONST", Addr, "SWAPA", "SWAPD", "BRANCHZ"],
    translateInstructions(Instr, VarList),
    [marker(Addr)].

translateIfElse(instrIfElse(LE, IF, ELSE), VarList) -->
    translateLogicalExpr(LE, VarList),
    ["CONST", Addr, "SWAPA", "SWAPD", "BRANCHZ"],
    translateInstructions(IF, VarList),
    ["CONST", AfterElse, "JUMP"],
    [marker(Addr)],
    translateInstructions(ELSE, VarList),
    [marker(AfterElse)].

translateWhile(instrWhile(LE, Instr), VarList) -->
    [marker(While)],
    translateLogicalExpr(LE, VarList),
    ["CONST", Addr, "SWAPA", "SWAPD", "BRANCHZ"],
    translateInstructions(Instr, VarList),
    ["CONST", While, "JUMP"],
    [marker(Addr)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

translateProgram(program(_, block(Decls, Instr))) -->
    {
	      phrase(translateVariables(Decls, 0), VarList),
	      length(VarList, NVars),
	      variableSpaceStart(VarStart),
	      lastStackFrame(StackFrame),
	      
	      StackStart is VarStart - NVars
    },
    % zainicjalizuj początek stosu
    ["CONST", StackStart, "SWAPD", "CONST", StackFrame, "SWAPA",
     "SWAPD", "STORE"],
    translateInstructions(Instr, VarList),
    ["CONST", 0, "SYSCALL"].

translateInstructions([H|Rest], VarList) -->
    (
	      translateRead(H, VarList),   !
     ;  translateWrite(H, VarList),  !
     ;  translateAssgn(H, VarList),  !
     ;  translateIf(H, VarList),     !
     ;  translateIfElse(H, VarList), !
     ;  translateWhile(H, VarList),  !
    ),
    translateInstructions(Rest, VarList), !.
translateInstructions([], _) --> [], !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% grupowanie komend w czwórki

addInTheFrontR([], Res, Res) :- !.
addInTheFrontR([H|T], To, Res) :- addInTheFrontR(T, [H|To], Res).

addInTheFront(From, To, Res) :- addInTheFrontR(From, To, Res).

fillWithNOP(Res, 4, Res) :- !.
fillWithNOP(List, N, Res) :-
    N1 is N+1,
    fillWithNOP(["NOP"|List], N1, Res).


makeLinesOfAddresses([], N, N, ResRev, Result) :- reverse(ResRev, Result), !.
makeLinesOfAddresses([H|T], N, AccN, Acc, Result) :-
    Line = line(AccN, [H]),
    N1 is AccN+1,
    makeLinesOfAddresses(T, N, N1, [Line|Acc], Result).

makeLinesOfAddresses(List, N, NewN, Res) :- makeLinesOfAddresses(List, NewN, N, [], Res).

makeLinesOfCommands([], N, 0, AccNum, _, AccRes, Result) :-
    reverse(AccNum, AccNumR),
    makeLinesOfAddresses(AccNumR, N, _, Res1),
    addInTheFront(Res1, AccRes, ResRev),
    reverse(ResRev, Result), !.

makeLinesOfCommands([], N, RestCnt, AccNum, Acc, AccRes, Result) :-
    fillWithNOP(Acc, RestCnt, ResNOP),
    reverse(ResNOP, ResN),
    Line = line(N, ResN),
    N1 is N+1,
    reverse(AccNum, AccNumR),
    makeLinesOfAddresses(AccNumR, N1, _, Res1),
    addInTheFront(Res1, [Line|AccRes], ResRev),
    reverse(ResRev, Result), !.

makeLinesOfCommands([H|T], N, Cnt, AccNum, Acc, AccRes, Result) :-
    H == "JUMP", !,
    (Cnt == 0
     -> NewLine = line(N, ["JUMP", "NOP", "NOP", "NOP"]),
	      N1 is N+1,
	      makeLinesOfCommands(T, N1, 0, [], [], [NewLine|AccRes], Result)
     ;  fillWithNOP(Acc, Cnt, ResNOP),
	      reverse(ResNOP, ResN),
	      Line = line(N, ResN),
	      N1 is N+1,
	      reverse(AccNum, AccNumR),
	      makeLinesOfAddresses(AccNumR, N1, NewN, Res1),
	      addInTheFront(Res1, [Line|AccRes], NewAcc),
	      NewLine = line(NewN, ["JUMP", "NOP", "NOP", "NOP"]),
	      N2 is NewN+1,
	      makeLinesOfCommands(T, N2, 0, [], [], [NewLine|NewAcc], Result)).

makeLinesOfCommands(List, N, 4, AccNum, [H|T], AccRes, Result) :-
    not(H == "CONST"), !,
    reverse([H|T], Res1),
    Line = line(N, Res1),
    N1 is N+1,
    reverse(AccNum, AccNumR),
    makeLinesOfAddresses(AccNumR, N1, NewN, Res2),
    addInTheFront(Res2, [Line|AccRes], NewRes),
    makeLinesOfCommands(List, NewN, 0, [], [], NewRes, Result).

makeLinesOfCommands([H|T], N, 4, AccNum, Acc, AccRes, Result) :-
    reverse(Acc, Res1),
    Line = line(N, Res1),
    N1 is N+1,
    reverse([H|AccNum], AccNumR),
    makeLinesOfAddresses(AccNumR, N1, NewN, Res2),
    addInTheFront(Res2, [Line|AccRes], NewRes),
    makeLinesOfCommands(T, NewN, 0, [], [], NewRes, Result), !.


makeLinesOfCommands([H|T], N, Cnt, AccNum, Acc, AccRes, Result) :-
    string(H), !,
    NewCnt is Cnt+1,
    makeLinesOfCommands(T, N, NewCnt, AccNum, [H|Acc], AccRes, Result).

makeLinesOfCommands([H|T], N, Cnt, AccNum, Acc, AccRes, Result) :-
    var(H), !,
    makeLinesOfCommands(T, N, Cnt, [H|AccNum], Acc, AccRes, Result).

makeLinesOfCommands([H|T], N, Cnt, AccNum, Acc, AccRes, Result) :-
    H = marker(NewN), !,
    (Cnt == 0
     ->  NewN = N,
	       makeLinesOfCommands(T, NewN, 0, [], [], AccRes, Result)
     ;   fillWithNOP(Acc, Cnt, ResNOP),
	       reverse(ResNOP, ResN),
	       Line = line(N, ResN),
	       N1 is N+1,
	       reverse(AccNum, AccNumR),
	       makeLinesOfAddresses(AccNumR, N1, NewN, Res1),
	       addInTheFront(Res1, [Line|AccRes], ResRev),
	       makeLinesOfCommands(T, NewN, 0, [], [], ResRev, Result)
    ).


makeLinesOfCommands([H|T], N, Cnt, AccNum, Acc, AccRes, Result) :-
    makeLinesOfCommands(T, N, Cnt, [H|AccNum], Acc, AccRes, Result).

makeLinesOfCommands(List, Result) :- makeLinesOfCommands(List, 0, 0, [], [], [], Result).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

commandToHex([], Acc, Res) :- reverse(Acc, Res).
commandToHex([H|T], Acc, Res) :- phrase(convertToHex(Hex), [H]),
				                         commandToHex(T, [Hex|Acc], Res).
commandToHex(List, HexList) :- commandToHex(List, [], HexList).

hexToDecimal([], Res, Res) :- !.
hexToDecimal([H|T], Acc, Res) :- X is Acc * 16,
				                         Y is H + X,
				                         hexToDecimal(T, Y, Res).

hexToDecimal(Command, Hex) :- hexToDecimal(Command, 0, Hex).

negativeToPositive(Number, Result) :- Number < 0, !,
				                              Result is Number + 65536.
negativeToPositive(Number, Number).

convertToHex(Result) --> (
			                       ["NOP"],     !, { Result = 0 }
			                    ;  ["SYSCALL"], !, { Result = 1 }
			                    ;  ["LOAD"],    !, { Result = 2 }
			                    ;  ["STORE"],   !, { Result = 3 }
			                    ;  ["SWAPA"],   !, { Result = 4 }
			                    ;  ["SWAPD"],   !, { Result = 5 }
			                    ;  ["BRANCHZ"], !, { Result = 6 }
			                    ;  ["BRANCHN"], !, { Result = 7 }
			                    ;  ["JUMP"],    !, { Result = 8 }
			                    ;  ["CONST"],   !, { Result = 9 }
			                    ;  ["ADD"],     !, { Result = 10 }
			                    ;  ["SUB"],     !, { Result = 11 }
			                    ;  ["MUL"],     !, { Result = 12 }
			                    ;  ["DIV"],     !, { Result = 13 }
			                    ;  ["SHIFT"],   !, { Result = 14 }
			                    ;  ["NAND"],    !, { Result = 15 }
			                   ).

commandsToHexToDecimal([], Acc, Result) :- reverse(Acc, Result).

commandsToHexToDecimal([line(_, [H])|T], Acc, Result) :-
    number(H), !,
    negativeToPositive(H, Pos),
    commandsToHexToDecimal(T, [Pos|Acc], Result).

commandsToHexToDecimal([line(_, List)|T], Acc, Result) :-
    commandToHex(List, [], Hex),
    hexToDecimal(Hex, Decimal),
    commandsToHexToDecimal(T, [Decimal|Acc], Result).

commandsToHexToDecimal(Commands, Decimal) :- commandsToHexToDecimal(Commands, [], Decimal).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dodatkowe predykaty do odczytu i zapisu do pliku
dumpListToFile(List, FileName) :- open(FileName, write, Stream),
				                          listToStream(List, Stream),
				                          close(Stream).

listToStream([], _).
listToStream([H|T], Stream) :- write(Stream, H),
			                         write(Stream, '\n'),
			                         listToStream(T, Stream).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

readProgramToListOfChars(FileName, Result) :-
    open(FileName, read, Stream),
    phrase(readFromFile(Stream), Result),
    close(Stream).

readFromFile(Stream) -->
    { get_code(Stream, CH) },
    (
	      { char_type(CH, ascii) }, !,
	      [CH], readFromFile(Stream)
     ;  { char_type(CH, end_of_file) },
	      []
    ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

