(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(*                                                             *)
(* Created by Luca Cardelli                                    *)
(* Last modified on Sun Aug 16 12:02:08 PDT 1998 by heydon     *)

MODULE Parse;
IMPORT TextRefTbl, String, Out, Fmt, Formatter, Err, Scanner;

REVEAL 
  GrammarEnvRoot = 
    BRANDED "GrammarEnvRoot" OBJECT
      table: TextRefTbl.T;
    END;

VAR failedName: TEXT;

PROCEDURE Setup() =
  BEGIN 
    failedName:="";
  END Setup;

(* Default methods returning NIL *)
PROCEDURE BuildNoAction(<*UNUSED*> self: ActionBase; <*UNUSED*> base: INTEGER; <*UNUSED*> READONLY info: Err.LocationInfo): Tree = BEGIN RETURN NIL END BuildNoAction;
PROCEDURE BuildNoEnvCapture(<*UNUSED*> self: EnvCapture; <*UNUSED*> base: INTEGER; <*UNUSED*> env: GrammarEnv; <*UNUSED*> READONLY info: Err.LocationInfo): Tree = BEGIN RETURN NIL END BuildNoEnvCapture;
PROCEDURE BuildNoGivenKeyword(<*UNUSED*> self: GivenKeywordBase; <*UNUSED*> READONLY info: Err.LocationInfo): Tree = BEGIN RETURN NIL END BuildNoGivenKeyword;
PROCEDURE BuildNoGivenIdentifier(<*UNUSED*> self: GivenIdentifier; <*UNUSED*> READONLY info: Err.LocationInfo): Tree = BEGIN RETURN NIL END BuildNoGivenIdentifier;
PROCEDURE BuildNoGivenName(<*UNUSED*> self: GivenName; <*UNUSED*> READONLY info: Err.LocationInfo): Tree = BEGIN RETURN NIL END BuildNoGivenName;
PROCEDURE BuildNoGivenDelimiter(<*UNUSED*> self: GivenDelimiter; <*UNUSED*> READONLY info: Err.LocationInfo): Tree = BEGIN RETURN NIL END BuildNoGivenDelimiter;
PROCEDURE BuildNoIdentifier(<*UNUSED*> self: Identifier; <*UNUSED*> name: TEXT; <*UNUSED*> READONLY info: Err.LocationInfo): Tree = BEGIN RETURN NIL END BuildNoIdentifier;
PROCEDURE BuildNoName(<*UNUSED*> self: Name; <*UNUSED*> name: TEXT; <*UNUSED*> READONLY info: Err.LocationInfo): Tree = BEGIN RETURN NIL END BuildNoName;
PROCEDURE BuildNoQuotedChar(<*UNUSED*> self: QuotedChar; <*UNUSED*> char: CHAR; <*UNUSED*> READONLY info: Err.LocationInfo): Tree = BEGIN RETURN NIL END BuildNoQuotedChar;
PROCEDURE BuildNoInteger(<*UNUSED*> self: Integer; <*UNUSED*> int: INTEGER; <*UNUSED*> READONLY info: Err.LocationInfo): Tree = BEGIN RETURN NIL END BuildNoInteger;
PROCEDURE BuildNoReal(<*UNUSED*> self: Real; <*UNUSED*> real: REAL; <*UNUSED*> READONLY info: Err.LocationInfo): Tree = BEGIN RETURN NIL END BuildNoReal;
PROCEDURE BuildNoQuotedString(<*UNUSED*> self: QuotedString; <*UNUSED*> string: String.T; <*UNUSED*> READONLY info: Err.LocationInfo): Tree = BEGIN RETURN NIL END BuildNoQuotedString;

  PROCEDURE Read(gram: Grammar; env: GrammarEnv; base: INTEGER:=0)
      : Tree RAISES ANY =
    VAR max: INTEGER; tree: Tree; failed: Grammar;
    BEGIN
      max:=0;
      tree:=Read1(gram, env, base, (*in-out*)max, (*out*)failed);
      IF failed#NIL THEN 
	Reset(base+max); Error(failed); Scanner.Syntax();
      END;
      RETURN tree;
    END Read;

  (* To be called when Read fails to reset the parse state without
     giving an error message. Set stackLevel=base+max, for the base given
     to, and the max returned by, Read. *)
  PROCEDURE Reset(stackLevel: INTEGER) =
    BEGIN
      FOR i:=0 TO stackLevel DO Stack[i]:=NIL END;
    END Reset;

  (* To be called when Read fails, to give an error message. 
     Should be followed by Scanner.Syntax(). *)
  PROCEDURE Error(failed: Grammar) RAISES ANY =
    VAR info: Err.LocationInfo;
    BEGIN
      Scanner.CurrentLocationInfo((*out*)info);
      Formatter.PutText(Out.out, "Parsing "&failedName&" "); 
	Err.PrintLocation(Out.out, failed.location, info.line);
	Formatter.PutChar(Out.out, '\n');
      Formatter.Flush(Out.out);
    END Error;

PROCEDURE NewEnv(): GrammarEnv =
  BEGIN
    RETURN NEW(GrammarEnv, table:=NEW(TextRefTbl.Default).init(),
	Lookup:=Lookup, Add:=Add, UndoAdd:=UndoAdd,
	Extend:=Extend, UndoExtend:=UndoExtend,
	ExtendIter:=ExtendIter, UndoExtendIter:=UndoExtendIter);
  END NewEnv;

PROCEDURE List(item1,item2,item3,item4,item5,item6,item7,item8, 
    item9, item10, item11, item12, item13, item14, item15, item16,
    item17, item18, item19, item20: Grammar:=NIL; 
  rest: GrammarList:=NIL): GrammarList =
  VAR list: GrammarList;
  BEGIN
    list:=rest;
    IF item20#NIL THEN list:=NEW(GrammarList, first:=item20, rest:=list) END;
    IF item19#NIL THEN list:=NEW(GrammarList, first:=item19, rest:=list) END;
    IF item18#NIL THEN list:=NEW(GrammarList, first:=item18, rest:=list) END;
    IF item17#NIL THEN list:=NEW(GrammarList, first:=item17, rest:=list) END;
    IF item16#NIL THEN list:=NEW(GrammarList, first:=item16, rest:=list) END;
    IF item15#NIL THEN list:=NEW(GrammarList, first:=item15, rest:=list) END;
    IF item14#NIL THEN list:=NEW(GrammarList, first:=item14, rest:=list) END;
    IF item13#NIL THEN list:=NEW(GrammarList, first:=item13, rest:=list) END;
    IF item12#NIL THEN list:=NEW(GrammarList, first:=item12, rest:=list) END;
    IF item11#NIL THEN list:=NEW(GrammarList, first:=item11, rest:=list) END;
    IF item10#NIL THEN list:=NEW(GrammarList, first:=item10, rest:=list) END;
    IF item9#NIL THEN list:=NEW(GrammarList, first:=item9, rest:=list) END;
    IF item8#NIL THEN list:=NEW(GrammarList, first:=item8, rest:=list) END;
    IF item7#NIL THEN list:=NEW(GrammarList, first:=item7, rest:=list) END;
    IF item6#NIL THEN list:=NEW(GrammarList, first:=item6, rest:=list) END;
    IF item5#NIL THEN list:=NEW(GrammarList, first:=item5, rest:=list) END;
    IF item4#NIL THEN list:=NEW(GrammarList, first:=item4, rest:=list) END;
    IF item3#NIL THEN list:=NEW(GrammarList, first:=item3, rest:=list) END;
    IF item2#NIL THEN list:=NEW(GrammarList, first:=item2, rest:=list) END;
    IF item1#NIL THEN list:=NEW(GrammarList, first:=item1, rest:=list) END;
    RETURN list;
  END List;

  PROCEDURE Store(position: INTEGER; grammar: Grammar): Grammar =
  BEGIN
    RETURN NEW(Storage, item:=grammar, position:=position);
  END Store;

  PROCEDURE Lookup(env: GrammarEnv; name: TEXT): Grammar RAISES ANY =
  VAR ref: REFANY; gram: Grammar; BEGIN
    IF env.table.get(name, (*OUT*) ref)
      THEN gram := NARROW(ref, Grammar)
      ELSE Scanner.Syntax("Unbound non-terminal: "&name);
    END;
    RETURN gram;
  END Lookup;

  PROCEDURE Add(env: GrammarEnv; name: TEXT; grammar: Grammar) RAISES ANY =
  VAR value: REFANY;
  BEGIN
    IF env.table.get(name, (*OUT*) value) THEN 
	Scanner.Syntax("Duplicated non-terminal: "&name);
    END;
    EVAL env.table.put(name, grammar);
  END Add;

  PROCEDURE UndoAdd(env: GrammarEnv; name: TEXT) RAISES ANY =
  VAR value: REFANY;
  BEGIN
    IF NOT env.table.delete(name, (*OUT*) value) THEN
      Scanner.Error("GrammarEnv.UndoAdd: could not find: "&name) 
    END;
  END UndoAdd;

  PROCEDURE Extend(env: GrammarEnv; name: TEXT; grammar: Grammar) RAISES ANY =
  VAR value: REFANY;
  BEGIN
    IF NOT env.table.get(name, (*OUT*) value) THEN 
      Scanner.Syntax("Could not find non-terminal to extend: "&name);
    END;
    TYPECASE grammar OF
    | Choice(node) => IF node.choice=NIL THEN RETURN END
(* -- attempt to allow redefinition of grammar extension; 
      (should disable the error message in Add to do this test). 
      After redefinition gave syntax error for non-undestood reasons.
    | NonTerminal(node) =>
	IF env.table.in(node.name, (*out*)oldValue) THEN
	  Err.Msg(Out.out, "Redefining " & node.name);
	  EVAL env.table.put(name, grammar);
	  RETURN
	END;
 *)
    ELSE
    END;
    EVAL 
      env.table.put(name, 
        NEW(Choice,
	  choice:=
	    NEW(GrammarList, first:=value, rest:=
	    NEW(GrammarList, first:=grammar, rest:=
	    NIL))));
  END Extend;

  PROCEDURE UndoExtend(env: GrammarEnv; name: TEXT; grammar: Grammar)
    RAISES ANY =
  VAR value: REFANY;
  BEGIN
    IF NOT env.table.get(name, (*OUT*) value) THEN 
      Scanner.Error("GrammarEnv.UndoExtend: could not find: "&name) 
    END;
    TYPECASE grammar OF
    | Choice(node) => IF node.choice=NIL THEN RETURN END
    ELSE
    END;
    TYPECASE value OF
    | Choice(node) =>
	IF grammar # node.choice.rest.first THEN
          Scanner.Error("GrammarEnv.UndoExtend: bad undo: "&name);
	END;
        EVAL env.table.put(name, node.choice.first);
    ELSE Scanner.Error("GrammarEnv.UndoExtend failed: "&name);
    END;
  END UndoExtend;

  PROCEDURE ExtendIter(env: GrammarEnv; name: TEXT; 
    iterPosPresent: BOOLEAN; iterPos: INTEGER; grammar: Grammar)
    RAISES ANY =
  VAR value: REFANY;
  BEGIN
    IF NOT env.table.get(name, (*OUT*) value) THEN 
      Scanner.Syntax("Could not find non-terminal to extend: "&name);
    END;
    TYPECASE value OF
    | Iter(node) =>
	IF iterPosPresent AND (iterPos#node.accumPosition) THEN
	  Scanner.Syntax("Does not mach iteration position: _"&Fmt.Int(iterPos));
	END;
	node.iter :=
          NEW(Choice, choice:=
	    NEW(GrammarList, first:=node.iter, rest:=
	    NEW(GrammarList, first:=grammar, rest:=
	    NIL)));
    ELSE
      Scanner.Syntax("Not a grammar iteration: "&name);
    END;
  END ExtendIter;

  PROCEDURE UndoExtendIter(env: GrammarEnv; name: TEXT; grammar: Grammar)
    RAISES ANY =
  VAR value: REFANY;
  BEGIN
    IF NOT env.table.get(name, (*OUT*) value) THEN 
      Scanner.Error("GrammarEnv.UndoExtendIter: could not find: "&name) 
    END;
    TYPECASE value OF
    | Iter(iterNode) =>
	TYPECASE iterNode.iter OF
	| Choice(choiceNode) =>
	    IF grammar # choiceNode.choice.rest.first THEN
              Scanner.Error("GrammarEnv.UndoExtendIter: bad undo: "&name);
	    END;
            iterNode.iter:=choiceNode.choice.first;
	ELSE Scanner.Error("GrammarEnv.UndoExtendIter failed: "&name);
	END;
    ELSE Scanner.Error("GrammarEnv.UndoExtendIter failed: "&name);
    END;
  END UndoExtendIter;

  (* Parse according to the given gram/env. The base should
     be the current stack level (usually 0); max should be 0.
     If parsing fails it returns failed#NIL; then Reset
     should be called, followed by either "Scanner.Reset()" or 
     "Error(failed); Scanner.Syntax()" *)
  PROCEDURE Read1(
	gram: Grammar; env: GrammarEnv;
	base: INTEGER; VAR (*in-out*) max: INTEGER;
    	VAR (*out*) failed: Grammar; name: TEXT:=NIL): Tree RAISES ANY =
    VAR tree: Tree;
    BEGIN
      TRY
	(* base is in-out so the stack can be cleaned up properly
	   even on Err.Fail exceptions occurring during parsing. *)
	tree:=Read0(gram, env, (*in-out*)base, (*in-out*)max, 
	  (*out*)failed, name);
      EXCEPT Err.Fail =>
	Reset(base+max);
	RAISE Err.Fail;
      END;
      RETURN tree;
    END Read1;

  PROCEDURE Read0(
	gram: Grammar; env: GrammarEnv;
	VAR (*in-out*) base, max: INTEGER;
    	VAR (*out*) failed: Grammar; name: TEXT:=NIL): Tree RAISES ANY =
  (*  A NIL result means that a client Build did not care about
      generating a parse grammar. *)
  VAR tree: Tree;
      ide, text: TEXT; char: CHAR; int: INTEGER; real: REAL; string: String.T;
      list: GrammarList; scanPoint: INTEGER;
      locInfo: Err.LocationInfo;
      saveBase, saveMax: INTEGER;
  BEGIN
    TYPECASE gram OF <*NOWARN*>
    | NonTerminal(node) =>
	saveBase := base; saveMax := max;
	INC(base,max);
	max := 0;
	tree := 
	  Read0(env.Lookup(node.name), env , (*in-out*)base, (*in-out*) max,
	    (*out*) failed, node.name);
	FOR i:=0 TO max-1 DO Stack[base+i]:=NIL END;
	base := saveBase; max := saveMax;
	IF failed#NIL THEN RETURN NIL; END;
	RETURN tree;
    | Storage(node) =>
	tree := 
	  Read0(node.item, env, (*in-out*)base, (*in-out*)max, 
	    (*out*)failed, name);
	IF failed#NIL THEN RETURN NIL END;
	IF node.position<0 THEN
	  Err.Fault(Out.out, "Invalid index: _" & Fmt.Int(node.position));
	END;
	IF Stack[base+node.position] # NIL THEN
	  Err.Fault(Out.out, "Redefinition of: _" & Fmt.Int(node.position));
	END;
	Stack[base+node.position] := tree;
	max := MAX(max, node.position+1);
	RETURN NIL;
    | Action(node) =>
	Scanner.CurrentLocationInfo((*out*)locInfo);
	tree := 
	  Read0(node.grammar, env, (*in-out*)base, (*in-out*)max, 
	    (*out*)failed, name);
	IF failed#NIL THEN RETURN NIL END;
	RETURN node.Build(base, (*in*)locInfo);
    | EnvCapture(node) =>
	Scanner.CurrentLocationInfo((*out*)locInfo);
  	tree := 
	  Read0(node.grammar, env, (*in-out*)base, (*in-out*)max, 
	    (*out*) failed, name); 
	IF failed#NIL THEN RETURN NIL END;
        RETURN node.Build(base, env, (*in*)locInfo);
     | GivenKeyword(node) =>
	Scanner.CurrentLocationInfo((*out*)locInfo);
	IF Scanner.HaveTokenKey(node.key) THEN failed:=NIL
	ELSE failed:=gram; failedName:=name; RETURN NIL;
	END;
	RETURN node.Build((*in*)locInfo);
     | GivenIdentifier(node) =>
	Scanner.CurrentLocationInfo((*out*)locInfo);
	IF Scanner.HaveTokenIde(node.ide) THEN failed:=NIL
	ELSE failed:=gram; failedName:=name; RETURN NIL;
	END;
	RETURN node.Build((*in*)locInfo);
     | GivenName(node) =>
	Scanner.CurrentLocationInfo((*out*)locInfo);
	IF Scanner.HaveTokenName(node.text) THEN failed:=NIL
	ELSE failed:=gram; failedName:=name; RETURN NIL;
	END;
	RETURN node.Build((*in*)locInfo);
     | GivenDelimiter(node) =>
	Scanner.CurrentLocationInfo((*out*)locInfo);
	IF Scanner.HaveTokenDelim(node.delim) THEN failed:=NIL
	ELSE failed:=gram; failedName:=name; RETURN NIL;
	END;
	RETURN node.Build((*in*)locInfo);
    | Identifier(node) =>
	Scanner.CurrentLocationInfo((*out*)locInfo);
	IF Scanner.GetTokenIde((*out*)ide) THEN failed:=NIL
	ELSE failed:=gram; failedName:=name; RETURN NIL;
	END;
	RETURN node.Build(ide, (*in*)locInfo);
    | Name(node) =>
	Scanner.CurrentLocationInfo((*out*)locInfo);
	IF Scanner.GetTokenName((*out*)text) THEN failed:=NIL
	ELSE failed:=gram; failedName:=name; RETURN NIL;
	END;
	RETURN node.Build(text, (*in*)locInfo);
     | QuotedChar(node) =>
	Scanner.CurrentLocationInfo((*out*)locInfo);
	IF Scanner.GetTokenChar((*out*)char) THEN failed:=NIL
	ELSE failed:=gram; failedName:=name; RETURN NIL;
	END;
	RETURN node.Build(char, (*in*)locInfo);
     | Integer(node) =>
	Scanner.CurrentLocationInfo((*out*)locInfo);
	IF Scanner.GetTokenInt((*out*)int) THEN failed:=NIL
	ELSE failed:=gram; failedName:=name; RETURN NIL;
	END;
	RETURN node.Build(int, (*in*)locInfo);
     | Real(node) =>
	Scanner.CurrentLocationInfo((*out*)locInfo);
	IF Scanner.GetTokenReal((*out*)real) THEN failed:=NIL
	ELSE failed:=gram; failedName:=name; RETURN NIL;
	END;
	RETURN node.Build(real, (*in*)locInfo);
     | QuotedString(node) =>
	Scanner.CurrentLocationInfo((*out*)locInfo);
	IF Scanner.GetTokenString((*out*)string) THEN failed:=NIL
	ELSE failed:=gram; failedName:=name; RETURN NIL;
	END;
	RETURN node.Build(string, (*in*)locInfo);
     | Sequence(node) =>
	Read0List(node.items, env, (*in-out*)base, (*in-out*)max, 
	  (*out*) failed, name);
	RETURN NIL;
    | Choice(node) =>
	list := node.choice;
	saveMax := max;
	LOOP
	  IF list=NIL THEN 
	    failed := gram; failedName:=name; RETURN NIL;
	  END;
	  scanPoint := Scanner.scanPoint;
	  tree := 
	    Read0(list.first, env, (*in-out*) base, (*in-out*) max, 
	      (*out*) failed, name);
	  FOR i:=saveMax TO max-1 DO Stack[base+i]:=NIL END;
	  max := saveMax;
	  IF failed=NIL THEN RETURN tree END;
	  IF failed#NIL AND scanPoint#Scanner.scanPoint THEN RETURN NIL END;
	  list := list.rest;
	END;
    | Iter(node) =>
	tree := 
	  Read0(node.base, env, (*in-out*)base, (*in-out*)max, 
	    (*out*) failed, name);
	IF failed#NIL THEN RETURN NIL END;
	IF node.accum THEN
	  IF node.accumPosition<0 THEN
	    Err.Fault(Out.out, "Invalid index: _" 
	      & Fmt.Int(node.accumPosition));
	  END;
	  Stack[base+node.accumPosition] := tree;
	  max := MAX(max, node.accumPosition+1);
	END;
	saveMax := max;
	LOOP
	  scanPoint := Scanner.scanPoint;
	  tree := 
	    Read0(node.iter, env, (*in-out*)base, (*in-out*)max, 
	      (*out*)failed, name);
	  FOR i:=saveMax TO max-1 DO Stack[base+i]:=NIL END;
	  max := saveMax;
	  IF failed#NIL AND scanPoint#Scanner.scanPoint THEN RETURN NIL END;
	  IF failed#NIL THEN
	    failed:=NIL; 
	    IF node.accum THEN
	      RETURN Stack[base+node.accumPosition];
	    ELSE RETURN NIL;
	    END;
	  END;
	  IF node.accum THEN
	    Stack[base+node.accumPosition] := tree;
	  END;
	END;
    END;
  END Read0;

  PROCEDURE Read0List(
	gramList: GrammarList; env: GrammarEnv; 
	VAR (*in-out*) base, max: INTEGER;
	VAR (*out*) failed: Grammar; name: TEXT:=NIL) RAISES ANY =
    VAR tree: Tree;
    BEGIN
      failed := NIL;
      IF gramList=NIL THEN RETURN
      ELSE
        tree := 
	  Read0(gramList.first, env, (*in-out*)base, (*in-out*)max, 
	    (*out*)failed, name);
	IF failed#NIL THEN RETURN 
	ELSE 
	  Read0List(gramList.rest, env, (*in-out*)base, (*in-out*)max, 
	    (*out*)failed, name);
	END;
      END;
    END Read0List;

BEGIN
END Parse.
