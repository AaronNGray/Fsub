(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

INTERFACE Parse;
IMPORT Err, String, Scanner;

TYPE

  Tree <: Located;

  Grammar <: Located;

  Located =
    BRANDED OBJECT
      location: Err.Location := NIL;
    END;

  GrammarEnvRoot <: ROOT;

  GrammarEnv =
    GrammarEnvRoot BRANDED OBJECT
    METHODS
      Lookup(name: TEXT): Grammar RAISES ANY;
      Add(name: TEXT; grammar: Grammar) RAISES ANY;
      UndoAdd(name: TEXT) RAISES ANY;
      Extend(name: TEXT; grammar: Grammar) RAISES ANY;
      UndoExtend(name: TEXT; grammar: Grammar) RAISES ANY;
      ExtendIter(name: TEXT; iterPosPresent: BOOLEAN; iterPos: INTEGER;
	grammar: Grammar) RAISES ANY;
      UndoExtendIter(name: TEXT; grammar: Grammar) RAISES ANY;
    END;

  GrammarListRoot <: ROOT;

  GrammarList =
    GrammarListRoot BRANDED OBJECT
      first: Grammar;
      rest: GrammarList;
    END;

  NonTerminal =
    Grammar BRANDED OBJECT
      name: TEXT;
    END;

  Storage =
    Grammar BRANDED OBJECT
      item: Grammar;
      position: INTEGER;
	(* returns a NIL Tree *)
    END;

  Action <: ActionBase;
  ActionBase =
    Grammar BRANDED OBJECT
      grammar: Grammar;
    METHODS
      Build(base: INTEGER; READONLY info: Err.LocationInfo): Tree RAISES ANY
	:= BuildNoAction;
    END;

  EnvCapture =
    Grammar BRANDED OBJECT
      grammar: Grammar;
    METHODS
      Build(base: INTEGER; env: GrammarEnv; READONLY info: Err.LocationInfo)
        : Tree RAISES ANY := BuildNoEnvCapture;
    END;

  GivenKeyword <: GivenKeywordBase;
  GivenKeywordBase =
    Grammar BRANDED OBJECT
      key: Scanner.Keyword;
    METHODS
      Build(READONLY info: Err.LocationInfo): Tree RAISES ANY
	:= BuildNoGivenKeyword;
    END;

  GivenIdentifier =
    Grammar BRANDED OBJECT
      ide: TEXT;
    METHODS
      Build(READONLY info: Err.LocationInfo): Tree RAISES ANY
	:= BuildNoGivenIdentifier;
    END;

  GivenName =
    Grammar BRANDED OBJECT
      text: TEXT;
    METHODS
      Build(READONLY info: Err.LocationInfo): Tree RAISES ANY
	:= BuildNoGivenName;
    END;

  GivenDelimiter =
    Grammar BRANDED OBJECT
      delim: CHAR;
    METHODS
      Build(READONLY info: Err.LocationInfo): Tree RAISES ANY
	:= BuildNoGivenDelimiter;
    END;

  Identifier =
    Grammar BRANDED OBJECT
    METHODS
      Build(name: TEXT; READONLY info: Err.LocationInfo): Tree RAISES ANY
	:= BuildNoIdentifier;
    END;

  Name =
    Grammar BRANDED OBJECT
    METHODS
      Build(name: TEXT; READONLY info: Err.LocationInfo): Tree RAISES ANY
	:= BuildNoName;
    END;

  QuotedChar =
    Grammar BRANDED OBJECT
    METHODS
      Build(char: CHAR; READONLY info: Err.LocationInfo): Tree RAISES ANY
	:= BuildNoQuotedChar;
    END;

  Integer =
    Grammar BRANDED OBJECT
    METHODS
      Build(int: INTEGER; READONLY info: Err.LocationInfo): Tree RAISES ANY
	:= BuildNoInteger;
    END;

  Real =
    Grammar BRANDED OBJECT
    METHODS
      Build(real: REAL; READONLY info: Err.LocationInfo): Tree RAISES ANY
	:= BuildNoReal;
   END;

  QuotedString =
    Grammar BRANDED OBJECT
    METHODS
      Build(string: String.T; READONLY info: Err.LocationInfo): Tree RAISES ANY
	:= BuildNoQuotedString;
    END;

  Sequence =
    Grammar BRANDED OBJECT
      items: GrammarList
	(* returns a NIL Tree *)
    END;

  Choice =
    Grammar BRANDED OBJECT
      choice: GrammarList;
	(* returns what the succesful choice returns *)
    END;

  Iter =
    Grammar BRANDED OBJECT
      accum: BOOLEAN;
      accumPosition: INTEGER;
      base, iter: Grammar;
	(* returns what the accumlated iterations returns *)
    END;
    (* Iter(a,b) produces the left-associative parse trees:    
	a, ab, (ab)b, ((ab)b)b, ...
       which are not otherwise expressible.
       Ex:
	Iter(ide,"'")
		ide, ide', (ide')', ...
	Iter(ide,["("ide")"])
		ide, ide(ide), (ide(ide))(ide), ...
	Iter(ide,["-"ide])
		ide, ide"-"ide, (ide-ide)-ide, ...
	Iter([], "$")
		$, ($$), (($$)$), ...
	Iter(ide, [";" ide])
		ide, ide;ide, (ide;ide);ide, ...
    *)

VAR Stack: ARRAY[0..1023] OF Tree;

PROCEDURE Setup() RAISES ANY;
(* To be called before any other use of this module. *)

PROCEDURE NewEnv():  GrammarEnv RAISES ANY;

PROCEDURE List(item1,item2,item3,item4,item5,item6,item7,item8,
    item9, item10, item11, item12, item13, item14, item15, item16,
    item17, item18, item19, item20: Grammar:=NIL; 
  rest: GrammarList:=NIL): GrammarList RAISES ANY;

PROCEDURE Store(position: INTEGER; grammar: Grammar): Grammar RAISES ANY;

PROCEDURE Read(gram: Grammar; env: GrammarEnv; base: INTEGER:=0)
  : Tree RAISES ANY;
  (* Parse according to the given gram/env. If parsing fails 
     gives an error. A non-zero base is for internal use only. *)

(* Default methods returning NIL *)
PROCEDURE BuildNoAction(self: ActionBase; base: INTEGER; READONLY info: Err.LocationInfo): Tree RAISES ANY;
PROCEDURE BuildNoEnvCapture(self: EnvCapture; base: INTEGER; env: GrammarEnv; READONLY info: Err.LocationInfo): Tree RAISES ANY;
PROCEDURE BuildNoGivenKeyword(self: GivenKeywordBase; READONLY info: Err.LocationInfo): Tree RAISES ANY;
PROCEDURE BuildNoGivenIdentifier(self: GivenIdentifier; READONLY info: Err.LocationInfo): Tree RAISES ANY;
PROCEDURE BuildNoGivenName(self: GivenName; READONLY info: Err.LocationInfo): Tree RAISES ANY;
PROCEDURE BuildNoGivenDelimiter(self: GivenDelimiter; READONLY info: Err.LocationInfo): Tree RAISES ANY;
PROCEDURE BuildNoIdentifier(self: Identifier; name: TEXT; READONLY info: Err.LocationInfo): Tree RAISES ANY;
PROCEDURE BuildNoName(self: Name; name: TEXT; READONLY info: Err.LocationInfo): Tree RAISES ANY;
PROCEDURE BuildNoQuotedChar(self: QuotedChar; char: CHAR; READONLY info: Err.LocationInfo): Tree RAISES ANY;
PROCEDURE BuildNoInteger(self: Integer; int: INTEGER; READONLY info: Err.LocationInfo): Tree RAISES ANY;
PROCEDURE BuildNoReal(self: Real; real: REAL; READONLY info: Err.LocationInfo): Tree RAISES ANY;
PROCEDURE BuildNoQuotedString(self: QuotedString; string: String.T; READONLY info: Err.LocationInfo): Tree RAISES ANY;

END Parse.
