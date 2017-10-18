(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(*                                                             *)
(* Created by Luca Cardelli                                    *)
(* Last modified on Sat Aug 15 19:50:52 PDT 1998 by heydon     *)

INTERFACE Scanner;
IMPORT Err, String, Rd;

TYPE
  Keyword <: ROOT;
  KeywordSet <: ROOT;

VAR
  scanPoint: INTEGER;

PROCEDURE Setup() RAISES ANY;
(* To be called before any other use of this module. *)

PROCEDURE Clear() RAISES ANY;
(* Clean up the scanner state, but preserve the input file queue. *)

PROCEDURE Reset() RAISES ANY;
(* Reinitialize the scanner state. *)

(* === Source === *)

PROCEDURE PushInput(fileName: TEXT; rd: Rd.T) RAISES ANY;
(* Switch the scanner input to a new reader. *)

PROCEDURE PopInput() RAISES ANY;
(* Switch back to the previous reader. *)

PROCEDURE EnqueueInput(fileName: TEXT) RAISES ANY;
(* Enqueue a file as the scanner input (after all the existing ones). *)

PROCEDURE CurrentLocationInfo(VAR(*out*) info: Err.LocationInfo) RAISES ANY;
(* Get the fileName and position of the current input reader. *)

PROCEDURE SetCharNo(charNo, lineNo, lineCharNo: INTEGER) RAISES ANY;
(* Set the charNo counter associtated with the current input reader to the
   given number, for error reporting purposes. The charNo is initialized to 0
   and incremented every time a new character is scanned. Similaraly,
   lineNo is incremented at each line, and lineCharNo is incremented at
   each character and reset to 0 at each line. *)

PROCEDURE TopLevel(): BOOLEAN RAISES ANY;
(* Whether the scanner is reading from the top level or a file. *)

PROCEDURE FlushInput() RAISES ANY;
(* Flush the pending input (for top-level use) *)

PROCEDURE SetPrompt(firstPrompt, nextPrompt: TEXT) RAISES ANY;
(* Set the prompt strings. The firstPrompt will be generated once, then the
   nextPrompt all the other times, until FirstPrompt is called. *)

PROCEDURE FirstPrompt() RAISES ANY;
(* Reset the first prompt (see SetPrompt). *)

(* === Keywords === *)

PROCEDURE GetKeywordSet(): KeywordSet RAISES ANY;
(* Get the current keyword set (initially an empty one). *)

PROCEDURE NewKeywordSet(): KeywordSet RAISES ANY;
(* Create a new keywordSet. *)

PROCEDURE CopyKeywordSet(keywordSet: KeywordSet): KeywordSet RAISES ANY;
(* Create a copy of keywordSet. *)

PROCEDURE UseKeywordSet(keywordSet: KeywordSet) RAISES ANY;
(* From now on, use this keywordSet for scanning identifiers. *)

PROCEDURE BeKeyword(ide: TEXT; keywordSet: KeywordSet): Keyword RAISES ANY;
(* From now on, the identifier "ide" is a keyword member of keywordSet.
   The (new or existing) keyword is returned. *)

PROCEDURE GetKeyword(ide: TEXT; keywordSet: KeywordSet): Keyword RAISES ANY;
(* Returns the keyword of identifier "ide" from keywordSet, or NIL
   if it does not exist. *)

(* === Token primitives === *)

PROCEDURE IsDelimiter(char: CHAR): BOOLEAN RAISES ANY;
(* Whether char is a legal DelimCase token. *)

PROCEDURE IsIdentifier(string: String.T): BOOLEAN RAISES ANY;
(* Whether string is a legal IdeCase or InfixCase token (including keywords) *)

PROCEDURE GetTokenChar(VAR (*out*) char: CHAR): BOOLEAN RAISES ANY;
(* Returns the value of the next (char) token (or FALSE) *)

PROCEDURE GetTokenNat(VAR (*out*) nat: CARDINAL): BOOLEAN RAISES ANY;
(* Returns the value of the next (natural) token (or FALSE) *)

PROCEDURE GetTokenInt(VAR (*out*) int: INTEGER): BOOLEAN RAISES ANY;
(* Returns the value of the next (integer) token (or FALSE) *)

PROCEDURE GetTokenReal(VAR (*ou*) real: REAL): BOOLEAN RAISES ANY;
(* Returns the value of the next (real) token (or FALSE) *)

PROCEDURE GetTokenString(VAR (*out*) string: String.T): BOOLEAN RAISES ANY;
(* Returns the value of the next (string) token (or FALSE) *)

PROCEDURE GetTokenIde(VAR (*ou*) ide: TEXT): BOOLEAN RAISES ANY;
(* Returns the value of the next non-keyword identifier (or FALSE) *)

PROCEDURE GetTokenName(VAR (*ou*) text: TEXT): BOOLEAN RAISES ANY;
(* Returns the value of the next keyword or non-keyword identifier 
  (or FALSE) *)

PROCEDURE HaveTokenIde(ide: TEXT): BOOLEAN RAISES ANY;
(* Tests the presence of a given non-keyword identifier. 
   Returns FALSE if not found *)

PROCEDURE HaveTokenKey(key: Keyword): BOOLEAN RAISES ANY;
(* Tests the presence of a given keyword. Returns FALSE if not found *)

PROCEDURE HaveTokenName(text: TEXT): BOOLEAN RAISES ANY;
(* Tests the presence of a given keyword or non-keyword identifer. 
   Returns FALSE if not found *)

PROCEDURE HaveTokenDelim(delim: CHAR): BOOLEAN RAISES ANY;
(* Tests the presence of a given delimiter. Returns FALSE if not found *)

(* === Error Messages === *)

TYPE ErrorReportStyle = {LinePlusChar, CharRange};
VAR errorReportStyle:= ErrorReportStyle.LinePlusChar;
(* Whether Syntax should report error positions by line number plus char in 
   line, or by range of characters from the beginning of file. *)

PROCEDURE PrintContext() RAISES ANY;
(* Print the current syntactic context (the input stream around the
   current scanner position). *)

PROCEDURE Error(msg: TEXT := "") RAISES ANY;
(* Print msg (followed by newline if non-empty), flush the input (if
   top-level), reset the scanner, and raise Err.Fail *)

PROCEDURE Syntax(cause: TEXT := ""; culprit: TEXT := "") RAISES ANY;
(* A more elaborate version of "Error". It uses PrintLocation and PrintContext
   to show the error context. Prints error position according to the current 
   errorReportStyle. *)

END Scanner.
