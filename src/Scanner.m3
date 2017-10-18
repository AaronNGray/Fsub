(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

MODULE Scanner;
IMPORT KeyTable, Text, String, Wr, Rd, Stdio, Out, Err, FileStream, 
  Fmt, Formatter;

REVEAL
  Keyword =  BRANDED OBJECT name: TEXT; keyword: BOOLEAN END;
  KeywordSet = BRANDED OBJECT table: KeyTable.T END;

TYPE
  Symbol = Keyword;

  CharacterClass =
    {IllegalCharCase, LetterCharCase, DigitCharCase, SpecialCharCase,
     DelimCharCase, ReservedCharCase, BlankCharCase};

  TokenClass =
    {IdeCase, InfixCase, CharCase, IntCase, RealCase, StringCase, DelimCase};

  InputList = REF InputListBase;
  InputListBase = RECORD
    rd: Rd.T;
    fileName: TEXT;
    acceptedCharPos, acceptedLinePos, acceptedLineCharPos: INTEGER;
    acceptedTokenBegPos, acceptedTokenEndPos: INTEGER;
    rest: InputList;
  END;

VAR
  scanBuffer: String.T;
  scanBufferSize: INTEGER;
  charTable: ARRAY CHAR OF CharacterClass;
  keySet: KeywordSet;
  lookAheadReady: BOOLEAN;
  lookAheadChar: CHAR;
  tokenReady: BOOLEAN;
  tokenClass: TokenClass;
  tokenBegPos: INTEGER;
  tokenEndPos: INTEGER;
  tokenChar: CHAR;
  tokenInt: INTEGER;
  tokenReal: REAL;
  tokenString: String.T;
  tokenDelim: CHAR;
  tokenSym: Symbol;
  input: InputList;
  firstPrompt, nextPrompt: TEXT;
  isFirstPrompt: BOOLEAN;

PROCEDURE NewKeywordSet(): KeywordSet RAISES ANY =
  BEGIN
    RETURN NEW(KeywordSet, table:=KeyTable.New());
  END NewKeywordSet;

PROCEDURE CopyKeywordSet(keywordSet: KeywordSet): KeywordSet RAISES ANY =
  VAR newKeySet: KeywordSet; key: TEXT; value: REFANY;
  BEGIN
    newKeySet := NEW(KeywordSet, table:=KeyTable.New());
    EVAL keywordSet.table.enumerate(EnumKeySet, newKeySet, 
		(*out*) key, (*out*) value);
    RETURN newKeySet;
  END CopyKeywordSet;

PROCEDURE EnumKeySet(data: REFANY; key: TEXT; VAR value: REFANY): BOOLEAN RAISES ANY =
  VAR symbol: Symbol; newKeySet: KeywordSet;
  BEGIN
    symbol := NARROW(value, Symbol);
    IF symbol.keyword THEN
      newKeySet := NARROW(data, KeywordSet);
      EVAL newKeySet.table.put(key, symbol);
    END;
    RETURN FALSE;
  END EnumKeySet;

PROCEDURE GetKeywordSet(): KeywordSet RAISES ANY =
  BEGIN
    RETURN keySet;
  END GetKeywordSet;

PROCEDURE UseKeywordSet(keywordSet: KeywordSet) RAISES ANY =
  BEGIN
    keySet := keywordSet;
  END UseKeywordSet;

PROCEDURE BeKeyword(ide: TEXT; keywordSet: KeywordSet): Keyword RAISES ANY =
  VAR value: REFANY; symbol: Symbol;
  BEGIN
    IF keywordSet.table.in(ide, (*VAR OUT*) value) THEN
      symbol := NARROW(value, Symbol);
      symbol.keyword := TRUE;
    ELSE
      symbol := NEW(Symbol, name:=Text.Sub(ide,0,Text.Length(ide)), keyword:=TRUE);
      EVAL keywordSet.table.put(ide, symbol);
    END;
    RETURN symbol;
  END BeKeyword;

PROCEDURE GetKeyword(ide: TEXT; keywordSet: KeywordSet): Keyword RAISES ANY =
  VAR value: REFANY; symbol: Symbol;
  BEGIN
    IF keywordSet.table.in(ide, (*VAR OUT*) value) THEN
      symbol := NARROW(value, Symbol);
      IF symbol.keyword THEN RETURN symbol ELSE RETURN NIL END;
    ELSE RETURN NIL;
    END;
  END GetKeyword;

PROCEDURE IsDelimiter(char: CHAR): BOOLEAN RAISES ANY =
  BEGIN
    RETURN charTable[char] = CharacterClass.DelimCharCase;
  END IsDelimiter;

PROCEDURE IsIdentifier(string: String.T): BOOLEAN RAISES ANY =
  VAR class: CharacterClass; length: INTEGER;
  BEGIN
    length := String.Length(string);
    IF length=0 THEN RETURN FALSE END;
    IF charTable[string[0]]=CharacterClass.LetterCharCase THEN
      FOR i:=0 TO length-1 DO 
	class := charTable[string[i]];     
        IF (class # CharacterClass.LetterCharCase) AND 
	    (class # CharacterClass.DigitCharCase) THEN RETURN FALSE END;
      END;
      RETURN TRUE;
    ELSIF charTable[string[0]]=CharacterClass.SpecialCharCase THEN
      FOR i:=0 TO length-1 DO 
	class := charTable[string[i]];     
        IF (class # CharacterClass.SpecialCharCase) THEN RETURN FALSE END;
      END;
      RETURN TRUE;
    ELSE RETURN FALSE;
    END;
  END IsIdentifier;

PROCEDURE NewInput(fileName: TEXT; rd: Rd.T; rest: InputList): InputList RAISES ANY =
  VAR res: InputList;
  BEGIN
    res := NEW(InputList);
    res^.fileName := fileName;
    res^.rd := rd;
    res^.acceptedCharPos := 0;
    res^.acceptedLinePos := 1;
    res^.acceptedLineCharPos := 0;
    res^.acceptedTokenBegPos := 0;
    res^.acceptedTokenEndPos := 0;
    res^.rest := rest;
    RETURN res;
  END NewInput;

PROCEDURE EnqueueInput(fileName: TEXT) RAISES ANY =
  VAR scan: InputList;
  BEGIN
    IF Text.Empty(input^.fileName) THEN
      input := NewInput(fileName, NIL, input);
      OpenInput();
    ELSE
      scan := input;
      LOOP
        IF Text.Equal(fileName, scan^.fileName) THEN RETURN END;
        IF Text.Empty(scan^.rest^.fileName) THEN EXIT END;
        scan := scan^.rest;
      END;
      scan^.rest := NewInput(fileName, NIL, scan^.rest);
    END;
  END EnqueueInput;

PROCEDURE PushInput(fileName: TEXT; rd: Rd.T) RAISES ANY =
  BEGIN input := NewInput(fileName, rd, input); END PushInput;

PROCEDURE PopInput() RAISES ANY =
  BEGIN
    Rd.Close(input^.rd);
    input := input^.rest;
    IF input = NIL THEN RAISE Rd.EndOfFile END;
    OpenInput();
  END PopInput;

PROCEDURE OpenInput() RAISES ANY =
  BEGIN
    IF input^.rd = NIL THEN
      TRY
        input^.rd :=
          FileStream.OpenRead(input^.fileName);
      EXCEPT
      ELSE Err.Fault(Out.out, "File not found: " & input^.fileName);
      END;
    END;
  END OpenInput;

PROCEDURE CurrentLocationInfo(VAR(*out*) info: Err.LocationInfo) RAISES ANY =
  BEGIN
    info.fileName:=input^.fileName;
    info.char := input^.acceptedCharPos;
    info.line := input^.acceptedLinePos;
    info.lineChar := input^.acceptedLineCharPos;
  END CurrentLocationInfo;

(* --
PROCEDURE BegCharLocation(
    VAR (*out*) fileName: TEXT;
    VAR (*out*) begPos: INTEGER) RAISES ANY =
  VAR tokenClass: TokenClass;
  BEGIN
    tokenClass := LookToken();
    fileName := input^.fileName;
    begPos := tokenBegPos;
  END BegCharLocation;

PROCEDURE EndCharLocation(VAR (*out*) endPos: INTEGER) RAISES ANY =
  BEGIN 
    endPos := input^.acceptedTokenEndPos; 
  END EndCharLocation;

PROCEDURE LineLocation(VAR (*out*) fileName: TEXT;
    VAR (*out*) linePos, lineCharPos: INTEGER) RAISES ANY =
  BEGIN
    fileName := input^.fileName;
    linePos := input^.acceptedLinePos;
    lineCharPos := input^.acceptedLineCharPos;
  END LineLocation;
-- *)

PROCEDURE SetCharNo(charNo, lineNo, lineCharNo: INTEGER) RAISES ANY =
  BEGIN
    input^.acceptedCharPos := charNo;
    input^.acceptedLinePos := lineNo;
    input^.acceptedLineCharPos := lineCharNo;
    input^.acceptedTokenBegPos := charNo;
    input^.acceptedTokenEndPos := charNo;
  END SetCharNo;

PROCEDURE LookChar(): CHAR RAISES ANY =
  VAR char: CHAR;
  BEGIN
    IF lookAheadReady THEN RETURN lookAheadChar END;
    LOOP
      IF Rd.CharsReady(input^.rd) = 0 THEN
        IF isFirstPrompt THEN
          Wr.PutText(Stdio.stdout, firstPrompt);
	  Wr.Flush(Stdio.stdout);
          INC(input^.acceptedCharPos, Text.Length(firstPrompt));
          INC(input^.acceptedLineCharPos, Text.Length(firstPrompt));
          isFirstPrompt := FALSE;
        ELSE
          Wr.PutText(Stdio.stdout, nextPrompt);
	  Wr.Flush(Stdio.stdout);
          INC(input^.acceptedCharPos, Text.Length(nextPrompt));
          INC(input^.acceptedLineCharPos, Text.Length(nextPrompt));
        END;
        EXIT;
      END;
      IF Rd.EOF(input^.rd) THEN PopInput() ELSE EXIT END;
    END;
    char := Rd.GetChar(input^.rd);
    lookAheadChar := char;
    lookAheadReady := TRUE;
    RETURN char;
  END LookChar;

PROCEDURE GetChar(): CHAR RAISES ANY =
  VAR char: CHAR;
  BEGIN
    IF lookAheadReady THEN
      lookAheadReady := FALSE;
      INC(input^.acceptedCharPos);
      INC(input^.acceptedLineCharPos);
      IF lookAheadChar='\n' THEN
	INC(input^.acceptedLinePos);
	input^.acceptedLineCharPos := 0;
      END;
      RETURN lookAheadChar;
    ELSE
      LOOP
        IF Rd.CharsReady(input^.rd) = 0 THEN
          IF isFirstPrompt THEN
            Wr.PutText(Stdio.stdout, firstPrompt);
	    Wr.Flush(Stdio.stdout);
            INC(input^.acceptedCharPos, Text.Length(firstPrompt));
            INC(input^.acceptedLineCharPos, Text.Length(firstPrompt));
            isFirstPrompt := FALSE;
          ELSE
            Wr.PutText(Stdio.stdout, nextPrompt);
 	    Wr.Flush(Stdio.stdout);
            INC(input^.acceptedCharPos, Text.Length(nextPrompt));
            INC(input^.acceptedLineCharPos, Text.Length(nextPrompt));
          END;
          EXIT;
        END;
        IF Rd.EOF(input^.rd) THEN PopInput() ELSE EXIT END;
      END;
      char := Rd.GetChar(input^.rd);
      lookAheadChar := char;
      INC(input^.acceptedCharPos);
      INC(input^.acceptedLineCharPos);
      IF lookAheadChar='\n' THEN
	INC(input^.acceptedLinePos);
	input^.acceptedLineCharPos := 0;
      END;
      RETURN char;
    END;
  END GetChar;

PROCEDURE HaveChar(char: CHAR): BOOLEAN RAISES ANY =
  BEGIN
    IF char = LookChar() THEN
      char := GetChar();
      RETURN TRUE
    ELSE
      RETURN FALSE
    END;
  END HaveChar;

PROCEDURE ScanNat(): CARDINAL RAISES ANY =
  VAR nat: CARDINAL;
  BEGIN
    nat := 0;
    WHILE charTable[LookChar()] = CharacterClass.DigitCharCase DO
      nat := (10 * nat) + (ORD(GetChar()) - ORD('0'));
    END;
    RETURN nat;
  END ScanNat;

PROCEDURE ScanNeg(): INTEGER RAISES ANY =
  VAR neg: INTEGER;
  BEGIN
    neg := 0;
    WHILE charTable[LookChar()] = CharacterClass.DigitCharCase DO
      neg := (10 * neg) - (ORD(GetChar()) - ORD('0'));
    END;
    RETURN neg;
  END ScanNeg;

(* 
PROCEDURE ScanInt(): INTEGER RAISES ANY =
  BEGIN
    IF HaveChar('~') THEN RETURN ScanNeg() ELSE RETURN ScanNat() END;
  END ScanInt;
*)

PROCEDURE ScanNumber(): TokenClass RAISES ANY =
  VAR negative: BOOLEAN; int: INTEGER; real, quot: REAL; ch: CHAR;
  BEGIN
    negative := HaveChar('~');
    IF negative THEN int := ScanNeg() ELSE int := ScanNat() END;
    IF LookChar() = '.' THEN
      ch := GetChar();
      real := FLOAT(int);
      quot := 1.0;
      WHILE charTable[LookChar()] = CharacterClass.DigitCharCase DO
        quot := quot * 10.0;
        IF negative THEN
          real := real - (FLOAT((ORD(GetChar()) - ORD('0'))) / quot);
        ELSE
          real := real + (FLOAT((ORD(GetChar()) - ORD('0'))) / quot);
        END;
      END;
      tokenReal := real;
      RETURN TokenClass.RealCase;
    ELSE
      tokenInt := int;
      RETURN TokenClass.IntCase
    END;
  END ScanNumber;

PROCEDURE DecodeChar(): CHAR RAISES ANY =
  VAR char: CHAR;
  BEGIN
    char := GetChar();
    IF char = '\\' THEN
      char := GetChar();
      IF char = 'n' THEN
        char := VAL(10, CHAR)
      ELSIF char = 't' THEN
        char := VAL(9, CHAR)
      ELSIF char = 'f' THEN
        char := VAL(12, CHAR)
      END;
    END;
    RETURN char;
  END DecodeChar;

PROCEDURE ScanChar(): CHAR RAISES ANY =
  VAR char: CHAR;
  BEGIN
    char := GetChar();
    char := DecodeChar();
    IF GetChar() # '\'' THEN
      Syntax("closing \' expected", "")
    END;
    RETURN char;
  END ScanChar;

PROCEDURE ScanString(): String.T RAISES ANY =
  VAR char: CHAR; string: String.T;
  BEGIN
    char := GetChar();
    WHILE LookChar() # '\"' DO
      char := DecodeChar();
      String.SetChar(scanBuffer, scanBufferSize, char);
      scanBufferSize := scanBufferSize + 1;
    END;
    char := GetChar();
    string := String.GetSub(scanBuffer, 0, scanBufferSize);
    scanBufferSize := 0;
    RETURN string;
  END ScanString;

PROCEDURE ScanAlphaNumIde() RAISES ANY =
  VAR class: CharacterClass;
  BEGIN
    LOOP
      class := charTable[LookChar()];
      IF (class # CharacterClass.LetterCharCase) AND (class # CharacterClass.DigitCharCase) THEN EXIT END;
      String.SetChar(scanBuffer, scanBufferSize, GetChar());
      scanBufferSize := scanBufferSize + 1;
    END;
  END ScanAlphaNumIde;

PROCEDURE ScanSpecialIde() RAISES ANY =
  BEGIN
    WHILE charTable[LookChar()] = CharacterClass.SpecialCharCase DO
      String.SetChar(scanBuffer, scanBufferSize, GetChar());
      scanBufferSize := scanBufferSize + 1;
    END;
  END ScanSpecialIde;

PROCEDURE ScanComment() RAISES ANY =
  VAR level: CARDINAL; char: CHAR;
  BEGIN
    level := 1;
    WHILE 0 < level DO
      char := GetChar();
      IF char = '*' THEN
        IF LookChar() = ')' THEN char := GetChar(); level := level - 1 END;
      ELSIF char = '(' THEN
        IF LookChar() = '*' THEN char := GetChar(); level := level + 1 END;
      END;
    END
  END ScanComment;

PROCEDURE NextToken(): TokenClass RAISES ANY =
  VAR char: CHAR; class: CharacterClass; tokenClass: TokenClass;
  BEGIN
    WHILE charTable[LookChar()] = CharacterClass.BlankCharCase DO 
      char := GetChar(); 
    END;
    tokenBegPos := input^.acceptedCharPos;
    char := LookChar();
    class := charTable[char];
    IF class = CharacterClass.LetterCharCase THEN
      ScanAlphaNumIde();
      tokenClass := TokenClass.IdeCase;
    ELSIF class = CharacterClass.DelimCharCase THEN
      tokenDelim := GetChar();
      IF (char = '(') AND (LookChar() = '*') THEN
        char := GetChar();
        ScanComment();
        tokenClass := NextToken();
      ELSE
        tokenClass := TokenClass.DelimCase;
      END;
    ELSIF class = CharacterClass.SpecialCharCase THEN
      ScanSpecialIde();
      tokenClass := TokenClass.InfixCase;
    ELSIF (char = '~') OR (class = CharacterClass.DigitCharCase) THEN
      tokenClass := ScanNumber();
    ELSIF char = '\'' THEN
      tokenChar := ScanChar();
      tokenClass := TokenClass.CharCase;
    ELSIF char = '\"' THEN
      tokenString := ScanString();
      tokenClass := TokenClass.StringCase;
    ELSE
      char := GetChar();
      Syntax("Illegal Char", "");
    END;
    tokenEndPos := input^.acceptedCharPos;
    RETURN tokenClass;
  END NextToken;

PROCEDURE LookToken(): TokenClass RAISES ANY =
  BEGIN
    IF tokenReady THEN RETURN tokenClass END;
    tokenClass := NextToken();
    tokenReady := TRUE;
    RETURN tokenClass;
  END LookToken;

PROCEDURE GetToken(): TokenClass RAISES ANY =
  VAR class: TokenClass;
  BEGIN
    IF tokenReady THEN
      tokenReady := FALSE;
      class := tokenClass;
    ELSE
      class := NextToken();
    END;
    INC(scanPoint);
    input^.acceptedTokenBegPos := tokenBegPos;
    input^.acceptedTokenEndPos := tokenEndPos;
    RETURN class;
  END GetToken;

PROCEDURE PrintContext() RAISES ANY =
  VAR string: String.T;
  BEGIN
    Formatter.PutText(Out.out, " before: ");
    CASE tokenClass OF
    | TokenClass.IdeCase, TokenClass.InfixCase =>
        string := String.GetSub(scanBuffer, 0, scanBufferSize);
        scanBufferSize := 0;
        Formatter.PutText(Out.out, String.ToText(string));
    | TokenClass.CharCase => Formatter.PutChar(Out.out, tokenChar);
    | TokenClass.IntCase => Formatter.PutText(Out.out, Fmt.Int(tokenInt));
    | TokenClass.RealCase => Formatter.PutText(Out.out, Fmt.Real(tokenReal));
    | TokenClass.StringCase =>
        Formatter.PutChar(Out.out, '\"');
        Formatter.PutText(Out.out, String.ToText(tokenString));
        Formatter.PutChar(Out.out, '\"');
    | TokenClass.DelimCase => Formatter.PutChar(Out.out, tokenDelim);
    END
  END PrintContext;

PROCEDURE PrintSequel() RAISES ANY =
  VAR n: INTEGER; ch: CHAR;
  BEGIN
    n := 40;
    WHILE (0 < n) AND (Rd.CharsReady(input^.rd) > 0) DO
      ch:=GetChar();
      IF (Rd.CharsReady(input^.rd)>0) OR (ch#'\n') THEN
        Formatter.PutChar(Out.out, ch);
      END;
      n := n - 1;
    END;
    IF Rd.CharsReady(input^.rd) > 0 THEN
      Formatter.PutText(Out.out, " ...");
    END;
  END PrintSequel;

PROCEDURE FlushInput() RAISES ANY =
  VAR char: CHAR;
  BEGIN
    WHILE Rd.CharsReady(input^.rd) > 0 DO char := GetChar() END;
  END FlushInput;

PROCEDURE Error(msg: TEXT := "") RAISES ANY =
  BEGIN
    IF NOT Text.Empty(msg) THEN
      Formatter.PutText(Out.out, msg);
      Formatter.PutChar(Out.out, '\n');
      Formatter.Flush(Out.out);
    END;
    Reset();
    RAISE Err.Fail;
  END Error;

PROCEDURE Syntax(cause: TEXT := ""; culprit: TEXT := "") RAISES ANY =
  VAR info: Err.LocationInfo;
  BEGIN
    CurrentLocationInfo((*out*)info);
    Formatter.PutText(Out.out, "Syntax error before: ");
    PrintSequel();
    IF (NOT Text.Empty(cause)) OR (NOT Text.Empty(culprit)) THEN
      Formatter.PutChar(Out.out, '\n');
      Formatter.PutText(Out.out, "  ");
      Formatter.PutText(Out.out, cause);
      Formatter.PutChar(Out.out, ' ');
      Formatter.PutText(Out.out, culprit);
    END;
    Formatter.PutChar(Out.out, '\n');
    Formatter.PutText(Out.out, "Error detected ");
    CASE errorReportStyle OF
    | ErrorReportStyle.LinePlusChar =>
        Err.PrintLocation(Out.out, Err.NewLineLocation(info), info.line);
    | ErrorReportStyle.CharRange =>
        Err.PrintLocation(Out.out, Err.NewCharLocation(info, info), info.line);
    END;
    Formatter.PutChar(Out.out, '\n');
    Formatter.Flush(Out.out);
    Reset();
    RAISE Err.Fail;
  END Syntax;

PROCEDURE GetTokenChar(VAR (*out*) char: CHAR): BOOLEAN RAISES ANY =
  BEGIN
    IF LookToken() = TokenClass.CharCase THEN
      EVAL(GetToken());
      char := tokenChar; 
      RETURN TRUE;
    ELSE RETURN FALSE; 
    END;
  END GetTokenChar;

PROCEDURE GetTokenNat(VAR (*out*) nat: CARDINAL): BOOLEAN RAISES ANY =
  BEGIN
    IF LookToken() = TokenClass.IntCase THEN
      IF tokenInt >= 0 THEN
	EVAL(GetToken());
        nat := tokenInt; 
	RETURN TRUE;
      ELSE
        RETURN FALSE;
      END;
    ELSE RETURN FALSE;
    END;
  END GetTokenNat;

PROCEDURE GetTokenInt(VAR (*out*) int: INTEGER): BOOLEAN RAISES ANY =
  BEGIN
    IF LookToken() = TokenClass.IntCase THEN 
      EVAL(GetToken());
      int := tokenInt;
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END GetTokenInt;

PROCEDURE GetTokenReal(VAR (*ou*) real: REAL): BOOLEAN RAISES ANY =
  BEGIN
    IF LookToken() = TokenClass.RealCase THEN 
      EVAL(GetToken());
      real := tokenReal;
      RETURN TRUE;
    ELSE RETURN FALSE;
    END;
  END GetTokenReal;

PROCEDURE GetTokenString(VAR (*out*) string: String.T): BOOLEAN RAISES ANY =
  BEGIN
    IF LookToken() = TokenClass.StringCase THEN 
      EVAL(GetToken());
      string := tokenString;
      RETURN TRUE;
    ELSE RETURN FALSE;
    END;
  END GetTokenString;

PROCEDURE GetTokenIde(VAR (*ou*) ide: TEXT): BOOLEAN RAISES ANY =
  VAR class: TokenClass; name: TEXT; value: REFANY;
  BEGIN
    class := LookToken();
    IF (class = TokenClass.IdeCase) OR (class = TokenClass.InfixCase) THEN
      IF scanBufferSize # 0 THEN
	IF keySet.table.inChars(SUBARRAY(scanBuffer^, 0, scanBufferSize),
		 (*VAR OUT*) value)
	THEN
	  tokenSym := NARROW(value, Symbol);
	ELSE
	  name := Text.FromChars(SUBARRAY(scanBuffer^, 0, scanBufferSize));
	  tokenSym := NEW(Symbol, name:=name, keyword:=FALSE);
	  EVAL keySet.table.put(name, tokenSym);
	END;
        scanBufferSize := 0;
      END;
      IF tokenSym.keyword THEN RETURN FALSE END;
      EVAL(GetToken());
      ide := tokenSym.name;
      RETURN TRUE;
    ELSE RETURN FALSE;
    END;
  END GetTokenIde;

PROCEDURE GetTokenName(VAR (*ou*) text: TEXT): BOOLEAN RAISES ANY =
  VAR class: TokenClass; name: TEXT; value: REFANY;
  BEGIN
    class := LookToken();
    IF (class = TokenClass.IdeCase) OR (class = TokenClass.InfixCase) THEN
      IF scanBufferSize # 0 THEN
	IF keySet.table.inChars(SUBARRAY(scanBuffer^, 0, scanBufferSize),
		 (*VAR OUT*) value)
	THEN
	  tokenSym := NARROW(value, Symbol);
	ELSE
	  name := Text.FromChars(SUBARRAY(scanBuffer^, 0, scanBufferSize));
	  tokenSym := NEW(Symbol, name:=name, keyword:=FALSE);
	  EVAL keySet.table.put(name, tokenSym);
	END;
        scanBufferSize := 0;
      END;
      EVAL(GetToken());
      text := tokenSym.name;
      RETURN TRUE;
    ELSE RETURN FALSE;
    END;
  END GetTokenName;

PROCEDURE HaveTokenIde(ide: TEXT): BOOLEAN RAISES ANY =
  VAR class: TokenClass; name: TEXT; value: REFANY;
  BEGIN
    class := LookToken();
    IF (class = TokenClass.IdeCase) OR (class = TokenClass.InfixCase) THEN
      IF scanBufferSize # 0 THEN
	IF keySet.table.inChars(SUBARRAY(scanBuffer^, 0, scanBufferSize),
		 (*VAR OUT*) value)
	THEN
	  tokenSym := NARROW(value, Symbol);
	ELSE
	  name := Text.FromChars(SUBARRAY(scanBuffer^, 0, scanBufferSize));
	  tokenSym := NEW(Symbol, name:=name, keyword:=FALSE);
	  EVAL keySet.table.put(name, tokenSym);
	END;
        scanBufferSize := 0;
      END;
      IF tokenSym.keyword THEN RETURN FALSE END;
      IF NOT Text.Equal(ide, tokenSym.name) THEN RETURN FALSE END;
      EVAL(GetToken());
      RETURN TRUE;
    ELSE RETURN FALSE;
    END;
  END HaveTokenIde;

PROCEDURE HaveTokenName(text: TEXT): BOOLEAN RAISES ANY =
  VAR class: TokenClass; name: TEXT; value: REFANY;
  BEGIN
    class := LookToken();
    IF (class = TokenClass.IdeCase) OR (class = TokenClass.InfixCase) THEN
      IF scanBufferSize # 0 THEN
	IF keySet.table.inChars(SUBARRAY(scanBuffer^, 0, scanBufferSize),
		 (*VAR OUT*) value)
	THEN
	  tokenSym := NARROW(value, Symbol);
	ELSE
	  name := Text.FromChars(SUBARRAY(scanBuffer^, 0, scanBufferSize));
	  tokenSym := NEW(Symbol, name:=name, keyword:=FALSE);
	  EVAL keySet.table.put(name, tokenSym);
	END;
        scanBufferSize := 0;
      END;
      IF NOT Text.Equal(text, tokenSym.name) THEN RETURN FALSE END;
      EVAL(GetToken());
      RETURN TRUE;
    ELSE RETURN FALSE;
    END;
  END HaveTokenName;

PROCEDURE HaveTokenKey(key: Keyword): BOOLEAN RAISES ANY =
  VAR class: TokenClass; name: TEXT; value: REFANY;
  BEGIN
    IF NOT key.keyword THEN Error("No longer a keyword: " & key.name) END;
    class := LookToken();
    IF (class = TokenClass.IdeCase) OR (class = TokenClass.InfixCase) THEN
      IF scanBufferSize # 0 THEN
	IF keySet.table.inChars(SUBARRAY(scanBuffer^, 0, scanBufferSize),
		 (*VAR OUT*) value)
	THEN
	  tokenSym := NARROW(value, Symbol);
	ELSE
	  name := Text.FromChars(SUBARRAY(scanBuffer^, 0, scanBufferSize));
	  tokenSym := NEW(Symbol, name:=name, keyword:=FALSE);
	  EVAL keySet.table.put(name, tokenSym);
	END;
        scanBufferSize := 0;
      END;
      IF key#tokenSym THEN RETURN FALSE END;
      EVAL(GetToken());
      RETURN TRUE;
    ELSE RETURN FALSE;
    END;
  END HaveTokenKey;

PROCEDURE HaveTokenDelim(delim: CHAR): BOOLEAN RAISES ANY =
  BEGIN
    IF LookToken() = TokenClass.DelimCase THEN
      IF delim # tokenDelim THEN RETURN FALSE END;
      EVAL(GetToken());
      RETURN TRUE;
    ELSE RETURN FALSE;
    END;
  END HaveTokenDelim;

PROCEDURE SetChar(n: CHAR; class: CharacterClass) RAISES ANY =
  BEGIN charTable[n] := class; END SetChar;

PROCEDURE SetRange(n: CHAR; m: CHAR; class: CharacterClass) RAISES ANY =
  BEGIN
    FOR i := ORD(n) TO ORD(m) DO charTable[VAL(i, CHAR)] := class; END;
  END SetRange;

PROCEDURE TopLevel(): BOOLEAN RAISES ANY =
  BEGIN RETURN Text.Empty(input^.fileName); END TopLevel;

PROCEDURE SetPrompt(newFirstPrompt, newNextPrompt: TEXT) RAISES ANY =
  BEGIN
    firstPrompt := newFirstPrompt;
    nextPrompt := newNextPrompt;
    isFirstPrompt := TRUE;
  END SetPrompt;

PROCEDURE FirstPrompt() RAISES ANY = BEGIN isFirstPrompt := TRUE; END FirstPrompt;

PROCEDURE Clear() RAISES ANY=
  VAR ch: CHAR; class: TokenClass;
  BEGIN
    IF TopLevel() THEN FlushInput() END;
    IF lookAheadReady THEN ch:=GetChar() END;
    IF tokenReady THEN class:=GetToken() END;
    scanBufferSize := 0;
  END Clear;

PROCEDURE Reset() RAISES ANY =
  BEGIN
    Clear();
    WHILE NOT(Text.Empty(input^.fileName)) DO PopInput() END;
  END Reset;

PROCEDURE Setup() RAISES ANY =
  BEGIN

    scanPoint := 0;
    scanBuffer := String.New(256, ' ');

    keySet := NewKeywordSet();

    lookAheadReady := FALSE;
    tokenReady := FALSE;
    scanBufferSize := 0;
    input := NIL;
    PushInput("", Stdio.stdin);

    SetPrompt("", "");

    SetRange(VAL(9, CHAR), VAL(10, CHAR), CharacterClass.BlankCharCase);
    SetRange(VAL(12, CHAR), VAL(13, CHAR), CharacterClass.BlankCharCase);
    SetChar(' ', CharacterClass.BlankCharCase);
    SetChar('!', CharacterClass.DelimCharCase);
    SetChar('\"', CharacterClass.ReservedCharCase);
    SetRange('#', '&', CharacterClass.SpecialCharCase);
    SetChar('\'', CharacterClass.ReservedCharCase);
    SetRange('(', ')', CharacterClass.DelimCharCase);
    SetRange('*', '+', CharacterClass.SpecialCharCase);
    SetChar(',', CharacterClass.DelimCharCase);
    SetChar('-', CharacterClass.SpecialCharCase);
    SetChar('.', CharacterClass.DelimCharCase);
    SetChar('/', CharacterClass.SpecialCharCase);
    SetRange('0', '9', CharacterClass.DigitCharCase);
    SetChar(':', CharacterClass.SpecialCharCase);
    SetChar(';', CharacterClass.DelimCharCase);
    SetRange('<', '>', CharacterClass.SpecialCharCase);
    SetChar('?', CharacterClass.DelimCharCase);
    SetChar('@', CharacterClass.SpecialCharCase);
    SetRange('A', 'Z', CharacterClass.LetterCharCase);
    SetChar('[', CharacterClass.DelimCharCase);
    SetChar('\\', CharacterClass.SpecialCharCase);
    SetChar(']', CharacterClass.DelimCharCase);
    SetChar('^', CharacterClass.SpecialCharCase);
    SetChar('_', CharacterClass.DelimCharCase);
    SetChar('`', CharacterClass.LetterCharCase);
    SetRange('a', 'z', CharacterClass.LetterCharCase);
    SetChar('{', CharacterClass.DelimCharCase);
    SetChar('|', CharacterClass.SpecialCharCase);
    SetChar('}', CharacterClass.DelimCharCase);
    SetChar('~', CharacterClass.ReservedCharCase);

  END Setup;

BEGIN
END Scanner.


