{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License Version
1.1 (the "License"); you may not use this file except in compliance with the
License. You may obtain a copy of the License at
http://www.mozilla.org/NPL/NPL-1_1Final.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: mwPasLex.PAS, released August 17, 1999.

The Initial Developer of the Original Code is Martin Waldenburg
(Martin.Waldenburg@T-Online.de).
Portions created by Martin Waldenburg are Copyright (C) 1998, 1999 Martin
Waldenburg.
All Rights Reserved.

Contributor(s): Roman Yankovsky, James Jacobson _____________________________________.

Last Modified: mm/dd/yyyy
Current Version: 2.25

Notes: This program is a very fast Pascal tokenizer. I'd like to invite the
Delphi community to develop it further and to create a fully featured Object
Pascal parser.

Modification history:

Daniel Rolf between 20010723 and 20020116

Made ready for Delphi 6

platform
deprecated
varargs
local

Known Issues:
-----------------------------------------------------------------------------}

unit SimpleParser.Lexer;

{$I SimpleParser.inc}

interface

uses
  //!! pruned uses
  SysUtils, Classes, System.Character, SimpleParser.Lexer.Types;

var
  Identifiers: array[#0..#255] of ByteBool;
  mHashTable: array[#0..#255] of Integer;

type
  TmwBasePasLex = class;
  TDirectiveEvent = procedure(Sender: TmwBasePasLex) of object;

  PDefineRec = ^TDefineRec;
  TDefineRec = record
    Defined: Boolean;
    StartCount: Integer;
    Next: PDefineRec;
  end;

  TmwBasePasLex = class(TObject)
  private
    fCommentState: TCommentState;
    fOrigin: PChar;
    fProcTable: array[#0..#255] of procedure of object;
    Run: Integer;
    RunAhead: Integer;
    TempRun: Integer;
    fIdentFuncTable: array[0..191] of function: TptTokenKind of object;
    fTokenPos: Integer;
    fLineNumber: Integer;
    FTokenID: TptTokenKind;
    fLinePos: Integer;
    fExID: TptTokenKind;
    FOnMessage: TMessageEvent;
    fOnCompDirect: TDirectiveEvent;
    fOnElseDirect: TDirectiveEvent;
    fOnEndIfDirect: TDirectiveEvent;
    fOnIfDefDirect: TDirectiveEvent;
    fOnIfNDefDirect: TDirectiveEvent;
    fOnResourceDirect: TDirectiveEvent;
    fOnIncludeDirect: TDirectiveEvent;
    fOnDefineDirect: TDirectiveEvent;
    fOnIfOptDirect: TDirectiveEvent;
    fOnIfDirect: TDirectiveEvent;
    fOnIfEndDirect: TDirectiveEvent;
    fOnElseIfDirect: TDirectiveEvent;
	fOnUnDefDirect: TDirectiveEvent;
  FDirectiveParamOrigin: PChar;

  	fAsmCode : Boolean;		// DR 2002-01-14

    FDefines: TStrings;
    FDefineStack: Integer;
    FTopDefineRec: PDefineRec;
    FUseDefines: Boolean;

	  function KeyHash: Integer;
    function KeyComp(const aKey: string): Boolean;
    function Func9: tptTokenKind;
    function Func15: TptTokenKind;
    function Func19: TptTokenKind;
    function Func20: TptTokenKind;
	function Func21: TptTokenKind;
    function Func23: TptTokenKind;
    function Func25: TptTokenKind;
    function Func27: TptTokenKind;
    function Func28: TptTokenKind;
    function Func29: TptTokenKind;
    function Func30: TptTokenKind;
    function Func32: TptTokenKind;
    function Func33: TptTokenKind;
    function Func35: TptTokenKind;
    function Func36: TptTokenKind;
    function Func37: TptTokenKind;
    function Func38: TptTokenKind;
    function Func39: TptTokenKind;
    function Func40: TptTokenKind;
    function Func41: TptTokenKind;
    {$IFDEF D8_NEWER} //JThurman 2004-03-2003
    function Func42: TptTokenKind;
    {$ENDIF}
    function Func43: TptTokenKind;
    function Func44: TptTokenKind;
    function Func45: TptTokenKind;
    function Func46: TptTokenKind;
    function Func47: TptTokenKind;
    function Func49: TptTokenKind;
    function Func52: TptTokenKind;
    function Func54: TptTokenKind;
	function Func55: TptTokenKind;
    function Func56: TptTokenKind;
    function Func57: TptTokenKind;
    function Func58: TptTokenKind;
    function Func59: TptTokenKind;
    function Func60: TptTokenKind;
    function Func61: TptTokenKind;
    function Func62: TptTokenKind;
    function Func63: TptTokenKind;
    function Func64: TptTokenKind;
	function Func65: TptTokenKind;
    function Func66: TptTokenKind;
    function Func69: TptTokenKind;
    function Func71: TptTokenKind;
    {$IFDEF D8_NEWER} //JThurman 2004-03-2003
    function Func72: TptTokenKind;
    {$ENDIF}
    function Func73: TptTokenKind;
    function Func75: TptTokenKind;
    function Func76: TptTokenKind;
    function Func78: TptTokenKind;
    function Func79: TptTokenKind;
    function Func81: TptTokenKind;
    function Func84: TptTokenKind;
	function Func85: TptTokenKind;
	function Func86: TptTokenKind;
    function Func87: TptTokenKind;
    function Func88: TptTokenKind;
    {$IFDEF D8_NEWER}
    function Func89: TptTokenKind; //JThurman 2004-03-03
    {$ENDIF}
    function Func91: TptTokenKind;
    function Func92: TptTokenKind;
    function Func94: TptTokenKind;
    function Func95: TptTokenKind;
    function Func96: TptTokenKind;
    function Func97: TptTokenKind;
    function Func98: TptTokenKind;
    function Func99: TptTokenKind;
    function Func100: TptTokenKind;
    function Func101: TptTokenKind;
    function Func102: TptTokenKind;
    function Func103: TptTokenKind;
    function Func104: TptTokenKind;
    function Func105: TptTokenKind;
    function Func106: TptTokenKind;
    function Func107: TptTokenKind;
    function Func108: TptTokenKind;
    function Func112: TptTokenKind;
    function Func117: TptTokenKind;
	function Func123: TptTokenKind;
    function Func126: TptTokenKind;
    function Func127: TptTokenKind;
    function Func128: TptTokenKind;
    function Func129: TptTokenKind;
    function Func130: TptTokenKind;
    function Func132: TptTokenKind;
    function Func133: TptTokenKind;
    function Func136: TptTokenKind;
    function Func141: TptTokenKind;
    function Func142: TptTokenKind;
    function Func143: TptTokenKind;
    function Func166: TptTokenKind;
    function Func167: TptTokenKind;
    function Func168: TptTokenKind;
    function Func191: TptTokenKind;
    function AltFunc: TptTokenKind;
    procedure InitIdent;
    function GetPosXY: TTokenPoint; // !! changed to TokenPoint //jdj 7/18/1999
    function IdentKind: TptTokenKind;
    procedure SetRunPos(Value: Integer);
    procedure MakeMethodTables;
    procedure AddressOpProc;
    {$IFDEF D8_NEWER} //JThurman 2004-04-06
    procedure AmpersandOpProc;
    {$ENDIF}
    procedure AsciiCharProc;
    procedure AnsiProc;
    procedure BorProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure ColonProc;
    procedure CommaProc;
    procedure CRProc;
    procedure EqualProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure IntegerProc;
	procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure NullProc;
    procedure NumberProc;
    procedure PlusProc;
    procedure PointerSymbolProc;
    procedure PointProc;
    procedure RoundCloseProc;
    procedure RoundOpenProc;
    procedure SemiColonProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure SquareCloseProc;
    procedure SquareOpenProc;
    procedure StarProc;
	procedure StringProc;
	procedure StringDQProc;
    procedure SymbolProc;
    procedure UnknownProc;
    function GetToken: string;
    function GetTokenLen: Integer;
    function GetCommentState: Pointer;
    function GetCompilerDirective: string;
    procedure SetCommentState(const Value: Pointer);
    procedure InitLine;
    function GetDirectiveKind: TptTokenKind;
    function GetDirectiveParam: string;
    function GetStringContent: string;
    function GetIsJunk: Boolean;
    function GetIsSpace: Boolean;
    function GetIsOrdIdent: Boolean;
    function GetIsRealType: Boolean;
    function GetIsStringType: Boolean;
    function GetIsVarantType: Boolean;
    function GetIsAddOperator: Boolean;
    function GetIsMulOperator: Boolean;
    function GetIsRelativeOperator: Boolean;
    function GetIsCompilerDirective: Boolean;
    function GetIsOrdinalType: Boolean;
    function GetGenID: TptTokenKind;procedure SetOnElseIfDirect(const Value: TDirectiveEvent);

    function IsDefined(const ADefine: string): Boolean;
    procedure EnterDefineBlock(ADefined: Boolean);
    procedure ExitDefineBlock;
    procedure CloneDefinesFrom(ALexer: TmwBasePasLex);

    procedure DoProcTable(AChar: Char);
    function IsIdentifiers(AChar: Char): Boolean; inline;
    function HashValue(AChar: Char): Integer;
  protected
    procedure SetLine(const Value: string); virtual;
    procedure SetOrigin(NewValue: PChar); virtual;
    procedure SetOnCompDirect(const Value: TDirectiveEvent); virtual;
    procedure SetOnDefineDirect(const Value: TDirectiveEvent); virtual;
    procedure SetOnElseDirect(const Value: TDirectiveEvent); virtual;
    procedure SetOnEndIfDirect(const Value: TDirectiveEvent); virtual;
    procedure SetOnIfDefDirect(const Value: TDirectiveEvent); virtual;
    procedure SetOnIfNDefDirect(const Value: TDirectiveEvent); virtual;
    procedure SetOnIfOptDirect(const Value: TDirectiveEvent); virtual;
    procedure SetOnIncludeDirect(const Value: TDirectiveEvent); virtual;
    procedure SetOnResourceDirect(const Value: TDirectiveEvent); virtual;
    procedure SetOnUnDefDirect(const Value: TDirectiveEvent); virtual;
    procedure SetOnIfDirect(const Value: TDirectiveEvent); virtual;
    procedure SetOnIfEndDirect(const Value: TDirectiveEvent); virtual;

  public
    constructor Create;
    destructor Destroy; override;
    function CharAhead: Char;
    procedure Next;
    procedure NextID(ID: TptTokenKind);
    procedure NextNoJunk;
    procedure NextNoSpace;
    procedure Init;
    procedure InitFrom(ALexer: TmwBasePasLex);
    function FirstInLine: Boolean;

    procedure AddDefine(const ADefine: string);
    procedure RemoveDefine(const ADefine: string);
    procedure ClearDefines;
    procedure InitDefines;

    property CommentState: Pointer read GetCommentState write SetCommentState;
    property CompilerDirective: string read GetCompilerDirective;
    property DirectiveParam: string read GetDirectiveParam;
	property IsJunk: Boolean read GetIsJunk;
    property IsSpace: Boolean read GetIsSpace;
    property Line: string write SetLine;
    //Note: setting the following two properties does not GO to that line, it just sets the internal counters
    property LineNumber: Integer read fLineNumber write fLineNumber;
    property LinePos: Integer read fLinePos write fLinePos;
    property Origin: PChar read fOrigin write SetOrigin;
    property PosXY: TTokenPoint read GetPosXY; // !! changed to TokenPoint //jdj 7/18/1999
    property RunPos: Integer read Run write SetRunPos;
    property Token: string read GetToken;
    property TokenLen: Integer read GetTokenLen;
    property TokenPos: Integer read fTokenPos;
    property TokenID: TptTokenKind read FTokenID;
    property ExID: TptTokenKind read fExID;
    property GenID: TptTokenKind read GetGenID;
    property StringContent: string read GetStringContent;
    property IsOrdIdent: Boolean read GetIsOrdIdent;
    property IsOrdinalType: Boolean read GetIsOrdinalType;
    property IsRealType: Boolean read GetIsRealType;
    property IsStringType: Boolean read GetIsStringType;
    property IsVariantType: Boolean read GetIsVarantType;
    property IsRelativeOperator: Boolean read GetIsRelativeOperator;
    property IsAddOperator: Boolean read GetIsAddOperator;
    property IsMulOperator: Boolean read GetIsMulOperator;
    property IsCompilerDirective: Boolean read GetIsCompilerDirective;
    property OnMessage: TMessageEvent read FOnMessage write FOnMessage;
    property OnCompDirect: TDirectiveEvent read fOnCompDirect write SetOnCompDirect;
    property OnDefineDirect: TDirectiveEvent read fOnDefineDirect write SetOnDefineDirect;
    property OnElseDirect: TDirectiveEvent read fOnElseDirect write SetOnElseDirect;
    property OnEndIfDirect: TDirectiveEvent read fOnEndIfDirect write SetOnEndIfDirect;
    property OnIfDefDirect: TDirectiveEvent read fOnIfDefDirect write SetOnIfDefDirect;
    property OnIfNDefDirect: TDirectiveEvent read fOnIfNDefDirect write SetOnIfNDefDirect;
    property OnIfOptDirect: TDirectiveEvent read fOnIfOptDirect write SetOnIfOptDirect;
    property OnIncludeDirect: TDirectiveEvent read fOnIncludeDirect write SetOnIncludeDirect;
    property OnIfDirect: TDirectiveEvent read fOnIfDirect write SetOnIfDirect;
    property OnIfEndDirect: TDirectiveEvent read fOnIfEndDirect write 
    SetOnIfEndDirect;
    property OnElseIfDirect: TDirectiveEvent read fOnElseIfDirect write 
    SetOnElseIfDirect;
	property OnResourceDirect: TDirectiveEvent read fOnResourceDirect write SetOnResourceDirect;
	property OnUnDefDirect: TDirectiveEvent read fOnUnDefDirect write SetOnUnDefDirect;

	property AsmCode : Boolean read fAsmCode write fAsmCode; // DR 2002-01-14
  property DirectiveParamOrigin: pchar read FDirectiveParamOrigin;

    property UseDefines: Boolean read FUseDefines write FUseDefines;

  end;

  TmwPasLex = class(TmwBasePasLex)
  private
    fAheadLex: TmwBasePasLex;
    function GetAheadExID: TptTokenKind;
    function GetAheadGenID: TptTokenKind;
    function GetAheadToken: string;
    function GetAheadTokenID: TptTokenKind;
    function GetStatus: TmwPasLexStatus;
    procedure SetStatus(const Value: TmwPasLexStatus);
  protected
    procedure SetLine(const Value: string); override;
    procedure SetOrigin(NewValue: PChar); override;
    procedure SetOnCompDirect(const Value: TDirectiveEvent); override;
    procedure SetOnDefineDirect(const Value: TDirectiveEvent); override;
    procedure SetOnElseDirect(const Value: TDirectiveEvent); override;
    procedure SetOnEndIfDirect(const Value: TDirectiveEvent); override;
    procedure SetOnIfDefDirect(const Value: TDirectiveEvent); override;
    procedure SetOnIfNDefDirect(const Value: TDirectiveEvent); override;
    procedure SetOnIfOptDirect(const Value: TDirectiveEvent); override;
    procedure SetOnIncludeDirect(const Value: TDirectiveEvent); override;
    procedure SetOnResourceDirect(const Value: TDirectiveEvent); override;
    procedure SetOnUnDefDirect(const Value: TDirectiveEvent); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure InitAhead;
    procedure AheadNext;
    property AheadLex: TmwBasePasLex read fAheadLex;
    property AheadToken: string read GetAheadToken;
    property AheadTokenID: TptTokenKind read GetAheadTokenID;
    property AheadExID: TptTokenKind read GetAheadExID;
    property AheadGenID: TptTokenKind read GetAheadGenID;
    property Status: TmwPasLexStatus read GetStatus write SetStatus;
  end;

implementation

uses Windows;

procedure MakeIdentTable;
var
  I, J: Char;
begin
  for I := #0 to #255 do
  begin
    case I of
      '_', '0'..'9', 'a'..'z', 'A'..'Z': Identifiers[I] := True;
    else Identifiers[I] := False;
    end;
    J := UpperCase(I)[1];
    case I of
	  'a'..'z', 'A'..'Z', '_': mHashTable[I] := Ord(J) - 64;
    else mHashTable[Char(I)] := 0;
    end;
  end;
end;

function TmwBasePasLex.CharAhead: Char;
begin
  RunAhead := Run;
//  while fOrigin[RunAhead] in [#1..#32] do
  while (fOrigin[RunAhead] > #0) and (fOrigin[RunAhead] < #33) do

    inc(RunAhead);
  Result := fOrigin[RunAhead];
end;

procedure TmwBasePasLex.ClearDefines;
var
  Frame: PDefineRec;
begin
  while FTopDefineRec <> nil do
  begin
    Frame := FTopDefineRec;
    FTopDefineRec := Frame^.Next;
    Dispose(Frame);
  end;
  FDefines.Clear;
  FDefineStack := 0;
end;

procedure TmwBasePasLex.CloneDefinesFrom(ALexer: TmwBasePasLex);
var
  Frame, LastFrame, SourceFrame: PDefineRec;
begin
  ClearDefines;
  FDefines.Assign(ALexer.FDefines);
  FDefineStack := ALexer.FDefineStack;

  Frame := nil;
  SourceFrame := ALexer.FTopDefineRec;
  while SourceFrame <> nil do
  begin
    New(Frame);
    if FTopDefineRec = nil then
      FTopDefineRec := Frame
    else
      LastFrame^.Next := Frame;
    Frame^.Defined := SourceFrame^.Defined;
    Frame^.StartCount := SourceFrame^.StartCount;
    LastFrame := Frame;

    SourceFrame := SourceFrame^.Next;
  end;
  if Frame <> nil then
    Frame^.Next := nil;

//  New(StackFrame);
//  StackFrame^.Next := FTopDefineRec;
//  StackFrame^.Defined := ADefined;
//  StackFrame^.StartCount := FDefineStack;
//  FTopDefineRec := StackFrame;
//  if not ADefined then
//    Inc(FDefineStack);

end;

function TmwBasePasLex.GetPosXY: TTokenPoint;
begin //jdj 7/18/1999
  // !! changed setting code
  Result.X:= FTokenPos - FLinePos;
  Result.Y:= FLineNumber;
end;

procedure TmwBasePasLex.InitIdent;
var
  I: Integer;
begin
  for I := 0 to 191 do
    case I of
      {$IFDEF D8_NEWER}
      9: fIdentFuncTable[I] := Func9;
      {$ENDIF}
      15: fIdentFuncTable[I] := Func15;
	  19: fIdentFuncTable[I] := Func19;
      20: fIdentFuncTable[I] := Func20;
      21: fIdentFuncTable[I] := Func21;
	  23: fIdentFuncTable[I] := Func23;
      25: fIdentFuncTable[I] := Func25;
      27: fIdentFuncTable[I] := Func27;
      28: fIdentFuncTable[I] := Func28;
      29: fIdentFuncTable[I] := Func29;
      30: fIdentFuncTable[I] := Func30;
      32: fIdentFuncTable[I] := Func32;
      33: fIdentFuncTable[I] := Func33;
      35: fIdentFuncTable[I] := Func35;
      36: fIdentFuncTable[I] := Func36;
      37: fIdentFuncTable[I] := Func37;
      38: fIdentFuncTable[I] := Func38;
	  39: fIdentFuncTable[I] := Func39;
      40: fIdentFuncTable[I] := Func40;
	  41: fIdentFuncTable[I] := Func41;
    {$IFDEF D8_NEWER} //JThurman 2004-03-2003
    42: fIdentFuncTable[I] := Func42;
    {$ENDIF}
	  43: fIdentFuncTable[I] := Func43;
      44: fIdentFuncTable[I] := Func44;
      45: fIdentFuncTable[I] := Func45;
      46: fIdentFuncTable[I] := Func46;
      47: fIdentFuncTable[I] := Func47;
      49: fIdentFuncTable[I] := Func49;
      52: fIdentFuncTable[I] := Func52;
      54: fIdentFuncTable[I] := Func54;
      55: fIdentFuncTable[I] := Func55;
      56: fIdentFuncTable[I] := Func56;
      57: fIdentFuncTable[I] := Func57;
      58: fIdentFuncTable[I] := Func58;
	  59: fIdentFuncTable[I] := Func59;
      60: fIdentFuncTable[I] := Func60;
      61: fIdentFuncTable[I] := Func61;
	  62: fIdentFuncTable[I] := Func62;
      63: fIdentFuncTable[I] := Func63;
	  64: fIdentFuncTable[I] := Func64;
      65: fIdentFuncTable[I] := Func65;
      66: fIdentFuncTable[I] := Func66;
      69: fIdentFuncTable[I] := Func69;
      71: fIdentFuncTable[I] := Func71;
      {$IFDEF D8_NEWER} //JThurman 2004-03-2003
      72: fIdentFuncTable[I] := Func72;
      {$ENDIF}
      73: fIdentFuncTable[I] := Func73;
      75: fIdentFuncTable[I] := Func75;
      76: fIdentFuncTable[I] := Func76;
      78: fIdentFuncTable[I] := Func78;
      79: fIdentFuncTable[I] := Func79;
      81: fIdentFuncTable[I] := Func81;
      84: fIdentFuncTable[I] := Func84;
	  85: fIdentFuncTable[I] := Func85;
	  86: fIdentFuncTable[I] := Func86;
      87: fIdentFuncTable[I] := Func87;
      88: fIdentFuncTable[I] := Func88;
      {$IFDEF D8_NEWER} //JThurman 2004-03-03
      89: fIdentFuncTable[I] := Func89;
      {$ENDIF}
      91: fIdentFuncTable[I] := Func91;
      92: fIdentFuncTable[I] := Func92;
      94: fIdentFuncTable[I] := Func94;
      95: fIdentFuncTable[I] := Func95;
      96: fIdentFuncTable[I] := Func96;
      97: fIdentFuncTable[I] := Func97;
      98: fIdentFuncTable[I] := Func98;
      99: fIdentFuncTable[I] := Func99;
      100: fIdentFuncTable[I] := Func100;
	  101: fIdentFuncTable[I] := Func101;
      102: fIdentFuncTable[I] := Func102;
      103: fIdentFuncTable[I] := Func103;
      104: fIdentFuncTable[I] := Func104;
      105: fIdentFuncTable[I] := Func105;
	  106: fIdentFuncTable[I] := Func106;
      107: fIdentFuncTable[I] := Func107;
      108: fIdentFuncTable[I] := Func108;
      112: fIdentFuncTable[I] := Func112;
      117: fIdentFuncTable[I] := Func117;
      123: fIdentFuncTable[I] := Func123;
      126: fIdentFuncTable[I] := Func126;
      127: fIdentFuncTable[I] := Func127;
      128: fIdentFuncTable[I] := Func128;
	  129: fIdentFuncTable[I] := Func129;
      130: fIdentFuncTable[I] := Func130;
      132: fIdentFuncTable[I] := Func132;
      133: fIdentFuncTable[I] := Func133;
	  136: fIdentFuncTable[I] := Func136;
      141: fIdentFuncTable[I] := Func141;
      142: fIdentFuncTable[I] := Func142;
      143: fIdentFuncTable[I] := Func143;
      166: fIdentFuncTable[I] := Func166;
      167: fIdentFuncTable[I] := Func167;
      168: fIdentFuncTable[I] := Func168;
      191: fIdentFuncTable[I] := Func191;
    else fIdentFuncTable[I] := AltFunc;
    end;
end;

function TmwBasePasLex.KeyHash: Integer;
begin
  Result := 0;
  while IsIdentifiers(fOrigin[Run]) do
  begin
    Inc(Result, HashValue(fOrigin[Run]));
    //inc(Result, mHashTable[fOrigin[Run]]);
    inc(Run);
  end;
end; { KeyHash }

function TmwBasePasLex.KeyComp(const aKey: string): Boolean;
var
  I: Integer;
  Temp: PChar;
begin
  if Length(aKey) = TokenLen then
  begin
    Temp := fOrigin + fTokenPos;
    Result := True;
    for i := 1 to TokenLen do
    begin
      if mHashTable[Temp^] <> mHashTable[aKey[i]] then
      begin
        Result := False;
        break;
      end;
      inc(Temp);
    end;
  end
  else Result := False;
end; { KeyComp }

function TmwBasePasLex.Func9: tptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Add') then
    FExID := ptAdd;
end;

function TmwBasePasLex.Func15: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('If') then Result := ptIf;
end;

function TmwBasePasLex.Func19: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Do') then Result := ptDo else
    if KeyComp('And') then Result := ptAnd;
end;

function TmwBasePasLex.Func20: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('As') then Result := ptAs;
end;

function TmwBasePasLex.Func21: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Of') then Result := ptOf else
    if KeyComp('At') then fExID := ptAt;
end;

function TmwBasePasLex.Func23: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('End') then Result := ptEnd else
    if KeyComp('In') then Result := ptIn;
end;

function TmwBasePasLex.Func25: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Far') then fExID := ptFar;
end;

function TmwBasePasLex.Func27: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Cdecl') then fExID := ptCdecl;
end;

function TmwBasePasLex.Func28: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Read') then fExID := ptRead else
    if KeyComp('Case') then Result := ptCase else
      if KeyComp('Is') then Result := ptIs;
end;

function TmwBasePasLex.Func29: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('On') then fExID := ptOn;
end;

function TmwBasePasLex.Func30: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Char') then fExID := ptChar;
end;

function TmwBasePasLex.Func32: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('File') then Result := ptFile else
    if KeyComp('Label') then Result := ptLabel else
      if KeyComp('Mod') then Result := ptMod;
end;

function TmwBasePasLex.Func33: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Or') then Result := ptOr else
    if KeyComp('Name') then fExID := ptName else
      if KeyComp('Asm') then Result := ptAsm;
end;

function TmwBasePasLex.Func35: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Nil') then Result := ptNil else
    if KeyComp('To') then Result := ptTo else
      if KeyComp('Div') then Result := ptDiv;
end;

function TmwBasePasLex.Func36: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Real') then fExID := ptReal else
    if KeyComp('Real48') then fExID := ptReal48;
end;

function TmwBasePasLex.Func37: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Begin') then Result := ptBegin else
    if KeyComp('Break') then fExID := ptBreak;
end;

function TmwBasePasLex.Func38: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Near') then fExID := ptNear;
end;

function TmwBasePasLex.Func39: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('For') then Result := ptFor else
    if KeyComp('Shl') then Result := ptShl;
end;

function TmwBasePasLex.Func40: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Packed') then Result := ptPacked;
end;

function TmwBasePasLex.Func41: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Var') then Result := ptVar else
    if KeyComp('Else') then Result := ptElse else
      if KeyComp('Halt') then fExID := ptHalt;
end;

{$IFDEF D8_NEWER} //JThurman 2004-03-2003
function TmwBasePasLex.Func42: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Final') then
    fExID := ptFinal; //TODO: Is this supposed to be an ExID?
end;
{$ENDIF}

function TmwBasePasLex.Func43: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Int64') then fExID := ptInt64
  else if KeyComp('local') then fExID := ptLocal;
end;

function TmwBasePasLex.Func44: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Set') then Result := ptSet else
    if KeyComp('Package') then fExID := ptPackage;
end;

function TmwBasePasLex.Func45: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Shr') then Result := ptShr;
end;

function TmwBasePasLex.Func46: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('PChar') then fExId := ptPChar
  {$IFDEF D8_NEWER} //JThurman 2004-03-19
   else
    if KeyComp('Sealed') then Result := ptSealed;
  {$ELSE}
  ;
  {$ENDIF}
end;

function TmwBasePasLex.Func47: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Then') then Result := ptThen else
    if KeyComp('Comp') then fExID := ptComp;
end;

function TmwBasePasLex.Func49: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Not') then Result := ptNot;
end;

function TmwBasePasLex.Func52: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Byte') then fExID := ptByte else
    if KeyComp('Raise') then Result := ptRaise else
      if KeyComp('Pascal') then fExID := ptPascal;
end;

function TmwBasePasLex.Func54: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Class') then Result := ptClass;
end;

function TmwBasePasLex.Func55: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Object') then Result := ptObject;
end;

function TmwBasePasLex.Func56: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Index') then fExID := ptIndex else
    if KeyComp('Out') then fExID := ptOut else // bug in Delphi's documentation: OUT is a directive
      if KeyComp('Abort') then fExID := ptAbort else
        if KeyComp('Delayed') then fExID := ptDelayed;
end;

function TmwBasePasLex.Func57: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('While') then Result := ptWhile else
    if KeyComp('Xor') then Result := ptXor else
      if KeyComp('Goto') then Result := ptGoto;
end;

function TmwBasePasLex.Func58: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Exit') then fExID := ptExit;
end;

function TmwBasePasLex.Func59: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Safecall') then fExID := ptSafecall else
    if KeyComp('Double') then fExID := ptDouble;
end;

function TmwBasePasLex.Func60: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('With') then Result := ptWith else
    if KeyComp('Word') then fExID := ptWord;
end;

function TmwBasePasLex.Func61: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Dispid') then fExID := ptDispid;
end;

function TmwBasePasLex.Func62: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Cardinal') then fExID := ptCardinal;
end;

function TmwBasePasLex.Func63: TptTokenKind;
begin
  Result := ptIdentifier;
  case fOrigin[fTokenPos] of
    'P', 'p': if KeyComp('Public') then fExID := ptPublic;
    'A', 'a': if KeyComp('Array') then Result := ptArray;
    'T', 't': if KeyComp('Try') then Result := ptTry;
    'R', 'r': if KeyComp('Record') then Result := ptRecord;
    'I', 'i': if KeyComp('Inline') then
     begin
       Result := ptInline;
       fExID := ptInline;
     end;
  end;
end;

function TmwBasePasLex.Func64: TptTokenKind;
begin
  Result := ptIdentifier;
  case fOrigin[fTokenPos] of
    'B', 'b': if KeyComp('Boolean') then fExID := ptBoolean;
    'D', 'd': if KeyComp('DWORD') then fExID := ptDWORD;
    'U', 'u': if KeyComp('Uses') then Result := ptUses
    else
      if KeyComp('Unit') then Result := ptUnit;
    {$IFDEF D8_NEWER}
    'H', 'h': if KeyComp('Helper') then Result := ptHelper;
    {$ENDIF}
  end;
end;

function TmwBasePasLex.Func65: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Repeat') then Result := ptRepeat;
end;

function TmwBasePasLex.Func66: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Single') then fExID := ptSingle else
    if KeyComp('Type') then Result := ptType
      {$IFDEF D8_NEWER}//JThurman 2004-03-23
      else
      if KeyComp('Unsafe') then Result := ptUnsafe
      {$ENDIF}
      ;
end;

function TmwBasePasLex.Func69: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Default') then fExID := ptDefault else
    if KeyComp('Dynamic') then fExID := ptDynamic else
      if KeyComp('Message') then fExID := ptMessage;
end;

function TmwBasePasLex.Func71: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('WideChar') then fExID := ptWideChar else
    if KeyComp('Stdcall') then fExID := ptStdcall else
      if KeyComp('Const') then Result := ptConst;
end;

{$IFDEF D8_NEWER} //JThurman 2004-03-2003
function TmwBasePasLex.Func72: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Static') then
    fExID := ptStatic;
end;
{$ENDIF}

function TmwBasePasLex.Func73: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Except') then Result := ptExcept;
end;

function TmwBasePasLex.Func75: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Write') then fExID := ptWrite;
end;

function TmwBasePasLex.Func76: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Until') then Result := ptUntil;
end;

function TmwBasePasLex.Func78: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Integer') then fExID := ptInteger
  {$IFDEF D8_NEWER}
    else if KeyComp('Remove') then
      FExID := ptRemove
  {$ENDIF}
  ;
end;

function TmwBasePasLex.Func79: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Finally') then Result := ptFinally
  {$IFDEF D12_NEWER}
  else if KeyComp('Reference') then fExID := ptReference;
  {$ENDIF}
       
end;

function TmwBasePasLex.Func81: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Extended') then fExID := ptExtended else
    if KeyComp('Stored') then fExID := ptStored else
	  if KeyComp('Interface') then Result := ptInterface
	    else if KeyComp('Deprecated') then fExID := ptDeprecated; // DR 2001-10-20
end;

function TmwBasePasLex.Func84: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Abstract') then fExID := ptAbstract;
end;

function TmwBasePasLex.Func85: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Library') then Result := ptLibrary else
	if KeyComp('Forward') then fExID := ptForward else
	  if KeyComp('Variant') then fExID := ptVariant;
end;

function TmwBasePasLex.Func87: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('String') then Result := ptString;
end;

function TmwBasePasLex.Func88: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Program') then Result := ptProgram;
end;

{$IFDEF D8_NEWER} //JThurman 2004-03-03
function TmwBasePasLex.Func89: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Strict') then Result := ptStrict;
end;
{$ENDIF}

function TmwBasePasLex.Func91: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Downto') then Result := ptDownto else
    if KeyComp('Private') then fExID := ptPrivate else
      if KeyComp('Longint') then fExID := ptLongint;
end;

function TmwBasePasLex.Func92: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Inherited') then Result := ptInherited else
    if KeyComp('LongBool') then fExID := ptLongBool else
      if KeyComp('Overload') then fExID := ptOverload;
end;

function TmwBasePasLex.Func94: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Resident') then fExID := ptResident else
    if KeyComp('Readonly') then fExID := ptReadonly else
      if KeyComp('Assembler') then fExID := ptAssembler;
end;

function TmwBasePasLex.Func95: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Contains') then fExID := ptContains else
    if KeyComp('Absolute') then fExID := ptAbsolute;
end;

function TmwBasePasLex.Func96: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('ByteBool') then fExID := ptByteBool else
    if KeyComp('Override') then fExID := ptOverride else
      if KeyComp('Published') then fExID := ptPublished;
end;

function TmwBasePasLex.Func97: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Threadvar') then Result := ptThreadvar;
end;

function TmwBasePasLex.Func98: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Export') then fExID := ptExport else
    if KeyComp('Nodefault') then fExID := ptNodefault;
end;

function TmwBasePasLex.Func99: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('External') then fExID := ptExternal;
end;

function TmwBasePasLex.Func100: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Automated') then fExID := ptAutomated else
    if KeyComp('Smallint') then fExID := ptSmallint;
end;

function TmwBasePasLex.Func101: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Register') then fExID := ptRegister
  else if KeyComp('Platform') then fExID := ptPlatform // DR 2001-10-20
  else if KeyComp('Continue') then fExID := ptContinue;
end;

function TmwBasePasLex.Func102: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Function') then Result := ptFunction;
end;

function TmwBasePasLex.Func103: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Virtual') then fExID := ptVirtual;
end;

function TmwBasePasLex.Func104: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('WordBool') then fExID := ptWordBool;
end;

function TmwBasePasLex.Func105: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Procedure') then Result := ptProcedure;
end;

function TmwBasePasLex.Func106: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Protected') then fExID := ptProtected;
end;

function TmwBasePasLex.Func107: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Currency') then fExID := ptCurrency;
end;

function TmwBasePasLex.Func108: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Longword') then fExID := ptLongword;
  {$IFDEF D8_NEWER} //JThurman 2004-03-20
    if KeyComp('Operator') then fExID := ptOperator;
  {$ENDIF}
end;

function TmwBasePasLex.Func112: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Requires') then fExID := ptRequires;
end;

function TmwBasePasLex.Func117: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Exports') then Result := ptExports else
    if KeyComp('OleVariant') then fExID := ptOleVariant;
end;

function TmwBasePasLex.Func123: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Shortint') then fExID := ptShortint;
end;

function TmwBasePasLex.Func126: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Implements') then fExID := ptImplements;
end;

function TmwBasePasLex.Func127: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Runerror') then fExID := ptRunError;
end;

function TmwBasePasLex.Func128: TptTokenKind;
begin
  if KeyComp('WideString') then fExID := ptWideString;
  Result := ptIdentifier;
end;

function TmwBasePasLex.Func129: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Dispinterface') then Result := ptDispinterface
end;

function TmwBasePasLex.Func130: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('AnsiString') then fExID := ptAnsiString;
end;

function TmwBasePasLex.Func132: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Reintroduce') then fExID := ptReintroduce;
end;

function TmwBasePasLex.Func133: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Property') then Result := ptProperty;
end;

function TmwBasePasLex.Func136: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Finalization') then Result := ptFinalization;
end;

function TmwBasePasLex.Func141: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Writeonly') then fExID := ptWriteonly;
end;

function TmwBasePasLex.Func142: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('experimental') then fExID := ptExperimental;
end;

function TmwBasePasLex.Func143: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Destructor') then Result := ptDestructor;
end;

function TmwBasePasLex.Func166: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Constructor') then Result := ptConstructor else
    if KeyComp('Implementation') then Result := ptImplementation;
end;

function TmwBasePasLex.Func167: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('ShortString') then fExID := ptShortString;
end;

function TmwBasePasLex.Func168: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Initialization') then Result := ptInitialization;
end;

function TmwBasePasLex.Func191: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Resourcestring') then Result := ptResourcestring else
    if KeyComp('Stringresource') then fExID := ptStringresource;
end;

function TmwBasePasLex.AltFunc: TptTokenKind;
begin
  Result := ptIdentifier;
end;

function TmwBasePasLex.IdentKind: TptTokenKind;
var
  HashKey: Integer;
begin
  HashKey := KeyHash;
  if HashKey < 192 then
	Result := fIdentFuncTable[HashKey]
  else Result := ptIdentifier;
end;

procedure TmwBasePasLex.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      #0: fProcTable[I] := NullProc;
	  #10: fProcTable[I] := LFProc;
	  #13: fProcTable[I] := CRProc;
      #1..#9, #11, #12, #14..#32:
        fProcTable[I] := SpaceProc;
      '#': fProcTable[I] := AsciiCharProc;
      '$': fProcTable[I] := IntegerProc;
      #39: fProcTable[I] := StringProc;
      '0'..'9': fProcTable[I] := NumberProc;
      'A'..'Z', 'a'..'z', '_':
        fProcTable[I] := IdentProc;
      '{': fProcTable[I] := BraceOpenProc;
      '}': fProcTable[I] := BraceCloseProc;
      '!', '"', '%', '&', '('..'/', ':'..'@', '['..'^', '`', '~':
        begin
          case I of
            '(': fProcTable[I] := RoundOpenProc;
            ')': fProcTable[I] := RoundCloseProc;
            '*': fProcTable[I] := StarProc;
			'+': fProcTable[I] := PlusProc;
            ',': fProcTable[I] := CommaProc;
            '-': fProcTable[I] := MinusProc;
            '.': fProcTable[I] := PointProc;
            '/': fProcTable[I] := SlashProc;
            ':': fProcTable[I] := ColonProc;
            ';': fProcTable[I] := SemiColonProc;
            '<': fProcTable[I] := LowerProc;
            '=': fProcTable[I] := EqualProc;
            '>': fProcTable[I] := GreaterProc;
            '@': fProcTable[I] := AddressOpProc;
            '[': fProcTable[I] := SquareOpenProc;
            ']': fProcTable[I] := SquareCloseProc;
			'^': fProcTable[I] := PointerSymbolProc;
			'"': fProcTable[I] := StringDQProc; // DR 2002-01-14
      {$IFDEF D8_NEWER} //JThurman 2004-04-06
      '&': fProcTable[I] := AmpersandOpProc;
      {$ENDIF}
          else fProcTable[I] := SymbolProc;
          end;
        end;
    else fProcTable[I] := UnknownProc;
    end;
end;

constructor TmwBasePasLex.Create;
begin
  inherited Create;
  fOrigin := nil;
  InitIdent;
  MakeMethodTables;
  fExID := ptUnKnown;

  FUseDefines := True;
  FDefines := TStringList.Create;
  FTopDefineRec := nil;
  InitDefines;
end; { Create }

destructor TmwBasePasLex.Destroy;
begin
  ClearDefines; //If we don't do this, we get a memory leak
  FDefines.Free;
  fOrigin := nil;
  inherited Destroy;
end;

procedure TmwBasePasLex.DoProcTable(AChar: Char);
begin
  if Ord(AChar) <= 255 then
    fProcTable[AChar]
  else
  begin
    IdentProc;
  end;
end;

{ Destroy }

procedure TmwBasePasLex.SetOrigin(NewValue: PChar);
begin
  fOrigin := NewValue;
  Init;
  Next;
end; { SetOrigin }

procedure TmwBasePasLex.SetRunPos(Value: Integer);
begin
  Run := Value;
  Next;
end;

procedure TmwBasePasLex.AddDefine(const ADefine: string);
begin
  FDefines.Add(ADefine);
end;

procedure TmwBasePasLex.AddressOpProc;
begin
  case FOrigin[Run + 1] of
    '@':
      begin
        fTokenID := ptDoubleAddressOp;
        inc(Run, 2);
      end;
  else
    begin
      fTokenID := ptAddressOp;
      inc(Run);
    end;
  end;
end;

procedure TmwBasePasLex.AsciiCharProc;
begin
  fTokenID := ptAsciiChar;
  Inc(Run);
  if FOrigin[Run] = '$' then
  begin
    Inc(Run);
    while CharInSet(FOrigin[Run], ['0'..'9', 'A'..'F', 'a'..'f']) do Inc(Run);
  end else
  begin
    while IsDigit(FOrigin[Run]) do
      Inc(Run);
  end;
end;

procedure TmwBasePasLex.BraceCloseProc;
begin
  inc(Run);
  fTokenId := ptError;
  if Assigned(FOnMessage) then
	FOnMessage(Self, meError, 'Illegal character', PosXY.X, PosXY.Y);
end;

procedure TmwBasePasLex.BorProc;
begin
  fTokenID := ptBorComment;
  case FOrigin[Run] of
    #0:
      begin
		NullProc;
        if Assigned(FOnMessage) then
          FOnMessage(Self, meError, 'Unexpected file end', PosXY.X, PosXY.Y);
        exit;
      end;
{				DR 2001-08-02

    #10:
      begin
        LFProc;
        exit;
      end;

	#13:
	  begin
		CRProc;
		exit;
	  end;
}
  end;

  while FOrigin[Run] <> #0 do
	case FOrigin[Run] of
	  '}':
		begin
		  fCommentState := csNo;
		  inc(Run);
		  break;
		end;
{				DR 2001-08-02
	  #10: break;

	  #13: break;
}
	  #10:
		begin
			inc(Run);
			inc(fLineNumber);
			fLinePos := Run;
		end;
	  #13:
		begin
			inc(Run);
			if FOrigin[Run] = #10 then inc( Run );
			inc(fLineNumber);
			fLinePos := Run;
		end;
	else inc(Run);
	end;
end;

procedure TmwBasePasLex.BraceOpenProc;
var
  Param, Def: string;
begin
  case FOrigin[Run + 1] of
    '$': fTokenID := GetDirectiveKind;
  else
    begin
      fTokenID := ptBorComment;
      fCommentState := csBor;
    end;
  end;
  inc(Run);
  while FOrigin[Run] <> #0 do
    case FOrigin[Run] of
      '}':
        begin
          fCommentState := csNo;
          inc(Run);
          break;
		end;
	  #10:
		begin
			inc(Run);
			inc(fLineNumber);
			fLinePos := Run;
		end;
	  #13:
		begin
			inc(Run);
			if FOrigin[Run] = #10 then inc( Run );
			inc(fLineNumber);
			fLinePos := Run;
		end;
{      #10: break;  DR 2001-10-12

      #13: break;}
    else inc(Run);
    end;
  case fTokenID of
    PtCompDirect:
      begin
        if Assigned(fOnCompDirect) then
          fOnCompDirect(Self);
      end;
    PtDefineDirect:
      begin
        if FUseDefines then
          AddDefine(DirectiveParam);
        if Assigned(fOnDefineDirect) then
          fOnDefineDirect(Self);
      end;
    PtElseDirect:
      begin
        if FUseDefines then
        begin
          if FTopDefineRec <> nil then
          begin
            if FTopDefineRec^.Defined then
              Inc(FDefineStack)
            else
              if FDefineStack > 0 then
                Dec(FDefineStack);
          end;
        end;
        if Assigned(fOnElseDirect) then
          fOnElseDirect(Self);
      end;
    PtEndIfDirect:
      begin
        if FUseDefines then
          ExitDefineBlock;
        if Assigned(fOnEndIfDirect) then
          fOnEndIfDirect(Self);
      end;
    PtIfDefDirect:
      begin
        if FUseDefines then
          EnterDefineBlock(IsDefined(DirectiveParam));
        if Assigned(fOnIfDefDirect) then
          fOnIfDefDirect(Self);
      end;
    PtIfNDefDirect:
      begin
        if FUseDefines then
          EnterDefineBlock(not IsDefined(DirectiveParam));
    		if Assigned(fOnIfNDefDirect) then
          fOnIfNDefDirect(Self);
      end;
    PtIfOptDirect:
      begin
        if FUseDefines then
          EnterDefineBlock(False);
        if Assigned(fOnIfOptDirect) then
          fOnIfOptDirect(Self);
      end;
    PtIfDirect:
      begin
        if FUseDefines then
        begin
          Param := DirectiveParam;
          if Pos('DEFINED', Param) = 1 then
          begin
            Def := Copy(Param, 9, Length(Param) - 9);
            EnterDefineBlock(IsDefined(Def));
          end else
          if Pos('NOT DEFINED', Param) = 1 then
          begin
            Def := Copy(Param, 13, Length(Param) - 13);
            EnterDefineBlock(not IsDefined(Def));
          end else
            EnterDefineBlock(False);
        end;
        if Assigned(fOnIfDirect) then
          fOnIfDirect(Self);
      end;
    PtIfEndDirect:
      begin
        if FUseDefines then
          ExitDefineBlock;
        if Assigned(fOnIfEndDirect) then
          fOnIfEndDirect(Self);
      end;
    PtElseIfDirect:
      begin
        if FUseDefines then
        begin
          if FTopDefineRec <> nil then
          begin
            if FTopDefineRec^.Defined then
              Inc(FDefineStack)
            else
            begin
              if FDefineStack > 0 then
                Dec(FDefineStack);
              Param := DirectiveParam;
              if Pos('DEFINED', Param) = 1 then
              begin
                Def := Copy(Param, 9, Length(Param) - 9);
                EnterDefineBlock(IsDefined(Def));
              end;
            end;
          end;
        end;
        if Assigned(fOnElseIfDirect) then
          fOnElseIfDirect(Self);
      end;
    PtIncludeDirect:
      begin
        if Assigned(fOnIncludeDirect) then
          fOnIncludeDirect(Self);
      end;
    PtResourceDirect:
      begin
        if Assigned(fOnResourceDirect) then
          fOnResourceDirect(Self);
      end;
    PtUndefDirect:
      begin
        if FUseDefines then
          RemoveDefine(DirectiveParam);
        if Assigned(fOnUndefDirect) then
          fOnUndefDirect(Self);
      end;
  end;
end;

procedure TmwBasePasLex.ColonProc;
begin
  case FOrigin[Run + 1] of
    '=':
      begin
        inc(Run, 2);
        fTokenID := ptAssign;
	  end;
  else
    begin
      inc(Run);
      fTokenID := ptColon;
    end;
  end;
end;

procedure TmwBasePasLex.CommaProc;
begin
  inc(Run);
  fTokenID := ptComma;
end;

procedure TmwBasePasLex.CRProc;
begin
  case fCommentState of
    csBor: fTokenID := ptCRLFCo;
    csAnsi: fTokenID := ptCRLFCo;
  else fTokenID := ptCRLF;
  end;

  case FOrigin[Run + 1] of
    #10: inc(Run, 2);
  else inc(Run);
  end;
  inc(fLineNumber);
  fLinePos := Run;
end;

procedure TmwBasePasLex.EnterDefineBlock(ADefined: Boolean);
var
  StackFrame: PDefineRec;
begin
  New(StackFrame);
  StackFrame^.Next := FTopDefineRec;
  StackFrame^.Defined := ADefined;
  StackFrame^.StartCount := FDefineStack;
  FTopDefineRec := StackFrame;
  if not ADefined then
    Inc(FDefineStack);
end;

procedure TmwBasePasLex.EqualProc;
begin
  inc(Run);
  fTokenID := ptEqual;
end;

procedure TmwBasePasLex.ExitDefineBlock;
var
  StackFrame: PDefineRec;
begin
  StackFrame := FTopDefineRec;
  if StackFrame <> nil then
  begin
    FDefineStack := StackFrame^.StartCount;
    FTopDefineRec := StackFrame^.Next;
    Dispose(StackFrame);
  end;
end;
procedure TmwBasePasLex.GreaterProc;
begin
  case FOrigin[Run + 1] of
    '=':
      begin
        inc(Run, 2);
        fTokenID := ptGreaterEqual;
      end;
  else
    begin
      inc(Run);
      fTokenID := ptGreater;
	end;
  end;
end;

function TmwBasePasLex.HashValue(AChar: Char): Integer;
begin
  if AChar <= #255 then
    Result := mHashTable[fOrigin[Run]]
  else
    Result := Ord(AChar);
end;

procedure TmwBasePasLex.IdentProc;
begin
  fTokenID := IdentKind;
end;

procedure TmwBasePasLex.IntegerProc;
begin
  inc(Run);
  fTokenID := ptIntegerConst;
  while CharInSet(FOrigin[Run], ['0'..'9', 'A'..'F', 'a'..'f']) do
    inc(Run);
end;

function TmwBasePasLex.IsDefined(const ADefine: string): Boolean;
begin
  Result := FDefines.IndexOf(ADefine) > -1;
end;

function TmwBasePasLex.IsIdentifiers(AChar: Char): Boolean;
begin
  Result := AChar.IsLetterOrDigit or (AChar = '_');
end;

procedure TmwBasePasLex.LFProc;
begin
  case fCommentState of
	csBor: fTokenID := ptCRLFCo;
	csAnsi: fTokenID := ptCRLFCo;
  else fTokenID := ptCRLF;
  end;
  inc(Run);
  inc(fLineNumber);
  fLinePos := Run;
end;

procedure TmwBasePasLex.LowerProc;
begin
  case FOrigin[Run + 1] of
    '=':
      begin
        inc(Run, 2);
        fTokenID := ptLowerEqual;
      end;
    '>':
      begin
        inc(Run, 2);
        fTokenID := ptNotEqual;
      end
  else
    begin
      inc(Run);
      fTokenID := ptLower;
    end;
  end;
end;

procedure TmwBasePasLex.MinusProc;
begin
  inc(Run);
  fTokenID := ptMinus;
end;

procedure TmwBasePasLex.NullProc;
begin
  fTokenID := ptNull;
end;

procedure TmwBasePasLex.NumberProc;
begin
  inc(Run);
  fTokenID := ptIntegerConst;
  while CharInSet(FOrigin[Run], ['0'..'9', '.', 'e', 'E']) do
  begin
    case FOrigin[Run] of
      '.':
        if FOrigin[Run + 1] = '.' then
          break
        else fTokenID := ptFloat
    end;
    inc(Run);
  end;
end;

procedure TmwBasePasLex.PlusProc;
begin
  inc(Run);
  fTokenID := ptPlus;
end;

procedure TmwBasePasLex.PointerSymbolProc;
begin
  inc(Run);
  fTokenID := ptPointerSymbol;
  
  //This is a wierd Pascal construct that rarely appears, but needs to be 
  //supported. ^M is a valid char reference (#13, in this case)
  if CharInSet(FOrigin[Run], ['a'..'z','A'..'Z']) and not IsIdentifiers(FOrigin[Run+1]) then
  begin
    inc(Run);
    fTokenID := ptAsciiChar;
  end;
end;

procedure TmwBasePasLex.PointProc;
begin
  case FOrigin[Run + 1] of
    '.':
      begin
        inc(Run, 2);
        fTokenID := ptDotDot;
      end;
    ')':
      begin
        inc(Run, 2);
        fTokenID := ptSquareClose;
      end;
  else
    begin
      inc(Run);
      fTokenID := ptPoint;
    end;
  end;
end;

procedure TmwBasePasLex.RemoveDefine(const ADefine: string);
var
  I: Integer;
begin
  I := FDefines.IndexOf(ADefine);
  if I > -1 then
    FDefines.Delete(I);
end;

procedure TmwBasePasLex.RoundCloseProc;
begin
  inc(Run);
  fTokenID := ptRoundClose;
end;

procedure TmwBasePasLex.AnsiProc;
begin
  fTokenID := ptAnsiComment;
  case FOrigin[Run] of
    #0:
      begin
        NullProc;
        if Assigned(FOnMessage) then
          FOnMessage(Self, meError, 'Unexpected file end', PosXY.X, PosXY.Y);
        exit;
      end;

{				DR 2001-08-02
	#10:
      begin
		LFProc;
        exit;
      end;

    #13:
      begin
        CRProc;
        exit;
      end;
}
  end;

  while fOrigin[Run] <> #0 do
    case fOrigin[Run] of
      '*':
        if fOrigin[Run + 1] = ')' then
        begin
          fCommentState := csNo;
          inc(Run, 2);
          break;
        end
        else inc(Run);
{                DR 2001-08-02
      #10: break;

	  #13: break;
}
	  #10:
		begin
			inc(Run);
			inc(fLineNumber);
			fLinePos := Run;
		end;
	  #13:
		begin
			inc(Run);
			if FOrigin[Run] = #10 then inc( Run );
			inc(fLineNumber);
			fLinePos := Run;
		end;
	else inc(Run);
    end;
end;

procedure TmwBasePasLex.RoundOpenProc;
begin
  inc(Run);
  case fOrigin[Run] of
    '*':
      begin
        fTokenID := ptAnsiComment;
        if FOrigin[Run + 1] = '$' then
          fTokenID := GetDirectiveKind
        else fCommentState := csAnsi;
        inc(Run);
        while fOrigin[Run] <> #0 do
          case fOrigin[Run] of
            '*':
			  if fOrigin[Run + 1] = ')' then
			  begin
				fCommentState := csNo;
				inc(Run, 2);
				break;
			  end
			  else inc(Run);
{								DR 2001-08-02
			#10: break;
			#13: break;
}
			  #10:
				begin
					inc(Run);
					inc(fLineNumber);
					fLinePos := Run;
				end;
			  #13:
				begin
					inc(Run);
					if FOrigin[Run] = #10 then inc( Run );
					inc(fLineNumber);
					fLinePos := Run;
				end;
			else inc(Run);
          end;
      end;
    '.':
      begin
        inc(Run);
        fTokenID := ptSquareOpen;
      end;
  else fTokenID := ptRoundOpen;
  end;
  case fTokenID of
    PtCompDirect:
      begin
        if Assigned(fOnCompDirect) then
          fOnCompDirect(Self);
      end;
    PtDefineDirect:
      begin
        if Assigned(fOnDefineDirect) then
          fOnDefineDirect(Self);
      end;
    PtElseDirect:
      begin
        if Assigned(fOnElseDirect) then
          fOnElseDirect(Self);
      end;
    PtEndIfDirect:
      begin
        if Assigned(fOnEndIfDirect) then
          fOnEndIfDirect(Self);
      end;
    PtIfDefDirect:
      begin
        if Assigned(fOnIfDefDirect) then
          fOnIfDefDirect(Self);
      end;
    PtIfNDefDirect:
      begin
        if Assigned(fOnIfNDefDirect) then
          fOnIfNDefDirect(Self);
      end;
    PtIfOptDirect:
      begin
        if Assigned(fOnIfOptDirect) then
          fOnIfOptDirect(Self);
      end;
    PtIncludeDirect:
      begin
        if Assigned(fOnIncludeDirect) then
          fOnIncludeDirect(Self);
      end;
    PtResourceDirect:
      begin
        if Assigned(fOnResourceDirect) then
          fOnResourceDirect(Self);
      end;
    PtUndefDirect:
      begin
        if Assigned(fOnUndefDirect) then
          fOnUndefDirect(Self);
      end;
  end;
end;

procedure TmwBasePasLex.SemiColonProc;
begin
  inc(Run);
  fTokenID := ptSemiColon;
end;

procedure TmwBasePasLex.SlashProc;
begin
  case FOrigin[Run + 1] of
    '/':
      begin
        inc(Run, 2);
        fTokenID := ptSlashesComment;
        while FOrigin[Run] <> #0 do
        begin
          case FOrigin[Run] of
            #10, #13: break;
          end;
          inc(Run);
        end;
      end;
  else
    begin
      inc(Run);
      fTokenID := ptSlash;
    end;
  end;
end;

procedure TmwBasePasLex.SpaceProc;
begin
  Inc(Run);
  fTokenID := ptSpace;
  while CharInSet(FOrigin[Run], [#1..#9, #11, #12, #14..#32]) do
    Inc(Run);
end;

procedure TmwBasePasLex.SquareCloseProc;
begin
  inc(Run);
  fTokenID := ptSquareClose;
end;

procedure TmwBasePasLex.SquareOpenProc;
begin
  inc(Run);
  fTokenID := ptSquareOpen;
end;

procedure TmwBasePasLex.StarProc;
begin
  inc(Run);
  fTokenID := ptStar;
end;

procedure TmwBasePasLex.StringProc;
begin
  fTokenID := ptStringConst;
  repeat
	inc(Run);
	case FOrigin[Run] of
	  #0, #10, #13:
		begin
		  if Assigned(FOnMessage) then
			FOnMessage(Self, meError, 'Unterminated string', PosXY.X, PosXY.Y);
		  break;
		end;
	  #39:
		begin
		  while (FOrigin[Run] = #39) and (FOrigin[Run + 1] = #39) do
		  begin
			inc(Run, 2);
		  end;
		end;
	end;
  until FOrigin[Run] = #39;
  if FOrigin[Run] = #39 then
  begin
	inc(Run);
	if TokenLen = 3 then
	begin
	  fTokenID := ptAsciiChar;
	end;
  end;
end;

procedure TmwBasePasLex.SymbolProc;
begin
  inc(Run);
  fTokenID := ptSymbol;
end;

procedure TmwBasePasLex.UnknownProc;
begin
  inc(Run);
  fTokenID := ptUnknown;
  if Assigned(FOnMessage) then
   FOnMessage(Self, meError, 'Unknown Character', PosXY.X, PosXY.Y);
end;

procedure TmwBasePasLex.Next;
begin
  fExID := ptUnKnown;
  fTokenPos := Run;
  case fCommentState of
    csNo:
    begin
      DoProcTable(fOrigin[Run]);
     (*{$IFDEF D10_NEWER}
     if fOrigin[Run] < #256 then
       fProcTable[fOrigin[Run]]
     else //non-ASCII unicode char
       IdentProc;
     {$ELSE}
     fProcTable[fOrigin[Run]];
     {$ENDIF}*)
    end;
  else
    case fCommentState of
      csBor: BorProc;
      csAnsi: AnsiProc;
    end;
  end;
end;


function TmwBasePasLex.GetIsJunk: Boolean;
begin
  result := IsTokenIDJunk(FTokenID) or (FUseDefines and (FDefineStack > 0) and (TokenID <> ptNull));
//  Result := fTokenID in [ptAnsiComment, ptBorComment, ptCRLF, ptCRLFCo, ptSlashesComment, ptSpace]; //XM 20001210
end;

function TmwBasePasLex.GetIsSpace: Boolean;
begin
  Result := fTokenID in [ptCRLF, ptSpace];
end;

function TmwBasePasLex.GetToken: string;
begin
  SetString(Result, (FOrigin + fTokenPos), GetTokenLen);
end;

function TmwBasePasLex.GetTokenLen: Integer;
begin
  Result := Run - fTokenPos;
end;

procedure TmwBasePasLex.NextID(ID: TptTokenKind);
begin
  repeat
    case fTokenID of
      ptNull: break;
    else Next;
    end;
  until fTokenID = ID;
end;

procedure TmwBasePasLex.NextNoJunk;
begin
  repeat
    Next;
  until not IsJunk;
end;

procedure TmwBasePasLex.NextNoSpace;
begin
  repeat
    Next;
  until not IsSpace;
end;

function TmwBasePasLex.FirstInLine: Boolean;
var
  RunBack: Integer;
begin
  Result := True;
  if fTokenPos = 0 then exit;
  RunBack := fTokenPos;
  dec(RunBack);
  while CharInSet(fOrigin[RunBack], [#1..#9, #11, #12, #14..#32]) do
    Dec(RunBack);
  if RunBack = 0 then exit;
  case fOrigin[RunBack] of
    #10, #13: exit;
  else
    begin
      Result := False;
      exit;
    end;
  end;
end;

function TmwBasePasLex.GetCommentState: Pointer;
begin
  Result := Pointer(fCommentState);
end;

function TmwBasePasLex.GetCompilerDirective: string;
var
  DirectLen: Integer;
begin
  if TokenID <> ptCompDirect then
    Result := ''
  else
    case fOrigin[fTokenPos] of
      '(':
        begin
          DirectLen := Run - fTokenPos - 4;
          SetString(Result, (FOrigin + fTokenPos + 2), DirectLen);
          Result := UpperCase(Result);
        end;
      '{':
        begin
          DirectLen := Run - fTokenPos - 2;
          SetString(Result, (FOrigin + fTokenPos + 1), DirectLen);
          Result := UpperCase(Result);
        end;
    end;
end;

function TmwBasePasLex.GetDirectiveKind: TptTokenKind;
var
  TempPos: Integer;
begin
  case fOrigin[fTokenPos] of
    '(': Run := FTokenPos + 3;
    '{': Run := FTokenPos + 2;
  end;
  FDirectiveParamOrigin := FOrigin + FTokenPos;
  TempPos := fTokenPos;
  fTokenPos := Run;
  case KeyHash of
    9:
      if KeyComp('I') then
        Result := ptIncludeDirect else
        Result := ptCompDirect;
    15:
      if KeyComp('IF') then
        Result := ptIfDirect else
        Result := ptCompDirect;
    18:
      if KeyComp('R') then
      begin
        if not CharInSet(fOrigin[Run], ['+', '-']) then
          Result := ptResourceDirect else Result := ptCompDirect;
      end else Result := ptCompDirect;
    30:
      if KeyComp('IFDEF') then
        Result := ptIfDefDirect else
        Result := ptCompDirect;
    38:
      if KeyComp('ENDIF') then
        Result := ptEndIfDirect else
      if KeyComp('IFEND') then
        Result := ptIfEndDirect else
        Result := ptCompDirect;
    41:
      if KeyComp('ELSE') then
        Result := ptElseDirect else
        Result := ptCompDirect;
    43:
      if KeyComp('DEFINE') then
        Result := ptDefineDirect else
        Result := ptCompDirect;
    44:
      if KeyComp('IFNDEF') then
        Result := ptIfNDefDirect else
        Result := ptCompDirect;
    50:
      if KeyComp('UNDEF') then
        Result := ptUndefDirect else
        Result := ptCompDirect;
    56:
      if KeyComp('ELSEIF') then
        Result := ptElseIfDirect else
        Result := ptCompDirect;
    66:
      if KeyComp('IFOPT') then
        Result := ptIfOptDirect else
        Result := ptCompDirect;
    68:
      if KeyComp('INCLUDE') then
        Result := ptIncludeDirect else
        Result := ptCompDirect;
    104:
      if KeyComp('Resource') then
        Result := ptResourceDirect else
        Result := ptCompDirect;
  else Result := ptCompDirect;
  end;
  fTokenPos := TempPos;
  dec(Run);
end;

function TmwBasePasLex.GetDirectiveParam: string;
var
  EndPos: Integer;
  ParamLen: Integer;
begin
  // !! without this set... there is a warning?
  EndPos:= 0;
  case fOrigin[fTokenPos] of
    '(':
      begin
        TempRun := FTokenPos + 3;
        EndPos := Run - 2;
      end;
    '{':
      begin
        TempRun := FTokenPos + 2;
        EndPos := Run - 1;
      end;
  end;
  while IsIdentifiers(fOrigin[TempRun]) do
    inc(TempRun);
  while CharInSet(fOrigin[TempRun], ['+', ',', '-']) do
  begin
    inc(TempRun);
    while IsIdentifiers(fOrigin[TempRun]) do
      inc(TempRun);
    if CharInSet(fOrigin[TempRun - 1], ['+', ',', '-']) and (fOrigin[TempRun] = ' ')
      then inc(TempRun);
  end;
  if fOrigin[TempRun] = ' ' then inc(TempRun);
  ParamLen := EndPos - TempRun;
  SetString(Result, (FOrigin + TempRun), ParamLen);
  Result := UpperCase(Result);
end;

procedure TmwBasePasLex.Init;
begin
  fCommentState := csNo;
  fLineNumber := 0;
  fLinePos := 0;
  Run := 0;
  InitDefines;
end;

procedure TmwBasePasLex.InitFrom(ALexer: TmwBasePasLex);
begin
  Origin := ALexer.Origin;
  fCommentState := ALexer.fCommentState;
  fLineNumber := ALexer.fLineNumber;
  fLinePos := ALexer.fLinePos;
  Run := ALexer.Run;
  CloneDefinesFrom(ALexer);
end;

procedure TmwBasePasLex.InitDefines;
begin
  ClearDefines;
  Exit;
  //Set up the defines that are defined by the compiler
  {$IFDEF VER130}
  AddDefine('VER130');
  {$ENDIF}
  {$IFDEF VER140}
  AddDefine('VER140');
  {$ENDIF}
  {$IFDEF VER150}
  AddDefine('VER150');
  {$ENDIF}
  {$IFDEF VER160}
  AddDefine('VER160');
  {$ENDIF}
  {$IFDEF VER170}
  AddDefine('VER170');
  {$ENDIF}
  {$IFDEF VER180}
  AddDefine('VER180');
  {$ENDIF}
  {$IFDEF VER185}
  AddDefine('VER185');
  {$ENDIF}
  {$IFDEF VER190}
  AddDefine('VER190');
  {$ENDIF}
  {$IFDEF VER200}
  AddDefine('VER200');
  {$ENDIF}
  {$IFDEF WIN32}
  AddDefine('WIN32');
  {$ENDIF}
  {$IFDEF LINUX}
  AddDefine('LINUX');
  {$ENDIF}
  {$IFDEF CPU386}
  AddDefine('CPU386');
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  AddDefine('MSWINDOWS');
  {$ENDIF}
  {$IFDEF CONDITIONALEXPRESSIONS}
  AddDefine('CONDITIONALEXPRESSIONS');
  {$ENDIF}
  {$IFDEF UNICODE}
  AddDefine('UNICODE');
  {$ENDIF}
end;

procedure TmwBasePasLex.InitLine;
begin
  fLineNumber := 0;
  fLinePos := 0;
  Run := 0;
end;

procedure TmwBasePasLex.SetCommentState(const Value: Pointer);
begin
  fCommentState := TCommentState(Value);
end;

procedure TmwBasePasLex.SetLine(const Value: string);
begin
  fOrigin := PChar(Value);
  InitLine;
  Next;
end;

function TmwBasePasLex.GetStringContent: string;
var
  TempString: string;
  sEnd: Integer;
begin
  if TokenID <> ptStringConst then
    Result := ''
  else
  begin
    TempString := Token;
    sEnd := Length(TempString);
    if TempString[sEnd] <> #39 then inc(sEnd);
    Result := Copy(TempString, 2, sEnd - 2);
    TempString := '';
  end;
end;

function TmwBasePasLex.GetIsOrdIdent: Boolean;
begin
  Result := False;
  if fTokenID = ptIdentifier then
    Result := fExID in [ptBoolean, ptByte, ptChar, ptDWord, ptInt64, ptInteger,
      ptLongInt, ptLongWord, ptPChar, ptShortInt, ptSmallInt, ptWideChar, ptWord]
end;

function TmwBasePasLex.GetIsOrdinalType: Boolean;
begin
  Result := GetIsOrdIdent or (fTokenID in [ptAsciiChar, ptIntegerConst]);
end;

function TmwBasePasLex.GetIsRealType: Boolean;
begin
  Result := False;
  if fTokenID = ptIdentifier then
    Result := fExID in [ptComp, ptCurrency, ptDouble, ptExtended, ptReal, ptReal48, ptSingle]
end;

function TmwBasePasLex.GetIsStringType: Boolean;
begin
  Result := False;
  if fTokenID = ptIdentifier then
    Result := fExID in [ptAnsiString, ptWideString]
  else
    if fTokenID = ptString then
      Result := True
    else
      if fTokenID = ptStringConst then Result := True;
end;

function TmwBasePasLex.GetIsVarantType: Boolean;
begin
  Result := False;
  if fTokenID = ptIdentifier then
    Result := fExID in [ptOleVariant, ptVariant]
end;

function TmwBasePasLex.GetIsAddOperator: Boolean;
begin
  Result := fTokenID in [ptMinus, ptOr, ptPlus, ptXor];
end;

function TmwBasePasLex.GetIsMulOperator: Boolean;
begin
  Result := fTokenID in [ptAnd, ptAs, ptDiv, ptMod, ptShl, ptShr, ptSlash, ptStar];
end;

function TmwBasePasLex.GetIsRelativeOperator: Boolean;
begin
  Result := fTokenID in [ptAs, ptEqual, ptGreater, ptGreaterEqual, ptLower, ptLowerEqual,
    ptIn, ptIs, ptNotEqual];
end;

function TmwBasePasLex.GetIsCompilerDirective: Boolean;
begin
  Result := fTokenID in [ptCompDirect, ptDefineDirect, ptElseDirect,
    ptEndIfDirect, ptIfDefDirect, ptIfNDefDirect, ptIfOptDirect,
    ptIncludeDirect, ptResourceDirect, ptUndefDirect];
end;

function TmwBasePasLex.GetGenID: TptTokenKind;
begin
  Result := fTokenID;
  if fTokenID = ptIdentifier then
    if fExID <> ptUnknown then Result := fExID;
end;

{ TmwPasLex }

constructor TmwPasLex.Create;
begin
  inherited Create;
  fAheadLex := TmwBasePasLex.Create;
end;

destructor TmwPasLex.Destroy;
begin
  fAheadLex.Free;
  inherited Destroy;
end;

procedure TmwPasLex.SetOrigin(NewValue: PChar);
begin
  inherited SetOrigin(NewValue);
  fAheadLex.SetOrigin(NewValue);
end;

procedure TmwPasLex.SetLine(const Value: string);
begin
  inherited SetLine(Value);
  fAheadLex.SetLine(Value);
end;

procedure TmwPasLex.AheadNext;
begin
  fAheadLex.NextNoJunk;
end;

function TmwPasLex.GetAheadExID: TptTokenKind;
begin
  Result := fAheadLex.ExID;
end;

function TmwPasLex.GetAheadGenID: TptTokenKind;
begin
  Result := fAheadLex.GenID;
end;

function TmwPasLex.GetAheadToken: string;
begin
  Result := fAheadLex.Token;
end;

function TmwPasLex.GetAheadTokenID: TptTokenKind;
begin
  Result := fAheadLex.TokenID;
end;

procedure TmwPasLex.InitAhead;
begin
  fAheadLex.CommentState := CommentState;
  fAheadLex.RunPos := RunPos;
  FAheadLex.fLineNumber := FLineNumber;
  FAheadLex.FLinePos := FLinePos;

  FAheadLex.CloneDefinesFrom(Self);

  //FAheadLex.FTokenPos := FTokenPos;
  while IsTokenIDJunk(fAheadLex.TokenID) do
    fAheadLex.Next;
end;

function TmwPasLex.GetStatus: TmwPasLexStatus;
begin
  Result.CommentState := fCommentState;
  Result.ExID := fExID;
  Result.LineNumber := fLineNumber;
  Result.LinePos := fLinePos;
  Result.Origin := fOrigin;
  Result.RunPos := Run;
  Result.TokenPos := fTokenPos;
  Result.TokenID := fTokenID;
end;

procedure TmwPasLex.SetStatus(const Value: TmwPasLexStatus);
begin
  fCommentState := Value.CommentState;
  fExID := Value.ExID;
  fLineNumber := Value.LineNumber;
  fLinePos := Value.LinePos;
  fOrigin := Value.Origin;
  Run := Value.RunPos;
  fTokenPos := Value.TokenPos;
  fTokenID := Value.TokenID;
  fAheadLex.Origin := Value.Origin;
end;

procedure TmwBasePasLex.SetOnCompDirect(const Value: TDirectiveEvent);
begin
  fOnCompDirect := Value;
end;

procedure TmwBasePasLex.SetOnDefineDirect(const Value: TDirectiveEvent);
begin
  fOnDefineDirect := Value;
end;

procedure TmwBasePasLex.SetOnElseDirect(const Value: TDirectiveEvent);
begin
  fOnElseDirect := Value;
end;

procedure TmwBasePasLex.SetOnElseIfDirect(const Value: TDirectiveEvent);
begin
  fOnElseIfDirect := Value;
end;

procedure TmwBasePasLex.SetOnEndIfDirect(const Value: TDirectiveEvent);
begin
  fOnEndIfDirect := Value;
end;

procedure TmwBasePasLex.SetOnIfDefDirect(const Value: TDirectiveEvent);
begin
  fOnIfDefDirect := Value;
end;

procedure TmwBasePasLex.SetOnIfDirect(const Value: TDirectiveEvent);
begin
  FOnIfDirect := Value;
end;

procedure TmwBasePasLex.SetOnIfEndDirect(const Value: TDirectiveEvent);
begin
  FOnIfEndDirect := Value;
end;

procedure TmwBasePasLex.SetOnIfNDefDirect(const Value: TDirectiveEvent);
begin
  fOnIfNDefDirect := Value;
end;

procedure TmwBasePasLex.SetOnIfOptDirect(const Value: TDirectiveEvent);
begin
  fOnIfOptDirect := Value;
end;

procedure TmwBasePasLex.SetOnIncludeDirect(const Value: TDirectiveEvent);
begin
  fOnIncludeDirect := Value;
end;

procedure TmwBasePasLex.SetOnResourceDirect(const Value: TDirectiveEvent);
begin
  fOnResourceDirect := Value;
end;

procedure TmwBasePasLex.SetOnUnDefDirect(const Value: TDirectiveEvent);
begin
  fOnUnDefDirect := Value;
end;

procedure TmwPasLex.SetOnCompDirect(const Value: TDirectiveEvent);
begin
  inherited;
  //AheadLex.OnCompDirect := Value;
end;

procedure TmwPasLex.SetOnDefineDirect(const Value: TDirectiveEvent);
begin
  inherited;
  //AheadLex.OnDefineDirect := Value;
end;

procedure TmwPasLex.SetOnElseDirect(const Value: TDirectiveEvent);
begin
  inherited;
  //AheadLex.OnElseDirect := Value;
end;

procedure TmwPasLex.SetOnEndIfDirect(const Value: TDirectiveEvent);
begin
  inherited;
  //AheadLex.OnEndIfDirect := Value;
end;

procedure TmwPasLex.SetOnIfDefDirect(const Value: TDirectiveEvent);
begin
  inherited;
  //AheadLex.OnIfDefDirect := Value;
end;

procedure TmwPasLex.SetOnIfNDefDirect(const Value: TDirectiveEvent);
begin
  inherited;
  //AheadLex.OnIfNDefDirect := Value;
end;

procedure TmwPasLex.SetOnIfOptDirect(const Value: TDirectiveEvent);
begin
  inherited;
  //AheadLex.OnIfOptDirect := Value;
end;

procedure TmwPasLex.SetOnIncludeDirect(const Value: TDirectiveEvent);
begin
  inherited;
  //AheadLex.OnIncludeDirect := Value;
end;

procedure TmwPasLex.SetOnResourceDirect(const Value: TDirectiveEvent);
begin
  inherited;
  //AheadLex.OnResourceDirect := Value;
end;

procedure TmwPasLex.SetOnUnDefDirect(const Value: TDirectiveEvent);
begin
  inherited;
  //AheadLex.OnUnDefDirect := Value;
end;

function TmwBasePasLex.Func86: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Varargs') then fExID := ptVarargs;
end;

procedure TmwBasePasLex.StringDQProc;
begin
	if not fAsmCode then
	begin
		SymbolProc;
		Exit;
	end;
  fTokenID := ptStringDQConst;
  repeat
	inc(Run);
	case FOrigin[Run] of
	  #0, #10, #13:
		begin
		  if Assigned(FOnMessage) then
			FOnMessage(Self, meError, 'Unterminated string', PosXY.X, PosXY.Y);
		  break;
		end;
	  '\':
		begin
		  Inc( Run );
		  if FOrigin[Run] in [#32..#255] then Inc( Run );
		end;
	end;
  until FOrigin[Run] = '"';
  if FOrigin[Run] = '"' then
	inc(Run);
end;

{$IFDEF D8_NEWER} //JThurman 2004-04-06
procedure TmwBasePasLex.AmpersandOpProc;
begin
  FTokenID := ptAmpersand;
  inc(Run);
  while CharInSet(FOrigin[Run], ['a'..'z', 'A'..'Z','0'..'9']) do
    inc(Run);
  FTokenID := ptIdentifier;
end;
{$ENDIF}

initialization
  MakeIdentTable;
end.

