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

Contributor(s):  James Jacobson, LaKraven Studios Ltd, Roman Yankovsky
(This list is ALPHABETICAL)

Last Modified: mm/dd/yyyy
Current Version: 2.25

Notes: This program is a very fast Pascal tokenizer. I'd like to invite the
Delphi community to develop it further and to create a fully featured Object
Pascal parser.

Modification history:

LaKraven Studios Ltd, January 2015:

- Cleaned up version-specifics up to XE8
- Fixed all warnings & hints

Daniel Rolf between 20010723 and 20020116

Made ready for Delphi 6

platform
deprecated
varargs
local

Known Issues:
-----------------------------------------------------------------------------}

unit SimpleParser.Lexer;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

{$I SimpleParser.inc}

interface

uses
  SysUtils, Classes, Character, SimpleParser.Lexer.Types;

var
  Identifiers: array[#0..#127] of ByteBool;
  mHashTable: array[#0..#127] of Integer;

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
    FCommentState: TCommentState;
    FOrigin: PChar;
    FProcTable: array[#0..#127] of procedure of object;
    Run: Integer;
    RunAhead: Integer;
    TempRun: Integer;
    EndOfIncludedArea: Integer;
    IncludedLineCount: Integer;
    BufferSize: integer;
    FIdentFuncTable: array[0..191] of function: TptTokenKind of object;
    FTokenPos: Integer;
    FLineNumber: Integer;
    FTokenID: TptTokenKind;
    FLinePos: Integer;
    FExID: TptTokenKind;
    FOnMessage: TMessageEvent;
    FOnCompDirect: TDirectiveEvent;
    FOnElseDirect: TDirectiveEvent;
    FOnEndIfDirect: TDirectiveEvent;
    FOnIfDefDirect: TDirectiveEvent;
    FOnIfNDefDirect: TDirectiveEvent;
    FOnResourceDirect: TDirectiveEvent;
    FOnIncludeDirect: TDirectiveEvent;
    FOnDefineDirect: TDirectiveEvent;
    FOnIfOptDirect: TDirectiveEvent;
    FOnIfDirect: TDirectiveEvent;
    FOnIfEndDirect: TDirectiveEvent;
    FOnElseIfDirect: TDirectiveEvent;
    FOnUnDefDirect: TDirectiveEvent;
    FDirectiveParamOrigin: PChar;
    FAsmCode: Boolean;
    FDefines: TStrings;
    FDefineStack: Integer;
    FTopDefineRec: PDefineRec;
    FUseDefines: Boolean;
    FIncludeHandler: IIncludeHandler;
    FUseSharedOrigin: Boolean;

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
    function Func42: TptTokenKind;
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
    function Func72: TptTokenKind;
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
    function Func89: TptTokenKind;
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
    function GetPosXY: TTokenPoint;
    function IdentKind: TptTokenKind;
    procedure SetRunPos(Value: Integer);
    procedure MakeMethodTables;
    procedure AddressOpProc;
    procedure AmpersandOpProc;
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

    procedure EnterDefineBlock(ADefined: Boolean);
    procedure ExitDefineBlock;
    procedure CloneDefinesFrom(ALexer: TmwBasePasLex);
    procedure DoProcTable(AChar: Char);
    function IsIdentifiers(AChar: Char): Boolean; inline;
    function HashValue(AChar: Char): Integer;
    function EvaluateConditionalExpression(const AParams: String): Boolean;
    procedure IncludeFile;
    function GetIncludeFileNameFromToken(const IncludeToken: string): string;
    procedure UpdateIncludedLineCount(const IncludedContent: string);
    procedure SetSharedOrigin(SharedValue: PChar);
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
    function IsDefined(const ADefine: string): Boolean;
    procedure ClearDefines;
    procedure InitDefinesDefinedByCompiler;

    property CommentState: Pointer read GetCommentState write SetCommentState;
    property CompilerDirective: string read GetCompilerDirective;
    property DirectiveParam: string read GetDirectiveParam;
    property IsJunk: Boolean read GetIsJunk;
    property IsSpace: Boolean read GetIsSpace;
    property Line: string write SetLine;
    property Origin: PChar read FOrigin write SetOrigin;
    property PosXY: TTokenPoint read GetPosXY;
    property RunPos: Integer read Run write SetRunPos;
    property Token: string read GetToken;
    property TokenLen: Integer read GetTokenLen;
    property TokenPos: Integer read FTokenPos;
    property TokenID: TptTokenKind read FTokenID;
    property ExID: TptTokenKind read FExID;
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
    property OnCompDirect: TDirectiveEvent read FOnCompDirect write SetOnCompDirect;
    property OnDefineDirect: TDirectiveEvent read FOnDefineDirect write SetOnDefineDirect;
    property OnElseDirect: TDirectiveEvent read FOnElseDirect write SetOnElseDirect;
    property OnEndIfDirect: TDirectiveEvent read FOnEndIfDirect write SetOnEndIfDirect;
    property OnIfDefDirect: TDirectiveEvent read FOnIfDefDirect write SetOnIfDefDirect;
    property OnIfNDefDirect: TDirectiveEvent read FOnIfNDefDirect write SetOnIfNDefDirect;
    property OnIfOptDirect: TDirectiveEvent read FOnIfOptDirect write SetOnIfOptDirect;
    property OnIncludeDirect: TDirectiveEvent read FOnIncludeDirect write SetOnIncludeDirect;
    property OnIfDirect: TDirectiveEvent read FOnIfDirect write SetOnIfDirect;
    property OnIfEndDirect: TDirectiveEvent read FOnIfEndDirect write SetOnIfEndDirect;
    property OnElseIfDirect: TDirectiveEvent read FOnElseIfDirect write SetOnElseIfDirect;
    property OnResourceDirect: TDirectiveEvent read FOnResourceDirect write SetOnResourceDirect;
    property OnUnDefDirect: TDirectiveEvent read FOnUnDefDirect write SetOnUnDefDirect;
    property AsmCode: Boolean read FAsmCode write FAsmCode;
    property DirectiveParamOrigin: PChar read FDirectiveParamOrigin;
    property UseDefines: Boolean read FUseDefines write FUseDefines;
    property IncludeHandler: IIncludeHandler read FIncludeHandler write FIncludeHandler;
  end;

  TmwPasLex = class(TmwBasePasLex)
  private
    FAheadLex: TmwBasePasLex;
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
    property AheadLex: TmwBasePasLex read FAheadLex;
    property AheadToken: string read GetAheadToken;
    property AheadTokenID: TptTokenKind read GetAheadTokenID;
    property AheadExID: TptTokenKind read GetAheadExID;
    property AheadGenID: TptTokenKind read GetAheadGenID;
    property Status: TmwPasLexStatus read GetStatus write SetStatus;
  end;

implementation

uses
  StrUtils;

type
  TmwPasLexExpressionEvaluation = (leeNone, leeAnd, leeOr);

const
  INCLUDE_BUFFER_SIZE = 1024*1024;

procedure MakeIdentTable;
var
  I, J: Char;
begin
  for I := #0 to #127 do
  begin
    case I of
      '_', '0'..'9', 'a'..'z', 'A'..'Z': Identifiers[I] := True;
    else
      Identifiers[I] := False;
    end;
    J := UpperCase(I)[1];
    case I of
      'a'..'z', 'A'..'Z', '_': mHashTable[I] := Ord(J) - 64;
    else
      mHashTable[Char(I)] := 0;
    end;
  end;
end;

function TmwBasePasLex.CharAhead: Char;
begin
  RunAhead := Run;
  while (FOrigin[RunAhead] > #0) and (FOrigin[RunAhead] < #33) do
    Inc(RunAhead);
  Result := FOrigin[RunAhead];
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
  LastFrame := nil;
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
end;

function TmwBasePasLex.GetPosXY: TTokenPoint;
begin
  if Run > EndOfIncludedArea then
  begin
    Result.X := FTokenPos - FLinePos + 1;
    Result.Y := FLineNumber + 1 - IncludedLineCount;
  end
  else
  begin
    Result.X := -1;
    Result.Y := -1;
  end;
end;

procedure TmwBasePasLex.InitIdent;
var
  I: Integer;
begin
  for I := 0 to 191 do
    case I of
      9: FIdentFuncTable[I] := Func9;
      15: FIdentFuncTable[I] := Func15;
      19: FIdentFuncTable[I] := Func19;
      20: FIdentFuncTable[I] := Func20;
      21: FIdentFuncTable[I] := Func21;
      23: FIdentFuncTable[I] := Func23;
      25: FIdentFuncTable[I] := Func25;
      27: FIdentFuncTable[I] := Func27;
      28: FIdentFuncTable[I] := Func28;
      29: FIdentFuncTable[I] := Func29;
      30: FIdentFuncTable[I] := Func30;
      32: FIdentFuncTable[I] := Func32;
      33: FIdentFuncTable[I] := Func33;
      35: FIdentFuncTable[I] := Func35;
      36: FIdentFuncTable[I] := Func36;
      37: FIdentFuncTable[I] := Func37;
      38: FIdentFuncTable[I] := Func38;
      39: FIdentFuncTable[I] := Func39;
      40: FIdentFuncTable[I] := Func40;
      41: FIdentFuncTable[I] := Func41;
      42: FIdentFuncTable[I] := Func42;
      43: FIdentFuncTable[I] := Func43;
      44: FIdentFuncTable[I] := Func44;
      45: FIdentFuncTable[I] := Func45;
      46: FIdentFuncTable[I] := Func46;
      47: FIdentFuncTable[I] := Func47;
      49: FIdentFuncTable[I] := Func49;
      52: FIdentFuncTable[I] := Func52;
      54: FIdentFuncTable[I] := Func54;
      55: FIdentFuncTable[I] := Func55;
      56: FIdentFuncTable[I] := Func56;
      57: FIdentFuncTable[I] := Func57;
      58: FIdentFuncTable[I] := Func58;
      59: FIdentFuncTable[I] := Func59;
      60: FIdentFuncTable[I] := Func60;
      61: FIdentFuncTable[I] := Func61;
      62: FIdentFuncTable[I] := Func62;
      63: FIdentFuncTable[I] := Func63;
      64: FIdentFuncTable[I] := Func64;
      65: FIdentFuncTable[I] := Func65;
      66: FIdentFuncTable[I] := Func66;
      69: FIdentFuncTable[I] := Func69;
      71: FIdentFuncTable[I] := Func71;
      72: FIdentFuncTable[I] := Func72;
      73: FIdentFuncTable[I] := Func73;
      75: FIdentFuncTable[I] := Func75;
      76: FIdentFuncTable[I] := Func76;
      78: FIdentFuncTable[I] := Func78;
      79: FIdentFuncTable[I] := Func79;
      81: FIdentFuncTable[I] := Func81;
      84: FIdentFuncTable[I] := Func84;
      85: FIdentFuncTable[I] := Func85;
      86: FIdentFuncTable[I] := Func86;
      87: FIdentFuncTable[I] := Func87;
      88: FIdentFuncTable[I] := Func88;
      89: FIdentFuncTable[I] := Func89;
      91: FIdentFuncTable[I] := Func91;
      92: FIdentFuncTable[I] := Func92;
      94: FIdentFuncTable[I] := Func94;
      95: FIdentFuncTable[I] := Func95;
      96: FIdentFuncTable[I] := Func96;
      97: FIdentFuncTable[I] := Func97;
      98: FIdentFuncTable[I] := Func98;
      99: FIdentFuncTable[I] := Func99;
      100: FIdentFuncTable[I] := Func100;
      101: FIdentFuncTable[I] := Func101;
      102: FIdentFuncTable[I] := Func102;
      103: FIdentFuncTable[I] := Func103;
      104: FIdentFuncTable[I] := Func104;
      105: FIdentFuncTable[I] := Func105;
      106: FIdentFuncTable[I] := Func106;
      107: FIdentFuncTable[I] := Func107;
      108: FIdentFuncTable[I] := Func108;
      112: FIdentFuncTable[I] := Func112;
      117: FIdentFuncTable[I] := Func117;
      123: FIdentFuncTable[I] := Func123;
      126: FIdentFuncTable[I] := Func126;
      127: FIdentFuncTable[I] := Func127;
      128: FIdentFuncTable[I] := Func128;
      129: FIdentFuncTable[I] := Func129;
      130: FIdentFuncTable[I] := Func130;
      132: FIdentFuncTable[I] := Func132;
      133: FIdentFuncTable[I] := Func133;
      136: FIdentFuncTable[I] := Func136;
      141: FIdentFuncTable[I] := Func141;
      142: FIdentFuncTable[I] := Func142;
      143: FIdentFuncTable[I] := Func143;
      166: FIdentFuncTable[I] := Func166;
      167: FIdentFuncTable[I] := Func167;
      168: FIdentFuncTable[I] := Func168;
      191: FIdentFuncTable[I] := Func191;
    else
      FIdentFuncTable[I] := AltFunc;
    end;
end;

function TmwBasePasLex.KeyHash: Integer;
begin
  Result := 0;
  while IsIdentifiers(FOrigin[Run]) do
  begin
    Inc(Result, HashValue(FOrigin[Run]));
    Inc(Run);
  end;
end;

function TmwBasePasLex.KeyComp(const aKey: string): Boolean;
var
  I: Integer;
  Temp: PChar;
begin
  if Length(aKey) = TokenLen then
  begin
    Temp := FOrigin + FTokenPos;
    Result := True;
    for i := 1 to TokenLen do
    begin
      if mHashTable[Temp^] <> mHashTable[aKey[i]] then
      begin
        Result := False;
        Break;
      end;
      Inc(Temp);
    end;
  end
  else
    Result := False;
end;

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
    if KeyComp('At') then FExID := ptAt;
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
  if KeyComp('Far') then FExID := ptFar;
end;

function TmwBasePasLex.Func27: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Cdecl') then FExID := ptCdecl;
end;

function TmwBasePasLex.Func28: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Read') then FExID := ptRead else
    if KeyComp('Case') then Result := ptCase else
      if KeyComp('Is') then Result := ptIs;
end;

function TmwBasePasLex.Func29: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('On') then FExID := ptOn;
end;

function TmwBasePasLex.Func30: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Char') then FExID := ptChar;
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
    if KeyComp('Name') then FExID := ptName else
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
  if KeyComp('Real') then FExID := ptReal else
    if KeyComp('Real48') then FExID := ptReal48;
end;

function TmwBasePasLex.Func37: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Begin') then Result := ptBegin else
    if KeyComp('Break') then FExID := ptBreak;
end;

function TmwBasePasLex.Func38: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Near') then FExID := ptNear;
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
      if KeyComp('Halt') then FExID := ptHalt;
end;

function TmwBasePasLex.Func42: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Final') then
    FExID := ptFinal; //TODO: Is this supposed to be an ExID?
end;

function TmwBasePasLex.Func43: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Int64') then FExID := ptInt64
  else if KeyComp('local') then FExID := ptLocal;
end;

function TmwBasePasLex.Func44: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Set') then Result := ptSet else
    if KeyComp('Package') then FExID := ptPackage;
end;

function TmwBasePasLex.Func45: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Shr') then Result := ptShr;
end;

function TmwBasePasLex.Func46: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('PChar') then FExID := ptPChar else
    if KeyComp('Sealed') then Result := ptSealed;
end;

function TmwBasePasLex.Func47: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Then') then Result := ptThen else
    if KeyComp('Comp') then FExID := ptComp;
end;

function TmwBasePasLex.Func49: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Not') then Result := ptNot;
end;

function TmwBasePasLex.Func52: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Byte') then FExID := ptByte else
    if KeyComp('Raise') then Result := ptRaise else
      if KeyComp('Pascal') then FExID := ptPascal;
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
  if KeyComp('Index') then FExID := ptIndex else
    if KeyComp('Out') then FExID := ptOut else // bug in Delphi's documentation: OUT is a directive
      if KeyComp('Abort') then FExID := ptAbort else
        if KeyComp('Delayed') then FExID := ptDelayed;
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
  if KeyComp('Exit') then FExID := ptExit;
end;

function TmwBasePasLex.Func59: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Safecall') then FExID := ptSafecall else
    if KeyComp('Double') then FExID := ptDouble;
end;

function TmwBasePasLex.Func60: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('With') then Result := ptWith else
    if KeyComp('Word') then FExID := ptWord;
end;

function TmwBasePasLex.Func61: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Dispid') then FExID := ptDispid;
end;

function TmwBasePasLex.Func62: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Cardinal') then FExID := ptCardinal;
end;

function TmwBasePasLex.Func63: TptTokenKind;
begin
  Result := ptIdentifier;
  case FOrigin[FTokenPos] of
    'P', 'p': if KeyComp('Public') then FExID := ptPublic;
    'A', 'a': if KeyComp('Array') then Result := ptArray;
    'T', 't': if KeyComp('Try') then Result := ptTry;
    'R', 'r': if KeyComp('Record') then Result := ptRecord;
    'I', 'i': if KeyComp('Inline') then
              begin
                Result := ptInline;
                FExID := ptInline;
              end;
  end;
end;

function TmwBasePasLex.Func64: TptTokenKind;
begin
  Result := ptIdentifier;
  case FOrigin[FTokenPos] of
    'B', 'b': if KeyComp('Boolean') then FExID := ptBoolean;
    'D', 'd': if KeyComp('DWORD') then FExID := ptDWORD;
    'U', 'u': if KeyComp('Uses') then Result := ptUses else
                if KeyComp('Unit') then Result := ptUnit;
    'H', 'h': if KeyComp('Helper') then FExID := ptHelper;
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
  if KeyComp('Single') then FExID := ptSingle else
    if KeyComp('Type') then Result := ptType else
      if KeyComp('Unsafe') then Result := ptUnsafe;
end;

function TmwBasePasLex.Func69: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Default') then FExID := ptDefault else
    if KeyComp('Dynamic') then FExID := ptDynamic else
      if KeyComp('Message') then FExID := ptMessage;
end;

function TmwBasePasLex.Func71: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('WideChar') then FExID := ptWideChar else
    if KeyComp('Stdcall') then FExID := ptStdcall else
      if KeyComp('Const') then Result := ptConst;
end;

function TmwBasePasLex.Func72: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Static') then FExID := ptStatic;
end;

function TmwBasePasLex.Func73: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Except') then Result := ptExcept;
end;

function TmwBasePasLex.Func75: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Write') then FExID := ptWrite;
end;

function TmwBasePasLex.Func76: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Until') then Result := ptUntil;
end;

function TmwBasePasLex.Func78: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Integer') then FExID := ptInteger else
    if KeyComp('Remove') then FExID := ptRemove;
end;

function TmwBasePasLex.Func79: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Finally') then Result := ptFinally else
    if KeyComp('Reference') then FExID := ptReference;
end;

function TmwBasePasLex.Func81: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Extended') then FExID := ptExtended else
    if KeyComp('Stored') then FExID := ptStored else
      if KeyComp('Interface') then Result := ptInterface else
        if KeyComp('Deprecated') then FExID := ptDeprecated;
end;

function TmwBasePasLex.Func84: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Abstract') then FExID := ptAbstract;
end;

function TmwBasePasLex.Func85: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Library') then Result := ptLibrary else
    if KeyComp('Forward') then FExID := ptForward else
      if KeyComp('Variant') then FExID := ptVariant;
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

function TmwBasePasLex.Func89: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Strict') then Result := ptStrict;
end;

function TmwBasePasLex.Func91: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Downto') then Result := ptDownto else
    if KeyComp('Private') then FExID := ptPrivate else
      if KeyComp('Longint') then FExID := ptLongint;
end;

function TmwBasePasLex.Func92: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Inherited') then Result := ptInherited else
    if KeyComp('LongBool') then FExID := ptLongBool else
      if KeyComp('Overload') then FExID := ptOverload;
end;

function TmwBasePasLex.Func94: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Resident') then FExID := ptResident else
    if KeyComp('Readonly') then FExID := ptReadonly else
      if KeyComp('Assembler') then FExID := ptAssembler;
end;

function TmwBasePasLex.Func95: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Contains') then FExID := ptContains else
    if KeyComp('Absolute') then FExID := ptAbsolute;
end;

function TmwBasePasLex.Func96: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('ByteBool') then FExID := ptByteBool else
    if KeyComp('Override') then FExID := ptOverride else
      if KeyComp('Published') then FExID := ptPublished;
end;

function TmwBasePasLex.Func97: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Threadvar') then Result := ptThreadvar;
end;

function TmwBasePasLex.Func98: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Export') then FExID := ptExport else
    if KeyComp('Nodefault') then FExID := ptNodefault;
end;

function TmwBasePasLex.Func99: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('External') then FExID := ptExternal;
end;

function TmwBasePasLex.Func100: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Automated') then FExID := ptAutomated else
    if KeyComp('Smallint') then FExID := ptSmallint;
end;

function TmwBasePasLex.Func101: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Register') then FExID := ptRegister else
    if KeyComp('Platform') then FExID := ptPlatform else
      if KeyComp('Continue') then FExID := ptContinue;
end;

function TmwBasePasLex.Func102: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Function') then Result := ptFunction;
end;

function TmwBasePasLex.Func103: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Virtual') then FExID := ptVirtual;
end;

function TmwBasePasLex.Func104: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('WordBool') then FExID := ptWordBool;
end;

function TmwBasePasLex.Func105: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Procedure') then Result := ptProcedure;
end;

function TmwBasePasLex.Func106: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Protected') then FExID := ptProtected;
end;

function TmwBasePasLex.Func107: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Currency') then FExID := ptCurrency;
end;

function TmwBasePasLex.Func108: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Longword') then FExID := ptLongword else
    if KeyComp('Operator') then FExID := ptOperator;
end;

function TmwBasePasLex.Func112: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Requires') then FExID := ptRequires;
end;

function TmwBasePasLex.Func117: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Exports') then Result := ptExports else
    if KeyComp('OleVariant') then FExID := ptOleVariant;
end;

function TmwBasePasLex.Func123: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Shortint') then FExID := ptShortint;
end;

function TmwBasePasLex.Func126: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Implements') then FExID := ptImplements;
end;

function TmwBasePasLex.Func127: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Runerror') then FExID := ptRunError;
end;

function TmwBasePasLex.Func128: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('WideString') then FExID := ptWideString;
end;

function TmwBasePasLex.Func129: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Dispinterface') then Result := ptDispinterface
end;

function TmwBasePasLex.Func130: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('AnsiString') then FExID := ptAnsiString;
end;

function TmwBasePasLex.Func132: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Reintroduce') then FExID := ptReintroduce;
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
  if KeyComp('Writeonly') then FExID := ptWriteonly;
end;

function TmwBasePasLex.Func142: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('experimental') then FExID := ptExperimental;
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
  if KeyComp('ShortString') then FExID := ptShortString;
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
    if KeyComp('Stringresource') then FExID := ptStringresource;
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
    Result := FIdentFuncTable[HashKey]
  else
    Result := ptIdentifier;
end;

procedure TmwBasePasLex.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #127 do
    case I of
      #0: FProcTable[I] := NullProc;
      #10: FProcTable[I] := LFProc;
      #13: FProcTable[I] := CRProc;
      #1..#9, #11, #12, #14..#32: FProcTable[I] := SpaceProc;
      '#': FProcTable[I] := AsciiCharProc;
      '$': FProcTable[I] := IntegerProc;
      #39: FProcTable[I] := StringProc;
      '0'..'9': FProcTable[I] := NumberProc;
      'A'..'Z', 'a'..'z', '_': FProcTable[I] := IdentProc;
      '{': FProcTable[I] := BraceOpenProc;
      '}': FProcTable[I] := BraceCloseProc;
      '!', '"', '%', '&', '('..'/', ':'..'@', '['..'^', '`', '~':
        begin
          case I of
            '(': FProcTable[I] := RoundOpenProc;
            ')': FProcTable[I] := RoundCloseProc;
            '*': FProcTable[I] := StarProc;
            '+': FProcTable[I] := PlusProc;
            ',': FProcTable[I] := CommaProc;
            '-': FProcTable[I] := MinusProc;
            '.': FProcTable[I] := PointProc;
            '/': FProcTable[I] := SlashProc;
            ':': FProcTable[I] := ColonProc;
            ';': FProcTable[I] := SemiColonProc;
            '<': FProcTable[I] := LowerProc;
            '=': FProcTable[I] := EqualProc;
            '>': FProcTable[I] := GreaterProc;
            '@': FProcTable[I] := AddressOpProc;
            '[': FProcTable[I] := SquareOpenProc;
            ']': FProcTable[I] := SquareCloseProc;
            '^': FProcTable[I] := PointerSymbolProc;
            '"': FProcTable[I] := StringDQProc;
            '&': FProcTable[I] := AmpersandOpProc;
          else
            FProcTable[I] := SymbolProc;
          end;
        end;
    else
      FProcTable[I] := UnknownProc;
    end;
end;

constructor TmwBasePasLex.Create;
begin
  inherited Create;
  BufferSize := INCLUDE_BUFFER_SIZE * SizeOf(Char);
  GetMem(FOrigin, BufferSize);
  InitIdent;
  MakeMethodTables;
  FExID := ptUnKnown;

  FUseDefines := True;
  FDefines := TStringList.Create;
  FTopDefineRec := nil;
  FUseSharedOrigin := false;
  ClearDefines;
end;

destructor TmwBasePasLex.Destroy;
begin
  ClearDefines; //If we don't do this, we get a memory leak
  FDefines.Free;
  if not FUseSharedOrigin then
    FreeMem(FOrigin, BufferSize);
  inherited Destroy;
end;

procedure TmwBasePasLex.DoProcTable(AChar: Char);
begin
  if Ord(AChar) <= 127 then
    FProcTable[AChar]
  else
  begin
    IdentProc;
  end;
end;

procedure TmwBasePasLex.SetOrigin(NewValue: PChar);
begin
  if not FUseSharedOrigin then
    FreeMem(FOrigin, BufferSize);
  FUseSharedOrigin := false;

  BufferSize := (Length(String(NewValue)) + INCLUDE_BUFFER_SIZE) * SizeOf(Char);
  GetMem(FOrigin, BufferSize);
  StrPCopy(FOrigin, NewValue);

  Init;
  Next;
end;

procedure TmwBasePasLex.SetSharedOrigin(SharedValue: PChar);
begin
  if not FUseSharedOrigin then
    FreeMem(FOrigin, BufferSize);

  FUseSharedOrigin := true;
  FOrigin := SharedValue;

  Init;
  Next;
end;

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
        FTokenID := ptDoubleAddressOp;
        Inc(Run, 2);
      end;
  else
    begin
      FTokenID := ptAddressOp;
      Inc(Run);
    end;
  end;
end;

procedure TmwBasePasLex.AsciiCharProc;
begin
  FTokenID := ptAsciiChar;
  Inc(Run);
  if FOrigin[Run] = '$' then
  begin
    Inc(Run);
    while CharInSet(FOrigin[Run], ['0'..'9', 'A'..'F', 'a'..'f']) do Inc(Run);
  end else
  begin
  {$IFDEF SUPPORTS_INTRINSIC_HELPERS}
    while Char(FOrigin[Run]).IsDigit do
  {$ELSE}
    while IsDigit(fOrigin[Run]) do
  {$ENDIF}
      Inc(Run);
  end;
end;

procedure TmwBasePasLex.BraceCloseProc;
begin
  Inc(Run);
  FTokenID := ptError;
  if Assigned(FOnMessage) then
    FOnMessage(Self, meError, 'Illegal character', PosXY.X, PosXY.Y);
end;

procedure TmwBasePasLex.BorProc;
begin
  FTokenID := ptBorComment;
  case FOrigin[Run] of
    #0:
      begin
        NullProc;
        if Assigned(FOnMessage) then
          FOnMessage(Self, meError, 'Unexpected file end', PosXY.X, PosXY.Y);
        Exit;
      end;
  end;

  while FOrigin[Run] <> #0 do
    case FOrigin[Run] of
      '}':
        begin
          FCommentState := csNo;
          Inc(Run);
          Break;
        end;
      #10:
        begin
          Inc(Run);
          Inc(FLineNumber);
          FLinePos := Run;
        end;
      #13:
        begin
          Inc(Run);
          if FOrigin[Run] = #10 then Inc(Run);
          Inc(FLineNumber);
          FLinePos := Run;
        end;
    else
      Inc(Run);
    end;
end;

procedure TmwBasePasLex.BraceOpenProc;
begin
  case FOrigin[Run + 1] of
    '$': FTokenID := GetDirectiveKind;
  else
    begin
      FTokenID := ptBorComment;
      FCommentState := csBor;
    end;
  end;
  Inc(Run);
  while FOrigin[Run] <> #0 do
    case FOrigin[Run] of
      '}':
        begin
          FCommentState := csNo;
          Inc(Run);
          Break;
        end;
      #10:
        begin
          Inc(Run);
          Inc(FLineNumber);
          FLinePos := Run;
        end;
      #13:
        begin
          Inc(Run);
          if FOrigin[Run] = #10 then Inc(Run);
          Inc(FLineNumber);
          FLinePos := Run;
        end;
    else
      Inc(Run);
    end;
  case FTokenID of
    PtCompDirect:
      begin
        if Assigned(FOnCompDirect) then
          FOnCompDirect(Self);
      end;
    PtDefineDirect:
      begin
        if FUseDefines and (FDefineStack = 0) then
          AddDefine(DirectiveParam);
        if Assigned(FOnDefineDirect) then
          FOnDefineDirect(Self);
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
        if Assigned(FOnElseDirect) then
          FOnElseDirect(Self);
      end;
    PtEndIfDirect:
      begin
        if FUseDefines then
          ExitDefineBlock;
        if Assigned(FOnEndIfDirect) then
          FOnEndIfDirect(Self);
      end;
    PtIfDefDirect:
      begin
        if FUseDefines then
          EnterDefineBlock(IsDefined(DirectiveParam));
        if Assigned(FOnIfDefDirect) then
          FOnIfDefDirect(Self);
      end;
    PtIfNDefDirect:
      begin
        if FUseDefines then
          EnterDefineBlock(not IsDefined(DirectiveParam));
        if Assigned(FOnIfNDefDirect) then
          FOnIfNDefDirect(Self);
      end;
    PtIfOptDirect:
      begin
        if FUseDefines then
          EnterDefineBlock(False);
        if Assigned(FOnIfOptDirect) then
          FOnIfOptDirect(Self);
      end;
    PtIfDirect:
      begin
        if FUseDefines then
          EnterDefineBlock(EvaluateConditionalExpression(DirectiveParam));
        if Assigned(FOnIfDirect) then
          FOnIfDirect(Self);
      end;
    PtIfEndDirect:
      begin
        if FUseDefines then
          ExitDefineBlock;
        if Assigned(FOnIfEndDirect) then
          FOnIfEndDirect(Self);
      end;
    PtElseIfDirect:
      begin
        if FUseDefines then
        begin
          if FTopDefineRec <> nil then
          begin
            if FTopDefineRec^.Defined then
              FDefineStack := FTopDefineRec.StartCount + 1
            else
            begin
              FDefineStack := FTopDefineRec.StartCount;
                if EvaluateConditionalExpression(DirectiveParam) then
                  FTopDefineRec^.Defined := True
                else
                  FDefineStack := FTopDefineRec.StartCount + 1
            end;
          end;
        end;
        if Assigned(FOnElseIfDirect) then
          FOnElseIfDirect(Self);
      end;
    PtIncludeDirect:
      begin
//        if Assigned(FOnIncludeDirect) then
//          FOnIncludeDirect(Self);
        if Assigned(FIncludeHandler) then
          IncludeFile
        else
          Next;
      end;
    PtResourceDirect:
      begin
        if Assigned(FOnResourceDirect) then
          FOnResourceDirect(Self);
      end;
    PtUndefDirect:
      begin
        if FUseDefines and (FDefineStack = 0) then
          RemoveDefine(DirectiveParam);
        if Assigned(FOnUnDefDirect) then
          FOnUnDefDirect(Self);
      end;
  end;
end;

function TmwBasePasLex.EvaluateConditionalExpression(const AParams: String): Boolean;
var
  LParams: String;
  LDefine: String;
  LEvaluation: TmwPasLexExpressionEvaluation;
begin
  { TODO : Expand support for <=> evaluations (complicated to do). Expand support for NESTED expressions }
  LEvaluation := leeNone;
  LParams := TrimLeft(AParams);
  if (Pos('DEFINED(', LParams) = 1) or (Pos('NOT DEFINED(', LParams) = 1) then
  begin
    Result := True; // Optimistic
    while (Pos('DEFINED(', LParams) = 1) or (Pos('NOT DEFINED(', LParams) = 1) do
    begin
      if Pos('DEFINED(', LParams) = 1 then
      begin
        LDefine := Copy(LParams, 9, Pos(')', LParams) - 9);
        LParams := TrimLeft(Copy(LParams, 10 + Length(LDefine), Length(AParams) - (9 + Length(LDefine))));
        case LEvaluation of
          leeNone: Result := IsDefined(LDefine);
          leeAnd: Result := Result and IsDefined(LDefine);
          leeOr: Result := Result or IsDefined(LDefine);
        end;
      end
      else if Pos('NOT DEFINED(', LParams) = 1 then
      begin
        LDefine := Copy(LParams, 13, Pos(')', LParams) - 13);
        LParams := TrimLeft(Copy(LParams, 14 + Length(LDefine), Length(AParams) - (13 + Length(LDefine))));
        case LEvaluation of
          leeNone: Result := (not IsDefined(LDefine));
          leeAnd: Result := Result and (not IsDefined(LDefine));
          leeOr: Result := Result or (not IsDefined(LDefine));
        end;
      end;
      // Determine next Evaluation
      if Pos('AND ', LParams) = 1 then
      begin
        LEvaluation := leeAnd;
        LParams := TrimLeft(Copy(LParams, 4, Length(LParams) - 3));
      end
      else if Pos('OR ', LParams) = 1 then
      begin
        LEvaluation := leeOr;
        LParams := TrimLeft(Copy(LParams, 3, Length(LParams) - 2));
      end;
    end;
  end else
    Result := False;
end;

procedure TmwBasePasLex.ColonProc;
begin
  case FOrigin[Run + 1] of
    '=':
      begin
        Inc(Run, 2);
        FTokenID := ptAssign;
      end;
  else
    begin
      Inc(Run);
      FTokenID := ptColon;
    end;
  end;
end;

procedure TmwBasePasLex.CommaProc;
begin
  Inc(Run);
  FTokenID := ptComma;
end;

procedure TmwBasePasLex.CRProc;
begin
  case FCommentState of
    csBor: FTokenID := ptCRLFCo;
    csAnsi: FTokenID := ptCRLFCo;
  else
    FTokenID := ptCRLF;
  end;

  case FOrigin[Run + 1] of
    #10: Inc(Run, 2);
  else
    Inc(Run);
  end;
  Inc(FLineNumber);
  FLinePos := Run;
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
  Inc(Run);
  FTokenID := ptEqual;
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
        Inc(Run, 2);
        FTokenID := ptGreaterEqual;
      end;
  else
    begin
      Inc(Run);
      FTokenID := ptGreater;
    end;
  end;
end;

function TmwBasePasLex.HashValue(AChar: Char): Integer;
begin
  if AChar <= #127 then
    Result := mHashTable[FOrigin[Run]]
  else
    Result := Ord(AChar);
end;

procedure TmwBasePasLex.IdentProc;
begin
  FTokenID := IdentKind;
end;

procedure TmwBasePasLex.IntegerProc;
begin
  Inc(Run);
  FTokenID := ptIntegerConst;
  while CharInSet(FOrigin[Run], ['0'..'9', 'A'..'F', 'a'..'f']) do
    Inc(Run);
end;

function TmwBasePasLex.IsDefined(const ADefine: string): Boolean;
begin
  Result := FDefines.IndexOf(ADefine) > -1;
end;

function TmwBasePasLex.IsIdentifiers(AChar: Char): Boolean;
begin
{$IFDEF SUPPORTS_INTRINSIC_HELPERS}
  Result := (AChar.IsLetterOrDigit) or (AChar = '_');
{$ELSE}
  Result := TCharacter.IsLetterOrDigit(AChar) or (AChar = '_');
{$ENDIF}
end;

procedure TmwBasePasLex.LFProc;
begin
  case FCommentState of
    csBor: FTokenID := ptCRLFCo;
    csAnsi: FTokenID := ptCRLFCo;
  else
    FTokenID := ptCRLF;
  end;
  Inc(Run);
  Inc(FLineNumber);
  FLinePos := Run;
end;

procedure TmwBasePasLex.LowerProc;
begin
  case FOrigin[Run + 1] of
    '=':
      begin
        Inc(Run, 2);
        FTokenID := ptLowerEqual;
      end;
    '>':
      begin
        Inc(Run, 2);
        FTokenID := ptNotEqual;
      end
  else
    begin
      Inc(Run);
      FTokenID := ptLower;
    end;
  end;
end;

procedure TmwBasePasLex.MinusProc;
begin
  Inc(Run);
  FTokenID := ptMinus;
end;

procedure TmwBasePasLex.NullProc;
begin
  FTokenID := ptNull;
end;

procedure TmwBasePasLex.NumberProc;
begin
  Inc(Run);
  FTokenID := ptIntegerConst;
  while CharInSet(FOrigin[Run], ['0'..'9', '.', 'e', 'E']) do
  begin
    case FOrigin[Run] of
      '.':
        if FOrigin[Run + 1] = '.' then
          Break
        else
          FTokenID := ptFloat
    end;
    Inc(Run);
  end;
end;

procedure TmwBasePasLex.PlusProc;
begin
  Inc(Run);
  FTokenID := ptPlus;
end;

procedure TmwBasePasLex.PointerSymbolProc;
begin
  Inc(Run);
  FTokenID := ptPointerSymbol;

  //This is a wierd Pascal construct that rarely appears, but needs to be
  //supported. ^M is a valid char reference (#13, in this case)
  if CharInSet(FOrigin[Run], ['a'..'z','A'..'Z']) and not IsIdentifiers(FOrigin[Run+1]) then
  begin
    Inc(Run);
    FTokenID := ptAsciiChar;
  end;
end;

procedure TmwBasePasLex.PointProc;
begin
  case FOrigin[Run + 1] of
    '.':
      begin
        Inc(Run, 2);
        FTokenID := ptDotDot;
      end;
    ')':
      begin
        Inc(Run, 2);
        FTokenID := ptSquareClose;
      end;
  else
    begin
      Inc(Run);
      FTokenID := ptPoint;
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
  Inc(Run);
  FTokenID := ptRoundClose;
end;

procedure TmwBasePasLex.AnsiProc;
begin
  FTokenID := ptAnsiComment;
  case FOrigin[Run] of
    #0:
      begin
        NullProc;
        if Assigned(FOnMessage) then
          FOnMessage(Self, meError, 'Unexpected file end', PosXY.X, PosXY.Y);
        Exit;
      end;
  end;

  while FOrigin[Run] <> #0 do
    case FOrigin[Run] of
      '*':
        if FOrigin[Run + 1] = ')' then
        begin
          FCommentState := csNo;
          Inc(Run, 2);
          Break;
        end
        else Inc(Run);
      #10:
        begin
          Inc(Run);
          Inc(FLineNumber);
          FLinePos := Run;
        end;
      #13:
        begin
          Inc(Run);
          if FOrigin[Run] = #10 then Inc(Run);
          Inc(FLineNumber);
          FLinePos := Run;
        end;
    else
      Inc(Run);
    end;
end;

procedure TmwBasePasLex.RoundOpenProc;
begin
  Inc(Run);
  case FOrigin[Run] of
    '*':
      begin
        FTokenID := ptAnsiComment;
        if FOrigin[Run + 1] = '$' then
          FTokenID := GetDirectiveKind
        else
          FCommentState := csAnsi;
        Inc(Run);
        while FOrigin[Run] <> #0 do
          case FOrigin[Run] of
            '*':
              if FOrigin[Run + 1] = ')' then
              begin
                FCommentState := csNo;
                Inc(Run, 2);
                Break;
              end
              else
                Inc(Run);
            #10:
              begin
                Inc(Run);
                Inc(FLineNumber);
                FLinePos := Run;
              end;
            #13:
              begin
                Inc(Run);
                if FOrigin[Run] = #10 then Inc(Run);
                Inc(FLineNumber);
                FLinePos := Run;
              end;
          else
            Inc(Run);
          end;
      end;
    '.':
      begin
        Inc(Run);
        FTokenID := ptSquareOpen;
      end;
  else
    FTokenID := ptRoundOpen;
  end;
  case FTokenID of
    PtCompDirect:
      begin
        if Assigned(FOnCompDirect) then
          FOnCompDirect(Self);
      end;
    PtDefineDirect:
      begin
        if Assigned(FOnDefineDirect) then
          FOnDefineDirect(Self);
      end;
    PtElseDirect:
      begin
        if Assigned(FOnElseDirect) then
          FOnElseDirect(Self);
      end;
    PtEndIfDirect:
      begin
        if Assigned(FOnEndIfDirect) then
          FOnEndIfDirect(Self);
      end;
    PtIfDefDirect:
      begin
        if Assigned(FOnIfDefDirect) then
          FOnIfDefDirect(Self);
      end;
    PtIfNDefDirect:
      begin
        if Assigned(FOnIfNDefDirect) then
          FOnIfNDefDirect(Self);
      end;
    PtIfOptDirect:
      begin
        if Assigned(FOnIfOptDirect) then
          FOnIfOptDirect(Self);
      end;
    PtIncludeDirect:
      begin
//        if Assigned(FOnIncludeDirect) then
//          FOnIncludeDirect(Self);
        if Assigned(FIncludeHandler) then
          IncludeFile;
      end;
    PtResourceDirect:
      begin
        if Assigned(FOnResourceDirect) then
          FOnResourceDirect(Self);
      end;
    PtUndefDirect:
      begin
        if Assigned(FOnUnDefDirect) then
          FOnUnDefDirect(Self);
      end;
  end;
end;

procedure TmwBasePasLex.SemiColonProc;
begin
  Inc(Run);
  FTokenID := ptSemiColon;
end;

procedure TmwBasePasLex.SlashProc;
begin
  case FOrigin[Run + 1] of
    '/':
      begin
        Inc(Run, 2);
        FTokenID := ptSlashesComment;
        while FOrigin[Run] <> #0 do
        begin
          case FOrigin[Run] of
            #10, #13: Break;
          end;
          Inc(Run);
        end;
      end;
  else
    begin
      Inc(Run);
      FTokenID := ptSlash;
    end;
  end;
end;

procedure TmwBasePasLex.SpaceProc;
begin
  Inc(Run);
  FTokenID := ptSpace;
  while CharInSet(FOrigin[Run], [#1..#9, #11, #12, #14..#32]) do
    Inc(Run);
end;

procedure TmwBasePasLex.SquareCloseProc;
begin
  Inc(Run);
  FTokenID := ptSquareClose;
end;

procedure TmwBasePasLex.SquareOpenProc;
begin
  Inc(Run);
  FTokenID := ptSquareOpen;
end;

procedure TmwBasePasLex.StarProc;
begin
  Inc(Run);
  FTokenID := ptStar;
end;

procedure TmwBasePasLex.StringProc;
begin
  FTokenID := ptStringConst;
  repeat
    Inc(Run);
    case FOrigin[Run] of
      #0, #10, #13:
        begin
          if Assigned(FOnMessage) then
            FOnMessage(Self, meError, 'Unterminated string', PosXY.X, PosXY.Y);
          Break;
        end;
      #39:
        begin
          while (FOrigin[Run] = #39) and (FOrigin[Run + 1] = #39) do
          begin
            Inc(Run, 2);
          end;
        end;
    end;
  until FOrigin[Run] = #39;
  if FOrigin[Run] = #39 then
  begin
    Inc(Run);
    if TokenLen = 3 then
    begin
      FTokenID := ptAsciiChar;
    end;
  end;
end;

procedure TmwBasePasLex.SymbolProc;
begin
  Inc(Run);
  FTokenID := ptSymbol;
end;

procedure TmwBasePasLex.UnknownProc;
begin
  Inc(Run);
  FTokenID := ptUnknown;
  if Assigned(FOnMessage) then
   FOnMessage(Self, meError, 'Unknown Character', PosXY.X, PosXY.Y);
end;

procedure TmwBasePasLex.Next;
begin
  FExID := ptUnKnown;
  FTokenPos := Run;
  case FCommentState of
    csNo: DoProcTable(FOrigin[Run]);
    csBor: BorProc;
    csAnsi: AnsiProc;
  end;
end;

function TmwBasePasLex.GetIsJunk: Boolean;
begin
  Result := IsTokenIDJunk(FTokenID) or (FUseDefines and (FDefineStack > 0) and (TokenID <> ptNull));
end;

function TmwBasePasLex.GetIsSpace: Boolean;
begin
  Result := FTokenID in [ptCRLF, ptSpace];
end;

function TmwBasePasLex.GetToken: string;
begin
  SetString(Result, (FOrigin + FTokenPos), GetTokenLen);
end;

function TmwBasePasLex.GetTokenLen: Integer;
begin
  Result := Run - FTokenPos;
end;

procedure TmwBasePasLex.NextID(ID: TptTokenKind);
begin
  repeat
    case FTokenID of
      ptNull: Break;
    else
      Next;
    end;
  until FTokenID = ID;
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
  if FTokenPos = 0 then Exit;
  RunBack := FTokenPos;
  Dec(RunBack);
  while CharInSet(FOrigin[RunBack], [#1..#9, #11, #12, #14..#32]) do
    Dec(RunBack);
  if RunBack = 0 then Exit;
  case FOrigin[RunBack] of
    #10, #13: Exit;
  else
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function TmwBasePasLex.GetCommentState: Pointer;
begin
  Result := Pointer(FCommentState);
end;

function TmwBasePasLex.GetCompilerDirective: string;
var
  DirectLen: Integer;
begin
  if TokenID <> ptCompDirect then
    Result := ''
  else
    case FOrigin[FTokenPos] of
      '(':
        begin
          DirectLen := Run - FTokenPos - 4;
          SetString(Result, (FOrigin + FTokenPos + 2), DirectLen);
          Result := UpperCase(Result);
        end;
      '{':
        begin
          DirectLen := Run - FTokenPos - 2;
          SetString(Result, (FOrigin + FTokenPos + 1), DirectLen);
          Result := UpperCase(Result);
        end;
    end;
end;

function TmwBasePasLex.GetDirectiveKind: TptTokenKind;
var
  TempPos: Integer;
begin
  case FOrigin[FTokenPos] of
    '(': Run := FTokenPos + 3;
    '{': Run := FTokenPos + 2;
  end;
  FDirectiveParamOrigin := FOrigin + FTokenPos;
  TempPos := FTokenPos;
  FTokenPos := Run;
  case KeyHash of
    9:
      if KeyComp('I') and (not CharInSet(FOrigin[Run], ['+', '-'])) then
        Result := ptIncludeDirect else
        Result := ptCompDirect;
    15:
      if KeyComp('IF') then
        Result := ptIfDirect else
        Result := ptCompDirect;
    18:
      if KeyComp('R') then
      begin
        if not CharInSet(FOrigin[Run], ['+', '-']) then
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
  FTokenPos := TempPos;
  Dec(Run);
end;

function TmwBasePasLex.GetDirectiveParam: string;
var
  EndPos: Integer;
  ParamLen: Integer;
begin
  case FOrigin[FTokenPos] of
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
  else
    EndPos := 0;
  end;
  while IsIdentifiers(FOrigin[TempRun]) do
    Inc(TempRun);
  while CharInSet(FOrigin[TempRun], ['+', ',', '-']) do
  begin
    Inc(TempRun);
    while IsIdentifiers(FOrigin[TempRun]) do
      Inc(TempRun);
    if CharInSet(FOrigin[TempRun - 1], ['+', ',', '-']) and (FOrigin[TempRun] = ' ')
      then Inc(TempRun);
  end;
  if FOrigin[TempRun] = ' ' then Inc(TempRun);
  ParamLen := EndPos - TempRun;
  SetString(Result, (FOrigin + TempRun), ParamLen);
  Result := UpperCase(Result);
end;

function TmwBasePasLex.GetIncludeFileNameFromToken(const IncludeToken: string): string;
var
  FileNameStartPos, CurrentPos: integer;
  TrimmedToken: string;
begin
  TrimmedToken := Trim(IncludeToken);
  CurrentPos := 1;
  while TrimmedToken[CurrentPos] > #32 do
    inc(CurrentPos);
  while TrimmedToken[CurrentPos] <= #32 do
    inc(CurrentPos);
  FileNameStartPos := CurrentPos;
  while (TrimmedToken[CurrentPos] > #32) and (TrimmedToken[CurrentPos] <> '}')  do
    inc(CurrentPos);

  Result := Copy(TrimmedToken, FileNameStartPos, CurrentPos - FileNameStartPos);
end;

procedure TmwBasePasLex.UpdateIncludedLineCount(const IncludedContent: string);
var
  i: Integer;
begin
  i := 1;
  while i <= Length(IncludedContent) do
  begin
    if IncludedContent[i] = ''#10'' then
      Inc(IncludedLineCount)
    else if IncludedContent[i] = ''#13'' then
    begin
      Inc(IncludedLineCount);
      if (i + 1 <= Length(IncludedContent)) and (IncludedContent[i + 1] = ''#10'') then
        inc(i);
    end;
    inc(i);
  end;
end;

procedure TmwBasePasLex.IncludeFile;
var
  IncludeFileName, IncludeDirective, Content, Origin, BehindIncludedContent: string;
  pBehindIncludedContent: PChar;
  TempBufferSize: integer;
begin
  IncludeDirective := Token;
  IncludeFileName := GetIncludeFileNameFromToken(IncludeDirective);
  Content := FIncludeHandler.GetIncludeFileContent(IncludeFileName) + #13#10;

  Origin := FOrigin;
  TempBufferSize := (Length(Origin) + INCLUDE_BUFFER_SIZE) * SizeOf(Char);
  GetMem(pBehindIncludedContent, TempBufferSize);
  try
    StrPCopy(pBehindIncludedContent, MidStr(Origin, Run+1, Length(Origin)));
    BehindIncludedContent := pBehindIncludedContent;
    Run := Run - Length(IncludeDirective);
    EndOfIncludedArea := Run + Length(Content);
    UpdateIncludedLineCount(Content);

    Content := Content + BehindIncludedContent;
    
    if (Run + Length(Content)) * SizeOf(Char) > BufferSize then
    begin
      Run := Run + Length(IncludeDirective);
      raise EIncludeError.Create('Content size of include file ' + IncludeFileName + ' exceeds buffer limit.');
    end
    else
    begin    
      StrPCopy(@FOrigin[Run], Content);
      FOrigin[Run + Length(Content)] := #0;
    end;

    Next;
  finally
    FreeMem(pBehindIncludedContent, TempBufferSize);
  end;
end;

procedure TmwBasePasLex.Init;
begin
  FCommentState := csNo;
  FLineNumber := 0;
  FLinePos := 0;
  Run := 0;
  EndOfIncludedArea := -1;
  IncludedLineCount := 0;
end;

procedure TmwBasePasLex.InitFrom(ALexer: TmwBasePasLex);
begin
  SetSharedOrigin(ALexer.Origin);
  FCommentState := ALexer.FCommentState;
  FLineNumber := ALexer.FLineNumber;
  FLinePos := ALexer.FLinePos;
  Run := ALexer.Run;
  CloneDefinesFrom(ALexer);
end;

procedure TmwBasePasLex.InitDefinesDefinedByCompiler;
begin
  //Set up the defines that are defined by the compiler
  {$IFDEF VER90}
  AddDefine('VER90'); // 2
  {$ENDIF}
  {$IFDEF VER100}
  AddDefine('VER100'); // 3
  {$ENDIF}
  {$IFDEF VER120}
  AddDefine('VER120'); // 4
  {$ENDIF}
  {$IFDEF VER130}
  AddDefine('VER130'); // 5
  {$ENDIF}
  {$IFDEF VER140} // 6
  AddDefine('VER140');
  {$ENDIF}
  {$IFDEF VER150} // 7/7.1
  AddDefine('VER150');
  {$ENDIF}
  {$IFDEF VER160} // 8
  AddDefine('VER160');
  {$ENDIF}
  {$IFDEF VER170} // 2005
  AddDefine('VER170');
  {$ENDIF}
  {$IFDEF VER180} // 2007
  AddDefine('VER180');
  {$ENDIF}
  {$IFDEF VER185} // 2007
  AddDefine('VER185');
  {$ENDIF}
  {$IFDEF VER190} // 2007.NET
  AddDefine('VER190');
  {$ENDIF}
  {$IFDEF VER200} // 2009
  AddDefine('VER200');
  {$ENDIF}
  {$IFDEF VER210} // 2010
  AddDefine('VER210');
  {$ENDIF}
  {$IFDEF VER220} // XE
  AddDefine('VER220');
  {$ENDIF}
  {$IFDEF VER230} // XE2
  AddDefine('VER230');
  {$ENDIF}
  {$IFDEF VER240} // XE3
  AddDefine('VER240');
  {$ENDIF}
  {$IFDEF VER250} // XE4
  AddDefine('VER250');
  {$ENDIF}
  {$IFDEF VER260} // XE5
  AddDefine('VER260');
  {$ENDIF}
  {$IFDEF VER270} // XE6
  AddDefine('VER270');
  {$ENDIF}
  {$IFDEF VER280} // XE7
  AddDefine('VER280');
  {$ENDIF}
  {$IFDEF VER290} // XE8
  AddDefine('VER290');
  {$ENDIF}
  {$IFDEF WIN32}
  AddDefine('WIN32');
  {$ENDIF}
  {$IFDEF WIN64}
  AddDefine('WIN64');
  {$ENDIF}
  {$IFDEF LINUX}
  AddDefine('LINUX');
  {$ENDIF}
  {$IFDEF LINUX32}
  AddDefine('LINUX32');
  {$ENDIF}
  {$IFDEF POSIX}
  AddDefine('POSIX');
  {$ENDIF}
  {$IFDEF POSIX32}
  AddDefine('POSIX32');
  {$ENDIF}
  {$IFDEF CPUARM}
  AddDefine('CPUARM');
  {$ENDIF}
  {$IFDEF CPU386}
  AddDefine('CPU386');
  {$ENDIF}
  {$IFDEF CPUX86}
  AddDefine('CPUX86');
  {$ENDIF}
  {$IFDEF CPUX64}
  AddDefine('CPUX64');
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  AddDefine('MSWINDOWS');
  {$ENDIF}
  {$IFDEF MACOS}
  AddDefine('MACOS');
  {$ENDIF}
  {$IFDEF MACOS32}
  AddDefine('MACOS32');
  {$ENDIF}
  {$IFDEF IOS}
  AddDefine('IOS');
  {$ENDIF}
  {$IFDEF ANDROID}
  AddDefine('ANDROID');
  {$ENDIF}
  {$IFDEF CONSOLE}
  AddDefine('CONSOLE');
  {$ENDIF}
  {$IFDEF NATIVECODE}
  AddDefine('NATIVECODE');
  {$ENDIF}
  {$IFDEF CONDITIONALEXPRESSIONS}
  AddDefine('CONDITIONALEXPRESSIONS');
  {$ENDIF}
  {$IFDEF UNICODE}
  AddDefine('UNICODE');
  {$ENDIF}
  {$IFDEF ALIGN_STACK}
  AddDefine('ALIGN_STACK');
  {$ENDIF}
  {$IFDEF ASSEMBLER}
  AddDefine('ASSEMBLER');
  {$ENDIF}
  {$IFDEF AUTOREFCOUNT}
  AddDefine('AUTOREFCOUNT');
  {$ENDIF}
  {$IFDEF EXTERNALLINKER}
  AddDefine('EXTERNALLINKER');
  {$ENDIF}
  {$IFDEF ELF}
  AddDefine('ELF');
  {$ENDIF}
  {$IFDEF NEXTGEN}
  AddDefine('NEXTGEN');
  {$ENDIF}
  {$IFDEF PC_MAPPED_EXCEPTIONS}
  AddDefine('PC_MAPPED_EXCEPTIONS');
  {$ENDIF}
  {$IFDEF PIC}
  AddDefine('PIC');
  {$ENDIF}
  {$IFDEF UNDERSCOREIMPORTNAME}
  AddDefine('UNDERSCOREIMPORTNAME');
  {$ENDIF}
  {$IFDEF WEAKREF}
  AddDefine('WEAKREF');
  {$ENDIF}
  {$IFDEF WEAKINSTREF}
  AddDefine('WEAKINSTREF');
  {$ENDIF}
  {$IFDEF WEAKINTREF}
  AddDefine('WEAKINTREF');
  {$ENDIF}
end;

procedure TmwBasePasLex.InitLine;
begin
  FLineNumber := 0;
  FLinePos := 0;
  Run := 0;
end;

procedure TmwBasePasLex.SetCommentState(const Value: Pointer);
begin
  FCommentState := TCommentState(Value);
end;

procedure TmwBasePasLex.SetLine(const Value: string);
begin
  if not FUseSharedOrigin then
    FreeMem(FOrigin, BufferSize);
  FOrigin := PChar(Value);
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
    if TempString[sEnd] <> #39 then Inc(sEnd);
    Result := Copy(TempString, 2, sEnd - 2);
    TempString := '';
  end;
end;

function TmwBasePasLex.GetIsOrdIdent: Boolean;
begin
  Result := False;
  if FTokenID = ptIdentifier then
    Result := FExID in [ptBoolean, ptByte, ptChar, ptDWord, ptInt64, ptInteger,
      ptLongInt, ptLongWord, ptPChar, ptShortInt, ptSmallInt, ptWideChar, ptWord];
end;

function TmwBasePasLex.GetIsOrdinalType: Boolean;
begin
  Result := GetIsOrdIdent or (FTokenID in [ptAsciiChar, ptIntegerConst]);
end;

function TmwBasePasLex.GetIsRealType: Boolean;
begin
  Result := False;
  if FTokenID = ptIdentifier then
    Result := FExID in [ptComp, ptCurrency, ptDouble, ptExtended, ptReal, ptReal48, ptSingle];
end;

function TmwBasePasLex.GetIsStringType: Boolean;
begin
  Result := False;
  if FTokenID = ptIdentifier then
    Result := FExID in [ptAnsiString, ptWideString]
  else
    if FTokenID = ptString then
      Result := True
    else
      if FTokenID = ptStringConst then Result := True;
end;

function TmwBasePasLex.GetIsVarantType: Boolean;
begin
  Result := False;
  if FTokenID = ptIdentifier then
    Result := FExID in [ptOleVariant, ptVariant]
end;

function TmwBasePasLex.GetIsAddOperator: Boolean;
begin
  Result := FTokenID in [ptMinus, ptOr, ptPlus, ptXor];
end;

function TmwBasePasLex.GetIsMulOperator: Boolean;
begin
  Result := FTokenID in [ptAnd, ptAs, ptDiv, ptMod, ptShl, ptShr, ptSlash, ptStar];
end;

function TmwBasePasLex.GetIsRelativeOperator: Boolean;
begin
  Result := FTokenID in [ptAs, ptEqual, ptGreater, ptGreaterEqual, ptLower, ptLowerEqual,
    ptIn, ptIs, ptNotEqual];
end;

function TmwBasePasLex.GetIsCompilerDirective: Boolean;
begin
  Result := FTokenID in [ptCompDirect, ptDefineDirect, ptElseDirect,
    ptEndIfDirect, ptIfDefDirect, ptIfNDefDirect, ptIfOptDirect,
    ptIncludeDirect, ptResourceDirect, ptUndefDirect];
end;

function TmwBasePasLex.GetGenID: TptTokenKind;
begin
  Result := FTokenID;
  if FTokenID = ptIdentifier then
    if FExID <> ptUnknown then Result := FExID;
end;

{ TmwPasLex }

constructor TmwPasLex.Create;
begin
  inherited Create;
  FAheadLex := TmwBasePasLex.Create;
end;

destructor TmwPasLex.Destroy;
begin
  FAheadLex.Free;
  inherited Destroy;
end;

procedure TmwPasLex.SetOrigin(NewValue: PChar);
begin
  inherited SetOrigin(NewValue);
  FAheadLex.SetSharedOrigin(Self.Origin);
end;

procedure TmwPasLex.SetLine(const Value: string);
begin
  inherited SetLine(Value);
  FAheadLex.SetLine(Value);
end;

procedure TmwPasLex.AheadNext;
begin
  FAheadLex.NextNoJunk;
end;

function TmwPasLex.GetAheadExID: TptTokenKind;
begin
  Result := FAheadLex.ExID;
end;

function TmwPasLex.GetAheadGenID: TptTokenKind;
begin
  Result := FAheadLex.GenID;
end;

function TmwPasLex.GetAheadToken: string;
begin
  Result := FAheadLex.Token;
end;

function TmwPasLex.GetAheadTokenID: TptTokenKind;
begin
  Result := FAheadLex.TokenID;
end;

procedure TmwPasLex.InitAhead;
begin
  FAheadLex.CommentState := CommentState;
  FAheadLex.RunPos := RunPos;
  FAheadLex.FLineNumber := FLineNumber;
  FAheadLex.FLinePos := FLinePos;

  FAheadLex.CloneDefinesFrom(Self);

  while FAheadLex.IsJunk do
    FAheadLex.Next;
end;

function TmwPasLex.GetStatus: TmwPasLexStatus;
begin
  Result.CommentState := FCommentState;
  Result.ExID := FExID;
  Result.LineNumber := FLineNumber;
  Result.LinePos := FLinePos;
  Result.Origin := FOrigin;
  Result.RunPos := Run;
  Result.TokenPos := FTokenPos;
  Result.TokenID := FTokenID;
end;

procedure TmwPasLex.SetStatus(const Value: TmwPasLexStatus);
begin
  FCommentState := Value.CommentState;
  FExID := Value.ExID;
  FLineNumber := Value.LineNumber;
  FLinePos := Value.LinePos;
  FOrigin := Value.Origin;
  Run := Value.RunPos;
  FTokenPos := Value.TokenPos;
  FTokenID := Value.TokenID;
  FAheadLex.Origin := Value.Origin;
end;

procedure TmwBasePasLex.SetOnCompDirect(const Value: TDirectiveEvent);
begin
  FOnCompDirect := Value;
end;

procedure TmwBasePasLex.SetOnDefineDirect(const Value: TDirectiveEvent);
begin
  FOnDefineDirect := Value;
end;

procedure TmwBasePasLex.SetOnElseDirect(const Value: TDirectiveEvent);
begin
  FOnElseDirect := Value;
end;

procedure TmwBasePasLex.SetOnElseIfDirect(const Value: TDirectiveEvent);
begin
  FOnElseIfDirect := Value;
end;

procedure TmwBasePasLex.SetOnEndIfDirect(const Value: TDirectiveEvent);
begin
  FOnEndIfDirect := Value;
end;

procedure TmwBasePasLex.SetOnIfDefDirect(const Value: TDirectiveEvent);
begin
  FOnIfDefDirect := Value;
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
  FOnIfNDefDirect := Value;
end;

procedure TmwBasePasLex.SetOnIfOptDirect(const Value: TDirectiveEvent);
begin
  FOnIfOptDirect := Value;
end;

procedure TmwBasePasLex.SetOnIncludeDirect(const Value: TDirectiveEvent);
begin
  FOnIncludeDirect := Value;
end;

procedure TmwBasePasLex.SetOnResourceDirect(const Value: TDirectiveEvent);
begin
  FOnResourceDirect := Value;
end;

procedure TmwBasePasLex.SetOnUnDefDirect(const Value: TDirectiveEvent);
begin
  FOnUnDefDirect := Value;
end;

procedure TmwPasLex.SetOnCompDirect(const Value: TDirectiveEvent);
begin
  inherited;
end;

procedure TmwPasLex.SetOnDefineDirect(const Value: TDirectiveEvent);
begin
  inherited;
end;

procedure TmwPasLex.SetOnElseDirect(const Value: TDirectiveEvent);
begin
  inherited;
end;

procedure TmwPasLex.SetOnEndIfDirect(const Value: TDirectiveEvent);
begin
  inherited;
end;

procedure TmwPasLex.SetOnIfDefDirect(const Value: TDirectiveEvent);
begin
  inherited;
end;

procedure TmwPasLex.SetOnIfNDefDirect(const Value: TDirectiveEvent);
begin
  inherited;
end;

procedure TmwPasLex.SetOnIfOptDirect(const Value: TDirectiveEvent);
begin
  inherited;
end;

procedure TmwPasLex.SetOnIncludeDirect(const Value: TDirectiveEvent);
begin
  inherited;
end;

procedure TmwPasLex.SetOnResourceDirect(const Value: TDirectiveEvent);
begin
  inherited;
end;

procedure TmwPasLex.SetOnUnDefDirect(const Value: TDirectiveEvent);
begin
  inherited;
end;

function TmwBasePasLex.Func86: TptTokenKind;
begin
  Result := ptIdentifier;
  if KeyComp('Varargs') then FExID := ptVarargs;
end;

procedure TmwBasePasLex.StringDQProc;
begin
  if not FAsmCode then
  begin
    SymbolProc;
    Exit;
  end;
  FTokenID := ptStringDQConst;
  repeat
    Inc(Run);
    case FOrigin[Run] of
      #0, #10, #13:
        begin
          if Assigned(FOnMessage) then
            FOnMessage(Self, meError, 'Unterminated string', PosXY.X, PosXY.Y);
          Break;
        end;
      '\':
        begin
          Inc(Run);
          if CharInSet(FOrigin[Run], [#32..#127]) then Inc(Run);
        end;
    end;
  until FOrigin[Run] = '"';
  if FOrigin[Run] = '"' then
    Inc(Run);
end;

procedure TmwBasePasLex.AmpersandOpProc;
begin
  FTokenID := ptAmpersand;
  Inc(Run);
  while CharInSet(FOrigin[Run], ['a'..'z', 'A'..'Z','0'..'9', '_']) do
    Inc(Run);
  FTokenID := ptIdentifier;
end;

initialization
  MakeIdentTable;
end.

