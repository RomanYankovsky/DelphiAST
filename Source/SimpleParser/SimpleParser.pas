{---------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License Version
1.1 (the "License"); you may not use this file except in compliance with the
License. You may obtain a copy of the License at
http://www.mozilla.org/NPL/NPL-1_1Final.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: mwSimplePasPar.pas, released November 14, 1999.

The Initial Developer of the Original Code is Martin Waldenburg
(Martin.Waldenburg@T-Online.de).
Portions created by Martin Waldenburg are Copyright (C) 1998, 1999 Martin
Waldenburg.
All Rights Reserved.
Portions CopyRight by Robert Zierer.

Contributor(s): Roman Yankovsky, James Jacobson, Dean Hill, Vladimir Churbanov___________________.

Last Modified: 2014/09/14
Current Version: 1.10

Notes: This program is an early beginning of a Pascal parser.
I'd like to invite the Delphi community to develop it further and to create
a fully featured Object Pascal parser.

Modification history:

Jacob Thurman between 20040301 and 20020401

Made ready for Delphi 8:

Added new directives and keywords: static, sealed, final, operator, unsafe.

Added parsing for custom attributes (based on ECMA C# specification).

Added support for nested types in class declarations.

Jeff Rafter between 20020116 and 20020302

Added AncestorId and AncestorIdList back in, but now treat them as Qualified
Identifiers per Daniel Rolf's fix. The separation from QualifiedIdentifierList
is need for descendent classes.

Added VarName and VarNameList back in for descendent classes, fixed to correctly
use Identifiers as in Daniel's verison

Removed fInJunk flags (they were never used, only set)

Pruned uses clause to remove windows dependency. This required changing
"TPoint" to "TTokenPoint". TTokenPoint was declared in mwPasLexTypes

Daniel Rolf between 20010723 and 20020116

Made ready for Delphi 6

ciClassClass for "class function" etc.
ciClassTypeEnd marks end of a class declaration (I needed that for the delphi-objectif-connector)
ciEnumeratedTypeItem for items of enumerations
ciDirectiveXXX for the platform, deprecated, varargs, local
ciForwardDeclaration for "forward" (until now it has been read but no event)
ciIndexSpecifier for properties
ciObjectTypeEnd marks end of an object declaration
ciObjectProperty property for objects
ciObjectPropertySpecifiers property for objects
ciPropertyDefault marking default of property
ciDispIDSpecifier for dispid

patched some functions for implementing the above things and patching the following bugs/improv.:

ObjectProperty handling overriden properties
ProgramFile, UnitFile getting Identifier instead of dropping it
InterfaceHeritage: Qualified identifiers
bugs in variant records
typedconstant failed with complex set constants. simple patch using ConstantExpression

German localization for the two string constants. Define GERMAN for german string constants.

Greg Chapman on 20010522
Better handling of defaut array property
Separate handling of X and Y in property Pixels[X, Y: Integer through identifier "event"
corrected spelling of "ForwardDeclaration"

James Jacobson on 20010223
semi colon before finalization fix

James Jacobson on 20010223
RecordConstant Fix

Martin waldenburg on 2000107
Even Faster lexer implementation !!!!

James Jacobson on 20010107
  Improper handling of the construct
      property TheName: Integer read FTheRecord.One.Two; (stop at second point)
      where one and two are "qualifiable" structures.

James Jacobson on 20001221
   Stops at the second const.
   property Anchor[const Section: string; const Ident:string]: string read
   changed TmwSimplePasPar.PropertyParameterList

On behalf of  Martin Waldenburg and James Jacobson
 Correction in array property Handling (Matin and James) 07/12/2000
 Use of ExId instead of TokenId in ExportsElements (James) 07/12/2000
 Reverting to old behavior in Statementlist [PtintegerConst put back in] (James) 07/12/2000

Xavier Masson InnerCircleProject : XM : 08/11/2000
  Integration of the new version delivered by Martin Waldenburg with the modification I made described just below

Xavier Masson InnerCircleProject : XM : 07/15/2000
  Added "states/events " for      spaces( SkipSpace;) CRLFco (SkipCRLFco) and
    CRLF (SkipCRLF) this way the parser can give a complete view on code allowing
    "perfect" code reconstruction.
    (I fully now that this is not what a standard parser will do but I think it is more usefull this way ;) )
    go to www.innercircleproject.com for more explanations or express your critisism ;)

previous modifications not logged sorry ;)

Known Issues:
-----------------------------------------------------------------------------}
{----------------------------------------------------------------------------
 Last Modified: 05/22/2001
 Current Version: 1.1
 official version
   Maintained by InnerCircle

   http://www.innercircleproject.org

 02/07/2001
   added property handling in Object types
   changed handling of forward declarations in ExportedHeading method
-----------------------------------------------------------------------------}
unit SimpleParser;

interface

uses
  //!! pruned uses
  SysUtils,
  Classes,
  SimpleParser.Lexer.Types,
  SimpleParser.Lexer,
  SimpleParser.Types;

{$INCLUDE SimpleParser.inc}

{$IFDEF GERMAN} // DR 2002-01-16
resourcestring
  rsExpected = '''%s'' erwartet, aber ''%s'' gefunden';
  rsEndOfFile = 'Dateiende';
{$ELSE}
resourcestring
  rsExpected = '''%s'' expected found ''%s''';
  rsEndOfFile = 'end of file';
{$ENDIF}

const

 ClassMethodDirectiveEnum = [ptAbstract, ptCdecl, ptDynamic, ptMessage, ptOverride,
    ptOverload, ptPascal, ptRegister, ptReintroduce, ptSafeCall, ptStdCall,
    ptVirtual,
    ptDeprecated, ptLibrary, ptPlatform // DR 2001-10-20
    {$IFDEF D8_NEWER}
    , ptStatic //JThurman 2004-11-10
    {$ENDIF}
    {$IFDEF D9_NEWER}
    , ptInline
    , ptFinal
    , ptExperimental
    {$ENDIF}
    ];  //XM 2002-01-29

type
  ESyntaxError = class(Exception)
  private //jdj 7/18/1999
    FPosXY: TTokenPoint;
  protected

  public
    constructor Create(const Msg: string);
    constructor CreateFmt(const Msg: string; const Args: array of const);
    constructor CreatePos(const Msg: string; aPosXY: TTokenPoint);
    property PosXY: TTokenPoint read FPosXY write FPosXY;
  end;

  PDefineRec = ^TDefineRec;
  TDefineRec = record
    Defined: Boolean;
    StartCount: Integer;
    Next: PDefineRec;
  end;

type
  TmwSimplePasPar = class(TObject)
  private
    FOnMessage: TMessageEvent;
    fLexer: TmwPasLex;
    fOwnStream: Boolean;
    fStream: TCustomMemoryStream;
    fInterfaceOnly: Boolean;
    fLastNoJunkPos: Integer;
    fLastNoJunkLen: Integer;

    FUseDefines: Boolean;
    FDefines: TStrings;

    AheadParse: TmwSimplePasPar;

    fInRound: Integer;

    FTopDefineRec: PDefineRec;
    procedure EnterDefineBlock(ADefined: Boolean);
    procedure ExitDefineBlock;
    procedure ClearDefines;

    procedure InitAhead;
    procedure VariableTail;
    function GetInRound: Boolean;
  protected
    FDefineStack: Integer;
// !! removed fInJunk
    procedure Expected(Sym: TptTokenKind); virtual;
    procedure ExpectedEx(Sym: TptTokenKind); virtual;
    procedure ExpectedFatal(Sym: TptTokenKind); virtual;
    procedure HandlePtCompDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtDefineDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtElseDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtEndIfDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtIfDefDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtIfNDefDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtIfOptDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtIncludeDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtResourceDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtUndefDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtIfDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtIfEndDirect(Sender: TmwBasePasLex); virtual;
    procedure HandlePtElseIfDirect(Sender: TmwBasePasLex); virtual;
    procedure NextToken; virtual;
    procedure SkipJunk; virtual;
    procedure TerminateStream; virtual;
    procedure SEMICOLON; virtual;
    function GetExID: TptTokenKind; virtual;
    function GetTokenID: TptTokenKind; virtual;
    function GetGenID: TptTokenKind; virtual;
    procedure AccessSpecifier; virtual;
    procedure AdditiveOperator; virtual;
    procedure AddressOp; virtual;
    procedure AsOp; virtual;
    procedure AncestorIdList; virtual; // !! Added ancestorIdList back in...
    procedure AncestorId; virtual; // !! Added ancestorId back in...
    procedure AnonymousMethod; virtual;
    procedure AnonymousMethodType; virtual;
    procedure ArrayConstant; virtual;
    procedure ArrayType; virtual;
    procedure AsmStatement; virtual;
    procedure AssignOp; virtual;
    procedure AtExpression; virtual;
    procedure Block; virtual;
    procedure CaseElseStatement; virtual;
    procedure CaseLabel; virtual;
    procedure CaseSelector; virtual;
    procedure CaseStatement; virtual;
    procedure CharString; virtual;
    procedure ClassField; virtual;
    procedure ClassForward; virtual;
    procedure ClassFunctionHeading; virtual;
    procedure ClassHeritage; virtual;
    procedure ClassMemberList; virtual;
    procedure ClassMethodDirective; virtual;
    procedure ClassMethodHeading; virtual;
    procedure ClassMethodOrProperty; virtual;
    procedure ClassMethodResolution; virtual;
    procedure ClassProcedureHeading; virtual;
    procedure ClassClass; virtual;
    procedure ClassMethod; virtual;
    procedure ClassProperty; virtual;
    procedure ClassReferenceType; virtual;
    procedure ClassType; virtual;
    procedure ClassTypeEnd; virtual; // DR 2001-07-31
    procedure ClassVisibility; virtual;
    procedure CompoundStatement; virtual;
    procedure ConstantColon; virtual;
    procedure ConstantDeclaration; virtual;
    procedure ConstantEqual; virtual;
    procedure ConstantExpression; virtual;
    procedure ConstantName; virtual;
//JR added constant type
    procedure ConstantType; virtual;
    procedure ConstantValue; virtual;
    procedure ConstantValueTyped; virtual;
    procedure ConstParameter; virtual;
    procedure ConstructorHeading; virtual;
    procedure ConstructorName; virtual;
    procedure ConstSection; virtual;
    procedure ContainsClause; virtual;
    procedure ContainsExpression; virtual;
    procedure ContainsIdentifier; virtual;
    procedure ContainsIdentifierId; virtual;
    procedure ContainsStatement; virtual;
    {$IFDEF D8_NEWER}
    procedure CustomAttribute; virtual; //JThurman 2004-03-03
    {$ENDIF}
    procedure DeclarationSection; virtual;
    procedure Designator; virtual;
    procedure DestructorHeading; virtual;
    procedure DestructorName; virtual;
    procedure Directive16Bit; virtual;
    procedure DirectiveBinding; virtual;
    procedure DirectiveCalling; virtual;
    procedure DirectiveDeprecated; virtual; // DR 2001-10-20
    procedure DirectiveLibrary; virtual; // DR 2001-10-20
    procedure DirectiveLocal; virtual; // DR 2001-11-14
    procedure DirectivePlatform; virtual; // DR 2001-10-20
    procedure DirectiveVarargs; virtual; // DR 2001-11-14
    procedure DispInterfaceForward; virtual;
    procedure DispIDSpecifier; virtual; // DR 2001-07-
    procedure DotOp; virtual;
    procedure ElseStatement; virtual;
    procedure EmptyStatement; virtual;
    procedure EnumeratedType; virtual;
    procedure EnumeratedTypeItem; virtual; // DR 2001-10-29
    procedure ExceptBlock; virtual;
    procedure ExceptionBlockElseBranch; virtual;
    procedure ExceptionClassTypeIdentifier; virtual;
    procedure ExceptionHandler; virtual;
    procedure ExceptionHandlerList; virtual;
    procedure ExceptionIdentifier; virtual;
    procedure ExceptionVariable; virtual;
    procedure ExplicitType; virtual; // !! changed spelling to "Explicit"
    procedure ExportedHeading; virtual;
    procedure ExportsClause; virtual;
    procedure ExportsElement; virtual;
    procedure Expression; virtual;
    procedure ExpressionList; virtual;
    procedure ExternalDirective; virtual;
    procedure ExternalDirectiveThree; virtual;
    procedure ExternalDirectiveTwo; virtual;
    procedure Factor; virtual;
    procedure FieldDeclaration; virtual;
    procedure FieldList; virtual;
    procedure FieldNameList; virtual;
    procedure FieldName; virtual;
    procedure FileType; virtual;
    procedure FinallyBlock; virtual;
    procedure FormalParameterList; virtual;
    procedure FormalParameterSection; virtual;
    procedure ForStatement; virtual;
    procedure ForwardDeclaration; virtual; {GLC: corrected spelling}
    procedure FunctionHeading; virtual;
    procedure FunctionMethodDeclaration; virtual;
    procedure FunctionMethodName; virtual;
    procedure FunctionProcedureBlock; virtual;
    procedure FunctionProcedureName; virtual;
    procedure GotoStatement; virtual;
    procedure Identifier; virtual;
    procedure IdentifierList; virtual;
    procedure IfStatement; virtual;
    procedure ImplementationSection; virtual;
    procedure IncludeFile; virtual;
    procedure IndexSpecifier; virtual; // DR 2001-07-26
    procedure IndexOp; virtual;
    procedure InheritedStatement; virtual;
    procedure InheritedVariableReference; virtual;
    procedure InitializationSection; virtual;
    procedure InlineStatement; virtual;
    procedure InParameter; virtual;
    procedure InterfaceDeclaration; virtual;
    procedure InterfaceForward; virtual;
    procedure InterfaceGUID; virtual;
    procedure InterfaceHeritage; virtual;
    procedure InterfaceMemberList; virtual;
    procedure InterfaceSection; virtual;
    procedure InterfaceType; virtual;
    procedure LabelDeclarationSection; virtual;
    procedure LabeledStatement; virtual;
    procedure LabelId; virtual;
    procedure LibraryFile; virtual;
    procedure MainUsedUnitExpression; virtual;
    procedure MainUsedUnitName; virtual;
    procedure MainUsedUnitStatement; virtual;
    procedure MainUsesClause; virtual;
    procedure MethodKind; virtual;
    procedure MultiplicativeOperator; virtual;
    procedure NewFormalParameterType; virtual;
    procedure NotOp; virtual;
    procedure NilToken; virtual;
    procedure Number; virtual;
    procedure ObjectConstructorHeading; virtual;
    procedure ObjectDestructorHeading; virtual;
    procedure ObjectField; virtual;
    procedure ObjectForward; virtual;
    procedure ObjectFunctionHeading; virtual;
    procedure ObjectHeritage; virtual;
    procedure ObjectMemberList; virtual;
    procedure ObjectMethodDirective; virtual;
    procedure ObjectMethodHeading; virtual;
    procedure ObjectNameOfMethod; virtual;
    procedure ObjectProperty; virtual;
    procedure ObjectPropertySpecifiers; virtual;
    procedure ObjectProcedureHeading; virtual;
    procedure ObjectType; virtual;
    procedure ObjectTypeEnd; virtual; // DR 2001-08-07
    procedure ObjectVisibility; virtual;
    procedure OldFormalParameterType; virtual;
    procedure OrdinalIdentifier; virtual;
    procedure OrdinalType; virtual;
    procedure OutParameter; virtual;
    procedure PackageFile; virtual;
    procedure ParameterFormal; virtual;
    procedure ParameterName; virtual;
    procedure ParameterNameList; virtual;
    procedure ParseFile; virtual;
    procedure PointerSymbol; virtual;
    procedure PointerType; virtual;
    procedure ProceduralDirective; virtual;
    procedure ProceduralType; virtual;
    procedure ProcedureDeclarationSection; virtual;
    procedure ProcedureHeading; virtual;
    procedure ProcedureProcedureName; virtual;
    procedure ProcedureMethodName; virtual;
    procedure ProgramBlock; virtual;
    procedure ProgramFile; virtual;
    procedure PropertyDefault; virtual;
    procedure PropertyInterface; virtual;
    procedure PropertyName; virtual;
    procedure PropertyParameterConst; virtual;
    procedure PropertyParameterList; virtual;
    procedure PropertySpecifiers; virtual;
    procedure QualifiedIdentifier; virtual;
    procedure QualifiedIdentifierList; virtual;
    procedure RaiseStatement; virtual;
    procedure ReadAccessIdentifier; virtual;
    procedure RealIdentifier; virtual;
    procedure RealType; virtual;
    procedure RecordConstant; virtual;
    procedure RecordFieldConstant; virtual;
    procedure RecordType; virtual;
    procedure RecordVariant; virtual;
    procedure RelativeOperator; virtual;
    procedure RepeatStatement; virtual;
    procedure RequiresClause; virtual;
    procedure RequiresIdentifier; virtual;
    procedure RequiresIdentifierId; virtual;
    procedure ResolutionInterfaceName; virtual;
    procedure ResourceDeclaration; virtual;
    procedure ReturnType; virtual;
    procedure RoundClose; virtual;
    procedure RoundOpen; virtual;
    procedure SetConstructor; virtual;
    procedure SetElement; virtual;
    procedure SetType; virtual;
    procedure SimpleExpression; virtual;
    procedure SimpleStatement; virtual;
    procedure SimpleType; virtual;
    procedure SkipAnsiComment; virtual;
    procedure SkipBorComment; virtual;
    procedure SkipSlashesComment; virtual;
    procedure SkipSpace; virtual; //XM Jul-2000
    procedure SkipCRLFco; virtual; //XM Jul-2000
    procedure SkipCRLF; virtual; //XM Jul-2000
    procedure Statement; virtual;
    procedure StatementList; virtual;
    procedure StorageExpression; virtual;
    procedure StorageIdentifier; virtual;
    procedure StorageDefault; virtual;
    procedure StorageNoDefault; virtual;
    procedure StorageSpecifier; virtual;
    procedure StorageStored; virtual;
    procedure StringConst; virtual;
    procedure StringConstSimple; virtual;
    procedure StringIdentifier; virtual;
    procedure StringStatement; virtual;
    procedure StringType; virtual;
    procedure StructuredType; virtual;
    procedure SubrangeType; virtual;
    procedure TagField; virtual;
    procedure TagFieldName; virtual;
    procedure TagFieldTypeName; virtual;
    procedure Term; virtual;
    procedure ThenStatement; virtual;
    procedure TryStatement; virtual;
    procedure TypedConstant; virtual;
    procedure TypeDeclaration; virtual;
    procedure TypeId; virtual;
    procedure TypeKind; virtual;
    procedure TypeName; virtual;
    procedure TypeSimple; virtual;
    //generics
    procedure TypeArgs; virtual;
    procedure TypeParams; virtual;
    procedure TypeParamDecl; virtual;
    procedure TypeParamDeclList; virtual;
    procedure TypeParamList; virtual;
    procedure ConstraintList; virtual;
    procedure Constraint; virtual;
    //end generics
    procedure TypeSection; virtual;
    procedure UnaryMinus; virtual;
    procedure UnitFile; virtual;
    procedure UnitId; virtual;
    procedure UnitName; virtual;
    procedure UsedUnitName; virtual;
    procedure UsedUnitsList; virtual;
    procedure UsesClause; virtual;
    procedure VarAbsolute; virtual;
    procedure VarEqual; virtual;
    procedure VarDeclaration; virtual;
    procedure Variable; virtual;
    procedure VariableList; virtual;
    procedure VariableReference; virtual;
//    procedure VariableTwo; virtual;
    procedure VariantIdentifier; virtual;
    procedure VariantSection; virtual;
    procedure VarParameter; virtual;
    procedure VarName; virtual; //!! Added VarName and VarNameList back in...
    procedure VarNameList; virtual;
    procedure VarSection; virtual;
    procedure VisibilityAutomated; virtual;
    procedure VisibilityPrivate; virtual;
    procedure VisibilityProtected; virtual;
    procedure VisibilityPublic; virtual;
    procedure VisibilityPublished; virtual;
    procedure VisibilityUnknown; virtual;
    procedure WhileStatement; virtual;
    procedure WithStatement; virtual;
    procedure WriteAccessIdentifier; virtual;
    {$IFDEF D8_NEWER}//JThurman 2004-03-21
    {This is the syntax for custom attributes, based quite strictly on the
    ECMA syntax specifications for C#, but with a Delphi expression being
    used at the bottom as opposed to a C# expression}
    procedure GlobalAttributes;
    procedure GlobalAttributeSections;
    procedure GlobalAttributeSection;
    procedure GlobalAttributeTargetSpecifier;
    procedure GlobalAttributeTarget;
    procedure Attributes;
    procedure AttributeSections;
    procedure AttributeSection;
    procedure AttributeTargetSpecifier;
    procedure AttributeTarget;
    procedure AttributeList;
    procedure Attribute;
    procedure AttributeName;
    procedure AttributeArguments;
    procedure PositionalArgumentList;
    procedure PositionalArgument;
    procedure NamedArgumentList;
    procedure NamedArgument;
    procedure AttributeArgumentExpression; 
    {$ENDIF}
    property ExID: TptTokenKind read GetExID;
    property GenID: TptTokenKind read GetGenID;
    property TokenID: TptTokenKind read GetTokenID;
    property InRound: Boolean read GetInRound;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SynError(Error: TmwParseError); virtual;
    procedure Run(UnitName: string; SourceStream: TCustomMemoryStream); virtual;

    procedure InitDefines;
    procedure AddDefine(const ADefine: string);
    procedure RemoveDefine(const ADefine: string);
    function IsDefined(const ADefine: string): Boolean;

    property InterfaceOnly: Boolean read fInterfaceOnly write fInterfaceOnly;
    property Lexer: TmwPasLex read fLexer;
    property OnMessage: TMessageEvent read FOnMessage write FOnMessage;
    property LastNoJunkPos: Integer read fLastNoJunkPos;
    property LastNoJunkLen: Integer read fLastNoJunkLen;

    property UseDefines: Boolean read FUseDefines write FUseDefines;
  end;

implementation

uses Windows;

{ ESyntaxError }

constructor ESyntaxError.Create(const Msg: string);
begin
  // !! changed initialization for TTokenPoint
  FPosXY.X:= -1;
  FPosXY.Y:= -1;
  inherited Create(Msg);
end;

constructor ESyntaxError.CreateFmt(const Msg: string; const Args: array of const);
begin
  // !! changed initialization for TTokenPoint
  FPosXY.X:= -1;
  FPosXY.Y:= -1;
  inherited CreateFmt(Msg, Args);
end;

constructor ESyntaxError.CreatePos(const Msg: string; aPosXY: TTokenPoint);
begin
  Message := Msg;
  FPosXY := aPosXY;
end;

{ TmwSimplePasPar }
(* DR 2002-01-16
const
  cnExpected = 'Expected ''%s'' found ''%s''';
//  cnOrExpected = 'Expected ''%s'' or ''%s'' found ''%s''';
  cnEndOfFile = 'end of file'; {jdj 7/22/1999}
//  cnIntegerOverflow = 'Integer constant too large'; {jdj 7/22/1999}
*)

 {range checks a ptIntegerConst-slightly faster than StrToInt}
{function IsValidInteger(const S: string): Boolean; jdj 7/22/1999
var jdj removed 02/07/2001
  C: Integer;
  N: Integer;
begin
  Val(S, N, C);
  Result := (C = 0);
end;}

procedure TmwSimplePasPar.ForwardDeclaration;
begin {jdj added method 02/07/2001}
  NextToken;
  SEMICOLON;
end;

procedure TmwSimplePasPar.ObjectProperty;
begin {jdj added method 02/07/2001}
 // DR 2001-08-07 -> changed. for array-property override failure
  Expected(ptProperty);
  PropertyName;
  case TokenID of
    ptColon, ptSquareOpen:
      begin
        PropertyInterface;
      end;
  end;
  ObjectPropertySpecifiers;
  case ExID of
    ptDefault:
      begin
        PropertyDefault; //DR 2001-07-16
        SEMICOLON;
      end;
  end;
end;

procedure TmwSimplePasPar.ObjectPropertySpecifiers;
begin {jdj added method 02/07/2001}
  if ExID = ptIndex then
  begin
    IndexSpecifier; // DR 2001-08-07
  end;
  while ExID in [ptRead, ptReadOnly, ptWrite, ptWriteOnly] do
  begin
    AccessSpecifier;
  end;
  while ExID in [ptDefault, ptNoDefault, ptStored] do
  begin
    StorageSpecifier;
  end;
  SEMICOLON;
end;

procedure TmwSimplePasPar.Run(UnitName: string; SourceStream: TCustomMemoryStream);
begin
  fStream := nil;
  fOwnStream := False;
  {if SourceStream = nil then
  begin
    fStream := TMemoryStream.Create;
    fOwnStream := True;
    fStream.LoadFromFile(UnitName);
  end
  else}
    fStream := SourceStream;
  TerminateStream;
  fLexer.Origin := fStream.Memory;
  ParseFile;
  if fOwnStream then
    fStream.Free;
end;

constructor TmwSimplePasPar.Create;
begin
  inherited Create;
  fLexer := TmwPasLex.Create;
  fLexer.OnCompDirect := HandlePtCompDirect;
  fLexer.OnDefineDirect := HandlePtDefineDirect;
  fLexer.OnElseDirect := HandlePtElseDirect;
  fLexer.OnEndIfDirect := HandlePtEndIfDirect;
  fLexer.OnIfDefDirect := HandlePtIfDefDirect;
  fLexer.OnIfNDefDirect := HandlePtIfNDefDirect;
  fLexer.OnIfOptDirect := HandlePtIfOptDirect;
  fLexer.OnIncludeDirect := HandlePtIncludeDirect;
  fLexer.OnResourceDirect := HandlePtResourceDirect;
  fLexer.OnUnDefDirect := HandlePtUndefDirect;
  fLexer.OnIfDirect := HandlePtIfDirect;
  fLexer.OnIfEndDirect := HandlePtIfEndDirect;
  fLexer.OnElseIfDirect := HandlePtElseIfDirect;

  FDefines := TStringList.Create;
  with TStringList(FDefines) do
  begin
    Sorted := True;
    Duplicates := dupIgnore;
  end;
  InitDefines;
  FDefineStack := 0;
  FUseDefines := False;
end;

destructor TmwSimplePasPar.Destroy;
begin
  ClearDefines; //Must do this here to avoid a memory leak
  FDefines.Free;

  AheadParse.Free;

  fLexer.Free;
  inherited Destroy;
end;

{next two check for ptNull and ExpectedFatal for an EOF Error}

procedure TmwSimplePasPar.Expected(Sym: TptTokenKind);
begin
  if Sym <> Lexer.TokenID then
  begin
    if TokenID = ptNull then
      ExpectedFatal(Sym) {jdj 7/22/1999}
    else
    begin
      if Assigned(FOnMessage) then
        FOnMessage(Self, meError, Format(rsExpected, [TokenName(Sym), fLexer.Token]),
          fLexer.PosXY.X, fLexer.PosXY.Y);
    end;
  end
  else
    NextToken;
end;

procedure TmwSimplePasPar.ExpectedEx(Sym: TptTokenKind);
begin
  if Sym <> Lexer.ExID then
  begin
    if Lexer.TokenID = ptNull then
      ExpectedFatal(Sym) {jdj 7/22/1999}
    else if Assigned(FOnMessage) then
      FOnMessage(Self, meError, Format(rsExpected, ['EX:' + TokenName(Sym), fLexer.Token]),
        fLexer.PosXY.X, fLexer.PosXY.Y);
  end
  else
    NextToken;
end;

{Replace Token with cnEndOfFile if TokenId = ptnull}

procedure TmwSimplePasPar.ExpectedFatal(Sym: TptTokenKind);
var
  tS: string;
begin
  if Sym <> Lexer.TokenID then
  begin
    {--jdj 7/22/1999--}
    if Lexer.TokenId = ptNull then
      tS := rsEndOfFile
    else
      tS := fLexer.Token;
    {--jdj 7/22/1999--}
    raise ESyntaxError.CreatePos(Format(rsExpected, [TokenName(Sym), tS]), fLexer.PosXY);
  end
  else
    NextToken;
end;

procedure TmwSimplePasPar.HandlePtCompDirect(Sender: TmwBasePasLex);
begin
  if Assigned(FOnMessage) then
    FOnMessage(Self, meNotSupported, 'Currently not supported ' + fLexer.Token, fLexer.PosXY.X, fLexer.PosXY.Y);
  //  Sender.NextNoJunk;
  Sender.Next; //XM Jul-2000
  { ToDo }
end;

procedure TmwSimplePasPar.HandlePtDefineDirect(Sender: TmwBasePasLex);
begin
//  if Assigned(FOnMessage) then
//    FOnMessage(Self, meNotSupported, 'Currently not supported ' + fLexer.Token, fLexer.PosXY.X, fLexer.PosXY.Y);

//  if FUseDefines then
//    AddDefine(Lexer.DirectiveParam)
//  else

  //  Sender.NextNoJunk;
  Sender.Next; //XM Jul-2000

  { ToDo }
end;

procedure TmwSimplePasPar.HandlePtElseDirect(Sender: TmwBasePasLex);
begin
//  if Assigned(FOnMessage) then
//    FOnMessage(Self, meNotSupported, 'Currently not supported ' + fLexer.Token, fLexer.PosXY.X, fLexer.PosXY.Y);

//  if FUseDefines then
//  begin
//    if FTopDefineRec <> nil then
//    begin
//      if FTopDefineRec^.Defined then
//        Inc(FDefineStack)
//      else
//        if FDefineStack > 0 then
//          Dec(FDefineStack);
//    end;
//  end;

  //  Sender.NextNoJunk;
  if Sender = Lexer then
    NextToken
  else
    Sender.Next; //XM Jul-2000

  { ToDo }
end;

procedure TmwSimplePasPar.HandlePtElseIfDirect(Sender: TmwBasePasLex);
var
  Param: string;
  Def: string;
begin
//  if FUseDefines then
//  begin
//    if FTopDefineRec <> nil then
//    begin
//      if FTopDefineRec^.Defined then
//        Inc(FDefineStack)
//      else
//      begin
//        if FDefineStack > 0 then
//          Dec(FDefineStack);
//        Param := Sender.DirectiveParam;
//        if Pos('DEFINED', Param) = 1 then
//        begin
//          Def := Copy(Param, 9, Length(Param) - 9);
//          EnterDefineBlock(IsDefined(Def));
//        end;
//      end;
//    end;
//  end;

  if Sender = Lexer then
    NextToken
  else
    Sender.Next;
end;

procedure TmwSimplePasPar.HandlePtEndIfDirect(Sender: TmwBasePasLex);
begin
//  if Assigned(FOnMessage) then
//    FOnMessage(Self, meNotSupported, 'Currently not supported ' + fLexer.Token, fLexer.PosXY.X, fLexer.PosXY.Y);

//  if FUseDefines then
//  begin
//    ExitDefineBlock;
//  end;

  //  Sender.NextNoJunk;
  if Sender = Lexer then
    NextToken
  else
    Sender.Next; //XM Jul-2000

  { ToDo }
end;

procedure TmwSimplePasPar.HandlePtIfDefDirect(Sender: TmwBasePasLex);
begin
//  if Assigned(FOnMessage) then
//    FOnMessage(Self, meNotSupported, 'Currently not supported ' + fLexer.Token, fLexer.PosXY.X, fLexer.PosXY.Y);
//  if FUseDefines then
//  begin
//    EnterDefineBlock(IsDefined(Sender.DirectiveParam));
//  end;

  //  Sender.NextNoJunk;
  if Sender = Lexer then
    NextToken
  else
    Sender.Next; //XM Jul-2000

  { ToDo }
end;

procedure TmwSimplePasPar.HandlePtIfDirect(Sender: TmwBasePasLex);
var
  Def: string;
  Param: string;
begin
//  Param := Sender.DirectiveParam;
//  if FUseDefines then
//  begin
//    if Pos('DEFINED', Param) = 1 then
//    begin
//      Def := Copy(Param, 9, Length(Param) - 9);
//      EnterDefineBlock(IsDefined(Def));
//    end;
//  end;
  if Sender = Lexer then
    NextToken
  else
    Sender.Next;
end;

procedure TmwSimplePasPar.HandlePtIfEndDirect(Sender: TmwBasePasLex);
begin
//  if FUseDefines then
//    ExitDefineBlock;

  if Sender = Lexer then
    NextToken
  else
    Sender.Next;
end;

procedure TmwSimplePasPar.HandlePtIfNDefDirect(Sender: TmwBasePasLex);
begin
//  if Assigned(FOnMessage) then
//    FOnMessage(Self, meNotSupported, 'Currently not supported ' + fLexer.Token, fLexer.PosXY.X, fLexer.PosXY.Y);
//  if FUseDefines then
//  begin
//    EnterDefineBlock(not IsDefined(Sender.DirectiveParam));
//  end;

  //  Sender.NextNoJunk;
  if Sender = Lexer then
    NextToken
  else
    Sender.Next; //XM Jul-2000

  { ToDo }
end;

procedure TmwSimplePasPar.HandlePtIfOptDirect(Sender: TmwBasePasLex);
begin
  if Assigned(FOnMessage) then
    FOnMessage(Self, meNotSupported, 'Currently not supported ' + fLexer.Token, fLexer.PosXY.X, fLexer.PosXY.Y);
  //  Sender.NextNoJunk;
  Sender.Next; //XM Jul-2000

  { ToDo }
end;

procedure TmwSimplePasPar.HandlePtIncludeDirect(Sender: TmwBasePasLex);
begin
  if Assigned(FOnMessage) then
    FOnMessage(Self, meNotSupported, 'Currently not supported ' + fLexer.Token, fLexer.PosXY.X, fLexer.PosXY.Y);
  //  Sender.NextNoJunk;
  Sender.Next; //XM Jul-2000

  { ToDo }
end;

procedure TmwSimplePasPar.HandlePtResourceDirect(Sender: TmwBasePasLex);
begin
  if Assigned(FOnMessage) then
    FOnMessage(Self, meNotSupported, 'Currently not supported ' + fLexer.Token, fLexer.PosXY.X, fLexer.PosXY.Y);
  //  Sender.NextNoJunk;
  Sender.Next; //XM Jul-2000

  { ToDo }
end;

procedure TmwSimplePasPar.HandlePtUndefDirect(Sender: TmwBasePasLex);
begin
//  if Assigned(FOnMessage) then
//    FOnMessage(Self, meNotSupported, 'Currently not supported ' + fLexer.Token, fLexer.PosXY.X, fLexer.PosXY.Y);

//  if FUseDefines then
//    RemoveDefine(Lexer.DirectiveParam);

  //  Sender.NextNoJunk;
  Sender.Next; //XM Jul-2000

  { ToDo }
end;

procedure TmwSimplePasPar.NextToken;
begin
//  if FUseDefines then
//  begin
//    repeat
//      FLexer.Next;
//    until (FDefineStack = 0) or (TokenID = ptNull);
//    SkipJunk;
//  end else
//  begin
    FLexer.NextNoJunk;
    //fLexer.Next;
    //SkipJunk;
//  end;
end;

procedure TmwSimplePasPar.NilToken;
begin
  Expected(ptNil);
end;

procedure TmwSimplePasPar.NotOp;
begin
  Expected(ptNot);
end;

procedure TmwSimplePasPar.SkipJunk;
begin
  if Lexer.IsJunk then
  begin
    case TokenID of
      ptAnsiComment:
        begin
          SkipAnsiComment;
        end;
      ptBorComment:
        begin
          SkipBorComment;
        end;
      ptSlashesComment:
        begin
          SkipSlashesComment;
        end;
      ptSpace:
        begin
          SkipSpace; //XM Jul-2000
        end;
      ptCRLFCo:
        begin
          SkipCRLFco;
        end;
      ptCRLF:
        begin
          SkipCRLF;
        end;
      {$IFDEF D8_NEWER} //JThurman 2004-3-19
      ptSquareOpen:
        begin
          CustomAttribute;
        end;
      {$ENDIF}
    else
      begin
        Lexer.Next;
      end;
    end;
  end;
  fLastNoJunkPos := Lexer.TokenPos;
  fLastNoJunkLen := Lexer.TokenLen;
end;

procedure TmwSimplePasPar.SkipAnsiComment;
begin
  Expected(ptAnsiComment);
  while TokenID in [ptAnsiComment] do
    Lexer.Next;
end;

procedure TmwSimplePasPar.SkipBorComment;
begin
  Expected(ptBorComment);
  while TokenID in [ptBorComment] do
    Lexer.Next;
end;

procedure TmwSimplePasPar.SkipSlashesComment;
begin
  Expected(ptSlashesComment);
end;

procedure TmwSimplePasPar.TerminateStream;
var
  aChar: Char;
begin
  fStream.Position := fStream.Size;
  aChar := #0;
  fStream.Write(aChar, sizeof(char));
end;

procedure TmwSimplePasPar.ThenStatement;
begin
  Expected(ptThen);
  Statement;
end;

procedure TmwSimplePasPar.SEMICOLON;
begin
  case Lexer.TokenID of
    ptElse, ptEnd, ptExcept, ptfinally, ptFinalization, ptRoundClose, ptUntil: // jdj 2.23.20001 added ptFinalization
      ;
  else
    Expected(ptSemiColon);
    //Check for semicolon before else - common syntax error - JT 11.10.2007
    //Doesn't work here - it fails a CASE statement
//    if Lexer.TokenID = ptElse then
//    begin
//      if Assigned(FOnMessage) then
//      begin
//        FOnMessage(Self, meError, ''';'' not allowed before ''ELSE''',
//          FLexer.PosXY.X, FLexer.PosXY.Y);
//      end;
//    end;
  end;
end;

function TmwSimplePasPar.GetExID: TptTokenKind;
begin
  Result := fLexer.ExID;
end;

function TmwSimplePasPar.GetTokenID: TptTokenKind;
begin
  Result := fLexer.TokenID;
end;

procedure TmwSimplePasPar.GotoStatement;
begin
  Expected(ptGoto);
  LabelId;
end;

function TmwSimplePasPar.GetGenID: TptTokenKind;
begin
  Result := fLexer.GenID;
end;

function TmwSimplePasPar.GetInRound: Boolean;
begin
  Result := fInRound > 0;
end;

procedure TmwSimplePasPar.SynError(Error: TmwParseError);
begin
  if Assigned(FOnMessage) then
    FOnMessage(Self, meError, ParserErrorName(Error) + ' found ' + fLexer.Token, fLexer.PosXY.X,
      fLexer.PosXY.Y);

end;

(******************************************************************************
 This part is oriented at the official grammar of Delphi 4
 and parialy based on Robert Zierers Delphi grammar.
 For more information about Delphi grammars take a look at:
 http://www.stud.mw.tu-muenchen.de/~rz1/Grammar.html
******************************************************************************)

procedure TmwSimplePasPar.ParseFile;
var
  I: Integer;
begin
//  OutputDebugString('ParseFile');
  //Copy the defines into the lexer
  for I := 0 to FDefines.Count - 1 do
  begin
    Lexer.AddDefine(FDefines[I]);
  end;

  SkipJunk;
  case GenID of
    ptLibrary:
      begin
        LibraryFile;
      end;
    ptPackage:
      begin
        PackageFile;
      end;
    ptProgram:
      begin
        ProgramFile;
      end;
    ptUnit:
      begin
        UnitFile;
      end;
  else
    begin
      IncludeFile;
    end;
  end;
end;

procedure TmwSimplePasPar.LibraryFile;
begin
  Expected(ptLibrary);
  UnitName;
  SEMICOLON;
  ProgramBlock;
  Expected(ptPoint);
end;

procedure TmwSimplePasPar.PackageFile;
begin
  ExpectedEx(ptPackage);
  UnitName;
  SEMICOLON;
  case ExID of
    ptRequires:
      begin
        RequiresClause;
      end;
  end;
  case ExID of
    ptContains:
      begin
        ContainsClause;
      end;
  end;

  {$IFDEF D8_NEWER}
  while Lexer.TokenID = ptSquareOpen do
  begin
    CustomAttribute;
  end;
  {$ENDIF}

  Expected(ptEnd);
  Expected(ptPoint);
end;

procedure TmwSimplePasPar.ProgramFile;
begin
 // DR 2002-01-11
  Expected(ptProgram);
  UnitName;
  if TokenID = ptRoundOpen then
  begin
    NextToken;
    IdentifierList;
    Expected(ptRoundClose);
  end;
  if not InterfaceOnly then
  begin
    SEMICOLON;
    ProgramBlock;
    Expected(ptPoint);
  end;
end;

procedure TmwSimplePasPar.UnaryMinus;
begin
  Expected(ptMinus);
end;

procedure TmwSimplePasPar.UnitFile;
begin
 // DR 2002-01-11

//??
  Expected(ptUnit);
  UnitName;

  while ExID in [ptDeprecated, ptLibrary, ptPlatform] do
    case ExID of
      ptDeprecated: DirectiveDeprecated;
      ptLibrary: DirectiveLibrary;
      ptPlatform: DirectivePlatform;
    end;

  SEMICOLON;
  InterfaceSection;
  if not InterfaceOnly then
  begin
    ImplementationSection;
    InitializationSection;
    Expected(ptPoint);
  end;
end;

procedure TmwSimplePasPar.ProgramBlock;
begin
  if TokenID = ptUses then
  begin
    MainUsesClause;
  end;
  Block;
end;

procedure TmwSimplePasPar.MainUsesClause;
begin
  Expected(ptUses);
  MainUsedUnitStatement;
  while TokenID = ptComma do
  begin
    NextToken;
    MainUsedUnitStatement;
  end;
  SEMICOLON;
end;

procedure TmwSimplePasPar.MethodKind;
begin
  case TokenID of
    ptConstructor:
      begin
        NextToken;
      end;
    ptDestructor:
      begin
        NextToken;
      end;
    ptProcedure:
      begin
        NextToken;
      end;
    ptFunction:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidProcedureMethodDeclaration);
    end;
  end;
end;

procedure TmwSimplePasPar.MainUsedUnitStatement;
begin
  MainUsedUnitName;
  if Lexer.TokenID = ptIn then
  begin
    NextToken;
    MainUsedUnitExpression;
  end;
end;

procedure TmwSimplePasPar.MainUsedUnitName;
begin
//  Expected(ptIdentifier);
  UsedUnitName; //JThurman 2004-11-10
end;

procedure TmwSimplePasPar.MainUsedUnitExpression;
begin
  ConstantExpression;
end;

procedure TmwSimplePasPar.UsesClause;
begin
  Expected(ptUses);
  UsedUnitsList;
  SEMICOLON;
end;

procedure TmwSimplePasPar.UsedUnitsList;
begin
  UsedUnitName;
  while TokenID = ptComma do
  begin
    NextToken;
    UsedUnitName;
  end;
end;

procedure TmwSimplePasPar.UsedUnitName;
begin
  {$IFDEF D8_NEWER} //JThurman 2004-03-03
  UnitId;
  while Lexer.TokenID = ptPoint do
  begin
    NextToken;
    UnitId;
  end;
  {$ELSE}
  UnitId;
  {$ENDIF}
end;

procedure TmwSimplePasPar.Block;
begin
  while TokenID in [ptClass, ptConst, ptConstructor, ptDestructor, ptExports,
    ptFunction, ptLabel, ptProcedure, ptResourceString, ptThreadVar, ptType,
    ptVar{$IFDEF D8_NEWER}, ptSquareOpen{$ENDIF}] do
  begin
    DeclarationSection;
  end;
  case TokenID of
    ptAsm:
      begin
        AsmStatement;
      end;
  else
    begin
      CompoundStatement;
    end;
  end;
end;

procedure TmwSimplePasPar.DeclarationSection;
begin
  case TokenID of
    ptClass:
      begin
        ProcedureDeclarationSection;
      end;
    ptConst:
      begin
        ConstSection;
      end;
    ptConstructor:
      begin
        ProcedureDeclarationSection;
      end;
    ptDestructor:
      begin
        ProcedureDeclarationSection;
      end;
    ptExports:
      begin
        ExportsClause;
      end;
    ptFunction:
      begin
        ProcedureDeclarationSection;
      end;
    ptLabel:
      begin
        LabelDeclarationSection;
      end;
    ptProcedure:
      begin
        ProcedureDeclarationSection;
      end;
    ptResourceString:
      begin
        ConstSection;
      end;
    ptType:
      begin
        TypeSection;
      end;
    ptThreadVar:
      begin
        VarSection;
      end;
    ptVar:
      begin
        VarSection;
      end;
    {$IFDEF D8_NEWER} //JThurman
    ptSquareOpen:
      begin
        CustomAttribute;
      end;
    {$ENDIF}
  else
    begin
      SynError(InvalidDeclarationSection);
    end;
  end;
end;

procedure TmwSimplePasPar.UnitId;
begin
  Expected(ptIdentifier);
end;

procedure TmwSimplePasPar.UnitName;
begin
  UnitId;
  while Lexer.TokenID = ptPoint do
  begin
    NextToken;
    UnitId;
  end;
end;

procedure TmwSimplePasPar.InterfaceHeritage;
begin
  Expected(ptRoundOpen);
  AncestorIdList; // JR moved qualified check into ancestorIdList // DR 2001-11-01 can also be qualified!
  Expected(ptRoundClose);
end;

procedure TmwSimplePasPar.InterfaceGUID;
begin
  Expected(ptSquareOpen);
  CharString;
  Expected(ptSquareClose);
end;

procedure TmwSimplePasPar.AccessSpecifier;
begin
  case ExID of
    ptRead:
      begin
        NextToken;
        ReadAccessIdentifier;
      end;
    ptWrite:
      begin
        NextToken;
        WriteAccessIdentifier;
      end;
    ptReadOnly:
      begin
        NextToken;
      end;
    ptWriteOnly:
      begin
        NextToken;
      end;
    {$IFDEF D8_NEWER}
    ptAdd:
      begin
        NextToken;
        QualifiedIdentifier; //TODO: AddAccessIdentifier
      end;
    ptRemove:
      begin
        NextToken;
        QualifiedIdentifier; //TODO: RemoveAccessIdentifier
      end;
    {$ENDIF}
  else
    begin
      SynError(InvalidAccessSpecifier);
    end;
  end;
end;

procedure TmwSimplePasPar.ReadAccessIdentifier;
begin
  variable;
end;

procedure TmwSimplePasPar.WriteAccessIdentifier;
begin
  variable;
end;

procedure TmwSimplePasPar.StorageSpecifier;
begin
  case ExID of
    ptStored:
      begin
        StorageStored;
      end;
    ptDefault:
      begin
        StorageDefault;
      end;
    ptNoDefault:
      begin
        StorageNoDefault;
      end
  else
    begin
      SynError(InvalidStorageSpecifier);
    end;
  end;
end;

procedure TmwSimplePasPar.StorageDefault;
begin
  ExpectedEx(ptDefault);
  StorageExpression;
end;

procedure TmwSimplePasPar.StorageNoDefault;
begin
  ExpectedEx(ptNoDefault);
end;

procedure TmwSimplePasPar.StorageStored;
begin
  ExpectedEx(ptStored);
  case TokenID of
    ptIdentifier:
      begin
        StorageIdentifier;
      end;
  else
    if TokenID <> ptSemiColon then
    begin
      StorageExpression;
    end;
  end;
end;

procedure TmwSimplePasPar.StorageExpression;
begin
  ConstantExpression;
end;

procedure TmwSimplePasPar.StorageIdentifier;
begin
  Expected(ptIdentifier);
end;

procedure TmwSimplePasPar.PropertyParameterList;
//changed James Jacobson on 20001221
begin
  Expected(ptSquareOpen);
  if TokenID = ptConst then
  begin
    PropertyParameterConst;
  end;
  IdentifierList;
  Expected(ptColon);
  TypeId;
  while TokenID = ptSemiColon do
  begin
    SEMICOLON;
    if TokenID = ptConst then
    begin //jdj 12-21-2000
      PropertyParameterConst;
    end;
    IdentifierList;
    Expected(ptColon);
    TypeId;
  end;
  Expected(ptSquareClose);
end;

(*begin
  Expected(ptSquareOpen);
  if TokenID = ptConst then
  begin
    PropertyParameterConst;
  end;
  IdentifierList;
  Expected(ptColon);
  TypeId;
  while TokenID = ptSemiColon do
  begin
    SEMICOLON;
    IdentifierList;
    Expected(ptColon);
    TypeId;
  end;
  Expected(ptSquareClose);
end;*)

procedure TmwSimplePasPar.PropertyParameterConst;
begin
  Expected(ptConst);
end;

procedure TmwSimplePasPar.PropertySpecifiers;
begin
  if ExID = ptIndex then
  begin
    IndexSpecifier; // DR 2001-07-26
  end;
  while ExID in [ptRead, ptReadOnly, ptWrite, ptWriteOnly
    {$IFDEF D8_NEWER}, ptAdd, ptRemove{$ENDIF}] do
  begin
    AccessSpecifier;
  end;
  if ExID = ptDispId then
  begin
    DispIDSpecifier; // DR 2001-07-26
  end;
  while ExID in [ptDefault, ptNoDefault, ptStored] do
  begin
    StorageSpecifier;
  end;
  if ExID = ptImplements then
  begin
    NextToken;
    QualifiedIdentifierList;
  end;
  SEMICOLON;
end;

procedure TmwSimplePasPar.PropertyInterface;
begin
  if TokenID = ptSquareOpen then
  begin
    PropertyParameterList;
  end;
  Expected(ptColon);
  TypeID;
end;

procedure TmwSimplePasPar.ClassMethodHeading;
begin
  InitAhead;
  AheadParse.NextToken;
  AheadParse.FunctionProcedureName;

  if AheadParse.TokenId = ptEqual then
    ClassMethodResolution
  else
  begin
    case TokenID of
      ptConstructor:
        begin
          ConstructorHeading;
        end;
      ptDestructor:
        begin
          DestructorHeading;
        end;
      {$IFDEF D8_NEWER} //JThurman 2004-03-2003
      ptFunction, ptIdentifier:
        begin
          if (TokenID = ptIdentifier) and (Lexer.ExID <> ptOperator) then
            Expected(ptOperator);
      {$ELSE}
      ptFunction:
        begin
      {$ENDIF}
          ClassFunctionHeading;
        end;
      ptProcedure:
        begin
          ClassProcedureHeading;
        end;
    else
      SynError(InvalidClassMethodHeading);
    end;
  end;
end;

procedure TmwSimplePasPar.ClassFunctionHeading;
begin
  {$IFDEF D8_NEWER} //JThurman 2004-03-2003
  if (TokenID = ptIdentifier) and (Lexer.ExID = ptOperator) then
    Expected(ptIdentifier) else
  {$ENDIF}
  Expected(ptFunction);
  FunctionProcedureName;
  if TokenID = ptRoundOpen then
  begin
    FormalParameterList;
  end;
  Expected(ptColon);
  ReturnType;
  if TokenId = ptSemicolon then // DR 2002-01-14
    SEMICOLON;
  if ExID = ptDispId then
  begin
    DispIDSpecifier; // DR 2001-07-26
    if TokenId = ptSemicolon then // DR 2002-01-14
      SEMICOLON;
  end;
  if ExID in ClassMethodDirectiveEnum     //XM 2002-01-29
   then ClassMethodDirective; //XM 2002-01-26
end;

procedure TmwSimplePasPar.FunctionMethodName;
begin
  Expected(ptIdentifier);
end;

procedure TmwSimplePasPar.ClassProcedureHeading;
begin
  Expected(ptProcedure);
  FunctionProcedureName;
  if TokenID = ptRoundOpen then
  begin
    FormalParameterList;
  end;
  if TokenId = ptSemicolon then // DR 2002-01-14
    SEMICOLON;

  if ExID = ptDispId then
  begin
    DispIDSpecifier; // DR 2001-07-26
    if TokenId = ptSemicolon then // DR 2002-01-14
      SEMICOLON;
  end;
  if exID in ClassMethodDirectiveEnum then // XM 2002-01-29
  ClassMethodDirective;
end;

procedure TmwSimplePasPar.ProcedureMethodName;
begin
  Expected(ptIdentifier);
end;

procedure TmwSimplePasPar.ClassMethodResolution;
begin
  case TokenID of
    ptFunction:
      begin
        NextToken;
      end;
    ptProcedure:
      begin
        NextToken;
      end;
    {$IFDEF D8_NEWER} //JThurman 2004-03-2003
    ptIdentifier:
      begin
        if Lexer.ExID = ptOperator then
          NextToken;
      end;
    {$ENDIF}
  end;
  FunctionProcedureName;
{  ResolutionInterfaceName;
  Expected(ptPoint);
  Expected(ptIdentifier); }
  Expected(ptEqual);
  Expected(ptIdentifier);
  SEMICOLON;
end;

procedure TmwSimplePasPar.ResolutionInterfaceName;
begin
  Expected(ptIdentifier);
end;

procedure TmwSimplePasPar.Constraint;
begin
  while TokenId in [ptConstructor, ptRecord, ptClass, ptIdentifier] do
  begin
    case TokenId of
      ptConstructor, ptRecord, ptClass: NextToken;
      ptIdentifier: TypeId;
    end;
    if TokenId = ptComma then
      NextToken;
  end;
end;

procedure TmwSimplePasPar.ConstraintList;
begin
  Constraint;
  while TokenId = ptComma do
  begin
    Constraint;
  end;
end;

procedure TmwSimplePasPar.ConstructorHeading;
begin
  Expected(ptConstructor);
  ConstructorName;
  if TokenID = ptRoundOpen then
  begin
    FormalParameterList;
  end;
  SEMICOLON;
  ClassMethodDirective;
end;

procedure TmwSimplePasPar.ConstructorName;
begin
  Expected(ptIdentifier);
end;

procedure TmwSimplePasPar.DestructorHeading;
begin
  Expected(ptDestructor);
  DestructorName;
  if TokenID = ptRoundOpen then
  begin
    FormalParameterList;
  end;
  SEMICOLON;
  ClassMethodDirective;
end;

procedure TmwSimplePasPar.DestructorName;
begin
  Expected(ptIdentifier);
end;

procedure TmwSimplePasPar.ClassMethod;
begin
  Expected(ptClass);
end;

procedure TmwSimplePasPar.ClassMethodDirective;
begin
  while ExId in ClassMethodDirectiveEnum do
  begin
    ProceduralDirective;
    if TokenId = ptSemicolon then // DR 2002-01-14
      SEMICOLON;
  end;
end;

procedure TmwSimplePasPar.ObjectMethodHeading;
begin
  case TokenID of
    ptConstructor:
      begin
        ObjectConstructorHeading;
      end;
    ptDestructor:
      begin
        ObjectDestructorHeading;
      end;
    ptFunction:
      begin
        ObjectFunctionHeading;
      end;
    ptProcedure:
      begin
        ObjectProcedureHeading;
      end;
  else
    begin
      SynError(InvalidMethodHeading);
    end;
  end;
end;

procedure TmwSimplePasPar.ObjectFunctionHeading;
begin
  Expected(ptFunction);
  FunctionMethodName;
  if TokenID = ptRoundOpen then
  begin
    FormalParameterList;
  end;
  Expected(ptColon);
  ReturnType;
  if TokenID = ptSemiColon then  SEMICOLON;
  ObjectMethodDirective;
end;

procedure TmwSimplePasPar.ObjectProcedureHeading;
begin
  Expected(ptProcedure);
  ProcedureMethodName;
  if TokenID = ptRoundOpen then
  begin
    FormalParameterList;
  end;
  if TokenID = ptSemiColon then SEMICOLON;
  ObjectMethodDirective;
end;

procedure TmwSimplePasPar.ObjectConstructorHeading;
begin
  Expected(ptConstructor);
  ConstructorName;
  if TokenID = ptRoundOpen then
  begin
    FormalParameterList;
  end;
  if TokenID = ptSemiColon then SEMICOLON;
  ObjectMethodDirective;
end;

procedure TmwSimplePasPar.ObjectDestructorHeading;
begin
  Expected(ptDestructor);
  DestructorName;
  if TokenID = ptRoundOpen then
  begin
    FormalParameterList;
  end;
  if TokenID = ptSemiColon then SEMICOLON;
  ObjectMethodDirective;
end;

procedure TmwSimplePasPar.ObjectMethodDirective;
begin
  while ExID in [ptAbstract, ptCdecl, ptDynamic, ptExport, ptExternal, ptFar,
    ptMessage, ptNear,
    ptOverload, // DR 2001-08-07
    ptPascal, ptRegister, ptSafeCall, ptStdCall, ptVirtual,
    ptDeprecated, ptLibrary, ptPlatform // DR 2001-10-20
    {$IFDEF D8_NEWER}
    , ptStatic
    {$ENDIF}
    {$IFDEF D9_NEWER}
    , ptInline
    {$ENDIF}
    ] do
  begin
    ProceduralDirective;
    if TokenID = ptSemiColon then SEMICOLON;
  end;
end;

procedure TmwSimplePasPar.Directive16Bit;
begin
  case ExID of
    ptNear:
      begin
        NextToken;
      end;
    ptFar:
      begin
        NextToken;
      end;
    ptExport:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidDirective16Bit);
    end;
  end;
end;

procedure TmwSimplePasPar.DirectiveBinding;
begin
  case ExID of
    ptVirtual:
      begin
        NextToken;
      end;
    ptDynamic:
      begin
        NextToken;
      end;
    ptMessage:
      begin
        NextToken;
        ConstantExpression;
      end;
    ptOverride:
      begin
        NextToken;
      end;
    ptOverload:
      begin
        NextToken;
      end;
    ptReintroduce:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidDirectiveBinding);
    end;
  end;
end;

procedure TmwSimplePasPar.ReturnType;
begin
  {$IFDEF D8_NEWER}
  while TokenID = ptSquareOpen do
    CustomAttribute;
  {$ENDIF}
  case TokenID of
    ptString:
      begin
        StringType;
      end;
  else
    begin
      TypeID;
    end;
  end;
end;

procedure TmwSimplePasPar.RoundClose;
begin
  Expected(ptRoundClose);
  Dec(fInRound);
end;

procedure TmwSimplePasPar.RoundOpen;
begin
  Expected(ptRoundOpen);
  Inc(fInRound);
end;

procedure TmwSimplePasPar.FormalParameterList;
begin
  Expected(ptRoundOpen);
  FormalParameterSection;
  while TokenID = ptSemiColon do
  begin
    SEMICOLON;
    FormalParameterSection;
  end;
  Expected(ptRoundClose);
end;

procedure TmwSimplePasPar.FormalParameterSection;
begin
  {$IFDEF D8_NEWER}//JThurman 2004-03-23
  while TokenID = ptSquareOpen do
    CustomAttribute;
  {$ENDIF}
  case TokenID of
    ptConst:
      begin
        ConstParameter;
      end;
    ptIdentifier:
      case ExID of
        ptOut: OutParameter;
      else
        ParameterFormal;
      end;
    ptIn:
      begin
        InParameter;
      end;
    ptVar:
      begin
        VarParameter;
      end;
  end;
end;

procedure TmwSimplePasPar.ConstParameter;
begin
  Expected(ptConst);
  ParameterNameList;
  case TokenID of
    ptColon:
      begin
        NextToken;
        NewFormalParameterType;
        if TokenID = ptEqual then
        begin
          NextToken;
          TypedConstant;
        end;
      end
  end;
end;

procedure TmwSimplePasPar.VarParameter;
begin
  Expected(ptVar);
  ParameterNameList;
  case TokenID of
    ptColon:
      begin
        NextToken;
        NewFormalParameterType;
      end
  end;
end;

procedure TmwSimplePasPar.OutParameter;
begin
  ExpectedEx(ptOut);
  ParameterNameList;
  case TokenID of
    ptColon:
      begin
        NextToken;
        NewFormalParameterType;
      end
  end;
end;

procedure TmwSimplePasPar.ParameterFormal;
begin
  case TokenID of
    ptIdentifier:
      begin
        ParameterNameList;
        Expected(ptColon);
        NewFormalParameterType;
        if TokenID = ptEqual then
        begin
          NextToken;
          TypedConstant;
        end;
      end;
  else
    begin
      SynError(InvalidParameter);
    end;
  end;
end;

procedure TmwSimplePasPar.ParameterNameList;
begin
  ParameterName;
  while TokenID = ptComma do
  begin
    NextToken;
    ParameterName;
  end;
end;

procedure TmwSimplePasPar.ParameterName;
begin
  Expected(ptIdentifier);
end;

procedure TmwSimplePasPar.NewFormalParameterType;
begin
  case TokenID of
    ptArray:
      begin
        NextToken;
        Expected(ptOf);
        case TokenID of
          ptConst: (*new in ObjectPascal80*)
            begin
              NextToken;
            end;
        else
          begin
            OldFormalParameterType;
          end;
        end;
      end;
  else
    begin
      OldFormalParameterType;
    end;
  end;
end;

procedure TmwSimplePasPar.OldFormalParameterType;
begin
  case TokenID of
    ptString:
      begin
        NextToken;
      end;
    ptFile:
      begin
        FileType;
      end;
  else
    begin
      TypeID;
    end;
  end;
end;

procedure TmwSimplePasPar.FunctionMethodDeclaration;
begin
  {$IFDEF D8_NEWER} //JThurman 2004-03-2003
  if (TokenID = ptIdentifier) and (Lexer.ExID = ptOperator) then
    NextToken else
  {$ENDIF}
  MethodKind;
  FunctionProcedureName;
  if TokenID = ptRoundOpen then
  begin
    FormalParameterList;
  end;
  case TokenID of
    ptSemiColon:
      begin
        FunctionProcedureBlock;
      end;
  else
    begin
      Expected(ptColon);
      ReturnType;
      FunctionProcedureBlock;
    end;
  end;
end;

procedure TmwSimplePasPar.ProcedureProcedureName;
begin
  MethodKind;
  FunctionProcedureName;
  if TokenID = ptRoundOpen then
  begin
    FormalParameterList;
  end;
  FunctionProcedureBlock;
end;

procedure TmwSimplePasPar.FunctionProcedureName;
begin
  ObjectNameOfMethod;
{  if TokenId = ptLower then
    TypeParams; }
end;

procedure TmwSimplePasPar.ObjectNameOfMethod;
begin
  if TokenID = ptIn then
    Expected(ptIn)
  else
    Expected(ptIdentifier);

  {$IFDEF D8_NEWER} //JThurman 2004-03-22
  if TokenId = ptLower then
    TypeParams;
  if TokenID = ptPoint then
  begin
    Expected(ptPoint);
    ObjectNameOfMethod;
  end;
  {$ENDIF}
end;

procedure TmwSimplePasPar.FunctionProcedureBlock;
var
  NoExternal: Boolean;
begin
  NoExternal := True;
  if TokenID = ptSemiColon
    then SEMICOLON;
  case ExID of
    ptForward:
      ForwardDeclaration; // DR 2001-07-23
  else
    while ExID in [ptAbstract, ptCdecl, ptDynamic, ptExport, ptExternal, ptDelayed, ptFar,
      ptMessage, ptNear, ptOverload, ptOverride, ptPascal, ptRegister,
      ptReintroduce, ptSafeCall, ptStdCall, ptVirtual,
      ptDeprecated, ptLibrary, ptPlatform, // DR 2001-10-20
      ptLocal, ptVarargs,
      ptAssembler //JT 2004-10-29
      {$IFDEF D8_NEWER}
      , ptStatic
      {$ENDIF}
      {$IFDEF D9_NEWER}
      , ptInline
      {$ENDIF}
       ] // DR 2001-11-14
    do
      begin
        case ExId of
          ptExternal:
            begin
              ProceduralDirective;
              if TokenID = ptSemiColon then SEMICOLON;
              NoExternal := False;
            end;
        else
          begin
            ProceduralDirective;
            if TokenID = ptSemiColon then SEMICOLON;
          end;
        end;
      end;
    if ExID = ptForward then
      ForwardDeclaration // DR 2001-07-23
    else if NoExternal then
    begin
      if ExId = ptAssembler then
      begin
        NextToken;
        SEMICOLON;
      end;
      case TokenID of
        ptAsm:
          begin
            AsmStatement;
          end;
      else
        begin
          Block;
        end;
      end;
      SEMICOLON;
    end;
  end;
end;

procedure TmwSimplePasPar.ExternalDirective;
begin
  ExpectedEx(ptExternal);
  case TokenID of
    ptSemiColon:
      begin
        SEMICOLON;
      end;
  else
    begin
      SimpleExpression;
      ExternalDirectiveTwo;
    end;
  end;
end;

procedure TmwSimplePasPar.ExternalDirectiveTwo;
begin
  case fLexer.ExID of
    ptIndex:
      begin
        NextToken;
      end;
    ptName:
      begin
        NextToken;
        SimpleExpression;
      end;
    ptSemiColon:
      begin
        SEMICOLON;
        ExternalDirectiveThree;
      end;
  end
end;

procedure TmwSimplePasPar.ExternalDirectiveThree;
begin
  case TokenID of
    ptMinus:
      begin
        NextToken;
      end;
  end;
  case TokenID of
    ptIdentifier, ptIntegerConst:
      begin
        NextToken;
      end;
  end;
end;

procedure TmwSimplePasPar.ForStatement;
begin
  Expected(ptFor);
  QualifiedIdentifier;
  {$IFDEF D8_NEWER}
  if Lexer.TokenID = ptAssign then
  begin
    Expected(ptAssign);
    Expression;
    case TokenID of
      ptTo:
        begin
          NextToken;
        end;
      ptDownTo:
        begin
          NextToken;
        end;
    else
      begin
        SynError(InvalidForStatement);
      end;
    end;
    Expression;
  end else
  if Lexer.TokenID = ptIn then
  begin
    Expected(ptIn);
    //QualifiedIdentifier;
    Expression;
  end;
  {$ELSE}
  Expected(ptAssign);
  Expression;
  case TokenID of
    ptTo:
      begin
        NextToken;
      end;
    ptDownTo:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidForStatement);
    end;
  end;
  Expression;
  {$ENDIF}
  Expected(ptDo);
  Statement;
end;

procedure TmwSimplePasPar.WhileStatement;
begin
  Expected(ptWhile);
  Expression;
  Expected(ptDo);
  Statement;
end;

procedure TmwSimplePasPar.RepeatStatement;
begin
  Expected(ptRepeat);
  StatementList;
  Expected(ptUntil);
  Expression;
end;

procedure TmwSimplePasPar.CaseStatement;
begin
  Expected(ptCase);
  Expression;
  Expected(ptOf);
  CaseSelector;
  while TokenID = ptSemiColon do
  begin
    SEMICOLON;
    case TokenID of
      ptElse, ptEnd: ;
    else
      CaseSelector;
    end;
  end;
  if TokenID = ptElse then
    CaseElseStatement;
  Expected(ptEnd);
end;

procedure TmwSimplePasPar.CaseSelector;
begin
  CaseLabel;
  while TokenID = ptComma do
  begin
    NextToken;
    CaseLabel;
  end;
  Expected(ptColon);
  case TokenID of
    ptSemiColon: ;
  else
    Statement;
  end;
end;

procedure TmwSimplePasPar.CaseElseStatement;
begin
  Expected(ptElse);
  StatementList;
  SEMICOLON;
end;

procedure TmwSimplePasPar.CaseLabel;
begin
  ConstantExpression;
  if TokenID = ptDotDot then
  begin
    NextToken;
    ConstantExpression;
  end;
end;

procedure TmwSimplePasPar.IfStatement;
begin
  Expected(ptIf);
  Expression;
  ThenStatement;
  //This breaks if you have an if statement immediately preceding the else
  //clause of a case statement
{  Lexer.InitAhead;
  if (TokenID = ptSemicolon) and (Lexer.AheadTokenID = ptElse) then
  begin
    if Assigned(FOnMessage) then
    begin
      FOnMessage(Self, meError, ''';'' not allowed before ''ELSE''',
        FLexer.PosXY.X, FLexer.PosXY.Y);
    end;
  end;}
  if TokenID = ptElse then
    ElseStatement;
end;

procedure TmwSimplePasPar.ExceptBlock;
begin
  case ExID of
    ptOn:
      begin
        ExceptionHandlerList;
        ExceptionBlockElseBranch
      end;
  else
    begin
      StatementList;
    end;
  end;
end;

procedure TmwSimplePasPar.ExceptionHandlerList;
begin
  while fLexer.ExID = ptOn do
  begin
    ExceptionHandler;
    SEMICOLON;
  end;
end;

procedure TmwSimplePasPar.ExceptionHandler;
begin
  ExpectedEx(ptOn);
  ExceptionIdentifier;
  Expected(ptDo);
  Statement;
end;

procedure TmwSimplePasPar.ExceptionBlockElseBranch;
begin
  case TokenID of
    ptElse:
      begin
        NextToken;
        StatementList;
      end;
  end;
end;

procedure TmwSimplePasPar.ExceptionIdentifier;
begin
  Lexer.InitAhead;
  case Lexer.AheadTokenID of
    ptPoint:
      begin
        ExceptionClassTypeIdentifier;
      end;
  else
    begin
      ExceptionVariable;
      case Lexer.TokenID of
        ptColon:
          begin
            NextToken;
            ExceptionClassTypeIdentifier;
          end;
      end;
    end;
  end;
end;

procedure TmwSimplePasPar.ExceptionClassTypeIdentifier;
begin
  QualifiedIdentifier;
end;

procedure TmwSimplePasPar.ExceptionVariable;
begin
  Expected(ptIdentifier);
end;

procedure TmwSimplePasPar.InlineStatement;
begin
  Expected(ptInline);
  Expected(ptRoundOpen);
  Expected(ptIntegerConst);
  while (TokenID = ptSlash) do
  begin
    NextToken;
    Expected(ptIntegerConst);
  end;
  Expected(ptRoundClose);
end;

procedure TmwSimplePasPar.InParameter;
begin
  Expected(ptIn);
  ParameterNameList;
  case TokenID of
    ptColon:
      begin
        NextToken;
        NewFormalParameterType;
        if TokenID = ptEqual then
        begin
          NextToken;
          TypedConstant;
        end;
      end
  end;
end;

procedure TmwSimplePasPar.AsmStatement;
begin
  Lexer.AsmCode := True;
  Expected(ptAsm);
  { should be replaced with a Assembler lexer }
  while TokenID <> ptEnd do
    case fLexer.TokenID of
      ptBegin, ptCase, ptEnd, ptIf, ptFunction, ptProcedure, ptRepeat, ptwhile: break;
      ptAddressOp:
        begin
          NextToken;
          NextToken;
        end;
      ptDoubleAddressOp:
        begin
          NextToken;
          NextToken;
        end;
      ptNull: //JThurman 10-26-2004.  Need another way out of this.
        begin
          Expected(ptEnd);
          Exit;
        end;
    else
      NextToken;
    end;
  Lexer.AsmCode := False;
  Expected(ptEnd);
end;

procedure TmwSimplePasPar.AsOp;
begin
  Expected(ptAs);
end;

procedure TmwSimplePasPar.AssignOp;
begin
  Expected(ptAssign);
end;

procedure TmwSimplePasPar.AtExpression;
begin
  ExpectedEx(ptAt);
  Expression;
end;

procedure TmwSimplePasPar.RaiseStatement;
begin
  Expected(ptRaise);
  case TokenID of
    ptAddressOp, ptDoubleAddressOp, ptIdentifier, ptRoundOpen:
      begin
        Expression;
      end;
  end;
  if ExID = ptAt then
    AtExpression;
end;

procedure TmwSimplePasPar.TryStatement;
begin
  Expected(ptTry);
  StatementList;
  case TokenID of
    ptExcept:
      begin
        NextToken;
        ExceptBlock;
        Expected(ptEnd);
      end;
    ptFinally:
      begin
        NextToken;
        FinallyBlock;
        Expected(ptEnd);
      end;
  else
    begin
      SynError(InvalidTryStatement);
    end;
  end;
end;

procedure TmwSimplePasPar.WithStatement;
begin
  Expected(ptWith);
  VariableList;
  Expected(ptDo);
  Statement;
end;

procedure TmwSimplePasPar.VariableList;
begin
  VariableReference; (* acessing func.recordfield not allowed here;as well as UNITNAMEID *)
  while fLexer.TokenID = ptComma do
  begin
    NextToken;
    VariableReference;
  end;
end;

procedure TmwSimplePasPar.StatementList;
begin {removed ptIntegerConst jdj-Put back in for labels}
  while TokenID in [ptAddressOp, ptAsm, ptBegin, ptCase, ptDoubleAddressOp,
    ptFor, ptGoTo, ptIdentifier, ptIf, ptInherited, ptInline, ptIntegerConst,
    ptPointerSymbol, ptRaise, ptRoundOpen, ptRepeat, ptSemiColon, ptString,
    ptTry, ptWhile, ptWith] do
  begin
    Statement;
    SEMICOLON;
  end;
end;

procedure TmwSimplePasPar.SimpleStatement;
begin
  case TokenID of
    ptAddressOp, ptDoubleAddressOp, ptIdentifier, ptRoundOpen:
      begin
        Designator;
        if TokenID = ptAssign then
        begin
          AssignOp;
          Expression;
        end;
      end;
    ptGoTo:
      begin
        GotoStatement;
      end;
  end;
end;

procedure TmwSimplePasPar.Statement;
begin
  case TokenID of
    ptAsm:
      begin
        AsmStatement;
      end;
    ptBegin:
      begin
        CompoundStatement;
      end;
    ptCase:
      begin
        CaseStatement;
      end;
    ptFor:
      begin
        ForStatement;
      end;
    ptIf:
      begin
        IfStatement;
      end;
    ptIdentifier:
      begin
        fLexer.InitAhead;
        case Lexer.AheadTokenID of
          ptColon:
            begin
              LabeledStatement;
            end;
        else
          begin
            SimpleStatement;
          end;
        end;
      end;
    ptInherited:
      begin
        InheritedStatement;
      end;
    ptInLine:
      begin
        InlineStatement;
      end;
    ptIntegerConst:
      begin
        fLexer.InitAhead;
        case Lexer.AheadTokenID of
          ptColon:
            begin
              LabeledStatement;
            end;
        else
          begin
            SynError(InvalidLabeledStatement);
            NextToken;
          end;
        end;
      end;
    ptRepeat:
      begin
        RepeatStatement;
      end;
    ptRaise:
      begin
        RaiseStatement;
      end;
    ptSemiColon:
      begin
        EmptyStatement;
      end;
    ptString:
      begin
        StringStatement;
      end;
    ptTry:
      begin
        TryStatement;
      end;
    ptWhile:
      begin
        WhileStatement;
      end;
    ptWith:
      begin
        WithStatement;
      end;
  else
    begin
      SimpleStatement;
    end;
  end;
end;

procedure TmwSimplePasPar.ElseStatement;
begin
  Expected(ptElse);
  Statement;
end;

procedure TmwSimplePasPar.EmptyStatement;
begin
  { Nothing to do here.
    The semicolon will be removed in StatementList }
end;

procedure TmwSimplePasPar.InheritedStatement;
begin
  Expected(ptInherited);
  if TokenID = ptIdentifier then
    Statement;
end;

procedure TmwSimplePasPar.LabeledStatement;
begin
  case TokenID of
    ptIdentifier:
      begin
        NextToken;
        Expected(ptColon);
        Statement;
      end;
    ptIntegerConst:
      begin
        NextToken;
        Expected(ptColon);
        Statement;
      end;
  else
    begin
      SynError(InvalidLabeledStatement);
    end;
  end;
end;

procedure TmwSimplePasPar.StringStatement;
begin
  Expected(ptString);
  Statement;
end;

procedure TmwSimplePasPar.SetElement;
begin
  Expression;
  if TokenID = ptDotDot then
  begin
    NextToken;
    Expression;
  end;
end;

procedure TmwSimplePasPar.QualifiedIdentifier;
begin //mw 12/7/2000
  Identifier;

  while TokenID = ptPoint do
  begin //jdj 1/7/2001
    DotOp;
    Identifier;
  end;

(*  Expected(ptIdentifier); // old code for information removed in next versions
  case TokenID of
    ptPoint:
      begin
        NextToken;
        Expected(ptIdentifier);
        if (TokenID = ptSquareOpen) then
        begin
          ConstantExpression;
        end;
      end;
    ptSquareOpen: {MW 20001207}
      begin
        ConstantExpression;
      end;
  end;*)

end;

procedure TmwSimplePasPar.SetConstructor;
begin
  Expected(ptSquareOpen);
  if TokenID <> ptSquareClose then
  begin
    SetElement;
    while TokenID = ptComma do
    begin
      NextToken;
      SetElement;
    end;
  end;
  Expected(ptSquareClose);
end;

procedure TmwSimplePasPar.Number;
begin
  case TokenID of
    ptFloat:
      begin
        NextToken;
      end;
    ptIntegerConst:
      begin
        NextToken;
      end;
    ptIdentifier:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidNumber);
    end;
  end;
end;

procedure TmwSimplePasPar.ExpressionList;
begin
  Expression;
  if TokenID = ptAssign then //JT Nov 26, 2004 - supporting ole automation syntax
    begin
      Expected(ptAssign);
      Expression;
    end;
  while TokenID = ptComma do
  begin
    NextToken;
    Expression;
    if TokenID = ptAssign then //JT Nov 26, 2004 - supporting ole automation syntax
    begin
      Expected(ptAssign);
      Expression;
    end;
  end;
end;

procedure TmwSimplePasPar.Designator;
begin
  VariableReference;
end;

procedure TmwSimplePasPar.MultiplicativeOperator;
begin
  case TokenID of
    ptAnd:
      begin
        NextToken;
      end;
    ptDiv:
      begin
        NextToken;
      end;
    ptMod:
      begin
        NextToken;
      end;
    ptShl:
      begin
        NextToken;
      end;
    ptShr:
      begin
        NextToken;
      end;
    ptSlash:
      begin
        NextToken;
      end;
    ptStar:
      begin
        NextToken;
      end;
  else
    begin SynError(InvalidMultiplicativeOperator);
    end;
  end;
end;

procedure TmwSimplePasPar.Factor;
begin
  case TokenID of
    ptAsciiChar, ptStringConst:
      begin
        CharString;
      end;
    ptAddressOp, ptDoubleAddressOp, ptIdentifier, ptInherited, ptPointerSymbol:
      begin
        Designator;
      end;
    ptRoundOpen:
      begin
        RoundOpen;
        ExpressionList;
        RoundClose;
      end;
    ptIntegerConst, ptFloat:
      begin
        Number;
      end;
    ptNil:
      begin
        NilToken;
      end;
    ptMinus:
      begin
        UnaryMinus;
        Factor;
      end;
    ptNot:
      begin
        NotOp;
        Factor;
      end;
    ptPlus:
      begin
        NextToken;
        Factor;
      end;
    ptSquareOpen:
      begin
        SetConstructor;
      end;
    ptString:
      begin
        TypeSimple;
      end;
    ptFunction, ptProcedure:
      AnonymousMethod;
  end;

  if TokenID in [ptRoundOpen, ptSquareOpen, ptPointerSymbol] then
    Factor;

  while TokenID = ptPoint do
  begin
    DotOp;
    Factor;
  end;
end;

procedure TmwSimplePasPar.AdditiveOperator;
begin
  if TokenID in [ptMinus, ptOr, ptPlus, ptXor] then
  begin
    NextToken; // DR 2001-12-19
 {
   case TokenID of
  ptMinus, ptPlus:
    begin
   while TokenID in [ptMinus, ptPlus] do
     case TokenID of
    ptMinus:
      begin
     NextToken;
      end;
    ptPlus:
      begin
     NextToken;
      end;
     end;
    end;
  ptOr:
    begin
   NextToken;
    end;
  ptXor:
    begin
   NextToken;
    end;
   end;}
  end
  else
  begin
    SynError(InvalidAdditiveOperator);
  end;
end;

procedure TmwSimplePasPar.AddressOp;
begin
  Expected(ptAddressOp);
end;

procedure TmwSimplePasPar.Term;
begin
  Factor;
  while TokenID in [ptAnd, ptDiv, ptMod, ptShl, ptShr, ptSlash, ptStar] do
  begin
    MultiplicativeOperator;
    Factor;
  end;
end;

procedure TmwSimplePasPar.RelativeOperator;
begin
  case TokenID of
    ptAs:
      begin
        NextToken;
      end;
    ptEqual:
      begin
        NextToken;
      end;
    ptGreater:
      begin
        NextToken;
      end;
    ptGreaterEqual:
      begin
        NextToken;
      end;
    ptIn:
      begin
        NextToken;
      end;
    ptIs:
      begin
        NextToken;
      end;
    ptLower:
      begin
        NextToken;
      end;
    ptLowerEqual:
      begin
        NextToken;
      end;
    ptNotEqual:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidRelativeOperator);
    end;
  end;
end;

procedure TmwSimplePasPar.SimpleExpression;
begin
(*  while TokenID in [ptMinus, ptPlus] do
 begin
   NextToken;								// DR 2001-12-19
 end;
*)
  Term;
  while TokenID in [ptMinus, ptOr, ptPlus, ptXor] do
  begin
    AdditiveOperator;
    Term;
  end;

  case TokenID of
    ptAs:
      begin
        AsOp;
        TypeId;
      end;
  end;
end;

procedure TmwSimplePasPar.Expression;
begin
  SimpleExpression;

  //JT 2006-07-17 The Delphi language guide has this as
  //Expression -> SimpleExpression [RelOp SimpleExpression]...
  //So this needs to be able to repeat itself.
  case TokenID of
  ptEqual, ptGreater, ptGreaterEqual, ptLower, ptLowerEqual, ptIn, ptIs,
    ptNotEqual:
    begin
      while TokenID in [ptEqual, ptGreater, ptGreaterEqual, ptLower, ptLowerEqual,
        ptIn, ptIs, ptNotEqual{, ptColon}] do
      begin
        RelativeOperator;
        SimpleExpression;
      end;
    end;
  ptColon:
    begin
      case InRound of
        False: ;
        True:
          while TokenID = ptColon do
          begin
            NextToken;
            SimpleExpression;
          end;
      end;
    end;
  end;
end;

procedure TmwSimplePasPar.VarDeclaration;
begin
  // !! Changed back to var name list from IdentifierList
  VarNameList;
  Expected(ptColon);
  TypeKind;
  while ExID in [ptDeprecated, ptLibrary, ptPlatform] do // DR 2001-10-20
    case ExID of
      ptDeprecated: DirectiveDeprecated;
      ptLibrary: DirectiveLibrary;
      ptPlatform: DirectivePlatform;
    end;
  case GenID of
    ptAbsolute:
      begin
        VarAbsolute;
      end;
    ptEqual:
      begin
        VarEqual;
      end;
  end;
  while ExID in [ptDeprecated, ptLibrary, ptPlatform] do // DR 2001-10-20
    case ExID of
      ptDeprecated: DirectiveDeprecated;
      ptLibrary: DirectiveLibrary;
      ptPlatform: DirectivePlatform;
    end;
end;

procedure TmwSimplePasPar.VarAbsolute;
begin
  ExpectedEx(ptAbsolute);
  ConstantValue;
end;

procedure TmwSimplePasPar.VarEqual;
begin
  Expected(ptEqual);
  ConstantValueTyped;
end;

procedure TmwSimplePasPar.VarNameList;
begin
  VarName;
  while TokenID = ptComma do
    begin
      NextToken;
      VarName;
    end;
end;

procedure TmwSimplePasPar.VarName;
begin
  Expected(ptIdentifier);
end;

procedure TmwSimplePasPar.DirectiveCalling;
begin
  case ExID of
    ptCdecl:
      begin
        NextToken;
      end;
    ptPascal:
      begin
        NextToken;
      end;
    ptRegister:
      begin
        NextToken;
      end;
    ptSafeCall:
      begin
        NextToken;
      end;
    ptStdCall:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidDirectiveCalling);
    end;
  end;
end;

procedure TmwSimplePasPar.RecordVariant;
begin
  ConstantExpression;
  while (TokenID = ptComma) do
  begin
    NextToken;
    ConstantExpression;
  end;
  Expected(ptColon);
  Expected(ptRoundOpen);
  if TokenID <> ptRoundClose then
  begin
    FieldList;
  end;
  Expected(ptRoundClose);
end;

procedure TmwSimplePasPar.VariantSection;
begin
  Expected(ptCase);
  TagField;
  Expected(ptOf);
  RecordVariant;
  while TokenID = ptSemiColon do
  begin
    SEMICOLON;
    case TokenID of //DR 2001-12-11
      ptEnd, ptRoundClose: Break;
    else
      RecordVariant;
    end;
  end;
end;

procedure TmwSimplePasPar.TagField;
begin
  TagFieldName;
  case fLexer.TokenID of
    ptColon:
      begin
        NextToken;
        TagFieldTypeName;
      end;
  end;
end;

procedure TmwSimplePasPar.TagFieldName;
begin
  Expected(ptIdentifier);
end;

procedure TmwSimplePasPar.TagFieldTypeName;
begin
  OrdinalType;
end;

procedure TmwSimplePasPar.FieldDeclaration;
begin
  //IdentifierList;
  FieldNameList;
  Expected(ptColon);
  TypeKind;
  while ExID in [ptDeprecated, ptLibrary, ptPlatform] do // DR 2002-01-09
    case ExID of
      ptDeprecated: DirectiveDeprecated;
      ptLibrary: DirectiveLibrary;
      ptPlatform: DirectivePlatform;
    end;
end;

procedure TmwSimplePasPar.FieldList;
begin
  while TokenID = ptIdentifier do
  begin
    FieldDeclaration;
    SEMICOLON;
  end;
  if TokenID = ptCase then
  begin
    VariantSection;
  end;
end;

procedure TmwSimplePasPar.FieldName;
begin
  Expected(ptIdentifier);
end;

procedure TmwSimplePasPar.FieldNameList;
begin
  FieldName;
  while TokenID = ptComma do
  begin
    NextToken;
    FieldName;
  end;
end;

procedure TmwSimplePasPar.RecordType;
begin
  Expected(ptRecord);
  if TokenID = ptSemicolon then
    Exit;

  if ExID = ptHelper then
  begin
    ExpectedEx(ptHelper);
    Expected(ptFor);
    TypeId;
  end;

  {$IFDEF D8_NEWER}
  if TokenID = ptRoundOpen then
  begin
    ClassHeritage;
    if TokenID = ptSemicolon then
      Exit;
  end;
  ClassMemberList;
  {$ELSE}
  FieldList;
  {$ENDIF}
  Expected(ptEnd);
end;

procedure TmwSimplePasPar.FileType;
begin
  Expected(ptFile);
  if TokenID = ptOf then
  begin
    NextToken;
    TypeId;
  end;
end;

procedure TmwSimplePasPar.FinallyBlock;
begin
  StatementList;
end;

procedure TmwSimplePasPar.SetType;
begin
  Expected(ptSet);
  Expected(ptOf);
  OrdinalType;
end;

procedure TmwSimplePasPar.ArrayType;
begin
  Expected(ptArray);
  if TokenID = ptSquareOpen then
  begin
    NextToken;
    OrdinalType;
    while TokenID = ptComma do
    begin
      NextToken;
      OrdinalType;
    end;
    Expected(ptSquareClose);
  end;
  Expected(ptOf);
  TypeKind;
end;

procedure TmwSimplePasPar.EnumeratedType;
begin
  Expected(ptRoundOpen);
  EnumeratedTypeItem;
  while TokenID = ptComma do
  begin
    NextToken;
    EnumeratedTypeItem;
  end;
  Expected(ptRoundClose);
end;

procedure TmwSimplePasPar.SubrangeType;
begin
  ConstantExpression;
  if TokenID = ptDotDot then
  begin
    NextToken;
    ConstantExpression;
  end;
end;

procedure TmwSimplePasPar.RealIdentifier;
begin
  case ExID of
    ptReal48:
      begin
        NextToken;
      end;
    ptReal:
      begin
        NextToken;
      end;
    ptSingle:
      begin
        NextToken;
      end;
    ptDouble:
      begin
        NextToken;
      end;
    ptExtended:
      begin
        NextToken;
      end;
    ptCurrency:
      begin
        NextToken;
      end;
    ptComp:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidRealIdentifier);
    end;
  end;
end;

procedure TmwSimplePasPar.RealType;
begin
  case TokenID of
    ptMinus:
      begin
        NextToken;
      end;
    ptPlus:
      begin
        NextToken;
      end;
  end;
  case TokenId of
    ptFloat:
      begin
        NextToken;
      end;
  else
    begin
      VariableReference;
    end;
  end;
end;

procedure TmwSimplePasPar.OrdinalIdentifier;
begin
  case ExID of
    ptBoolean:
      begin
        NextToken;
      end;
    ptByte:
      begin
        NextToken;
      end;
    ptBytebool:
      begin
        NextToken;
      end;
    ptCardinal:
      begin
        NextToken;
      end;
    ptChar:
      begin
        NextToken;
      end;
    ptDWord:
      begin
        NextToken;
      end;
    ptInt64:
      begin
        NextToken;
      end;
    ptInteger:
      begin
        NextToken;
      end;
    ptLongBool:
      begin
        NextToken;
      end;
    ptLongInt:
      begin
        NextToken;
      end;
    ptLongWord:
      begin
        NextToken;
      end;
    ptPChar:
      begin
        NextToken;
      end;
    ptShortInt:
      begin
        NextToken;
      end;
    ptSmallInt:
      begin
        NextToken;
      end;
    ptWideChar:
      begin
        NextToken;
      end;
    ptWord:
      begin
        NextToken;
      end;
    ptWordbool:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidOrdinalIdentifier);
    end;
  end;
end;

procedure TmwSimplePasPar.OrdinalType;
begin
  case TokenID of
    ptIdentifier:
      begin
        Lexer.InitAhead;
        case Lexer.AheadTokenID of
          ptPoint:
            begin
              Expression;
            end;
          ptRoundOpen:
            begin //jdj
              ConstantExpression;
            end;
        else
          begin
            TypeID;
          end;
        end;
      end;
    ptRoundOpen:
      begin
        EnumeratedType;
      end;
    ptSquareOpen:
      begin
        NextToken;
        SubrangeType;
        Expected(ptSquareClose);
      end;
  else
    begin
      Expression;
    end;
  end;
  if TokenID = ptDotDot then
  begin
    NextToken;
    ConstantExpression;
  end;
end;

procedure TmwSimplePasPar.VariableReference;
begin
  case TokenID of
    ptRoundOpen:
    begin
      RoundOpen;
      Expression;
      RoundClose;
      VariableTail;
    end;
    ptSquareOpen:
    begin
      SetConstructor;
    end;
    ptAddressOp:
      begin
        AddressOp;
        VariableReference;
      end;
    ptDoubleAddressOp:
      begin
        NextToken;
        VariableReference;
      end;
    ptInherited:
      begin
        InheritedVariableReference;
      end;
{    ptPointerSymbol:
      begin
        NextToken;
        case TokenID of
          ptRoundClose, ptSquareClose: ;
        else
          begin
            variable;
          end;
        end;
      end;   }
  else
    variable;
  end;
end;

procedure TmwSimplePasPar.Variable; (* Attention: could also came from proc_call ! ! *)
begin
  QualifiedIdentifier;
  VariableTail;
end;

procedure TmwSimplePasPar.VariableTail;
begin
  case TokenID of
    ptRoundOpen:
      begin
        RoundOpen;
        ExpressionList;
        RoundClose;
      end;
    ptSquareOpen:
      begin
        IndexOp;
      end;
    ptPointerSymbol:
      begin
        PointerSymbol;
      end;
    {$IFDEF D11_NEWER}
    ptLower:
      begin
        InitAhead;
        AheadParse.NextToken;
        AheadParse.TypeArgs;

        if AheadParse.TokenId = ptGreater then
        begin
          NextToken;
          TypeArgs;
          Expected(ptGreater);
          case TokenID of
          ptAddressOp, ptDoubleAddressOp, ptIdentifier:
            begin
              VariableReference;
            end;
          ptPoint, ptPointerSymbol, ptRoundOpen, ptSquareOpen:
            begin
              VariableTail;
            end;
          end;
        end;
      end;
    {$ENDIF}
  end;

  case TokenID of
    ptRoundOpen, ptSquareOpen, ptPointerSymbol:
    begin
      VariableTail;
    end;
    ptPoint:
    begin
      DotOp;
      Variable;
    end;
    ptAs:
    begin
      AsOp;
      SimpleExpression;
    end;
  end;
end;

(*procedure TmwSimplePasPar.VariableTwo;
begin
  case TokenID of
    ptPoint:
      begin
        Designator;
{        NextToken;
        case TokenID of
          ptAddressOp, ptDoubleAddressOp, ptIdentifier:
            begin
              VariableReference;
            end;
          ptPointerSymbol, ptRoundOpen, ptSquareOpen:
            begin
              VariableTwo;
            end;
        end; }
      end;
    ptPointerSymbol:
      begin
        NextToken;
        case TokenID of
          ptAddressOp, ptDoubleAddressOp, ptIdentifier:
            begin
              VariableReference;
            end;
          ptPoint, ptPointerSymbol, ptRoundOpen, ptSquareOpen:
            begin
              VariableTwo;
            end;
        end;
      end;
    ptRoundOpen:
      begin
        RoundOpen;
        ExpressionList;
        RoundClose;
{        case TokenID of
          ptRoundClose:
            begin
              // NextToken;
              //Expected(ptRoundClose);
              RoundClose;
            end;
        else
          begin
            case TokenID of
              ptAddressOp, ptDoubleAddressOp:
                begin
                  VariableReference;
                end;
              ptPoint, ptPointerSymbol, ptRoundOpen:
                begin
                  VariableTwo;
                end;
              ptSquareOpen:
                begin
                  SetConstructor;
                end;
              else
                ExpressionList;
            end;
            //RY: fInRound := True;
            //ExpressionList;
            //RY: fInRound := True;
            RoundClose;
          end;
        end; }
        case TokenID of
          ptAddressOp, ptDoubleAddressOp:
            begin
              VariableReference;
            end;
          ptPointerSymbol:
            begin
              PointerSymbol;
            end;
          ptPoint, ptRoundOpen:
            begin
              VariableTwo;
            end;
          ptSquareOpen:
            begin
              IndexOp;
            end;
        end;
      end;
    ptSquareOpen:
      begin
        Lexer.InitAhead;
        while Lexer.AheadTokenID <> ptSemiColon do
        begin
          case Lexer.AheadTokenID of
            ptBegin, ptClass, ptConst, ptEnd, ptDotDot, ptIn, ptNull, ptThreadVar, ptType,
              ptVar: break;
          else
            Lexer.AheadNext;
          end;
        end;
        case Lexer.AheadTokenID of
          ptDotDot:
            begin
              SubrangeType;
            end;
        else
          begin
            NextToken;
            case TokenID of
              ptSquareClose:
                begin
                  NextToken;
                end;
            else
              begin
                case TokenID of
                  ptAddressOp, ptDoubleAddressOp:
                    begin
                      VariableReference;
                    end;
                  ptPoint, ptPointerSymbol, ptRoundOpen, ptSquareOpen:
                    begin
                      VariableTwo;
                    end;
                end;
                ExpressionList;
                Expected(ptSquareClose);
              end;
            end;
            case TokenID of
              ptAddressOp, ptDoubleAddressOp:
                begin
                  VariableReference;
                end;
              ptPoint, ptPointerSymbol, ptRoundOpen, ptSquareOpen:
                begin
                  VariableTwo;
                end;
            end;

          end;
        end;
      end;
    {$IFDEF D11_NEWER}
    ptLower:
      begin
        InitAhead;
        AheadParse.NextToken;
        AheadParse.TypeKind;

        if AheadParse.TokenId = ptGreater then
        begin
          NextToken;
          TypeKind;
          Expected(ptGreater);
          case TokenID of
          ptAddressOp, ptDoubleAddressOp, ptIdentifier:
            begin
              VariableReference;
            end;
          ptPoint, ptPointerSymbol, ptRoundOpen, ptSquareOpen:
            begin
              VariableTwo;
            end;
          end;
        end;
      end;
    {$ENDIF}
  end;
end; *)

procedure TmwSimplePasPar.InterfaceType;
begin
  case TokenID of
    ptInterface:
      begin
        NextToken;
      end;
    ptDispInterface:
      begin
        NextToken;
      end
  else
    begin
      SynError(InvalidInterfaceType);
    end;
  end;
  case TokenID of
    ptEnd:
      begin
        NextToken; { Direct descendant without new members }
      end;
    ptRoundOpen:
      begin
        InterfaceHeritage;
        case TokenID of
          ptEnd:
            begin
              NextToken; { No new members }
            end;
          ptSemiColon: ; { No new members }
        else
          begin
            if TokenID = ptSquareOpen then
            begin
              InterfaceGUID;
            end;
            InterfaceMemberList;
            Expected(ptEnd);
          end;
        end;
      end;
  else
    begin
      if TokenID = ptSquareOpen then
      begin
        InterfaceGUID;
      end;
      InterfaceMemberList; { Direct descendant }
      Expected(ptEnd);
    end;
  end;
end;

procedure TmwSimplePasPar.InterfaceMemberList;
begin
  while TokenID in [ptFunction, ptProcedure, ptProperty] do
  begin
    ClassMethodOrProperty;
  end;
end;

procedure TmwSimplePasPar.ClassType;
begin
  Expected(ptClass);
  {$IFDEF D8_NEWER} //JThurman 2004-03-19
  case TokenID of
    ptIdentifier: //NASTY hack because Abstract is generally an ExID, except in this case when it should be a keyword.
      begin
        if Lexer.ExID = ptAbstract then
          Expected(ptIdentifier);

        if Lexer.ExID = ptHelper then
        begin
          ExpectedEx(ptHelper);
          Expected(ptFor);
          TypeId;
        end;
      end;
    ptSealed:
      Expected(ptSealed);
  end;
  {$ENDIF}
  case TokenID of
    ptEnd:
      begin
        ClassTypeEnd; // DR 2001-07-31
        NextToken; { Direct descendant of TObject without new members }
      end;
    ptRoundOpen:
      begin
        ClassHeritage;
        case TokenID of
          ptEnd:
            begin
              Expected(ptEnd); // DR 2001-07-31
              ClassTypeEnd; // DR 2001-07-31
            end;
          ptSemiColon: ClassTypeEnd; // DR 2001-07-31
        else
          begin
            ClassMemberList; { Direct descendant of TObject }
            Expected(ptEnd); // DR 2001-07-31
            ClassTypeEnd; // DR 2001-07-31
          end;
        end;
      end;
  else
    begin
      ClassMemberList; { Direct descendant of TObject }
      Expected(ptEnd); // DR 2001-07-31
      ClassTypeEnd; // DR 2001-07-31
    end;
  end;
end;

procedure TmwSimplePasPar.ClassHeritage;
begin
  Expected(ptRoundOpen);
  AncestorIdList;
  Expected(ptRoundClose);
end;

procedure TmwSimplePasPar.ClassVisibility;
begin
  {$IFDEF D8_NEWER} //JThurman 2004-03-03
  if TokenID = ptStrict then
    Expected(ptStrict);
  {$ENDIF}
  while ExID in [ptAutomated, ptPrivate, ptProtected, ptPublic, ptPublished] do
  begin
    Lexer.InitAhead;
    case Lexer.AheadExID of
      ptColon, ptComma: ;
    else
      case ExID of
        ptAutomated:
          begin
            VisibilityAutomated;
          end;
        ptPrivate:
          begin
            VisibilityPrivate;
          end;
        ptProtected:
          begin
            VisibilityProtected;
          end;
        ptPublic:
          begin
            VisibilityPublic;
          end;
        ptPublished:
          begin
            VisibilityPublished;
          end;
      end;
    end;
  end;
end;

procedure TmwSimplePasPar.VisibilityAutomated;
begin
  ExpectedEx(ptAutomated);
end;

procedure TmwSimplePasPar.VisibilityPrivate;
begin
  ExpectedEx(ptPrivate);
end;

procedure TmwSimplePasPar.VisibilityProtected;
begin
  ExpectedEx(ptProtected);
end;

procedure TmwSimplePasPar.VisibilityPublic;
begin
  ExpectedEx(ptPublic);
end;

procedure TmwSimplePasPar.VisibilityPublished;
begin
  ExpectedEx(ptPublished);
end;

procedure TmwSimplePasPar.VisibilityUnknown;
begin
  //
end;

procedure TmwSimplePasPar.ClassMemberList;
begin
  while TokenID in [ptClass, ptConstructor, ptDestructor, ptFunction,
    ptIdentifier, ptProcedure, ptProperty
    {$IFDEF D8_NEWER}, ptType, ptSquareOpen, ptVar, ptConst, ptStrict,
     ptCase{$ENDIF}] do
  begin
    ClassVisibility;

    {$IFDEF D8_NEWER}
    if TokenID = ptSquareOpen then
      CustomAttribute;
    {$ENDIF}

    if (TokenID = ptIdentifier) and
      not (ExID in [ptPrivate, ptProtected, ptPublished, ptPublic]) then
    begin
      ClassField;
      SEMICOLON;
    end
    else if TokenID in [ptClass, ptConstructor, ptDestructor, ptFunction,
      ptProcedure, ptProperty{$IFDEF D8_NEWER}, ptVar, ptConst{$ENDIF}] then
    begin
      ClassMethodOrProperty;
    end;
    {$IFDEF D8_NEWER}//JThurman 2004-03-22
    {Nested types for D8}
    if TokenID = ptType then
      TypeSection;
    if TokenID = ptCase then
    begin
      VariantSection;
    end;
    {$ENDIF}
  end;
end;

procedure TmwSimplePasPar.ClassMethodOrProperty;
begin
  if TokenID = ptClass
    then ClassClass; //DR 2001-07-16
  case TokenID of
    ptProperty:
      begin
        ClassProperty;
      end;
    {$IFDEF D8_NEWER}
    ptVar:
      begin
        NextToken;
        while (TokenID = ptIdentifier) and (ExID = ptUnknown) do
        begin
          ClassField;
          SemiColon;
        end;
      end;
    ptConst:
      begin
        NextToken;
        while (TokenID = ptIdentifier) and (ExID = ptUnknown) do
        begin
          ConstantDeclaration;
          SemiColon;
        end;
      end;
    {$ENDIF}
  else
    begin
      ClassMethodHeading;
    end;
  end;
end;

procedure TmwSimplePasPar.ClassProperty;
begin
 // DR 2001-07-19 -> changed. for array-property override failure
  Expected(ptProperty);
  PropertyName;
  case TokenID of
    ptColon, ptSquareOpen:
      begin
        PropertyInterface;
      end;
  end;
  PropertySpecifiers;
  case ExID of
    ptDefault:
      begin
        PropertyDefault; //DR 2001-07-16
        SEMICOLON;
      end;
  end;
end;

procedure TmwSimplePasPar.PropertyName;
begin
  Expected(ptIdentifier);
end;

procedure TmwSimplePasPar.ClassField;
begin
  {$IFDEF D8_NEWER}
  if TokenID = ptSquareOpen then
    CustomAttribute;
  {$ENDIF}
  //IdentifierList;
  FieldNameList;
  Expected(ptColon);
  TypeKind;
  while ExID in [ptDeprecated, ptLibrary, ptPlatform] do // DR 2001-10-20
    case ExID of
      ptDeprecated: DirectiveDeprecated;
      ptLibrary: DirectiveLibrary;
      ptPlatform: DirectivePlatform;
    end;
end;

procedure TmwSimplePasPar.ObjectType;
begin
  Expected(ptObject);
  case TokenID of
    ptEnd:
      begin
        ObjectTypeEnd; // DR 2001-07-31
        NextToken; { Direct descendant without new members }
      end;
    ptRoundOpen:
      begin
        ObjectHeritage;
        case TokenID of
          ptEnd:
            begin
              Expected(ptEnd); // DR 2001-07-31
              ObjectTypeEnd; // DR 2001-07-31
            end;
          ptSemiColon: ObjectTypeEnd; // DR 2001-07-31
        else
          begin
            ObjectMemberList; { Direct descendant }
            Expected(ptEnd); // DR 2001-07-31
            ObjectTypeEnd; // DR 2001-07-31
          end;
        end;
      end;
  else
    begin
      ObjectMemberList; { Direct descendant }
      Expected(ptEnd); // DR 2001-07-31
      ObjectTypeEnd; // DR 2001-07-31
    end;
  end;
end;

procedure TmwSimplePasPar.ObjectHeritage;
begin
  Expected(ptRoundOpen);
  AncestorIdList;
  Expected(ptRoundClose);
end;

procedure TmwSimplePasPar.ObjectMemberList;
begin {jdj added ptProperty-call to ObjectProperty 02/07/2001}
  ObjectVisibility;
  while TokenID in [ptConstructor, ptDestructor, ptFunction, ptIdentifier,
    ptProcedure, ptProperty] do
  begin
    while TokenID = ptIdentifier do
    begin
      ObjectField;
      SEMICOLON;
      ObjectVisibility;
    end;
    while TokenID in [ptConstructor, ptDestructor, ptFunction, ptProcedure, ptProperty] do
    begin
      case TokenID of
        ptConstructor, ptDestructor, ptFunction, ptProcedure:
          ObjectMethodHeading;
        ptProperty:
          ObjectProperty;
      end;
    end;
    ObjectVisibility;
  end;
end;

procedure TmwSimplePasPar.ObjectVisibility;
begin
  while ExID in [ptPrivate, ptProtected, ptPublic] do
  begin
    Lexer.InitAhead;
    case Lexer.AheadExID of
      ptColon, ptComma: ;
    else
      case ExID of
        ptPrivate:
          begin
            VisibilityPrivate;
          end;
        ptProtected:
          begin
            VisibilityProtected;
          end;
        ptPublic:
          begin
            VisibilityPublic;
          end;
      end;
    end;
  end;
end;

procedure TmwSimplePasPar.ObjectField;
begin
  IdentifierList;
  Expected(ptColon);
  TypeKind;
  while ExID in [ptDeprecated, ptLibrary, ptPlatform] do // DR 2001-10-20
    case ExID of
      ptDeprecated: DirectiveDeprecated;
      ptLibrary: DirectiveLibrary;
      ptPlatform: DirectivePlatform;
    end;
end;

procedure TmwSimplePasPar.ClassReferenceType;
begin
  Expected(ptClass);
  Expected(ptOf);
  TypeId;
end;

procedure TmwSimplePasPar.VariantIdentifier;
begin
  case ExID of
    ptOleVariant:
      begin
        NextToken;
      end;
    ptVariant:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidVariantIdentifier);
    end;
  end;
end;

procedure TmwSimplePasPar.ProceduralType;
var
  TheTokenID: TptTokenKind;
begin
  case TokenID of
    ptFunction:
      begin
        NextToken;
        if TokenID = ptRoundOpen then
        begin
          FormalParameterList;
        end;
        Expected(ptColon);
        ReturnType;
      end;
    ptProcedure:
      begin
        NextToken;
        if TokenID = ptRoundOpen then
        begin
          FormalParameterList;
        end;
      end;
  else
    begin
      SynError(InvalidProceduralType);
    end;
  end;
  if TokenID = ptOf then
  begin
    NextToken;
    Expected(ptObject);
  end;
  Lexer.InitAhead;
  case TokenID of
    ptSemiColon: TheTokenID := Lexer.AheadExID;
  else
    TheTokenID := ExID;
  end;
  while TheTokenID in [ptAbstract, ptCdecl, ptDynamic, ptExport, ptExternal, ptFar,
    ptMessage, ptNear, ptOverload, ptOverride, ptPascal, ptRegister,
    ptReintroduce, ptSafeCall, ptStdCall, ptVirtual
    {$IFDEF D8_NEWER}, ptStatic{$ENDIF}{$IFDEF D9_NEWER}, ptInline{$ENDIF}
    ] do
 // DR 2001-11-14 no checking for deprecated etc. since it's captured by the typedecl
  begin
    if TokenID = ptSemiColon then SEMICOLON;
    ProceduralDirective;
    Lexer.InitAhead;
    case TokenID of
      ptSemiColon: TheTokenID := Lexer.AheadExID;
    else
      TheTokenID := ExID;
    end;
  end;
end;

procedure TmwSimplePasPar.StringConst;
begin
  StringConstSimple;
  while TokenID in [ptStringConst, ptAsciiChar] do
    StringConstSimple;
end;

procedure TmwSimplePasPar.StringConstSimple;
begin
  NextToken;
end;

procedure TmwSimplePasPar.StringIdentifier;
begin
  case ExID of
    ptAnsiString:
      begin
        NextToken;
      end;
    ptShortString:
      begin
        NextToken;
      end;
    ptWideString:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidStringIdentifier);
    end;
  end;
end;

procedure TmwSimplePasPar.StringType;
begin
  case TokenID of
    ptString:
      begin
        NextToken;
        if TokenID = ptSquareOpen then
        begin
          NextToken;
          ConstantExpression;
          Expected(ptSquareClose);
        end;
      end;
  else
    begin
      VariableReference;
    end;
  end;
end;

procedure TmwSimplePasPar.PointerSymbol;
begin
  Expected(ptPointerSymbol);
end;

procedure TmwSimplePasPar.PointerType;
begin
  Expected(ptPointerSymbol);
  TypeId;
end;

procedure TmwSimplePasPar.StructuredType;
begin
  if TokenID = ptPacked then
  begin
    NextToken;
  end;
  case TokenID of
    ptArray:
      begin
        ArrayType;
      end;
    ptFile:
      begin
        FileType;
      end;
    ptRecord:
      begin
        RecordType;
      end;
    ptSet:
      begin
        SetType;
      end;
    ptObject:
      begin
        ObjectType;
      end
  else
    begin
      SynError(InvalidStructuredType);
    end;
  end;
end;

procedure TmwSimplePasPar.SimpleType;
begin
  case TokenID of
    ptMinus:
      begin
        NextToken;
      end;
    ptPlus:
      begin
        NextToken;
      end;
  end;
  case fLexer.TokenID of
    ptAsciiChar, ptIntegerConst:
      begin
        OrdinalType;
      end;
    ptFloat:
      begin
        RealType;
      end;
    ptIdentifier:
      begin
        fLexer.InitAhead;
        case Lexer.AheadTokenID of
          ptPoint, ptSemiColon:
            begin
              TypeID;
            end;
        else
          begin
            SimpleExpression;
            if fLexer.TokenID = ptDotDot then
            begin
              NextToken;
              SimpleExpression;
            end;
          end;
        end;
      end;
    ptRoundOpen:
      begin
        EnumeratedType;
      end;
    ptSquareOpen:
      begin
        SubrangeType;
      end;
  else
    begin
      VariableReference;
    end;
  end;
end;

procedure TmwSimplePasPar.RecordFieldConstant;
begin
  Expected(ptIdentifier);
  Expected(ptColon);
  TypedConstant;
end;

procedure TmwSimplePasPar.RecordConstant;
begin
  Expected(ptRoundOpen);
  RecordFieldConstant;
  while (TokenID = ptSemiColon) do
  begin
    SEMICOLON;
    if TokenId <> ptRoundClose then //jdj 2.23.2001
      RecordFieldConstant;
  end;
  Expected(ptRoundClose);
end;

procedure TmwSimplePasPar.ArrayConstant;
begin
  Expected(ptRoundOpen);
  TypedConstant;
  while (TokenID = ptComma) do
  begin
    NextToken;
    TypedConstant;
  end;
  Expected(ptRoundClose);
end;

procedure TmwSimplePasPar.ClassForward;
begin
  Expected(ptClass);
end;

procedure TmwSimplePasPar.DispInterfaceForward;
begin
  Expected(ptDispInterface);
end;

procedure TmwSimplePasPar.DotOp;
begin
  Expected(ptPoint);
end;

procedure TmwSimplePasPar.InterfaceForward;
begin
  Expected(ptInterface);
end;

procedure TmwSimplePasPar.ObjectForward;
begin
  Expected(ptObject);
end;

procedure TmwSimplePasPar.TypeDeclaration;
begin
  TypeName;
  //For generics
//  if TokenId = ptLower then
//    TypeParams;
  //end generics
  Expected(ptEqual);
  if TokenID = ptType then
  begin
    ExplicitType;
  end;
  Lexer.InitAhead;
  case TokenID of
    ptPointerSymbol:
      begin
        PointerType;
      end;
    ptClass:
      begin
        case Lexer.AheadTokenID of
          ptOf:
            begin
              ClassReferenceType;
            end;
          ptSemiColon:
            begin
              ClassForward;
            end;
        else
          begin
            ClassType;
          end;
        end;
      end;
    ptInterface:
      begin
        case Lexer.AheadTokenID of
          ptSemiColon:
            begin
              InterfaceForward;
            end;
        else
          begin
            InterfaceType;
          end;
        end;
      end;
    ptDispInterface:
      begin
        case Lexer.AheadTokenID of
          ptSemiColon:
            begin
              DispInterfaceForward;
            end;
        else
          begin
            InterfaceType;
          end;
        end;
      end;
    ptObject:
      begin
        case Lexer.AheadTokenID of
          ptSemiColon:
            begin
              ObjectForward;
            end;
        else
          begin
            ObjectType;
          end;
        end;
      end;
  else
    begin
      {$IFDEF D12_NEWER}
      if ExID = ptReference then
        AnonymousMethodType
      else
      {$ENDIF}
      TypeKind;
    end;
  end;
  while ExID in [ptDeprecated, ptLibrary, ptPlatform] do // DR 2001-10-20
    case ExID of
      ptDeprecated: DirectiveDeprecated;
      ptLibrary: DirectiveLibrary;
      ptPlatform: DirectivePlatform;
    end;
end;

procedure TmwSimplePasPar.TypeName;
begin
  Expected(ptIdentifier);
  if TokenId = ptLower then
    TypeParams;
end;

procedure TmwSimplePasPar.ExplicitType;
begin
  Expected(ptType);
end;

procedure TmwSimplePasPar.TypeKind;
begin
  case TokenID of
    ptAsciiChar, ptFloat, ptIntegerConst, ptMinus, ptNil, ptPlus, ptRoundOpen,
      ptSquareOpen, ptStringConst:
      begin
        SimpleType;
      end;
    ptArray, ptFile, ptPacked, ptRecord, ptSet:
      begin
        StructuredType;
      end;
    ptFunction, ptProcedure:
      begin
        ProceduralType;
      end;
    ptIdentifier:
      begin
        Lexer.InitAhead;
        case Lexer.AheadTokenID of
          ptPoint, ptSemiColon, ptLower:
            begin
              TypeId;
            end;
        else
          begin
            SimpleExpression;
            if Lexer.TokenID = ptDotDot then
            begin
              NextToken;
              SimpleExpression;
            end;
          end;
        end;
      end;
    ptPointerSymbol:
      begin
        PointerType;
      end;
    ptString:
      begin
        StringType;
      end;
  else
    begin
      SynError(InvalidTypeKind);
    end;
  end;
end;

procedure TmwSimplePasPar.TypeArgs;
begin
  TypeId;
  while TokenId = ptComma do
  begin
    NextToken;
    TypeId;
  end;
end;

procedure TmwSimplePasPar.TypedConstant;
begin
  case TokenID of
    ptRoundOpen:
      begin
        Lexer.InitAhead;
        while Lexer.AheadTokenID <> ptSemiColon do
          case Lexer.AheadTokenID of
            ptAnd, ptBegin, ptCase, ptColon, ptEnd, ptElse, ptIf, ptMinus, ptNull,
              ptOr, ptPlus, ptShl, ptShr, ptSlash, ptStar, ptWhile, ptWith,
              ptXor: break;
            ptRoundOpen:
              begin
                repeat
                  case Lexer.AheadTokenID of
                    ptBegin, ptCase, ptEnd, ptElse, ptIf, ptNull, ptWhile, ptWith: break;
                  else
                    begin
                      case Lexer.AheadTokenID of
                        ptRoundClose:
                          begin
                            NextToken;
                            break;
                          end;
                      else
                        Lexer.AheadNext;
                      end;
                    end;
                  end;
                until Lexer.AheadTokenID = ptRoundClose;
              end;
          else
            Lexer.AheadNext;
          end;
        case Lexer.AheadTokenID of
          ptColon:
            begin
              RecordConstant;
            end;
          ptNull: ;
          ptAnd, ptMinus, ptOr, ptPlus, ptShl, ptShr, ptSlash, ptStar, ptXor:
            begin
              ConstantExpression;
            end;
        else
          begin
            ArrayConstant;
          end;
        end;
      end;
    ptSquareOpen:
      ConstantExpression; // DR 2002-01-11

 { DR: fails with constructed set constants like
    WordDelimiters: set of Char = [#0..#255] - ['a'..'z','A'..'Z','1'..'9','0'];

 (*empty; there mustn't be all fields of a record mentioned*)
   begin
  NextToken;
  if TokenID <> ptSquareClose then
    begin
   case TokenID of
     ptDotDot:
    begin
      NextToken;
      NextToken;
    end;
     else
    NextToken;
    case TokenID of
      ptDotDot:
     begin
       NextToken;
       NextToken;
     end;
    end;
   end;
   while TokenID = ptComma do
     begin
    NextToken;
    NextToken;
    case TokenID of
      ptDotDot:
     begin
       NextToken;
       NextToken;
     end;
    end;
     end;
   Expected(ptSquareClose);
    end
  else NextToken;
   end;}
  else
    begin
      ConstantExpression;
    end;
  end;
end;

procedure TmwSimplePasPar.TypeId;
begin
  TypeSimple;
  if TokenId = ptLower then
  begin
    Expected(ptLower);
    TypeArgs;
    Expected(ptGreater);
  end;

  while TokenID = ptPoint do
  begin
    Expected(ptPoint);
    TypeSimple;
    if TokenId = ptLower then
    begin
      Expected(ptLower);
      TypeArgs;
      Expected(ptGreater);
    end;
  end;
end;

procedure TmwSimplePasPar.ConstantExpression;
begin
  SimpleExpression;
end;

procedure TmwSimplePasPar.ResourceDeclaration;
begin
  Identifier;
  Expected(ptEqual);

  CharString;
  while TokenID = ptPlus do
  begin
    NextToken;
    CharString;
  end;

  while ExID in [ptDeprecated, ptLibrary, ptPlatform] do // DR 2002-01-10
    case ExID of
      ptDeprecated: DirectiveDeprecated;
      ptLibrary: DirectiveLibrary;
      ptPlatform: DirectivePlatform;
    end;
end;

procedure TmwSimplePasPar.ConstantDeclaration;
begin
  ConstantName;
  case TokenID of
    ptEqual:
      begin
        ConstantEqual;
      end;
    ptColon:
      begin
        ConstantColon;
      end;
  else
    begin
      SynError(InvalidConstantDeclaration);
    end;
  end;
  while ExID in [ptDeprecated, ptLibrary, ptPlatform] do // DR 2001-10-20
    case ExID of
      ptDeprecated: DirectiveDeprecated;
      ptLibrary: DirectiveLibrary;
      ptPlatform: DirectivePlatform;
    end;
end;

procedure TmwSimplePasPar.ConstantColon;
begin
  Expected(ptColon);
//JR changed to constant Type
  ConstantType;
  Expected(ptEqual);
  ConstantValueTyped;
end;

procedure TmwSimplePasPar.ConstantEqual;
begin
  Expected(ptEqual);
  ConstantValue;
end;

procedure TmwSimplePasPar.ConstantValue;
begin
  ConstantExpression;
end;

procedure TmwSimplePasPar.ConstantValueTyped;
begin
  TypedConstant;
end;

procedure TmwSimplePasPar.ConstantName;
begin
  Expected(ptIdentifier);
end;

procedure TmwSimplePasPar.ConstantType;
begin
  TypeKind;
end;
procedure TmwSimplePasPar.LabelId;
begin
  case TokenID of
    ptIntegerConst:
      begin
        NextToken;
      end;
    ptIdentifier:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidLabelId);
    end;
  end;
end;

procedure TmwSimplePasPar.ProcedureDeclarationSection;
begin
  if TokenID = ptClass then
  begin
    ClassMethod;
  end;
  case TokenID of
    ptConstructor:
      begin
        ProcedureProcedureName;
      end;
    ptDestructor:
      begin
        ProcedureProcedureName;
      end;
    ptProcedure:
      begin
        ProcedureProcedureName;
      end;
    ptFunction:
      begin
        FunctionMethodDeclaration;
      end;
    {$IFDEF D8_NEWER} //JThurman 2004-03-2003
    ptIdentifier:
      begin
        if Lexer.ExID = ptOperator then
        begin
          FunctionMethodDeclaration;
        end
        else
          SynError(InvalidProcedureDeclarationSection);
      end;
    {$ENDIF}
  else
    begin
      SynError(InvalidProcedureDeclarationSection);
    end;
  end;
end;

procedure TmwSimplePasPar.LabelDeclarationSection;
begin
  Expected(ptLabel);
  LabelId;
  while (TokenID = ptComma) do
  begin
    NextToken;
    LabelId;
  end;
  SEMICOLON;
end;

procedure TmwSimplePasPar.ProceduralDirective; //TODO: Add STATIC and FINAL
begin
  case ExID of
    ptAbstract:
      begin
        NextToken;
      end;
    ptCdecl, ptPascal, ptRegister, ptSafeCall, ptStdCall:
      begin
        DirectiveCalling;
      end;
    ptExport, ptFar, ptNear:
      begin
        Directive16Bit;
      end;
    ptExternal:
      begin
        ExternalDirective;
      end;
    ptDynamic, ptMessage, ptOverload, ptOverride, ptReintroduce, ptVirtual:
      begin
        DirectiveBinding;
      end;
    ptAssembler:
      begin
        NextToken;
      end;
    {$IFDEF D8_NEWER}
    ptStatic:
      begin
        NextToken;
      end;
    {$ENDIF}
    {$IFDEF D9_NEWER}
     ptInline:
       begin
         NextToken;
       end;
    {$ENDIF}
    ptDeprecated:
      DirectiveDeprecated; // DR 2001-10-20
    ptLibrary:
      DirectiveLibrary; // DR 2001-10-20
    ptPlatform:
      DirectivePlatform; // DR 2001-10-20
    ptLocal:
      DirectiveLocal; // DR 2001-11-14
    ptVarargs:
      DirectiveVarargs; // DR 2001-11-14
    ptFinal, ptExperimental, ptDelayed:
      NextToken;
  else
    begin
      SynError(InvalidProceduralDirective);
    end;
  end;
end;

procedure TmwSimplePasPar.ExportedHeading;
begin
  case TokenID of
    ptFunction:
      begin
        FunctionHeading;
      end;
    ptProcedure:
      begin
        ProcedureHeading;
      end;
  else
    begin
      SynError(InvalidExportedHeading);
    end;
  end;
  if TokenID = ptSemiColon then SEMICOLON;
  case ExID of
    ptForward:
      begin
        ForwardDeclaration; //jdj added 02/07/2001
//        NextToken;
//        SEMICOLON;
      end;
    ptAssembler:
      begin
        NextToken;
        SEMICOLON;
        if Exid = ptForward then
          ForwardDeclaration; //jdj added 02/07/2001
      end;
  else  //TODO: Add STATIC and FINAL
    while ExID in [ptAbstract, ptCdecl, ptDynamic, ptExport, ptExternal, ptFar,
      ptMessage, ptNear, ptOverload, ptOverride, ptPascal, ptRegister,
      ptReintroduce, ptSafeCall, ptStdCall, ptVirtual,
      ptDeprecated, ptLibrary, ptPlatform, // DR 2001-10-20
      ptLocal, ptVarargs // DR 2001-11-14
      {$IFDEF D8_NEWER}, ptStatic{$ENDIF}{$IFDEF D9_NEWER}, ptInline{$ENDIF}
      ] do
    begin
      ProceduralDirective;
      if TokenID = ptSemiColon then SEMICOLON;
    end;
    if ExId = ptForward then
      ForwardDeclaration; //jdj added 02/07/2001
  end;
end;

procedure TmwSimplePasPar.FunctionHeading;
begin
  Expected(ptFunction);
  FunctionProcedureName;
  if TokenID = ptRoundOpen then
  begin
    FormalParameterList;
  end;
  Expected(ptColon);
  ReturnType;
end;

procedure TmwSimplePasPar.ProcedureHeading;
begin
  Expected(ptProcedure);
  FunctionProcedureName;
  if TokenID = ptRoundOpen then
  begin
    FormalParameterList;
  end;

end;

procedure TmwSimplePasPar.VarSection;
begin
  case TokenID of
    ptThreadVar:
      begin
        NextToken;
      end;
    ptVar:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidVarSection);
    end;
  end;
  {$IFDEF D8_NEWER}//JThurman 2004-03-22
  while TokenID in [ptIdentifier, ptSquareOpen] do
  begin
    if TokenID = ptSquareOpen then
      CustomAttribute
    else
    begin
      VarDeclaration;
      SEMICOLON;
    end;
  end;
  {$ELSE}
  while TokenID = ptIdentifier do
  begin
    VarDeclaration;
    SEMICOLON;
  end;
  {$ENDIF}
end;

procedure TmwSimplePasPar.TypeSection;
begin
  Expected(ptType);

  while (TokenID = ptIdentifier) or (Lexer.TokenID = ptSquareOpen) do
  begin
    if TokenID = ptSquareOpen then
      CustomAttribute
    else
    begin
      InitAhead;
      AheadParse.NextToken;
      AheadParse.TypeName;

      if AheadParse.TokenId <> ptEqual then
        Break;

      TypeDeclaration;
      if TokenID = ptEqual then
        TypedConstant;
      SEMICOLON;
    end;
  end;
end;

procedure TmwSimplePasPar.TypeSimple;
begin
  case GenID of
    ptBoolean, ptByte, ptChar, ptDWord, ptInt64, ptInteger, ptLongInt,
      ptLongWord, ptPChar, ptShortInt, ptSmallInt, ptWideChar, ptWord:
      begin
        OrdinalIdentifier;
      end;
    ptComp, ptCurrency, ptDouble, ptExtended, ptReal, ptReal48, ptSingle:
      begin
        RealIdentifier;
      end;
    ptAnsiString, ptShortString, ptWideString:
      begin
        StringIdentifier;
      end;
    ptOleVariant, ptVariant:
      begin
        VariantIdentifier;
      end;
    ptString:
      begin
        StringType;
      end;
  else
    Expected(ptIdentifier);
  end;
end;

procedure TmwSimplePasPar.TypeParamDecl;
begin
  TypeParamList;
  if TokenId = ptColon then
  begin
    NextToken;
    ConstraintList;
  end;
end;

procedure TmwSimplePasPar.TypeParamDeclList;
begin
  TypeParamDecl;
  while TokenId = ptSemicolon do
  begin
    NextToken;
    TypeParamDecl;
  end;
end;

procedure TmwSimplePasPar.TypeParamList;
begin
  {$IFDEF D8_NEWER}
  if TokenId = ptSquareOpen then
    AttributeSection;
  {$ENDIF}
  Identifier;
  while TokenId = ptComma do
  begin
    NextToken;
    {$IFDEF D8_NEWER}
    if TokenId = ptSquareOpen then
      AttributeSection;
    {$ENDIF}
    Identifier;
  end;
end;

procedure TmwSimplePasPar.TypeParams;
begin
  Expected(ptLower);
  TypeParamDeclList;
  // workaround for TSomeClass< T >= class(TObject)
  if TokenID = ptGreaterEqual then
    Lexer.RunPos := Lexer.RunPos - 1
  else
    Expected(ptGreater);
end;

procedure TmwSimplePasPar.ConstSection;
begin
  case TokenID of
    ptConst:
      begin
        NextToken;
        {$IFDEF D8_NEWER} //JThurman 2004-03-22
        while TokenID in [ptIdentifier, ptSquareOpen] do
        begin
          if TokenID = ptSquareOpen then
            CustomAttribute
          else
          begin
            ConstantDeclaration;
            SEMICOLON;
          end;
        end;
        {$ELSE}
        while (TokenID = ptIdentifier) do
        begin
          ConstantDeclaration;
          SEMICOLON;
        end;
        {$ENDIF}
      end;
    ptResourceString:
      begin
        NextToken;
        while (TokenID = ptIdentifier) do
        begin
          ResourceDeclaration;
          SEMICOLON;
        end;
      end
  else
    begin
      SynError(InvalidConstSection);
    end;
  end;
end;

procedure TmwSimplePasPar.InterfaceDeclaration;
begin
  case TokenID of
    ptConst:
      begin
        ConstSection;
      end;
    ptFunction:
      begin
        ExportedHeading;
      end;
    ptProcedure:
      begin
        ExportedHeading;
      end;
    ptResourceString:
      begin
        ConstSection;
      end;
    ptType:
      begin
        TypeSection;
      end;
    ptThreadVar:
      begin
        VarSection;
      end;
    ptVar:
      begin
        VarSection;
      end;
    ptExports:
      begin
        ExportsClause;
      end;
    {$IFDEF D8_NEWER} //JThurman 2004-03-03
    ptSquareOpen:
      begin
        CustomAttribute;
      end;
    {$ENDIF}
  else
    begin
      SynError(InvalidInterfaceDeclaration);
    end;
  end;
end;

procedure TmwSimplePasPar.ExportsElement;
begin
  Expected(ptIdentifier);
  //  if TokenID = ptIndex then
  if FLexer.ExID = ptIndex then //jdj 20001207
  begin
    NextToken;
    Expected(ptIntegerConst);
  end;
  //  if TokenID = ptName then
  if FLexer.ExID = ptName then //jdj 20001207
  begin
    NextToken;
    CharString;
  end;
  //  if TokenID = ptResident then
  if FLexer.ExID = ptResident then //jdj 20001207
  begin
    NextToken;
  end;
end;

procedure TmwSimplePasPar.CompoundStatement;
begin
  Expected(ptBegin);
  StatementList;
  Expected(ptEnd);
end;

procedure TmwSimplePasPar.ExportsClause;
begin
  Expected(ptExports);
  ExportsElement;
  while TokenID = ptComma do
  begin
    NextToken;
    ExportsElement;
  end;
  SEMICOLON;
end;

procedure TmwSimplePasPar.ContainsClause;
begin
  ExpectedEx(ptContains);
  ContainsStatement;
  while TokenID = ptComma do
  begin
    NextToken;
    ContainsStatement;
  end;
  SEMICOLON;
end;

procedure TmwSimplePasPar.ContainsStatement;
begin
  ContainsIdentifier;
  if fLexer.TokenID = ptIn then
  begin
    NextToken;
    ContainsExpression;
  end;
end;

procedure TmwSimplePasPar.ContainsIdentifier;
begin
  ContainsIdentifierId;
  while Lexer.TokenID = ptPoint do
  begin
    NextToken;
    ContainsIdentifierId;
  end;
end;

procedure TmwSimplePasPar.ContainsIdentifierId;
begin
  Expected(ptIdentifier);
end;

procedure TmwSimplePasPar.ContainsExpression;
begin
  ConstantExpression;
end;

procedure TmwSimplePasPar.RequiresClause;
begin
  ExpectedEx(ptRequires);
  RequiresIdentifier;
  while TokenID = ptComma do
  begin
    NextToken;
    RequiresIdentifier;
  end;
  SEMICOLON;
end;

procedure TmwSimplePasPar.RequiresIdentifier;
begin
  RequiresIdentifierId;
  {$IFDEF D8_NEWER}
  while Lexer.TokenID = ptPoint do
  begin
    NextToken;
    RequiresIdentifierId;
  end;
  {$ENDIF}
end;

procedure TmwSimplePasPar.RequiresIdentifierId;
begin
  Expected(ptIdentifier);
end;

procedure TmwSimplePasPar.InitializationSection;
begin
  case TokenID of
    ptInitialization:
      begin
        NextToken;
        StatementList;
        if TokenID = ptFinalization then
        begin
          NextToken;
          StatementList;
        end;
        Expected(ptEnd);
      end;
    ptBegin:
      begin
        CompoundStatement;
      end;
    ptEnd:
      begin
        NextToken;
      end;
  else
    begin
      SynError(InvalidInitializationSection);
    end;
  end;
end;

procedure TmwSimplePasPar.ImplementationSection;
begin
  Expected(ptImplementation);
  if TokenID = ptUses then
  begin
    UsesClause;
  end;
  while TokenID in [ptClass, ptConst, ptConstructor, ptDestructor, ptFunction,
    ptLabel, ptProcedure, ptResourceString, ptThreadVar, ptType, ptVar,
    ptExports
    {$IFDEF D8_NEWER}//JThurman 2004-03-22
    , ptSquareOpen
    {$ENDIF}
    ] do //ptResourceString added jdj
  begin
    DeclarationSection;
  end;
end;

procedure TmwSimplePasPar.InterfaceSection;
begin
  Expected(ptInterface);
  if TokenID = ptUses then
  begin
    UsesClause;
  end;
  while TokenID in [ptConst, ptFunction, ptResourceString, ptProcedure,
    ptThreadVar, ptType, ptVar, ptExports
    {$IFDEF D8_NEWER} //JThurman 2004-03-03
    , ptSquareOpen
    {$ENDIF}
    ] do
  begin
    InterfaceDeclaration;
  end;
end;

procedure TmwSimplePasPar.IdentifierList;
begin
  Identifier; // DR 2001-10-20
  while TokenID = ptComma do
  begin
    NextToken;
    Identifier;
  end;
end;

procedure TmwSimplePasPar.QualifiedIdentifierList;
begin
  QualifiedIdentifier;
  while (TokenID = ptComma) do
  begin
    NextToken;
    QualifiedIdentifier;
  end;
end;

procedure TmwSimplePasPar.CharString;
begin //updated mw 2/22/00, JThurman 6/24/2004
  case TokenID of
    ptAsciiChar, ptIdentifier, ptRoundOpen, ptStringConst:
      while TokenID in
        [ptAsciiChar, ptIdentifier, ptRoundOpen, ptStringConst,
        ptString] do
      begin
        case TokenID of
          ptIdentifier, ptRoundOpen:
            begin
              VariableReference;
            end;
          ptString: //JT
            begin
              StringStatement;
            end;
        else
          StringConst;
        end;
        {$IFDEF D8_NEWER}
        if Lexer.TokenID = ptPoint then
        begin
          NextToken;
          VariableReference;
        end;
        {$ENDIF}
      end;
  else
    begin
      SynError(InvalidCharString);
    end;
  end;
end;

(*procedure TmwSimplePasPar.CharString;
begin //updated mw 2/22/00
  case TokenID of
    ptAsciiChar, ptIdentifier, ptRoundOpen, ptStringConst:
      while TokenID in
        [ptAsciiChar, ptIdentifier, ptPlus, ptRoundOpen, ptStringConst] do
      begin
        case TokenID of
          ptIdentifier, ptRoundOpen:
            begin
              VariableReference;
            end;
        else
          NextToken;
        end;
      end;
  else
    begin
      SynError(InvalidCharString);
    end;
  end;
end;*)

(*procedure TmwSimplePasPar.CharString;
begin
  case TokenID of
    ptAsciiChar, ptStringConst:
      while TokenID in [ptAsciiChar, ptPlus, ptStringConst] do
      begin
        case TokenID of
          ptPlus:
            begin
              NextToken;
              if TokenID = ptIdentifier then
              begin
                VariableReference;
              end;
            end;
        else
          begin
            NextToken;
          end;
        end;
      end;
    ptIdentifier:
      begin
        VariableReference;
        case TokenID of
          ptPlus:
            begin
              NextToken;
              while TokenID in [ptAsciiChar, ptPlus, ptStringConst] do
              begin
                case TokenID of
                  ptPlus:
                    begin
                      NextToken;
                      if TokenID = ptIdentifier then
                      begin
                        VariableReference;
                      end;
                    end;
                else
                  begin
                    NextToken;
                  end;
                end;
              end;
   end;
        end;
      end
  else
    begin
      SynError(InvalidCharString);
    end;
  end;
end;*)

procedure TmwSimplePasPar.IncludeFile;
begin
  while TokenID <> ptNull do
    case TokenID of
      ptClass:
        begin
          ProcedureDeclarationSection;
        end;
      ptConst:
        begin
          ConstSection;
        end;
      ptConstructor:
        begin
          ProcedureDeclarationSection;
        end;
      ptDestructor:
        begin
          ProcedureDeclarationSection;
        end;
      ptExports:
        begin
          ExportsClause;
        end;
      ptFunction:
        begin
          ProcedureDeclarationSection;
        end;
      ptIdentifier:
        begin
          Lexer.InitAhead;
          if Lexer.AheadTokenID in [ptColon, ptEqual] then
          begin
            ConstantDeclaration;
            if TokenID = ptSemiColon then SEMICOLON;
          end
          else
            NextToken;
        end;
      ptLabel:
        begin
          LabelDeclarationSection;
        end;
      ptProcedure:
        begin
          ProcedureDeclarationSection;
        end;
      ptResourceString:
        begin
          ConstSection;
        end;
      ptType:
        begin
          TypeSection;
        end;
      ptThreadVar:
        begin
          VarSection;
        end;
      ptVar:
        begin
          VarSection;
        end;
    else
      begin
        NextToken;
      end;
    end;
end;

procedure TmwSimplePasPar.SkipSpace; //XM Jul-2000
begin
  Expected(ptSpace);
  while TokenID in [ptSpace] do
    Lexer.Next;
end;

procedure TmwSimplePasPar.SkipCRLFco; //XM Jul-2000
begin
  Expected(ptCRLFCo);
  while TokenID in [ptCRLFCo] do
    Lexer.Next;
end;

procedure TmwSimplePasPar.SkipCRLF; //XM Jul-2000
begin
  Expected(ptCRLF);
  while TokenID in [ptCRLF] do
    Lexer.Next;
end;

procedure TmwSimplePasPar.ClassClass;
begin
  Expected(ptClass);
end;

procedure TmwSimplePasPar.PropertyDefault;
begin
  ExpectedEx(ptDefault);
end;

procedure TmwSimplePasPar.DispIDSpecifier; // DR 2001-07-26
begin
  ExpectedEx(ptDispid);
  ConstantExpression;
end;

procedure TmwSimplePasPar.IndexOp;
begin
  Expected(ptSquareOpen);
  Expression;
  while TokenID = ptComma do
  begin
    NextToken;
    Expression;
  end;
  Expected(ptSquareClose);
end;

procedure TmwSimplePasPar.IndexSpecifier;
begin
  ExpectedEx(ptIndex);
  ConstantExpression;
end;

procedure TmwSimplePasPar.ClassTypeEnd;
begin
end;

procedure TmwSimplePasPar.ObjectTypeEnd;
begin
end;

procedure TmwSimplePasPar.DirectiveDeprecated;
begin
  ExpectedEx(ptDeprecated);
  if TokenID = ptStringConst then
    NextToken;
end;

procedure TmwSimplePasPar.DirectiveLibrary;
begin
  ExpectedEx(ptLibrary);
end;

procedure TmwSimplePasPar.DirectivePlatform;
begin
  ExpectedEx(ptPlatform);
end;

procedure TmwSimplePasPar.EnumeratedTypeItem;
begin
  QualifiedIdentifier;
  if TokenID = ptEqual then
  begin
    Expected(ptEqual);
    ConstantExpression;
  end;
end;

procedure TmwSimplePasPar.Identifier;
begin
  NextToken;
end;

procedure TmwSimplePasPar.DirectiveLocal;
begin
  ExpectedEx(ptLocal);
end;

procedure TmwSimplePasPar.DirectiveVarargs;
begin
  ExpectedEx(ptVarargs);
end;

procedure TmwSimplePasPar.AncestorId;
begin
  TypeId;
end;

procedure TmwSimplePasPar.AncestorIdList;
begin
  // !! Added this function back in
  AncestorId;
  while(TokenID = ptComma) do
    begin
      NextToken;
      AncestorId;
    end;
end;



procedure TmwSimplePasPar.AnonymousMethod;
begin
  case TokenID of
    ptFunction:
      begin
        NextToken;
        if TokenID = ptRoundOpen then
          FormalParameterList;
        Expected(ptColon);
        ReturnType;
      end;
    ptProcedure:
      begin
        NextToken;
        if TokenId = ptRoundOpen then
          FormalParameterList;
      end;
  end;
  Block;
end;

procedure TmwSimplePasPar.AnonymousMethodType;
begin
{$IFDEF D11_NEWER}
  ExpectedEx(ptReference); //ExID = ptReference
  Expected(ptTo);
  case TokenID of
    ptProcedure:
      begin
        NextToken;
        if TokenID = ptRoundOpen then
          FormalParameterList;
      end;
    ptFunction:
      begin
        NextToken;
        if TokenID = ptRoundOpen then
          FormalParameterList;
        Expected(ptColon);
        ReturnType;
      end;
  end;
{$ENDIF}
end;

procedure TmwSimplePasPar.AddDefine(const ADefine: string);
begin
  FDefines.Add(ADefine);
end;

procedure TmwSimplePasPar.RemoveDefine(const ADefine: string);
var
  I: Integer;
begin
  I := FDefines.IndexOf(ADefine);
  if I > -1 then
    FDefines.Delete(I);
end;

function TmwSimplePasPar.IsDefined(const ADefine: string): Boolean;
begin
  Result := FDefines.IndexOf(ADefine) > -1;
end;

procedure TmwSimplePasPar.InheritedVariableReference;
begin
  Expected(ptInherited);
  if TokenID = ptIdentifier then
    VariableReference;
end;

procedure TmwSimplePasPar.ClearDefines;
var
  Frame: PDefineRec;
begin
  FDefines.Clear;
  FDefineStack := 0;
  while FTopDefineRec <> nil do
  begin
    Frame := FTopDefineRec;
    FTopDefineRec := Frame^.Next;
    Dispose(Frame);
  end;
end;

procedure TmwSimplePasPar.InitAhead;
begin
  if AheadParse = nil then
    AheadParse := TmwSimplePasPar.Create;
  AheadParse.Lexer.InitFrom(Lexer);
end;

procedure TmwSimplePasPar.InitDefines;
begin
  ClearDefines;
  //Set up the defines that are defined by the compiler
//  {$IFDEF VER130}
//  AddDefine('VER130');
//  {$ENDIF}
//  {$IFDEF VER140}
//  AddDefine('VER140');
//  {$ENDIF}
//  {$IFDEF VER150}
//  AddDefine('VER150');
//  {$ENDIF}
//  {$IFDEF VER160}
//  AddDefine('VER160');
//  {$ENDIF}
//  {$IFDEF VER170}
//  AddDefine('VER170');
//  {$ENDIF}
//  {$IFDEF VER180}
//  AddDefine('VER180');
//  {$ENDIF}
//  {$IFDEF VER185}
//  AddDefine('VER185');
//  {$ENDIF}
//  {$IFDEF VER190}
//  AddDefine('VER190');
//  {$ENDIF}
//  {$IFDEF VER200}
//  AddDefine('VER200');
//  {$ENDIF}
//  {$IFDEF WIN32}
//  AddDefine('WIN32');
//  {$ENDIF}
//  {$IFDEF LINUX}
//  AddDefine('LINUX');
//  {$ENDIF}
//  {$IFDEF CPU386}
//  AddDefine('CPU386');
//  {$ENDIF}
//  {$IFDEF MSWINDOWS}
//  AddDefine('MSWINDOWS');
//  {$ENDIF}
//  {$IFDEF CONDITIONALEXPRESSIONS}
//  AddDefine('CONDITIONALEXPRESSIONS');
//  {$ENDIF}
end;

procedure TmwSimplePasPar.EnterDefineBlock(ADefined: Boolean);
var
  StackFrame: PDefineRec;
begin
  Exit;
  New(StackFrame);
  StackFrame^.Next := FTopDefineRec;
  StackFrame^.Defined := ADefined;
  StackFrame^.StartCount := FDefineStack;
  FTopDefineRec := StackFrame;
//  if not ADefined then
//  begin
//    Inc(FDefineStack);
//    repeat
//      NextToken;
//      if TokenID = ptNull then
//        Break;
//    until FDefineStack = 0;
//  end
//  else
//    NextToken;
  if not ADefined then
    Inc(FDefineStack);

//  while FDefineStack > 0 do
//  begin
//    NextToken;
//    if TokenID = ptNull then
//      Break;
//  end;
end;

procedure TmwSimplePasPar.ExitDefineBlock;
var
  StackFrame: PDefineRec;
begin
  Exit;
  StackFrame := FTopDefineRec;
  if StackFrame <> nil then
  begin
    FDefineStack := StackFrame^.StartCount;
    FTopDefineRec := StackFrame^.Next;
    Dispose(StackFrame);
  end;
end;

{$IFDEF D8_NEWER} //JThurman 2004-03-03

procedure TmwSimplePasPar.GlobalAttributes;
begin
  GlobalAttributeSections;
end;

procedure TmwSimplePasPar.GlobalAttributeSections;
begin
  while TokenID = ptSquareOpen do
    GlobalAttributeSection;
end;

procedure TmwSimplePasPar.GlobalAttributeSection;
begin
  Expected(ptSquareOpen);
  GlobalAttributeTargetSpecifier;
  AttributeList;
  while TokenID = ptComma do
  begin
    Expected(ptComma);
    GlobalAttributeTargetSpecifier;
    AttributeList;
  end;
  Expected(ptSquareClose);
end;

procedure TmwSimplePasPar.GlobalAttributeTargetSpecifier;
begin
  GlobalAttributeTarget;
  Expected(ptColon);
end;

procedure TmwSimplePasPar.GlobalAttributeTarget;
begin
  Expected(ptIdentifier);
end;

procedure TmwSimplePasPar.Attributes;
begin
  AttributeSections;
end;

procedure TmwSimplePasPar.AttributeSections;
begin
  while TokenID = ptSquareOpen do
    AttributeSection;
end;

procedure TmwSimplePasPar.AttributeSection;
begin
  Expected(ptSquareOpen);
  Lexer.InitAhead;
  if Lexer.AheadTokenID = ptColon then
    AttributeTargetSpecifier;
  AttributeList;
  while TokenID = ptComma do
  begin
    Lexer.InitAhead;
    if Lexer.AheadTokenID = ptColon then
      AttributeTargetSpecifier;
    AttributeList;
  end;
  Expected(ptSquareClose);
end;

procedure TmwSimplePasPar.AttributeTargetSpecifier;
begin
  AttributeTarget;
  Expected(ptColon);
end;

procedure TmwSimplePasPar.AttributeTarget;
begin
  case TokenID of
    ptProperty:
      Expected(ptProperty);
    ptType:
      Expected(ptType);
    else
      Expected(ptIdentifier);
  end;
end;

procedure TmwSimplePasPar.AttributeList;
begin
  Attribute;
  while TokenID = ptComma do
  begin
    Expected(ptComma);
    AttributeList;
  end;
end;

procedure TmwSimplePasPar.Attribute;
begin
  AttributeName;
  if TokenID = ptRoundOpen then
    AttributeArguments;
end;

procedure TmwSimplePasPar.AttributeName;
begin
  case TokenID of
    ptIn, ptOut, ptConst, ptVar:
      NextToken;
  else
    Expected(ptIdentifier);
  end;
end;

procedure TmwSimplePasPar.AttributeArguments;
begin
  Expected(ptRoundOpen);
  if TokenID <> ptRoundClose then
  begin
    Lexer.InitAhead;
    if Lexer.AheadTokenID = ptEqual then
      NamedArgumentList
    else
      PositionalArgumentList;
    if Lexer.TokenID = ptEqual then
      NamedArgumentList;
  end;
  Expected(ptRoundClose);
end;

procedure TmwSimplePasPar.PositionalArgumentList;
begin
  PositionalArgument;
  while TokenID = ptComma do
  begin
    Expected(ptComma);
    PositionalArgument;
  end;
end;

procedure TmwSimplePasPar.PositionalArgument;
begin
  AttributeArgumentExpression;
end;

procedure TmwSimplePasPar.NamedArgumentList;
begin
  NamedArgument;
  while TokenID = ptComma do
  begin
    Expected(ptComma);
    NamedArgument;
  end;
end;

procedure TmwSimplePasPar.NamedArgument;
begin
  Expected(ptIdentifier);
  Expected(ptEqual);
  AttributeArgumentExpression;
end;

procedure TmwSimplePasPar.AttributeArgumentExpression;
begin
  Expression;
end;

procedure TmwSimplePasPar.CustomAttribute;
begin
  AttributeSection;//TODO: Global vs. Local attributes
{  Lexer.InitAhead;
  if (Lexer.AheadToken = 'assembly') or (Lexer.AheadToken = 'module') then
    GlobalAttributeSections
  else}
    AttributeSections;

end;
{$ENDIF}

end.

