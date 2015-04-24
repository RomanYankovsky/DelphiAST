unit DelphiAST.Consts;

interface

type
  TSyntaxNodeType = (ntUnknown, ntAdd, ntAddr, ntAlignmentParam, ntAnd, ntAnonymousMethod, ntArguments, ntAs, ntAssign,
    ntAt, ntAttribute, ntAttributes, ntBounds, ntCall, ntCase, ntCaseElse, ntCaseLabel, ntCaseLabels, ntCaseSelector,
    ntConstant, ntConstants, ntContains, ntDeref, ntDimension, ntDirective, ntDirectives, ntDiv, ntDot, ntDownTo,
    ntElement, ntElse, ntEmptyStatement, ntEnum, ntEqual, ntExcept, ntExceptionHandler, ntExpression, ntExpressions,
    ntFDiv, ntField, ntFields, ntFinally, ntFor, ntFrom, ntGeneric, ntGoto, ntGreater, ntGreaterEqual, ntGuid,
    ntIdentifier, ntIf, ntImplementation, ntImplements, ntIn, ntIndexed, ntInherited, ntInterface, ntIs, ntLabel, ntLHS,
    ntLiteral, ntLower, ntLowerEqual, ntMethod, ntMod, ntMul, ntName, ntNamedArgument, ntNotEqual, ntNot, ntOr,
    ntPackage, ntParameter, ntParameters, ntPath, ntPositionalArgument, ntProtected, ntPrivate, ntProperty, ntPublic,
    ntPublished, ntRaise, ntRead, ntRepeat, ntRequires, ntReturnType, ntRHS, ntRoundClose, ntRoundOpen, ntSet, ntShl,
    ntShr, ntStatement, ntStatements, ntSub, ntSubrange, ntThen, ntTo, ntTry, ntType, ntTypeArgs, ntTypeDecl,
    ntTypeParams, ntTypeSection, ntValue, ntVariable, ntVariables, ntXor, ntUnaryMinus, ntUnit, ntUses, ntWhile,
    ntWith, ntWrite);

const
  SyntaxNodeNames: array [TSyntaxNodeType] of string = ('unknown', 'add', 'addr', 'alignmentparam', 'and',
    'anonymousmethod', 'arguments', 'as', 'assign', 'at', 'attribute', 'attributes', 'bounds', 'call', 'case',
    'caseelse', 'caselabel', 'caselabels', 'caseselector', 'constant', 'constants', 'contains', 'deref', 'dimension',
    'directive', 'directives', 'div', 'dot', 'downto', 'element', 'else', 'emptystatement', 'enum', 'equal', 'except',
    'exceptionhandler', 'expression', 'expressions', 'fdiv', 'field', 'fields', 'finally', 'for', 'from', 'generic',
    'goto', 'greater', 'greaterequal', 'guid', 'identifier', 'if', 'implementation', 'implements', 'in', 'indexed',
    'inherited', 'interface', 'is', 'label', 'lhs', 'literal', 'lower', 'lowerequal', 'method', 'mod', 'mul', 'name',
    'namedargument', 'notequal', 'not', 'or', 'package', 'parameter', 'parameters', 'path', 'positionalargument',
    'protected', 'private', 'property', 'public', 'published', 'raise', 'read', 'repeat', 'requires', 'returntype',
    'rhs', 'roundclose', 'roundopen', 'set', 'shl', 'shr', 'statement', 'statements', 'sub', 'subrange', 'then', 'to',
    'try', 'type', 'typeargs', 'typedecl', 'typeparams', 'typesection', 'value', 'variable', 'variables', 'xor',
    'unaryminus', 'unit', 'uses', 'while', 'with', 'write');

type
  //Less than 64 different attributes (takes 8 bytes in a set)
  //All the attributes that have additional Value are in front.
  //Attributes with no associated Value are at the back.
  //The complex attributes have duplicated entries.
  //This enables mutally exclusive attributes to share the same Value space
  TAttribute = (//Attribute numbers
    aFirst = 0,
    aLine = 0, //Line number
    aCol = 1, //Column number
    aValue = 2, //Lexer.Token
    aType = 3, //asm
    aKind = 4, //const, constructor
    aPath = 4, //For pathnames in uses clause
    aExternal = 5, aName = 6, //Lexer.Token
  {TODO -oJ -cValidate : Make sure a combination of external 'user.dll' name 'test' index 2 is illegal}
    aIndex = 6, //External, does not combine with name
    aDeprecated = 7, //Has an optional description string.
    aMessage = 6, //dynamic, but with extra info for the message
    aDispid = 6, //Has a numeric id
    aRead = 2, //for properties only
    aWrite = 3, aDefault = 4, aImplements = 5, aLastValue = 7,

  //The following attributes have no Value associated with them.
    aFirstBoolean = 8, aStored = 8, aNoDefault, aClassForward, //true
    aClass, //Class and record members
    aSealed, //Only for classes
    aAutomated, //Class members
    aPublic, //Class and record members
    aPrivate, //Class and record members
    aPublished, //Class members
    aStrictPrivate, //Class and record members
    aStrictProtected, //Class members
    aProtected, //Class members
    aAbsolute, //Local, record and class variables
  //Parameter directives
    aOut, aVar, aConst,
  //Method directives
    aReintroduce, aOverload, aAbstract, aVirtual, //Differen bindings
    aDynamic, aOverride, aFinal, //Last override of a virtual method
    aDelayed, //With External only
    aForward, aCCRegister, //Calling conventions
    aCCPascal, aCCCdecl, aCCVarArgs, //Extra for CDecl only
    aCCStdcall, aCCSafecall, aInline,
    //Warning directives
    aUnsafe, //for .Net only
    aPlatform, aExperimental, aLibrary, aStatic, //methods and variables
    aAssembler,
    aCompound, //SetOf, ArrayOf, FileOf
    //For dispinterface only
    aReadOnly, aWriteOnly, aLastBoolean = aWriteOnly, aInvalid);

  TAttributes = set of TAttribute;
  TValueAttribute = aFirst..aLastValue;
  TValueAttributes = set of TValueAttribute;
  TSimpleAttribute = aFirstBoolean..aLastBoolean;
  TSimpleAttributes = set of TSimpleAttribute;

const
  Visibility: TAttributes = [aAutomated..aProtected];

type
  Pair<K, V> = record
    Key: K;
    Value: V;
    class function Create(Key: K; Value: V): Pair<K, V>; static;
    procedure Free; inline;
    class operator Implicit(A: Pair<K,V>): V;
  end;

  TExtAttribute = Pair<TValueAttribute, string>;

  TExtHelper = record helper for TExtAttribute
     function IsEmpty: boolean;
  end;

  TAttributeRec = record
  private
    FAttributes: TAttributes;
    FData: array [aFirst .. aLastValue] of string;
    function GetValue(index: TValueAttribute): string;
    function GetData(index: TValueAttribute): TExtAttribute;
    function GetLine: integer;
    function GetCol: integer;
    function GetEndCol: integer;
    function GetEndLine: integer;
    procedure SetCol(const Value: integer);
    procedure SetEndCol(const Value: integer);
    procedure SetEndLine(const Value: integer);
    procedure SetLine(const Value: integer);
  public
    class operator Add(const A: TAttributeRec; B: TSimpleAttribute): TAttributeRec; inline;
    class operator Add(const A: TAttributeRec; B: TExtAttribute): TAttributeRec;
    class operator in(A: TAttribute; const B: TAttributeRec): Boolean; inline;
    class operator in(A: TExtAttribute; const B: TAttributeRec): Boolean; inline;
    function IsEmpty: boolean; inline;
    property Line: integer read GetLine write SetLine;
    property Col: integer read GetCol write SetCol;
    property EndLine: integer read GetEndLine write SetEndLine;
    property EndCol: integer read GetEndCol write SetEndCol;
    property Value[index: TValueAttribute]: string read GetValue;
    property Data[index: TValueAttribute]: TExtAttribute read GetData;
  end;

const
  sENUM = 'enum';
  sNAME = 'name';
  sPATH = 'path';
  sSUBRANGE = 'subrange';
  sTYPE = 'type';
  sVALUE = 'value';
  sEXTERNAL = 'external';
  sCALLING = 'callingconvention';
  sDEPRECATED = 'deprecated';
  sExperimental = 'experimental';
  sBINDING = 'binding';
  s16BIT = '16bit';
  sFORWARD = 'forward';
  sKIND = 'kind';
  sVISIBILITY = 'visibility';
  sLIBRARY = 'library';
  sTRUE = 'true';
  sFALSE = 'false';
  sLINE = 'line';
  sCOL = 'col';
  sCLASS = 'class';
  sDELAYED = 'delayed';

implementation

uses
  System.SysUtils,
  System.StrUtils;

{ TAttributeRec }

class operator TAttributeRec.Add(const A: TAttributeRec; B: TExtAttribute): TAttributeRec;
begin
  Result:= A;
  Include(Result.FAttributes, B.Key);
  Result.FData[B.Key]:= B.Value;
end;

class operator TAttributeRec.Add(const A: TAttributeRec; B: TSimpleAttribute): TAttributeRec;
begin
  Result:= A;
  Include(Result.FAttributes, B);
end;

class operator TAttributeRec.In(A: TAttribute; const B: TAttributeRec): Boolean;
begin
  Result:= A in B.FAttributes;
end;

function TAttributeRec.GetValue(index: TValueAttribute): string;
begin
  if index in FAttributes then case index of
    aLine: Result:= IntToStr(Line);
    aCol: Result:= IntToStr(Col);
    else Result:= FData[index];
  end
  else Result:= '';
end;

class operator TAttributeRec.in(A: TExtAttribute; const B: TAttributeRec): Boolean;
begin
  Result:= A.Key in B.FAttributes;
end;

function TAttributeRec.GetData(index: TValueAttribute): TExtAttribute;
begin
  if index in FAttributes then begin
    Result.Key:= index;
    case index of
      aLine: Result.Value:= IntToStr(Line);
      aCol: Result.Value:= IntToStr(Col);
      else Result.Value:= FData[index];
    end
  end
  else Result:= Default(TExtAttribute);
end;

function TAttributeRec.IsEmpty: boolean;
begin
  Result:= Self.FAttributes = [];
end;

function ExtractNumber(Full: string; index: integer = 0): integer;
var
  Start, Count: integer;
  Number: string;
begin
  Start:= 1;
  repeat
    while not(Full[Start] in ['0'..'9']) do Inc(Start);
    Count:= 1;
    while (Full[Start+Count] in ['0'..'9']) do Inc(Count);
    if index > 0 then Start:= Start + Count;
    Dec(Index);
  until index < 0;
  Number:= MidStr(Full,Start,Count);
  Result:= StrToInt(Number);
end;

function TAttributeRec.GetLine: integer;
begin
  Result:= ExtractNumber(FData[aLine]);
end;

function TAttributeRec.GetCol: integer;
begin
  Result:= ExtractNumber(FData[aCol]);
end;

function TAttributeRec.GetEndLine: integer;
begin
  Result:= ExtractNumber(FData[aLine],1);
end;

function TAttributeRec.GetEndCol: integer;
begin
  Result:= ExtractNumber(FData[aCol],1);
end;


procedure TAttributeRec.SetCol(const Value: integer);
begin
  Include(FAttributes,aCol);
  FData[aCol]:= IntToStr(Value);
end;

procedure TAttributeRec.SetEndCol(const Value: integer);
begin
  Assert(aLine in FAttributes);
  FData[aCol]:= fData[aCol] + 'end:'+IntToStr(Value);
end;

procedure TAttributeRec.SetEndLine(const Value: integer);
begin
  Assert(aLine in FAttributes);
  FData[aLine]:= fData[aLine] + 'end:'+IntToStr(Value);
end;

procedure TAttributeRec.SetLine(const Value: integer);
begin
  Include(FAttributes,aLine);
  FData[aLine]:= IntToStr(Value);
end;

{ Pair<K, V> }

class function Pair<K, V>.Create(Key: K; Value: V): Pair<K, V>;
begin
  Result.Key:= Key;
  Result.Value:= Value;
end;

procedure Pair<K, V>.Free;
begin
  //No op.
end;


class operator Pair<K, V>.Implicit(A: Pair<K, V>): V;
begin
  Result:= A.Value;
end;

{ TExtHelper }

function TExtHelper.IsEmpty: boolean;
begin
  Result:= (Self.Key = aFirst) and (Self.Value = '');
end;


end.
