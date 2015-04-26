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
  //Exactly 64 different attributes (takes 8 bytes in a set)
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
    aExternal = 5,
    aName = 6, //Lexer.Token
    aDeprecated = 7, //Has an optional description string.
    aRead = 2+8, //for properties only
    aWrite = 3+8,
    aDefault = 4+8,
    aImplements = 5+8,
    aIndex = 6+8, //External, does not combine with name
    aPath = 4+16, //For pathnames in uses clause
    aDispid = 5+16, //Has a numeric id
    aMessage = 6+16, //dynamic, but with extra info for the message
    aLastValue = 7+16,
  //The following attributes have no Value associated with them.
    aFirstBoolean = aLastValue+1, aStored = aFirstBoolean, aNoDefault, aClassForward, //true
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
    //aUnsafe, //for .Net only
    aPlatform, aExperimental, aLibrary, aStatic, //methods and variables
    aAssembler,
    aCompound, //SetOf, ArrayOf, FileOf
    //For dispinterface only
    aReadOnly, aWriteOnly, aLastBoolean = aWriteOnly);//, aInvalid);

const
  AttributeNames: array [TAttribute] of string = (
    'Line',       //0
    'Col',        //1
    'Value',      //2
    'Type',       //3
    'Kind',       //4
    'External',   //5
    'Name',       //6
    'Deprecated', //7
    'Invalid0+8',   //
    'Invalid1+8',   //
    'Read',       // = 2+8, //for properties only
    'Write',      //  = 3+8,
    'Default',    // = 4+8,
    'Implements', // = 5+8,
    'Index',      // = 6+8, //External, does not combine with name
    'Invalid7+8',
    'Invalid0+16',
    'Invalid1+16',
    'Invalid2+16',
    'Invalid3+16',
    'Path',       // = 4+16, //For pathnames in uses clause
    'Dispid',     // = 5+16, //Has a numeric id
    'Message',    // = 6+16, //dynamic, but with extra info for the message
    'Invalid7+16',  //  = 7+16,
  //The following attributes have no Value associated with them.
    //FirstBoolean = aLastValue+1,
    'Stored',     //= aFirstBoolean,
    'NoDefault',
    'ClassForward', //true
    'Class', //Class 'nd record members
    'Sealed', //Only for classes
    'Automated', //Class members
    'Public', //Class 'nd record members
    'Private', //Class 'nd record members
    'Published', //Class members
    'StrictPrivate', //Class 'nd record members
    'StrictProtected', //Class members
    'Protected', //Class members
    'Absolute', //Local', record 'nd class variables
  //Parameter directives
    'Out', 'Var', 'Const',
  //Method directives
    'Reintroduce', 'Overload', 'Abstract', 'Virtual', //Differen bindings
    'Dynamic', 'Override', 'Final', //Last override of ' virtual method
    'Delayed', //With External only
    'Forward', 'CCRegister', //Calling conventions
    'CCPascal', 'CCCdecl', 'CCVarArgs', //Extra for CDecl only
    'CCStdcall', 'CCSafecall', 'Inline',
    //Warning directives
    //'Unsafe', //for .Net only
    'Platform', 'Experimental', 'Library', 'Static', //methods 'nd variables
    'Assembler',
    'Compound', //SetOf', 'rrayOf', FileOf
    //For dispinterface only
    'ReadOnly', 'WriteOnly'
    //aLastBoolean = 'WriteOnly',
    );

type
  TAttributes = set of TAttribute;
  TValueAttribute = aFirst..aLastValue;
  TValueAttributes = set of TValueAttribute;
  TSimpleAttribute = aFirstBoolean..aLastBoolean;
  TSimpleAttributes = set of TSimpleAttribute;

  ValueAttributes = aFirst..aLastValue;

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
     function KeyName: string;
  end;

  PAttributeRec = ^TAttributeRec;
  TAttributeRec = record
  public type
    TAttributeEnumerator = record
      //FTestAttributes: Int64;
      FIndex: TAttribute;
      FRec: PAttributeRec;
      class function Create(const ARec: TAttributeRec): TAttributeEnumerator; static;
      function GetCurrent: TExtAttribute; inline;
      function MoveNext: boolean; inline;
      property Current: TExtAttribute read GetCurrent;
    end;
  private
    FAttributes: TAttributes;
    FData: array [aFirst .. aDeprecated] of string;
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
    function GetEnumerator: TAttributeEnumerator;
    function IsEmpty: boolean; inline;
    property Line: integer read GetLine write SetLine;
    property Col: integer read GetCol write SetCol;
    property EndLine: integer read GetEndLine write SetEndLine;
    property EndCol: integer read GetEndCol write SetEndCol;
    property Value[index: TValueAttribute]: string read GetValue;
    property Data[index: TValueAttribute]: TExtAttribute read GetData;
  end;

  TAttributeEnumerator = TAttributeRec.TAttributeEnumerator;

const
  sENUM = 'enum';
//  sNAME = 'name';
//  sPATH = 'path';
  sSUBRANGE = 'subrange';
//  sTYPE = 'type';
//  sVALUE = 'value';
  sEXTERNAL = 'external';
  sCALLING = 'callingconvention';
//  sDEPRECATED = 'deprecated';
//  sExperimental = 'experimental';
  sBINDING = 'binding';
  s16BIT = '16bit';
  sFORWARD = 'forward';
//  sKIND = 'kind';
//  sVISIBILITY = 'visibility';
//  sLIBRARY = 'library';
//  sTRUE = 'true';
//  sFALSE = 'false';
//  sLINE = 'line';
//  sCOL = 'col';
//  sCLASS = 'class';
  sDELAYED = 'delayed';

implementation

uses
  System.SysUtils,
  System.StrUtils;

{ TAttributeRec }

function Test: Boolean; external 'user32.dll' name 'Test' index 4;

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
  if Index in FAttributes then begin
    Index:= TAttribute(Integer(Index) and $7);
    Result:= FData[index];
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
    Result.Value:= Value[index];
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
    while not(CharInSet(Full[Start], ['0'..'0'])) do Inc(Start);
    //while not(Full[Start] in ['0'..'9']) do Inc(Start);
    Count:= 1;
    while CharInSet(Full[Start+Count], ['0'..'9']) do Inc(Count);
    //while (Full[Start+Count] in ['0'..'9']) do Inc(Count);
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

function TAttributeRec.GetEnumerator: TAttributeEnumerator;
begin
  Result:= TAttributeEnumerator.Create(Self);
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


function TExtHelper.KeyName: string;
begin
  Result:= AttributeNames[Self.Key];
end;

{ TAttributeRec.TAttributeEnumerator }

class function TAttributeRec.TAttributeEnumerator.Create(const
  ARec: TAttributeRec): TAttributeEnumerator;
begin
  //Result.FTestAttributes:= Int64(ARec.FAttributes);
  Result.FRec:= @ARec;
  Result.FIndex:= TAttribute(-1);
end;

function TAttributeRec.TAttributeEnumerator.GetCurrent: TExtAttribute;
begin
  Result:= FRec.Data[FIndex];
end;

function TAttributeRec.TAttributeEnumerator.MoveNext: boolean;
//var
//  LowestBitSet: Int64;
//  lsb: integer;
begin
  while true do begin  //to allow inlining.
    if ShortInt(FIndex) > ShortInt(aLastBoolean) then Exit(false);
    FIndex:= Succ(FIndex);
    if FIndex in FRec.FAttributes then Exit(true);
  end;
  //Does not work...
//  //Extract lowest set bit: (x and -x)
//  //Reset lowest set bit: (x and (x - 1))
//  if FTestAttributes = 0 then Exit(false);
//  LowestBitSet:= FTestAttributes and -FTestAttributes;
//  lsb:= LowestBitSet or LowestBitSet shr 32;
//  FIndex:= TAttribute(Integer(LowestBitset shr 32 <> 0) * 2 +
//           Integer((lsb and $ffff0000) <> 0) * 2  +
//           Integer((lsb and $ff00ff00) <> 0) * 2 +
//           Integer((lsb and $f0f0f0f0) <> 0) * 2 +
//           Integer((lsb and $cccccccc) <> 0) * 2 +
//           Integer((lsb and $aaaaaaaa) <> 0));
//  FTestAttributes:= FTestAttributes and (FTestAttributes - 1);
//  Result:= true;
end;

end.
