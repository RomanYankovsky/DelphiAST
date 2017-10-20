unit DelphiAST;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  SysUtils, Classes, Generics.Collections, SimpleParser, SimpleParser.Lexer,
  SimpleParser.Lexer.Types, DelphiAST.Classes, DelphiAST.Consts;

type
  ESyntaxTreeException = class(EParserException)
  strict private
    FSyntaxTree: TSyntaxNode;
  public
    constructor Create(Line, Col: Integer; const FileName, Msg: string; SyntaxTree: TSyntaxNode); reintroduce;
    destructor Destroy; override;

    property SyntaxTree: TSyntaxNode read FSyntaxTree;
  end;

  TStringEvent = procedure(var s: string) of object;

  TPasLexer = class
  private
    FLexer: TmwPasLex;
    FOnHandleString: TStringEvent;
    function GetToken: string; inline;
    function GetPosXY: TTokenPoint; inline;
    function GetFileName: string; inline;
  public
    constructor Create(const Lexer: TmwPasLex; OnHandleString: TStringEvent);

    property FileName: string read GetFileName;
    property PosXY: TTokenPoint read GetPosXY;
    property Token: string read GetToken;
  end;

  TNodeStack = class
  strict private
    FLexer: TPasLexer;
    FStack: TStack<TSyntaxNode>;

    function GetCount: Integer; inline;
  public
    constructor Create(Lexer: TPasLexer);
    destructor Destroy; override;

    function AddChild(Typ: TSyntaxNodeType): TSyntaxNode; overload;
    function AddChild(Node: TSyntaxNode): TSyntaxNode; overload;
    function AddValuedChild(Typ: TSyntaxNodeType; const Value: string): TSyntaxNode;

    procedure Clear; inline;
    function Peek: TSyntaxNode; inline;
    function Pop: TSyntaxNode; inline;

    function Push(Typ: TSyntaxNodeType): TSyntaxNode; overload;
    function Push(Node: TSyntaxNode): TSyntaxNode; overload;
    function PushCompoundSyntaxNode(Typ: TSyntaxNodeType): TSyntaxNode;
    function PushValuedNode(Typ: TSyntaxNodeType; const Value: string): TSyntaxNode;

    property Count: Integer read GetCount;
  end;

  TPasSyntaxTreeBuilder = class(TmwSimplePasPar)
  private type
    TTreeBuilderMethod = procedure of object;
  private
    procedure BuildExpressionTree(ExpressionMethod: TTreeBuilderMethod);
    procedure BuildParametersList(ParametersListMethod: TTreeBuilderMethod);
    procedure ParserMessage(Sender: TObject; const Typ: TMessageEventType; const Msg: string; X, Y: Integer);
    function NodeListToString(NamesNode: TSyntaxNode): string;
    procedure MoveMembersToVisibilityNodes(TypeNode: TSyntaxNode);
    procedure CallInheritedConstantExpression; inline;
    procedure CallInheritedExpression; inline;
    procedure CallInheritedFormalParameterList; inline;
    procedure CallInheritedPropertyParameterList; inline;
    procedure SetCurrentCompoundNodesEndPosition; inline;
    procedure DoOnComment(Sender: TObject; const Text: string);
    procedure DoHandleString(var s: string); inline;
    function GetMainSection(Node: TSyntaxNode): TSyntaxNode;
  protected
    FStack: TNodeStack;
    FComments: TObjectList<TCommentNode>;
    FLexer: TPasLexer;
    FOnHandleString: TStringEvent;

    procedure AccessSpecifier; override;
    procedure AdditiveOperator; override;
    procedure AddressOp; override;
    procedure AlignmentParameter; override;
    procedure AnonymousMethod; override;
    procedure AnonymousMethodType; override;
    procedure AnonymousMethodTypeFunction; override;
    procedure AnonymousMethodTypeProcedure; override;
    procedure ArrayBounds; override;
    procedure ArrayConstant; override;
    procedure ArrayDimension; override;
    procedure ArrayOfConst; override;
    procedure AsmFragment; override;
    procedure AsmLabelAt; override;
    procedure AsmStatement; override;
    procedure AsmStatements; override;
    procedure AsOp; override;
    procedure AssignOp; override;
    procedure AtExpression; override;
    procedure CaseElseStatement; override;
    procedure CaseLabel; override;
    procedure CaseLabelList; override;
    procedure CaseSelector; override;
    procedure CaseStatement; override;
    procedure ClassClass; override;
    procedure ClassConstraint; override;
    procedure ClassField; override;
    procedure ClassForward; override;
    procedure ClassFunctionHeading; override;
    procedure ClassHelper; override;
    procedure ClassMethod; override;
    procedure ClassMethodResolution; override;
    procedure ClassMethodHeading; override;
    procedure ClassProcedureHeading; override;
    procedure ClassProperty; override;
    procedure ClassReferenceType; override;
    procedure ClassType; override;
    procedure CompilerDirective; override;
    procedure CompoundStatement; override;
    procedure ConstParameter; override;
    procedure ConstantDeclaration; override;
    procedure ConstantExpression; override;
    procedure ConstantName; override;
    procedure ConstraintList; override;
    procedure ConstSection; override;
    procedure ConstantValue; override;
    procedure ConstantValueTyped; override;
    procedure ConstructorConstraint; override;
    procedure ConstructorName; override;
    procedure ContainsClause; override;
    procedure DestructorName; override;
    procedure DirectiveAbstract; override;
    procedure DirectiveBinding; override;
    procedure DirectiveBindingMessage; override;
    procedure DirectiveCalling; override;
    procedure DirectiveDeprecated; override;
    procedure DirectiveExperimental; override;
    procedure DirectiveInline; override;
    procedure DirectiveLibrary; override;
    procedure DirectivePlatform; override;
    procedure DirectiveSealed; override;
    procedure DirectiveVarargs; override;
    procedure DispInterfaceForward; override;
    procedure DoubleAddressOp; override;
    procedure DotOp; override;
    procedure ElseStatement; override;
    procedure EmptyStatement; override;
    procedure EnumeratedType; override;
    procedure ExceptBlock; override;
    procedure ExceptionBlockElseBranch; override;
    procedure ExceptionHandler; override;
    procedure ExceptionVariable; override;
    procedure ExplicitType; override;
    procedure ExportedHeading; override;
    procedure ExportsClause; override;
    procedure ExportsElement; override;
    procedure ExportsName; override;
    procedure ExportsNameId; override;
    procedure Expression; override;
    procedure ExpressionList; override;
    procedure ExternalDirective; override;
    procedure ExternalDependency; override;
    procedure FieldList; override;
    procedure FieldName; override;
    procedure FinalizationSection; override;
    procedure FinallyBlock; override;
    procedure FormalParameterList; override;
    procedure ForStatement; override;
    procedure ForStatementDownTo; override;
    procedure ForStatementFrom; override;
    procedure ForStatementIn; override;
    procedure ForStatementTo; override;
    procedure ForwardDeclaration; override;
    procedure FunctionHeading; override;
    procedure FunctionMethodName; override;
    procedure FunctionProcedureName; override;
    procedure GotoStatement; override;
    procedure IfStatement; override;
    procedure Identifier; override;
    procedure ImplementationSection; override;
    procedure ImplementsSpecifier; override;
    procedure IndexSpecifier; override;
    procedure IndexOp; override;
    procedure InheritedStatement; override;
    procedure InheritedVariableReference; override;
    procedure InitializationSection; override;
    procedure InterfaceForward; override;
    procedure InterfaceGUID; override;
    procedure InterfaceSection; override;
    procedure InterfaceType; override;
    procedure LabelDeclarationSection; override;
    procedure LabeledStatement; override;
    procedure LabelId; override;
    procedure LibraryFile; override;
    procedure MainUsesClause; override;
    procedure MainUsedUnitStatement; override;
    procedure MethodKind; override;
    procedure MultiplicativeOperator; override;
    procedure NameSpecifier; override;
    procedure NotOp; override;
    procedure NilToken; override;
    procedure Number; override;
    procedure ObjectField; override;
    procedure ObjectForward; override;
    procedure ObjectNameOfMethod; override;
    procedure ObjectType; override;
    procedure OutParameter; override;
    procedure PackageFile; override;
    procedure ParameterFormal; override;
    procedure ParameterName; override;
    procedure PointerSymbol; override;
    procedure PointerType; override;
    procedure ProceduralDirectiveOf; override;
    procedure ProceduralType; override;
    procedure ProcedureHeading; override;
    procedure ProcedureDeclarationSection; override;
    procedure ProcedureProcedureName; override;
    procedure ProgramFile; override;
    procedure PropertyName; override;
    procedure PropertyParameterList; override;
    procedure RaiseStatement; override;
    procedure RecordConstraint; override;
    procedure RecordConstant; override;
    procedure RecordFieldConstant; override;
    procedure RecordType; override;
    procedure RecordVariant; override;
    procedure RecordVariantSection; override;
    procedure RecordVariantTag; override;
    procedure RelativeOperator; override;
    procedure RepeatStatement; override;
    procedure ResourceDeclaration; override;
    procedure ResourceValue; override;
    procedure RequiresClause; override;
    procedure RequiresIdentifier; override;
    procedure RequiresIdentifierId; override;
    procedure Resident; override;
    procedure ReturnType; override;
    procedure RoundClose; override;
    procedure RoundOpen; override;
    procedure SetConstructor; override;
    procedure SetElement; override;
    procedure SimpleStatement; override;
    procedure SimpleType; override;
    procedure StatementList; override;
    procedure StorageDefault; override;
    procedure StringConst; override;
    procedure StringConstSimple; override;
    procedure StringStatement; override;
    procedure StructuredType; override;
    procedure SubrangeType; override;
    procedure TagField; override;
    procedure TagFieldTypeName; override;
    procedure ThenStatement; override;
    procedure TryStatement; override;
    procedure TypeArgs; override;
    procedure TypeDeclaration; override;
    procedure TypeId; override;
    procedure TypeParamDecl; override;
    procedure TypeParams; override;
    procedure TypeSection; override;
    procedure TypeSimple; override;
    procedure UnaryMinus; override;
    procedure UnitFile; override;
    procedure UnitName; override;
    procedure UnitId; override;
    procedure UsesClause; override;
    procedure UsedUnitName; override;
    procedure VarAbsolute; override;
    procedure VarDeclaration; override;
    procedure VarName; override;
    procedure VarParameter; override;
    procedure VarSection; override;
    procedure VisibilityAutomated; override;
    procedure VisibilityPrivate; override;
    procedure VisibilityProtected; override;
    procedure VisibilityPublic; override;
    procedure VisibilityPublished; override;
    procedure VisibilityStrictPrivate; override;
    procedure VisibilityStrictProtected; override;
    procedure WhileStatement; override;
    procedure WithExpressionList; override;
    procedure WithStatement; override;

    procedure AttributeSections; override;
    procedure Attribute; override;
    procedure AttributeName; override;
    procedure AttributeArguments; override;
    procedure PositionalArgument; override;
    procedure NamedArgument; override;
    procedure AttributeArgumentName; override;
    procedure AttributeArgumentExpression; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function Run(SourceStream: TStream): TSyntaxNode; reintroduce; overload; virtual;
    class function Run(const FileName: string; InterfaceOnly: Boolean = False;
      IncludeHandler: IIncludeHandler = nil;
      OnHandleString: TStringEvent = nil): TSyntaxNode; reintroduce; overload; static;

    property Comments: TObjectList<TCommentNode> read FComments;
    property Lexer: TPasLexer read FLexer;
    property OnHandleString: TStringEvent read FOnHandleString write FOnHandleString;
  end;

implementation

uses
  TypInfo, StrUtils;

{$IFDEF FPC}
  type
    TStringStreamHelper = class helper for TStringStream
      class function Create: TStringStream; overload;
      procedure LoadFromFile(const FileName: string);
    end;

  { TStringStreamHelper }

  class function TStringStreamHelper.Create: TStringStream;
  begin
    Result := TStringStream.Create('');
  end;

  procedure TStringStreamHelper.LoadFromFile(const FileName: string);
  var
    Strings: TStringList;
  begin
    Strings := TStringList.Create;
    try
      Strings.LoadFromFile(FileName);
      Strings.SaveToStream(Self);
    finally
      FreeAndNil(Strings);
    end;
  end;
{$ENDIF}

// do not use const strings here to prevent allocating new strings every time

type
  TAttributeValue = (atAsm, atTrue, atFunction, atProcedure, atOperator, atClass_Of, atClass,
    atConst, atConstructor, atDestructor, atEnum, atInterface, atNil, atNumeric,
    atOut, atPointer, atName, atString, atSubRange, atVar, atType{ExplicitType},
    atObject, atSealed, atAbstract, atBegin, atOf_Object{procedure of object},
    atVarargs, atExternal{Varargs and external are mutually exclusive},
    atStatic, atAbsolute);

var
  AttributeValues: array[TAttributeValue] of string;

procedure InitAttributeValues;
var
  value: TAttributeValue;
  AttributeStr: string;
begin
  for value := Low(TAttributeValue) to High(TAttributeValue) do begin
    AttributeStr:= Copy(LowerCase(GetEnumName(TypeInfo(TAttributeValue), Ord(value))), 3);
    AttributeStr:= StringReplace(AttributeStr, '_', ' ', [rfReplaceAll]);
    AttributeValues[value] := AttributeStr;
  end;
end;

procedure AssignLexerPositionToNode(const Lexer: TPasLexer; const Node: TSyntaxNode);
begin
  Node.Col := Lexer.PosXY.X;
  Node.Line := Lexer.PosXY.Y;
  Node.FileName := Lexer.FileName;
end;

{ TPasLexer }

constructor TPasLexer.Create(const Lexer: TmwPasLex; OnHandleString: TStringEvent);
begin
  inherited Create;
  FLexer := Lexer;
  FOnHandleString := OnHandleString;
end;

function TPasLexer.GetFileName: string;
begin
  Result := FLexer.Buffer.FileName;
end;

function TPasLexer.GetPosXY: TTokenPoint;
begin
  Result := FLexer.PosXY;
end;

function TPasLexer.GetToken: string;
begin
  SetString(Result, FLexer.Buffer.Buf + FLexer.TokenPos, FLexer.TokenLen);
  FOnHandleString(Result);
end;

{ TNodeStack }

function TNodeStack.AddChild(Typ: TSyntaxNodeType): TSyntaxNode;
begin
  Result := FStack.Peek.AddChild(TSyntaxNode.Create(Typ));
  AssignLexerPositionToNode(FLexer, Result);
end;

function TNodeStack.AddChild(Node: TSyntaxNode): TSyntaxNode;
begin
  Result := FStack.Peek.AddChild(Node);
end;

function TNodeStack.AddValuedChild(Typ: TSyntaxNodeType;
  const Value: string): TSyntaxNode;
begin
  Result := FStack.Peek.AddChild(TValuedSyntaxNode.Create(Typ));
  AssignLexerPositionToNode(FLexer, Result);

  TValuedSyntaxNode(Result).Value := Value;
end;

procedure TNodeStack.Clear;
begin
  FStack.Clear;
end;

constructor TNodeStack.Create(Lexer: TPasLexer);
begin
  FLexer := Lexer;
  FStack := TStack<TSyntaxNode>.Create;
end;

destructor TNodeStack.Destroy;
begin
  FStack.Free;
  inherited;
end;

function TNodeStack.GetCount: Integer;
begin
  Result := FStack.Count;
end;

function TNodeStack.Peek: TSyntaxNode;
begin
  Result := FStack.Peek;
end;

function TNodeStack.Pop: TSyntaxNode;
begin
  Result := FStack.Pop;
end;

function TNodeStack.Push(Node: TSyntaxNode): TSyntaxNode;
begin
  FStack.Push(Node);
  Result := Node;
  AssignLexerPositionToNode(FLexer, Result);
end;

function TNodeStack.PushCompoundSyntaxNode(Typ: TSyntaxNodeType): TSyntaxNode;
begin
  Result := Push(Peek.AddChild(TCompoundSyntaxNode.Create(Typ)));
end;

function TNodeStack.PushValuedNode(Typ: TSyntaxNodeType;
  const Value: string): TSyntaxNode;
begin
  Result := Push(Peek.AddChild(TValuedSyntaxNode.Create(Typ)));
  TValuedSyntaxNode(Result).Value := Value;
end;

function TNodeStack.Push(Typ: TSyntaxNodeType): TSyntaxNode;
begin
  Result := FStack.Peek.AddChild(TSyntaxNode.Create(Typ));
  Push(Result);
end;

{ TPasSyntaxTreeBuilder }

procedure TPasSyntaxTreeBuilder.AccessSpecifier;
begin
  case ExID of
    ptRead:
      FStack.Push(ntRead);
    ptWrite:
      FStack.Push(ntWrite);
    else
      FStack.Push(ntUnknown);
  end;
  try
    inherited AccessSpecifier;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.AdditiveOperator;
begin
  case TokenID of
    ptMinus: FStack.AddChild(ntSub);
    ptOr: FStack.AddChild(ntOr);
    ptPlus: FStack.AddChild(ntAdd);
    ptXor: FStack.AddChild(ntXor);
  end;

  inherited;
end;

procedure TPasSyntaxTreeBuilder.AddressOp;
begin
  FStack.Push(ntAddr);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.AlignmentParameter;
begin
  FStack.Push(ntAlignmentParam);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.AnonymousMethod;
begin
  FStack.Push(ntAnonymousMethod).Attribute[anKind]:= Lexer.Token;  //function or procedure
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.AnonymousMethodType;
begin
  FStack.Push(ntAnonymousMethodType);
  try
    inherited;
  finally
    FStack.Pop
  end;
end;

procedure TPasSyntaxTreeBuilder.AnonymousMethodTypeProcedure;
begin
  FStack.Peek.Attribute[anKind]:= Lexer.Token; //procedure
end;

procedure TPasSyntaxTreeBuilder.AnonymousMethodTypeFunction;
begin
  FStack.Peek.Attribute[anKind]:= Lexer.Token; //function
end;

procedure TPasSyntaxTreeBuilder.ArrayBounds;
begin
  FStack.Push(ntBounds);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ArrayConstant;
begin
  FStack.Push(ntExpressions);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ArrayDimension;
begin
  FStack.Push(ntDimension);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ArrayOfConst;
begin
  //do not fill the name attribute. const is a keyword, not a type.
  FStack.Push(ntType).Attribute[anKind]:= AttributeValues[atConst];
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.AsmFragment;
begin
  FStack.AddValuedChild(ntAsmFragment, Lexer.Token);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.AsmLabelAt;
begin
  FStack.AddValuedChild(ntAsmFragment, Lexer.Token);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.AsmStatement;
var
  Node, Child: TSyntaxNode;
  ValuedNode: TValuedSyntaxNode absolute Node;
  ValuedChild: TValuedSyntaxNode absolute Child;
  Optional: string;
  Previous: char;
begin
  Node:= FStack.PushValuedNode(ntAsmStatement,'');
  try
    inherited;
    Optional:= '';
    Previous:= ' ';
    for Child in Node.ChildNodes do begin
      //Store the whole statement as well as the parts.
      if (ValuedChild.Value[1] in [',', '+', '*', ']', ')', ' ','-',':']) or (Previous in ['(','[',',','+','*','-','@']) then Optional:= '';
      Previous:= ValuedChild.Value[1];
      ValuedNode.Value:= ValuedNode.Value + Optional + ValuedChild.Value;
      Optional:= ' ';
    end;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.AsmStatements;
begin
  FStack.PushCompoundSyntaxNode(ntStatements).Attribute[anKind]:= AttributeValues[atAsm];
  try
    inherited;
    SetCurrentCompoundNodesEndPosition;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.AsOp;
begin
  FStack.AddChild(ntAs);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.AssignOp;
begin
  FStack.AddChild(ntAssign);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.AtExpression;
begin
  FStack.Push(ntAt);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.Attribute;
begin
  FStack.Push(ntAttribute);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.AttributeArgumentExpression;
begin
  FStack.Push(ntValue);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.AttributeArgumentName;
begin
  //FStack.AddValuedChild(ntName, Lexer.Token);
  FStack.AddChild(ntName).Attribute[anName]:= Lexer.Token;
  inherited;
end;

procedure TPasSyntaxTreeBuilder.AttributeArguments;
begin
  FStack.Push(ntArguments);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.AttributeName;
begin
  //FStack.AddValuedChild(ntName, Lexer.Token);
  FStack.AddChild(ntname).Attribute[anName]:= Lexer.Token;
  inherited;
end;

procedure TPasSyntaxTreeBuilder.AttributeSections;
begin
  FStack.Push(ntAttributes);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.BuildExpressionTree(
  ExpressionMethod: TTreeBuilderMethod);
var
  RawExprNode: TSyntaxNode;
  ExprNode: TSyntaxNode;

  NodeList: TList<TSyntaxNode>;
  Node: TSyntaxNode;
  Col, Line: Integer;
  FileName: string;
begin
  Line := Lexer.PosXY.Y;
  Col := Lexer.PosXY.X;
  FileName := Lexer.FileName;

  RawExprNode := TSyntaxNode.Create(ntExpression);
  try
    FStack.Push(RawExprNode);
    try
      ExpressionMethod;
    finally
      FStack.Pop;
    end;

    if RawExprNode.HasChildren then
    begin
      ExprNode := FStack.Push(ntExpression);
      try
        ExprNode.Line := Line;
        ExprNode.Col := Col;
        ExprNode.FileName := FileName;

        NodeList := TList<TSyntaxNode>.Create;
        try
          for Node in RawExprNode.ChildNodes do
            NodeList.Add(Node);
          TExpressionTools.RawNodeListToTree(RawExprNode, NodeList, ExprNode);
        finally
          NodeList.Free;
        end;
      finally
        FStack.Pop;
      end;
    end;
  finally
    RawExprNode.Free;
  end;
end;

//ExtractChild causes corruption of the childlist. need to investigate why.
//procedure TPasSyntaxTreeBuilder.BuildParametersList(
//  ParametersListMethod: TTreeBuilderMethod);
//var
//  Params, Temp: TSyntaxNode;
//  Attributes: TSyntaxNode;
//  ParamList, Param, TypeInfo, ParamExpr: TSyntaxNode;
//  ParamKind: string;
//begin
//  Params := TSyntaxNode.Create(ntUnknown);
//  try
//    FStack.Push(ntParameters);
//
//    FStack.Push(Params);
//    try
//      ParametersListMethod;
//    finally
//      FStack.Pop;
//    end;
//
//    for ParamList in Params.ChildNodes do
//    begin
//      TypeInfo := ParamList.ExtractChild(ntType);
//      ParamKind := ParamList.Attribute[anKind];
//      ParamExpr := ParamList.ExtractChild(ntExpression);
//      Attributes:= ParamList.ExtractChild(ntAttributes);
//
//      for Param in ParamList.ChildNodes do
//      begin
//        if Param.Typ = ntName then begin
//
//          Temp:= FStack.Push(ntParameter);
//          if ParamKind <> '' then Temp.Attribute[anKind]:= ParamKind;
//
//          Temp.Col:= Param.Col;
//          Temp.Line:= Param.Line;
//
//          if Assigned(Attributes) then FStack.AddChild(Attributes);
//
//          FStack.AddChild(Param.Clone);
//          if Assigned(TypeInfo) then FStack.AddChild(TypeInfo);
//
//          if Assigned(ParamExpr) then FStack.AddChild(ParamExpr);
//
//          FStack.Pop;
//        end;
//      end;
//    end;
//    FStack.Pop;
//  finally
//    Params.Free;
//  end;
//end;

procedure TPasSyntaxTreeBuilder.BuildParametersList(
  ParametersListMethod: TTreeBuilderMethod);
var
  Params, Temp, Attributes: TSyntaxNode;
  ParamList, Param, TypeInfo, ParamExpr: TSyntaxNode;
  ParamKind: string;
begin
  Params := TSyntaxNode.Create(ntUnknown);
  try
    FStack.Push(ntParameters);

    FStack.Push(Params);
    try
      ParametersListMethod;
    finally
      FStack.Pop;
    end;

    for ParamList in Params.ChildNodes do
    begin
      TypeInfo := ParamList.FindNode(ntType);
      ParamKind := ParamList.Attribute[anKind];
      ParamExpr := ParamList.FindNode(ntExpression);
      Attributes:= ParamList.FindNode(ntAttributes);

      for Param in ParamList.ChildNodes do
      begin
        if Param.Typ <> ntName then
          Continue;

        Temp := FStack.Push(ntParameter);
        if ParamKind <> '' then
          Temp.Attribute[anKind]:= ParamKind;

        Temp.Col := Param.Col;
        Temp.Line := Param.Line;

        if Assigned(Attributes) then
          FStack.AddChild(Attributes.Clone);

        FStack.AddChild(Param.Clone);
        if Assigned(TypeInfo) then
          FStack.AddChild(TypeInfo.Clone);

        if Assigned(ParamExpr) then
          FStack.AddChild(ParamExpr.Clone);

        FStack.Pop;
      end;
    end;
    FStack.Pop;
  finally
    Params.Free;
  end;
end;

procedure TPasSyntaxTreeBuilder.CaseElseStatement;
begin
  FStack.Push(ntCaseElse);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.CaseLabel;
begin
  FStack.Push(ntCaseLabel);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.CaseLabelList;
begin
  FStack.Push(ntCaseLabels);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.CaseSelector;
begin
  FStack.Push(ntCaseSelector);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.CaseStatement;
begin
  FStack.Push(ntCase);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.RecordVariant;
begin
  FStack.Push(ntRecordVariant);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ClassClass;
begin
  FStack.Peek.Attribute[anClass]:= AttributeValues[atTrue];
  inherited;
end;

procedure TPasSyntaxTreeBuilder.ClassField;
var
  Fields, Temp: TSyntaxNode;
  Field, TypeInfo, TypeArgs: TSyntaxNode;
  IsClassVarSection: boolean;
begin
  IsClassVarSection:= FStack.Peek.HasAttribute(anClass);
  Fields := TSyntaxNode.Create(ntFields);
  try
    FStack.Push(Fields);
    try
      inherited;
    finally
      FStack.Pop;
    end;

    TypeInfo := Fields.FindNode(ntType);
    TypeArgs := Fields.FindNode(ntTypeArgs);
    for Field in Fields.ChildNodes do
    begin
      if Field.Typ <> ntName then
        Continue;

      Temp := FStack.Push(ntField);
      if (IsClassVarSection) then Temp.Attribute[anClass]:= AttributeValues[atTrue];
      try
        Temp.AssignPositionFrom(Field);

        FStack.AddChild(Field.Clone);
        TypeInfo := TypeInfo.Clone;
        if Assigned(TypeArgs) then
          TypeInfo.AddChild(TypeArgs.Clone);
        FStack.AddChild(TypeInfo);
      finally
        FStack.Pop;
      end;
    end;
  finally
    Fields.Free;
  end;
end;

procedure TPasSyntaxTreeBuilder.ObjectField;
var
  Fields, Temp: TSyntaxNode;
  Field, TypeInfo, TypeArgs: TSyntaxNode;
  IsClassVarSection: boolean;
begin
  IsClassVarSection:= FStack.Peek.HasAttribute(anClass);
  Fields := TSyntaxNode.Create(ntFields);
  try
    FStack.Push(Fields);
    try
      inherited;
    finally
      FStack.Pop;
    end;

    TypeInfo := Fields.FindNode(ntType);
    TypeArgs := Fields.FindNode(ntTypeArgs);
    for Field in Fields.ChildNodes do
    begin
      if Field.Typ <> ntName then
        Continue;

      Temp := FStack.Push(ntField);
      if (IsClassVarSection) then Temp.Attribute[anClass]:= AttributeValues[atTrue];
      try
        Temp.AssignPositionFrom(Field);

        FStack.AddChild(Field.Clone);
        TypeInfo := TypeInfo.Clone;
        if Assigned(TypeArgs) then
          TypeInfo.AddChild(TypeArgs.Clone);
        FStack.AddChild(TypeInfo);
      finally
        FStack.Pop;
      end;
    end;
  finally
    Fields.Free;
  end;
end;

procedure TPasSyntaxTreeBuilder.ClassForward;
begin
  FStack.Peek.Attribute[anForwarded]:= AttributeValues[atTrue];
  FStack.Peek.Attribute[anType]:= AttributeValues[atClass];
  inherited;
end;

procedure TPasSyntaxTreeBuilder.ClassFunctionHeading;
begin
  if (FLexer.Token = 'operator') then FStack.Peek.Attribute[anKind]:= AttributeValues[atOperator]
  else FStack.Peek.Attribute[anKind]:= AttributeValues[atFunction];
  inherited;
end;

procedure TPasSyntaxTreeBuilder.ClassHelper;
begin
  FStack.Push(ntHelper);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ClassMethod;
begin
  FStack.Peek.Attribute[anClass]:= AttributeValues[atTrue];
  inherited;
end;

procedure TPasSyntaxTreeBuilder.ClassMethodResolution;
var
  PrevNode: TSyntaxNode;
begin
  PrevNode:= FStack.Peek; //Get the ntMethod node above
  PrevNode.Attribute[anKind]:= FLexer.Token;
  FStack.Push(ntResolutionClause);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ClassMethodHeading;
begin
  FStack.PushCompoundSyntaxNode(ntMethod);
  try
    inherited;
    SetCurrentCompoundNodesEndPosition;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ClassProcedureHeading;
begin
  FStack.Peek.Attribute[anKind]:= AttributeValues[atProcedure];
  inherited;
end;

procedure TPasSyntaxTreeBuilder.ClassProperty;
begin
  FStack.Push(ntProperty);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ClassReferenceType;
begin
  FStack.Push(ntType).Attribute[anType]:= AttributeValues[atClass_of];
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ClassType;
begin
  FStack.Push(ntType).Attribute[anType]:= AttributeValues[atClass];
  try
    inherited;
  finally
    MoveMembersToVisibilityNodes(FStack.Pop);
  end;
end;

procedure TPasSyntaxTreeBuilder.MoveMembersToVisibilityNodes(TypeNode: TSyntaxNode);
var
  child, vis: TSyntaxNode;
  i: Integer;
  extracted: Boolean;
begin
  vis := nil;
  i := 0;
  while i < Length(TypeNode.ChildNodes) do
  begin
    child := TypeNode.ChildNodes[i];
    extracted := false;
    if child.HasAttribute(anVisibility) then
      vis := child
    else if Assigned(vis) then
    begin
      TypeNode.ExtractChild(child);
      vis.AddChild(child);
      extracted := true;
    end;
    if not extracted then
      inc(i);
  end;
end;

procedure TPasSyntaxTreeBuilder.ConstParameter;
begin
  FStack.Push(ntParameters).Attribute[anKind]:= AttributeValues[atConst];
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ConstructorName;
var
  Temp: TSyntaxNode;
begin
  Temp := FStack.Peek;
  Temp.Attribute[anKind]:= AttributeValues[atConstructor];
  Temp.Attribute[anName]:= Lexer.Token;
  inherited;
end;

function TPasSyntaxTreeBuilder.GetMainSection(Node: TSyntaxNode): TSyntaxNode;
var
  Temp: TSyntaxNode;
begin
  If Node.Typ = ntUnknown then begin
    //Get the next item on the stack.
    Temp:= FStack.Pop;
    Node:= FStack.Peek;
    FStack.Push(Temp);
  end;
  if not(Assigned(Node.ParentNode)) then Exit(Node); //return the root node.
  while Assigned(Node.ParentNode.ParentNode) do Node:= Node.ParentNode;
  if (Node.ParentNode.Typ in [ntProgram, ntLibrary, ntPackage]) then Exit(Node.ParentNode);
  Result:= Node;
end;

procedure TPasSyntaxTreeBuilder.CompilerDirective;
var
  Directive: string;
  Node: TValuedSyntaxNode;
  Part2: integer;
  Root: TSyntaxNode;
begin
  Directive:= Uppercase(Lexer.Token);
  //Always place the compiler directive directly under the `ntInterface` or `ntImplementation` node
  //or in the main section in a library, program or package.
  Root:= GetMainSection(FStack.Peek);
  Node:= TValuedSyntaxNode.Create(ntCompilerDirective);
  AssignLexerPositionToNode(Lexer, Node);
  Node.Value:= Directive;
  Root.AddChild(Node);
  //Parse the directive
  if (Directive.StartsWith('(*$')) then begin
    Delete(Directive, 1, 3);
    StringReplace(Directive,'*)','}',[]);
  end else begin
    Delete(Directive, 1, 2);
  end;
  Part2:= 1;
  while not CharInSet(Directive[Part2],[' ', '+', '-', '}']) do begin
    Inc(Part2);
  end;
  Node.Attribute[anType]:= LeftStr(Directive, Part2-1);
  Delete(Directive, 1, Part2-1);
  Delete(Directive, Length(Directive), 1);
  Directive:= Trim(Directive);
  Node.Attribute[anKind]:= Directive;
  inherited;
end;

procedure TPasSyntaxTreeBuilder.CompoundStatement;
begin
  FStack.PushCompoundSyntaxNode(ntStatements).Attribute[anKind]:= AttributeValues[atBegin];
  try
    inherited;
    SetCurrentCompoundNodesEndPosition;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ConstantDeclaration;
begin
  FStack.Push(ntConstant);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ConstantExpression;
var
  ExpressionMethod: TTreeBuilderMethod;
begin
  ExpressionMethod := CallInheritedConstantExpression;
  BuildExpressionTree(ExpressionMethod);
end;

procedure TPasSyntaxTreeBuilder.CallInheritedFormalParameterList;
begin
  inherited FormalParameterList;
end;

procedure TPasSyntaxTreeBuilder.CallInheritedConstantExpression;
begin
  inherited ConstantExpression;
end;

procedure TPasSyntaxTreeBuilder.ConstantName;
begin
  //FStack.AddValuedChild(ntName, Lexer.Token);
  FStack.AddChild(ntName).Attribute[anName]:= Lexer.Token;
  inherited;
end;

procedure TPasSyntaxTreeBuilder.ConstantValue;
begin
  FStack.Push(ntValue);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ConstantValueTyped;
begin
  FStack.Push(ntValue);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ConstraintList;
begin
  FStack.Push(ntConstraints);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ClassConstraint;
begin
  FStack.Push(ntClassConstraint);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ConstructorConstraint;
begin
  FStack.Push(ntConstructorConstraint);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.RecordConstant;
begin
  FStack.Push(ntRecordConstant);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.RecordConstraint;
begin
  FStack.Push(ntRecordConstraint);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ConstSection;
var
  ConstSect, Temp: TSyntaxNode;
  ConstList, Constant, TypeInfo, Value: TSyntaxNode;
begin
  ConstSect := TSyntaxNode.Create(ntConstants);
  try
    FStack.Push(ntConstants).Attribute[anKind]:= Lexer.Token; //resourcestring or const

    FStack.Push(ConstSect);
    try
      inherited;
    finally
      FStack.Pop;
    end;

    for ConstList in ConstSect.ChildNodes do
    begin
      TypeInfo := ConstList.FindNode(ntType);
      Value := ConstList.FindNode(ntValue);
      for Constant in ConstList.ChildNodes do
      begin
        if Constant.Typ <> ntName then
          Continue;

        Temp := FStack.Push(ConstList.Typ);
        try
          Temp.AssignPositionFrom(Constant);

          FStack.AddChild(Constant.Clone);
          if Assigned(TypeInfo) then
            FStack.AddChild(TypeInfo.Clone);
          FStack.AddChild(Value.Clone);
        finally
          FStack.Pop;
        end;
      end;
    end;
    FStack.Pop;
  finally
    ConstSect.Free;
  end;
end;

procedure TPasSyntaxTreeBuilder.ContainsClause;
begin
  FStack.Push(ntContains);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

constructor TPasSyntaxTreeBuilder.Create;
begin
  inherited;
  FLexer := TPasLexer.Create(inherited Lexer, DoHandleString);
  FStack := TNodeStack.Create(FLexer);
  FComments := TObjectList<TCommentNode>.Create(True);

  OnComment := DoOnComment;
end;

procedure TPasSyntaxTreeBuilder.DoHandleString(var s: string);
begin
  if Assigned(FOnHandleString) then
    FOnHandleString(s);
end;

destructor TPasSyntaxTreeBuilder.Destroy;
begin
  FLexer.Free;
  FStack.Free;
  FComments.Free;
  inherited;
end;

procedure TPasSyntaxTreeBuilder.DestructorName;
var
  Temp: TSyntaxNode;
begin
  Temp := FStack.Peek;
  Temp.Attribute[anKind]:= AttributeValues[atDestructor];
  Temp.Attribute[anName]:= Lexer.Token;
  inherited;
end;

procedure TPasSyntaxTreeBuilder.DirectiveAbstract;
begin
  //anAbstract Attribute can contain either 'sealed' or 'abstract' or `final`
  FStack.Peek.Attribute[anAbstract]:= Lexer.Token;
  inherited;
end;

procedure TPasSyntaxTreeBuilder.DirectiveBinding;
var
  Token: string;
begin
  token := Lexer.Token;
  // Method bindings:
  if SameText(Token, 'override') or SameText(token, 'virtual')
    or SameText(Token, 'dynamic') or SameText(Token, 'static')
  then
    FStack.Peek.Attribute[anMethodBinding]:= Token
  // Other directives
  else if SameText(token, 'reintroduce') then
    FStack.Peek.Attribute[anReintroduce]:= AttributeValues[atTrue]
  else if SameText(Token, 'overload') then
    FStack.Peek.Attribute[anOverload]:=  AttributeValues[atTrue]
  else if SameText(Token, 'abstract') or SameText(Token, 'final') then
    FStack.Peek.Attribute[anAbstract]:= Token;

  inherited;
end;

procedure TPasSyntaxTreeBuilder.DirectiveBindingMessage;
begin
  //message is a method binding directive, for correctness we should record this.
  FStack.Peek.Attribute[anMethodBinding]:= 'message';
  FStack.Push(ntMessage);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.DirectiveCalling;
begin
  FStack.Peek.Attribute[anCallingConvention]:= Lexer.Token;
  inherited;
end;

procedure TPasSyntaxTreeBuilder.DirectiveDeprecated;
begin
  FStack.Push(ntDeprecated);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.DirectiveExperimental;
begin
  FStack.Push(ntExperimental);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.DirectiveInline;
begin
  //'inline' and 'assembler' are both inline directives.
  FStack.Peek.Attribute[anInline]:= Lexer.Token;
  inherited;
end;

procedure TPasSyntaxTreeBuilder.DirectiveLibrary;
begin
  FStack.Push(ntLibrary);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.DirectivePlatForm;
begin
  FStack.Push(ntPlatform);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.DirectiveSealed;
begin
  //hack, must go to a better attributeType, however sealed, abstract cannot coexist
  //Perhaps sealed, abstract and final should all share a attribute type called anInheritance
  FStack.Peek.Attribute[anAbstract]:= Lexer.Token;
  inherited;
end;

procedure TPasSyntaxTreeBuilder.DirectiveVarargs;
begin
  FStack.Push(ntExternal).Attribute[anKind]:= AttributeValues[atVarArgs];
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.DispInterfaceForward;
begin
  FStack.Peek.Attribute[anForwarded]:= AttributeValues[atTrue];
  inherited;
end;

procedure TPasSyntaxTreeBuilder.DotOp;
begin
  FStack.AddChild(ntDot);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.DoubleAddressOp;
begin
  FStack.Push(ntDoubleAddr);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ElseStatement;
begin
  FStack.Push(ntElse);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.EmptyStatement;
begin
  FStack.Push(ntEmptyStatement);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.EnumeratedType;
var
  TypeNode: TSyntaxNode;
begin
  TypeNode := FStack.Push(ntType);
  try
    TypeNode.Attribute[anType]:= AttributeValues[atEnum];
    if ScopedEnums then
      TypeNode.Attribute[anVisibility]:= 'scoped';
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ExceptBlock;
begin
  FStack.Push(ntExcept);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ExceptionBlockElseBranch;
begin
  FStack.Push(ntExceptElse); //#223
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ExceptionHandler;
begin
  FStack.Push(ntExceptionHandler);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ExceptionVariable;
begin
  FStack.Push(ntVariable);
  //FStack.AddValuedChild(ntName, Lexer.Token);
  FStack.AddChild(ntName).Attribute[anName]:= Lexer.Token;
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ExplicitType;  //#220+#181
begin
  FStack.Peek.Attribute[anKind]:= AttributeValues[atType];
  inherited;
end;

procedure TPasSyntaxTreeBuilder.ExportedHeading;
begin
  FStack.PushCompoundSyntaxNode(ntMethod);
  try
    inherited;
    SetCurrentCompoundNodesEndPosition;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ExportsClause;
begin
  FStack.Push(ntExports);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ExportsElement;
begin
  FStack.Push(ntElement);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ExportsName;
var
  NamesNode: TSyntaxNode;
begin
  NamesNode := TSyntaxNode.Create(ntUnknown);
  try
    FStack.Push(NamesNode);
    try
      inherited;
    finally
      FStack.Pop;
    end;

    FStack.Peek.Attribute[anName]:= NodeListToString(NamesNode);
  finally
    NamesNode.Free;
  end;
end;

procedure TPasSyntaxTreeBuilder.ExportsNameId;
begin
  FStack.AddChild(ntUnknown).Attribute[anName]:= Lexer.Token;
  inherited;
end;

procedure TPasSyntaxTreeBuilder.Expression;
var
  ExpressionMethod: TTreeBuilderMethod;
begin
  ExpressionMethod := CallInheritedExpression;
  BuildExpressionTree(ExpressionMethod);
end;

procedure TPasSyntaxTreeBuilder.SetCurrentCompoundNodesEndPosition;
var
  Temp: TCompoundSyntaxNode;
begin
  Temp := TCompoundSyntaxNode(FStack.Peek);
  Temp.EndCol := Lexer.PosXY.X;
  Temp.EndLine := Lexer.PosXY.Y;
  Temp.FileName := Lexer.FileName;
end;

procedure TPasSyntaxTreeBuilder.CallInheritedExpression;
begin
  inherited Expression;
end;

procedure TPasSyntaxTreeBuilder.CallInheritedPropertyParameterList;
begin
  inherited PropertyParameterList;
end;

procedure TPasSyntaxTreeBuilder.ExpressionList;
begin
  FStack.Push(ntExpressions);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ExternalDependency;
begin
  FStack.Push(ntDependency);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ExternalDirective;
begin
  FStack.Push(ntExternal).Attribute[anKind]:= AttributeValues[atExternal];
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.FieldList;
var
  Fields, Temp: TSyntaxNode;
  Field, TypeInfo, TypeArgs: TSyntaxNode;
  IsClassVarSection: boolean;
begin
  IsClassVarSection:= FStack.Peek.HasAttribute(anClass);
  Fields := TSyntaxNode.Create(ntFields);
  try
    FStack.Push(Fields);
    try
      inherited;
    finally
      FStack.Pop;
    end;

    TypeInfo := Fields.FindNode(ntType);
    TypeArgs := Fields.FindNode(ntTypeArgs);
    for Field in Fields.ChildNodes do
    begin
      if Field.Typ <> ntName then
        Continue;

      Temp := FStack.Push(ntField);
      if (IsClassVarSection) then Temp.Attribute[anClass]:= AttributeValues[atTrue];
      try
        Temp.AssignPositionFrom(Field);

        FStack.AddChild(Field.Clone);
        TypeInfo := TypeInfo.Clone;
        if Assigned(TypeArgs) then
          TypeInfo.AddChild(TypeArgs.Clone);
        FStack.AddChild(TypeInfo);
      finally
        FStack.Pop;
      end;
    end;
  finally
    Fields.Free;
  end;
end;

procedure TPasSyntaxTreeBuilder.FieldName;
begin
  //FStack.AddValuedChild(ntName, Lexer.Token);
  FStack.AddChild(ntname).Attribute[anName]:= Lexer.Token;
  inherited;
end;

procedure TPasSyntaxTreeBuilder.FinalizationSection;
begin
  FStack.PushCompoundSyntaxNode(ntFinalization);
  try
    inherited;
    SetCurrentCompoundNodesEndPosition;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.FinallyBlock;
begin
  FStack.Push(ntFinally);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.FormalParameterList;
var
  TreeBuilderMethod: TTreeBuilderMethod;
begin
  TreeBuilderMethod := CallInheritedFormalParameterList;
  BuildParametersList(TreeBuilderMethod);
end;

procedure TPasSyntaxTreeBuilder.ForStatement;
begin
  FStack.Push(ntFor);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ForStatementDownTo;
begin
  FStack.Push(ntDownTo);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ForStatementFrom;
begin
  FStack.Push(ntFrom);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ForStatementIn;
begin
  FStack.Push(ntIn);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ForStatementTo;
begin
  FStack.Push(ntTo);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ForwardDeclaration;
begin
  if FStack.Peek.ParentNode.Typ = ntImplementation then begin  //#166
    FStack.Peek.Attribute[anForwarded]:= AttributeValues[atTrue];
  end;
  inherited;
end;

procedure TPasSyntaxTreeBuilder.FunctionHeading;
begin
  FStack.Peek.Attribute[anKind]:= AttributeValues[atFunction];
  inherited;
end;

procedure TPasSyntaxTreeBuilder.FunctionMethodName;
begin
  //FStack.AddValuedChild(ntName, Lexer.Token);
  FStack.AddChild(ntName).Attribute[anName]:= Lexer.Token;
  inherited;
end;

procedure TPasSyntaxTreeBuilder.FunctionProcedureName;   //#221  record method type params explicitly, keep the full name as well.
var
  ChildNode, NameNode, TypeParam: TSyntaxNode;
  FullName, Dot, Comma: string;
begin
  //Temp:= FStack.Peek;
  NameNode:= FStack.Push(ntName);
  try
    inherited;
  finally
    FStack.Pop;
  end;
  //Traverse the name node and reconstruct the full name
  Assert(NameNode.HasChildren);
  Dot:= '';
  for ChildNode in NameNode.ChildNodes do begin
    case ChildNode.Typ of
      ntName: begin
        FullName:= Fullname + Dot + ChildNode.Attribute[anName];
        Dot:= '.';
      end; {ntName}
      ntTypeParams: begin
        Comma:= '';
        Fullname:= Fullname + '<';
        for TypeParam in ChildNode.ChildNodes do begin
          FullName:= FullName + Comma + TypeParam.FindNode(ntType).Attribute[anName];
          Comma:= ',';
        end; {for}
        Fullname:= Fullname + '>';
      end; {ntTypeParams:}
    end; {case}
  end; {for ChildNode}
  NameNode.Attribute[anName]:= FullName;
end;
procedure TPasSyntaxTreeBuilder.GotoStatement;
begin
  FStack.Push(ntGoto);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.Identifier;
begin
  FStack.AddChild(ntIdentifier).Attribute[anName]:= Lexer.Token;
  inherited;
end;

procedure TPasSyntaxTreeBuilder.IfStatement;
begin
  FStack.Push(ntIf);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ImplementationSection;
begin
  FStack.PushCompoundSyntaxNode(ntImplementation);
  try
    inherited;
    SetCurrentCompoundNodesEndPosition;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ImplementsSpecifier;
begin
  FStack.Push(ntImplements);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.IndexOp;
begin
  FStack.Push(ntIndexed);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.IndexSpecifier;
begin
  FStack.Push(ntIndex);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.InheritedStatement;
begin
  FStack.Push(ntInherited);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.InheritedVariableReference;
begin
  FStack.Push(ntInherited);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.InitializationSection;
begin
  FStack.PushCompoundSyntaxNode(ntInitialization);
  try
    inherited;
    SetCurrentCompoundNodesEndPosition;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.InterfaceForward;
begin
  FStack.Peek.Attribute[anForwarded]:= AttributeValues[atTrue];
  inherited InterfaceForward;
end;

procedure TPasSyntaxTreeBuilder.InterfaceGUID;
begin
  FStack.Push(ntGuid);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.InterfaceSection;
begin
  FStack.PushCompoundSyntaxNode(ntInterface);
  try
    inherited;
    SetCurrentCompoundNodesEndPosition;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.InterfaceType;
begin
  FStack.Push(ntType).Attribute[anType]:= AttributeValues[atInterface];
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.LabeledStatement;
var
  Node, Name: TSyntaxNode;
begin
  Node:= FStack.Push(ntLabeledStatement);
  Name:= Node.AddChild(ntName).AssignPositionFrom(Node);
  Name.Attribute[anName]:= Lexer.Token;
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.LabelDeclarationSection;
begin
  FStack.Push(ntLabel);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.LabelId;
var
  Node, Name: TSyntaxNode;
begin
  FStack.Push(ntName).Attribute[anName]:= Lexer.Token;
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.LibraryFile;
begin
  //Assert(FStack.Peek.ParentNode = nil);
  FStack.Push(TSyntaxNode.Create(ntLibrary));
  AssignLexerPositionToNode(Lexer, FStack.Peek);
  inherited;
  //Stack.pop is done in `Run`
end;

procedure TPasSyntaxTreeBuilder.MainUsedUnitStatement;
var
  NameNode, PathNode, PathLiteralNode, Temp: TSyntaxNode;
begin
  FStack.Push(ntUnit);
  try
    inherited;

    NameNode := FStack.Peek.FindNode(ntUnit);

    if Assigned(NameNode) then
    begin
      Temp := FStack.Peek;
      Temp.Attribute[anName]:= NameNode.Attribute[anName];
      Temp.DeleteChild(NameNode);
    end;

    PathNode := FStack.Peek.FindNode(ntExpression);
    if Assigned(PathNode) then
    begin
      FStack.Peek.ExtractChild(PathNode);
      try
        PathLiteralNode := PathNode.FindNode(ntLiteral);

        if PathLiteralNode is TValuedSyntaxNode then
          FStack.Peek.Attribute[anPath]:= TValuedSyntaxNode(PathLiteralNode).Value;
      finally
        PathNode.Free;
      end;
    end;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.MainUsesClause;
begin
  FStack.PushCompoundSyntaxNode(ntUses);
  try
    inherited;
    SetCurrentCompoundNodesEndPosition;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.MethodKind;
var
  value: string;
begin
  DoHandleString(value);
  FStack.Peek.Attribute[anKind]:= Lexer.Token;
  inherited;
end;

procedure TPasSyntaxTreeBuilder.MultiplicativeOperator;
begin
  case TokenID of
    ptAnd:
      FStack.AddChild(ntAnd);
    ptDiv:
      FStack.AddChild(ntDiv);
    ptMod:
      FStack.AddChild(ntMod);
    ptShl:
      FStack.AddChild(ntShl);
    ptShr:
      FStack.AddChild(ntShr);
    ptSlash:
      FStack.AddChild(ntFDiv);
    ptStar:
      FStack.AddChild(ntMul);
    else
      FStack.AddChild(ntUnknown);
  end;

  inherited;
end;

procedure TPasSyntaxTreeBuilder.NamedArgument;
begin
  FStack.Push(ntNamedArgument);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.NameSpecifier;
begin
  FStack.Push(ntExternalName);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.NilToken;
var
  Node: TSyntaxNode;
begin
  Node:= FStack.AddValuedChild(ntLiteral, AttributeValues[atNil]);
  Node.Attribute[anType]:= AttributeValues[atNil];
  inherited;
end;

procedure TPasSyntaxTreeBuilder.NotOp;
begin
  FStack.AddChild(ntNot);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.Number;
var
  Node: TSyntaxNode;
begin
  Node := FStack.AddValuedChild(ntLiteral, Lexer.Token);
  Node.Attribute[anType]:= AttributeValues[atNumeric];
  inherited;
end;

procedure TPasSyntaxTreeBuilder.ObjectForward;
begin
  FStack.Peek.Attribute[anForwarded]:= AttributeValues[atTrue];
  FStack.Peek.Attribute[anType]:= AttributeValues[atObject];
  inherited;
end;

procedure TPasSyntaxTreeBuilder.ObjectNameOfMethod;
begin
  //FStack.AddValuedChild(ntName, Lexer.Token);
  FStack.AddChild(ntName).Attribute[anName]:= Lexer.Token;
  inherited;
end;

procedure TPasSyntaxTreeBuilder.ObjectType;
begin
  FStack.Push(ntType).Attribute[anType]:= AttributeValues[atObject];
  try
    inherited;
  finally
    MoveMembersToVisibilityNodes(FStack.Pop);
  end;
end;

procedure TPasSyntaxTreeBuilder.DoOnComment(Sender: TObject; const Text: string);
var
  Node: TCommentNode;
begin
  case TokenID of
    ptAnsiComment: Node := TCommentNode.Create(ntAnsiComment);
    ptBorComment: Node := TCommentNode.Create(ntAnsiComment);
    ptSlashesComment: Node := TCommentNode.Create(ntSlashesComment);
  else
    raise EParserException.Create(Lexer.PosXY.Y, Lexer.PosXY.X, Lexer.FileName, 'Invalid comment type');
  end;

  AssignLexerPositionToNode(Lexer, Node);
  Node.Text := Text;

  FComments.Add(Node);
end;

procedure TPasSyntaxTreeBuilder.ParserMessage(Sender: TObject;
  const Typ: TMessageEventType; const Msg: string; X, Y: Integer);
begin
  if Typ = TMessageEventType.meError then
    raise EParserException.Create(Y, X, Lexer.FileName, Msg);
end;

procedure TPasSyntaxTreeBuilder.OutParameter;
begin
  FStack.Push(ntParameters).Attribute[anKind]:= AttributeValues[atOut];
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.PackageFile;
begin
  //Assert(FStack.Peek.ParentNode = nil);
  FStack.Push(TSyntaxNode.Create(ntPackage));
  AssignLexerPositionToNode(Lexer, FStack.Peek);
  inherited;
  //Stack.pop is done in `Run`
end;

procedure TPasSyntaxTreeBuilder.ParameterFormal;
begin
  FStack.Push(ntParameters);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ParameterName;
begin
  //FStack.AddValuedChild(ntName, Lexer.Token);
  FStack.AddChild(ntname).Attribute[anName]:= Lexer.Token;
  inherited;
end;

procedure TPasSyntaxTreeBuilder.PointerSymbol;
begin
  FStack.AddChild(ntDeref);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.PointerType;
begin
  FStack.Push(ntType).Attribute[anType]:= AttributeValues[atPointer];
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.PositionalArgument;
begin
  FStack.Push(ntPositionalArgument);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ProceduralDirectiveOf;
begin
  //anType is already used for set/enum/subrange/class/record/interface/object.
  //It could be reused for this data, but it's a directive, not a type as such.
  //And it's to close to `object` proper.
  //It should not be a subnode, because only 'of object' is allowed.
  FStack.Peek.Attribute[anKind]:= AttributeValues[atOf_Object];
  inherited;
end;

procedure TPasSyntaxTreeBuilder.ProceduralType;
begin
  //procedure/function is a reserved word, so it cannot be the same as an identifier.
  FStack.Push(ntType).Attribute[anType]:= Lexer.Token;
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ProcedureDeclarationSection;
begin
  FStack.PushCompoundSyntaxNode(ntMethod);
  try
    inherited;
    SetCurrentCompoundNodesEndPosition;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ProcedureHeading;
begin
  FStack.Peek.Attribute[anKind]:= AttributeValues[atProcedure];
  inherited;
end;

procedure TPasSyntaxTreeBuilder.ProcedureProcedureName;
begin
  //FStack.Peek.Attribute[anName, Lexer.Token);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.ProgramFile;
begin
  //Assert(FStack.Peek.ParentNode = nil);
  FStack.Push(TSyntaxNode.Create(ntProgram));
  AssignLexerPositionToNode(Lexer, FStack.Peek);
  inherited;
  //Stack.pop is done in `Run`
end;

procedure TPasSyntaxTreeBuilder.PropertyName;
begin
  FStack.Peek.Attribute[anName]:= Lexer.Token;
  inherited PropertyName;
end;

procedure TPasSyntaxTreeBuilder.PropertyParameterList;
var
  TreeBuilderMethod: TTreeBuilderMethod;
begin
  TreeBuilderMethod := CallInheritedPropertyParameterList;
  BuildParametersList(TreeBuilderMethod);
end;

procedure TPasSyntaxTreeBuilder.RaiseStatement;
begin
  FStack.Push(ntRaise);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.RecordFieldConstant;
begin
  //A field in a record constant should have exactly the same layout
  //as a field in a class.
  //ntField (class)
  //+-- ntName (anName = name)
  //+-- ntType
  //Recordconstant
  //ntField (recordconstant)
  //+-- ntName
  //+-- ntExpression.
  FStack.Push(ntField).AddChild(ntName).Attribute[anName]:= Lexer.Token;
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.RecordType;
begin
  inherited RecordType;
  MoveMembersToVisibilityNodes(FStack.Peek);
end;

procedure TPasSyntaxTreeBuilder.RelativeOperator;
begin
  case TokenID of
    ptAs:
      FStack.AddChild(ntAs);
    ptEqual:
      FStack.AddChild(ntEqual);
    ptGreater:
      FStack.AddChild(ntGreater);
    ptGreaterEqual:
      FStack.AddChild(ntGreaterEqual);
    ptIn:
      FStack.AddChild(ntIn);
    ptIs:
      FStack.AddChild(ntIs);
    ptLower:
      FStack.AddChild(ntLower);
    ptLowerEqual:
      FStack.AddChild(ntLowerEqual);
    ptNotEqual:
      FStack.AddChild(ntNotEqual);
    else
      FStack.AddChild(ntUnknown);
  end;

  inherited;
end;

procedure TPasSyntaxTreeBuilder.RepeatStatement;
begin
  FStack.Push(ntRepeat);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.RequiresClause;
begin
  FStack.Push(ntRequires);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.RequiresIdentifier;
var
  NamesNode: TSyntaxNode;
begin
  NamesNode := TSyntaxNode.Create(ntUnknown);
  try
    FStack.Push(NamesNode);
    try
      inherited;
    finally
      FStack.Pop;
    end;

    FStack.AddChild(ntPackage).Attribute[anName]:= NodeListToString(NamesNode);
  finally
    NamesNode.Free;
  end;
end;

procedure TPasSyntaxTreeBuilder.RequiresIdentifierId;
begin
  FStack.AddChild(ntUnknown).Attribute[anName]:= Lexer.Token;
  inherited;
end;

procedure TPasSyntaxTreeBuilder.Resident;
begin
  FStack.Push(ntResident);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ResourceDeclaration;
begin
  FStack.Push(ntResourceString);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ResourceValue;
begin
  FStack.Push(ntValue);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ReturnType;
begin
  FStack.Push(ntReturnType);
  try
    inherited;
  finally
    FStack.Pop
  end;
end;

procedure TPasSyntaxTreeBuilder.RoundClose;
begin
  FStack.AddChild(ntRoundClose);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.RoundOpen;
begin
  FStack.AddChild(ntRoundOpen);
  inherited;
end;

class function TPasSyntaxTreeBuilder.Run(const FileName: string;
  InterfaceOnly: Boolean; IncludeHandler: IIncludeHandler;
  OnHandleString: TStringEvent): TSyntaxNode;
var
  Stream: TStringStream;
  Builder: TPasSyntaxTreeBuilder;
begin
  Stream := TStringStream.Create;
  try
    Stream.LoadFromFile(FileName);
    Builder := TPasSyntaxTreeBuilder.Create;
    Builder.InterfaceOnly := InterfaceOnly;
    Builder.OnHandleString := OnHandleString;
    try
      Builder.InitDefinesDefinedByCompiler;
      Builder.IncludeHandler := IncludeHandler;
      Result := Builder.Run(Stream);
    finally
      Builder.Free;
    end;
  finally
    Stream.Free;
  end;
end;

function TPasSyntaxTreeBuilder.Run(SourceStream: TStream): TSyntaxNode;
begin
  Result:= nil;
  try
    FStack.Clear;
    try
      self.OnMessage := ParserMessage;
      inherited Run('', SourceStream);
    finally
      Result:= FStack.Peek;
      FStack.Pop;
    end;
  except
    on E: EParserException do
      raise ESyntaxTreeException.Create(E.Line, E.Col, Lexer.FileName, E.Message, Result);
    on E: ESyntaxError do
      raise ESyntaxTreeException.Create(E.PosXY.X, E.PosXY.Y, Lexer.FileName, E.Message, Result);
    else
      FreeAndNil(Result);
      raise;
  end;

  Assert(FStack.Count = 0);
end;

function TPasSyntaxTreeBuilder.NodeListToString(NamesNode: TSyntaxNode): string;
var
  NamePartNode: TSyntaxNode;
begin
  Result := '';
  for NamePartNode in NamesNode.ChildNodes do
  begin
    //do not add empty parts (in case non-name and name node are mixed.
    if (Result <> '') then
      Result := Result + '.';
    Result:= Result + NamePartNode.Attribute[anName];
  end;
  DoHandleString(Result);
end;

procedure TPasSyntaxTreeBuilder.SetConstructor;
begin
  FStack.Push(ntSet);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.SetElement;
begin
  FStack.Push(ntElement);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.SimpleStatement;
var
  RawStatement, Temp: TSyntaxNode;
  Node, LHS, RHS: TSyntaxNode;
  NodeList: TList<TSyntaxNode>;
  I, AssignIdx: Integer;
  Position: TTokenPoint;
  FileName: string;
begin
  Position := Lexer.PosXY;
  FileName := Lexer.FileName;

  RawStatement := TSyntaxNode.Create(ntStatement);
  try
    FStack.Push(RawStatement);
    try
      inherited;
    finally
      FStack.Pop;
    end;

    if not RawStatement.HasChildren then
      Exit;

    if RawStatement.FindNode(ntAssign) <> nil then
    begin
      Temp := FStack.Push(ntAssign);
      try
        Temp.Col := Position.X;
        Temp.Line := Position.Y;
        Temp.FileName := FileName;

        NodeList := TList<TSyntaxNode>.Create;
        try
          AssignIdx := -1;
          for I := 0 to Length(RawStatement.ChildNodes) - 1 do
          begin
            if RawStatement.ChildNodes[I].Typ = ntAssign then
            begin
              AssignIdx := I;
              Break;
            end;
            NodeList.Add(RawStatement.ChildNodes[I]);
          end;

          if NodeList.Count = 0 then
            raise EParserException.Create(Position.Y, Position.X, Lexer.FileName, 'Illegal expression');

          LHS := FStack.AddChild(ntLHS);
          LHS.AssignPositionFrom(NodeList[0]);

          TExpressionTools.RawNodeListToTree(RawStatement, NodeList, LHS);

          NodeList.Clear;

          for I := AssignIdx + 1 to Length(RawStatement.ChildNodes) - 1 do
            NodeList.Add(RawStatement.ChildNodes[I]);

          if NodeList.Count = 0 then
            raise EParserException.Create(Position.Y, Position.X, Lexer.FileName, 'Illegal expression');

          RHS := FStack.AddChild(ntRHS);
          RHS.AssignPositionFrom(NodeList[0]);

          TExpressionTools.RawNodeListToTree(RawStatement, NodeList, RHS);
        finally
          NodeList.Free;
        end;
      finally
        FStack.Pop;
      end;
    end else
    begin
      Temp := FStack.Push(ntCall);
      try
        Temp.Col := Position.X;
        Temp.Line := Position.Y;

        NodeList := TList<TSyntaxNode>.Create;
        try
          for Node in RawStatement.ChildNodes do
            NodeList.Add(Node);
          TExpressionTools.RawNodeListToTree(RawStatement, NodeList, FStack.Peek);
        finally
          NodeList.Free;
        end;
      finally
        FStack.Pop;
      end;
    end;
  finally
    RawStatement.Free;
  end;
end;

procedure TPasSyntaxTreeBuilder.SimpleType;
begin
  FStack.Push(ntType).Attribute[anName]:= Lexer.Token;
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.StatementList;
begin
  FStack.PushCompoundSyntaxNode(ntStatements);
  try
    inherited;
    SetCurrentCompoundNodesEndPosition;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.StorageDefault;
begin
  FStack.Push(ntDefault);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.StringConst;
var
  StrConst: TSyntaxNode;
  Literal, Node: TSyntaxNode;
  Str: string;
begin
  StrConst := TSyntaxNode.Create(ntUnknown);
  try
    FStack.Push(StrConst);
    try
      inherited;
    finally
      FStack.Pop;
    end;

    Str := '';
    for Literal in StrConst.ChildNodes do
      Str := Str + TValuedSyntaxNode(Literal).Value;
  finally
    StrConst.Free;
  end;

  DoHandleString(Str);
  Node := FStack.AddValuedChild(ntLiteral, Str);
  Node.Attribute[anType]:= AttributeValues[atString];
end;

procedure TPasSyntaxTreeBuilder.StringConstSimple;
begin
  //TODO support ptAsciiChar
  FStack.AddValuedChild(ntLiteral, {AnsiDequotedStr(}Lexer.Token{, '''')});
  inherited;
end;

procedure TPasSyntaxTreeBuilder.StringStatement;
begin
  FStack.AddChild(ntType).Attribute[anName]:= Lexer.Token;
  inherited;
end;

procedure TPasSyntaxTreeBuilder.StructuredType;
begin
  FStack.Push(ntType).Attribute[anType]:= Lexer.Token;
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.SubrangeType;
begin
  FStack.Push(ntType).Attribute[anType]:= AttributeValues[atSubRange];
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.TagField;
var
  TagNode: TSyntaxNode;
  TypeNode: TSyntaxNode;
begin
  TagNode:= FStack.Push(ntCaseSelector);
  TagNode.Attribute[anKind]:= Lexer.Token;
  try
    inherited;
    TypeNode:= FStack.Peek.FindNode(ntIdentifier);
    if (Assigned(TypeNode)) then begin
      //move the name to the correct pos.
      TagNode.Attribute[anName]:= TagNode.Attribute[anKind];
      //Fill in the type of te node
      TagNode.Attribute[anKind]:= TypeNode.Attribute[anKind];
      TagNode.DeleteChild(TypeNode);
    end;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.TagFieldTypeName;
begin
  FStack.Push(ntIdentifier).Attribute[anKind]:= Lexer.Token;
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ThenStatement;
begin
  FStack.Push(ntThen);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.TryStatement;
begin
  FStack.Push(ntTry);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.TypeArgs;
begin
  FStack.Push(ntTypeArgs);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.TypeDeclaration;
begin
  FStack.PushCompoundSyntaxNode(ntTypeDecl).Attribute[anName]:= Lexer.Token;
  try
    inherited;
    SetCurrentCompoundNodesEndPosition;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.TypeId;
var
  TypeNode, InnerTypeNode, SubNode: TSyntaxNode;
  TypeName, InnerTypeName: string;
  i: integer;
begin
  TypeNode := FStack.Push(ntType);
  try
    inherited;

    InnerTypeName := '';
    InnerTypeNode := TypeNode.FindNode(ntType);
    if Assigned(InnerTypeNode) then
    begin
      InnerTypeName := InnerTypeNode.Attribute[anName];
      for SubNode in InnerTypeNode.ChildNodes do
        TypeNode.AddChild(SubNode.Clone);

      TypeNode.DeleteChild(InnerTypeNode);
    end;

    TypeName := '';
    for i := Length(TypeNode.ChildNodes) - 1 downto 0 do
    begin
      SubNode := TypeNode.ChildNodes[i];
      if SubNode.Typ = ntType then
      begin
        if TypeName <> '' then
          TypeName := '.' + TypeName;

        TypeName := SubNode.Attribute[anName] + TypeName;
        TypeNode.DeleteChild(SubNode);
      end;
    end;

    if TypeName <> '' then
      TypeName := '.' + TypeName;
    TypeName := InnerTypeName + TypeName;

    DoHandleString(TypeName);
    TypeNode.Attribute[anName]:= TypeName;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.TypeParamDecl;
var
  OriginTypeParamNode, NewTypeParamNode, Constraints, TypeNode: TSyntaxNode;
  TypeNodeCount: integer;
  TypeNodesToDelete: TList<TSyntaxNode>;
begin
  OriginTypeParamNode := FStack.Push(ntTypeParam);
  try
    inherited;
  finally
    FStack.Pop;
  end;

  Constraints := OriginTypeParamNode.FindNode(ntConstraints);
  TypeNodeCount := 0;
  TypeNodesToDelete := TList<TSyntaxNode>.Create;
  try
    for TypeNode in OriginTypeParamNode.ChildNodes do
    begin
      if TypeNode.Typ = ntType then
      begin
        inc(TypeNodeCount);
        if TypeNodeCount > 1 then
        begin
          NewTypeParamNode := FStack.Push(ntTypeParam);
          try
            NewTypeParamNode.AddChild(TypeNode.Clone);
            if Assigned(Constraints) then
              NewTypeParamNode.AddChild(Constraints.Clone);
            TypeNodesToDelete.Add(TypeNode);
          finally
            FStack.Pop;
          end;
        end;
      end;
    end;

    for TypeNode in TypeNodesToDelete do
      OriginTypeParamNode.DeleteChild(TypeNode);
  finally
    TypeNodesToDelete.Free;
  end;
end;

procedure TPasSyntaxTreeBuilder.TypeParams;
begin
  FStack.Push(ntTypeParams);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.TypeSection;
begin
  FStack.Push(ntTypeSection);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.TypeSimple;
begin
  FStack.Push(ntType).Attribute[anName]:= Lexer.Token;
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.UnaryMinus;
begin
  FStack.AddChild(ntUnaryMinus);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.UnitFile;
begin
  //Assert(FStack.Peek.ParentNode = nil);
  FStack.Push(TSyntaxNode.Create(ntUnit));
  AssignLexerPositionToNode(Lexer, FStack.Peek);
  inherited;
  //Stack.pop is done in `Run`
end;

procedure TPasSyntaxTreeBuilder.UnitId;
begin
  FStack.AddChild(ntUnknown).Attribute[anName]:= Lexer.Token;
  inherited;
end;

procedure TPasSyntaxTreeBuilder.UnitName;
var
  NamesNode: TSyntaxNode;
begin
  NamesNode := TSyntaxNode.Create(ntUnknown);
  try
    FStack.Push(NamesNode);
    try
      inherited;
    finally
      FStack.Pop;
    end;

    FStack.Peek.Attribute[anName]:= NodeListToString(NamesNode);
  finally
    NamesNode.Free;
  end;
end;

procedure TPasSyntaxTreeBuilder.UsedUnitName;
var
  NamesNode, UnitNode: TSyntaxNode;
  Position: TTokenPoint;
  FileName: string;
begin
  Position := Lexer.PosXY;
  FileName := Lexer.FileName;

  NamesNode:= TSyntaxNode.Create(ntUnit);
  try
    FStack.Push(NamesNode);
    try
      inherited;
    finally
      FStack.Pop;
    end;

    UnitNode := FStack.AddChild(ntUnit);
    UnitNode.Attribute[anName]:= NodeListToString(NamesNode);
    UnitNode.Col  := Position.X;
    UnitNode.Line := Position.Y;
    UnitNode.FileName := FileName;
  finally
    NamesNode.Free;
  end;
end;

procedure TPasSyntaxTreeBuilder.UsesClause;
begin
  FStack.PushCompoundSyntaxNode(ntUses);
  try
    inherited;
    SetCurrentCompoundNodesEndPosition;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.VarAbsolute;
var
  AbsoluteNode: TSyntaxNode;
  ValueNode: TSyntaxNode;
begin
  AbsoluteNode:= TSyntaxNode.Create(ntUnknown);
  FStack.Push(AbsoluteNode);
  try
    inherited;
  finally
    FStack.Pop;
    ValueNode:= AbsoluteNode.ExtractChild(AbsoluteNode.ChildNode[0]);
    ValueNode.Attribute[anKind]:= AttributeValues[atAbsolute];
    AbsoluteNode.Free;
    FStack.Peek.AddChild(ValueNode);
  end;
end;

procedure TPasSyntaxTreeBuilder.VarDeclaration;
begin
  FStack.Push(ntVariables);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.RecordVariantSection;
begin
  FStack.Push(ntVariantSection);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.RecordVariantTag;
var
  Temp: TSyntaxNode;
begin
  Temp:= FStack.Push(ntVariantTag);
  try
    inherited;
    if Temp.ChildCount = 2 then begin
      Temp.Attribute[anName]:= Temp.ChildNode[0].Attribute[anName];
      Temp.Attribute[anType]:= Temp.ChildNode[1].Attribute[anName];
      Temp.DeleteChild(Temp.ChildNode[1]);
    end else begin
      Temp.Attribute[anType]:= Temp.ChildNode[0].Attribute[anName];
    end;
    Temp.DeleteChild(Temp.ChildNode[0]);
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.VarName;
begin
  //FStack.AddValuedChild(ntName, Lexer.Token);
  FStack.AddChild(ntName).Attribute[anName]:= Lexer.Token; //#222
  inherited;
end;

procedure TPasSyntaxTreeBuilder.VarParameter;
begin
  FStack.Push(ntParameters).Attribute[anKind]:= AttributeValues[atVar];
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.VarSection;
var
  VarSect, Temp: TSyntaxNode;
  VarList, Variable, TypeInfo, ValueInfo: TSyntaxNode;
begin
  VarSect := TSyntaxNode.Create(ntUnknown);
  try
    FStack.Push(ntVariables);

    FStack.Push(VarSect);
    try
      inherited VarSection;
    finally
      FStack.Pop;
    end;

    for VarList in VarSect.ChildNodes do
    begin
      TypeInfo := VarList.FindNode(ntType);
      ValueInfo := VarList.FindNode(ntValue);
      for Variable in VarList.ChildNodes do
      begin
        if Variable.Typ <> ntName then
          Continue;

        Temp := FStack.Push(ntVariable);
        try
          Temp.AssignPositionFrom(Variable);

          FStack.AddChild(Variable.Clone);
          if Assigned(TypeInfo) then
            FStack.AddChild(TypeInfo.Clone);

          if Assigned(ValueInfo) then
            FStack.AddChild(ValueInfo.Clone);
        finally
          FStack.Pop;
        end;
      end;
    end;

    FStack.Pop;
  finally
    VarSect.Free;
  end;
end;

procedure TPasSyntaxTreeBuilder.VisibilityStrictPrivate;
var
  Temp: TSyntaxNode;
begin
  Temp := FStack.Push(ntStrictPrivate);
  try
    Temp.Attribute[anVisibility]:= AttributeValues[atTrue];
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.VisibilityPrivate;
var
  Temp: TSyntaxNode;
begin
  Temp := FStack.Push(ntPrivate);
  try
    Temp.Attribute[anVisibility]:= AttributeValues[atTrue];
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.VisibilityStrictProtected;
var
  Temp: TSyntaxNode;
begin
  Temp := FStack.Push(ntStrictProtected);
  try
    Temp.Attribute[anVisibility]:= AttributeValues[atTrue];
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.VisibilityProtected;
var
  Temp: TSyntaxNode;
begin
  Temp := FStack.Push(ntProtected);
  try
    Temp.Attribute[anVisibility]:= AttributeValues[atTrue];
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.VisibilityPublic;
var
  Temp: TSyntaxNode;
begin
  Temp := FStack.Push(ntPublic);
  try
    Temp.Attribute[anVisibility]:= AttributeValues[atTrue];
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.VisibilityPublished;
var
  Temp: TSyntaxNode;
begin
  Temp := FStack.Push(ntPublished);
  try
    Temp.Attribute[anVisibility]:= AttributeValues[atTrue];
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.VisibilityAutomated;
var
  Temp: TSyntaxNode;
begin
  Temp := FStack.Push(ntAutomated);
  try
    Temp.Attribute[anVisibility]:= AttributeValues[atTrue];
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.WhileStatement;
begin
  FStack.Push(ntWhile);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.WithExpressionList;
begin
  FStack.Push(ntExpressions);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.WithStatement;
begin
  FStack.Push(ntWith);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

{ ESyntaxTreeException }

constructor ESyntaxTreeException.Create(Line, Col: Integer; const FileName, Msg: string;
  SyntaxTree: TSyntaxNode);
begin
  inherited Create(Line, Col, FileName, Msg);
  FSyntaxTree := SyntaxTree;
end;

destructor ESyntaxTreeException.Destroy;
begin
  FSyntaxTree.Free;
  inherited;
end;

initialization
  InitAttributeValues;
end.