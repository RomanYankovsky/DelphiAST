unit DelphiAST;

interface

uses
  SysUtils, Classes, Generics.Collections, SimpleParser,
  SimpleParser.Lexer.Types, DelphiAST.Classes, DelphiAST.Consts;

type
  EParserException = class(Exception)
  strict private
    FLine, FCol: Integer;
  public
    constructor Create(Line, Col: Integer; Msg: string); reintroduce;

    property Line: Integer read FLine;
    property Col: Integer read FCol;
  end;

  TNodeStack = class
  strict private
    FParser: TmwSimplePasPar;
    FStack: TStack<TSyntaxNode>;
    //FStack: TMiniStack<TSyntaxNode>;

    function GetCount: Integer;
  public
    constructor Create(Parser: TmwSimplePasPar);
    destructor Destroy; override;

    function AddChild(Typ: TSyntaxNodeType; SetPositionAttributes: Boolean = True): TSyntaxNode; overload;
    function AddChild(Node: TSyntaxNode): TSyntaxNode; overload;

    procedure Clear;
    function Peek: TSyntaxNode;
    function Pop: TSyntaxNode;

    function Push(Typ: TSyntaxNodeType; SetPositionAttributes: Boolean = True): TSyntaxNode; overload;
    function Push(Node: TSyntaxNode; SetPositionAttributes: Boolean = True): TSyntaxNode; overload;

    property Count: Integer read GetCount;
  end;

  TPasSyntaxTreeBuilder = class(TmwSimplePasPar)
  private type
    TExpressionMethod = reference to procedure;
  private
    procedure BuildExpressionTree(ExpressionMethod: TExpressionMethod);
    procedure ParserMessage(Sender: TObject; const Typ: TMessageEventType; const Msg: string; X, Y: Integer);
    function NodeListToString(NamesNode: TSyntaxNode): string;
  protected
    FStack: TNodeStack;

    procedure AccessSpecifier; override;
    procedure AdditiveOperator; override;
    procedure AddressOp; override;
    procedure AlignmentParameter; override;
    procedure AnonymousMethod; override;
    procedure ArrayBounds; override;
    procedure ArrayConstant; override;
    procedure ArrayDimension; override;
    procedure AsmStatement; override;
    procedure AsOp; override;
    procedure AssignOp; override;
    procedure AtExpression; override;
    procedure CaseElseStatement; override;
    procedure CaseLabel; override;
    procedure CaseLabelList; override;
    procedure CaseSelector; override;
    procedure CaseStatement; override;
    procedure ClassClass; override;
    procedure ClassField; override;
    procedure ClassForward; override;
    procedure ClassMethod; override;
    procedure ClassMethodHeading; override;
    procedure ClassProperty; override;
    procedure ClassReferenceType; override;
    procedure ClassType; override;
    procedure ConstParameter; override;
    procedure ConstantDeclaration; override;
    procedure ConstantExpression; override;
    procedure ConstantName; override;
    procedure ConstSection; override;
    procedure ConstantValue; override;
    procedure ConstantValueTyped; override;
    procedure ConstructorName; override;
    procedure ContainsClause; override;
    procedure Designator; override;
    procedure DestructorName; override;
    function Directive(const Kind: string): TSyntaxNode;
    procedure DeclarationForward; override;
    procedure Directive16Bit; override;
    procedure DirectiveBinding; override;
    procedure DirectiveCalling; override;
    procedure DirectiveDeprecated; override;
    procedure DirectiveExperimental; override;
    procedure DirectiveExternal; override;
    procedure DirectiveExternalTwo; override;
    procedure DirectiveExternalThree; override;
    procedure DirectiveLibrary; override;
    procedure DotOp; override;
    procedure ElseStatement; override;
    procedure EmptyStatement; override;
    procedure EnumeratedType; override;
    procedure ExceptBlock; override;
    procedure ExceptionHandler; override;
    procedure ExportedHeading; override;
    procedure Expression; override;
    procedure ExpressionList; override;

    procedure FieldName; override;
    procedure FinallyBlock; override;
    procedure FormalParameterList; override;
    procedure ForStatement; override;
    procedure ForStatementDownTo; override;
    procedure ForStatementFrom; override;
    procedure ForStatementIn; override;
    procedure ForStatementTo; override;
    procedure FunctionHeading; override;
    procedure FunctionMethodName; override;
    procedure FunctionProcedureName; override;
    procedure GotoStatement; override;
    procedure IfStatement; override;
    procedure Identifier; override;
    procedure ImplementationSection; override;
    procedure ImplementsSpecifier; override;
    procedure IndexOp; override;
    procedure InheritedStatement; override;
    procedure InheritedVariableReference; override;
    procedure InterfaceGUID; override;
    procedure InterfaceSection; override;
    procedure InterfaceType; override;
    procedure LabelId; override;
    procedure MainUsesClause; override;
    procedure MainUsedUnitStatement; override;
    procedure MethodKind; override;
    procedure MultiplicativeOperator; override;
    procedure NewFormalParameterType; override;
    procedure NotOp; override;
    procedure NilToken; override;
    procedure Number; override;
    procedure ObjectNameOfMethod; override;
    procedure OutParameter; override;
    procedure ParameterFormal; override;
    procedure ParameterName; override;
    procedure PointerSymbol; override;
    procedure PointerType; override;
    procedure ProceduralType; override;
    procedure ProcedureHeading; override;
    procedure ProcedureDeclarationSection; override;
    procedure ProceduralDirective; override;
    procedure ProcedureProcedureName; override;
    procedure PropertyName; override;
    procedure PropertyParameterList; override;
    procedure RaiseStatement; override;
    procedure RecordFieldConstant; override;
    procedure RelativeOperator; override;
    procedure RepeatStatement; override;
    procedure RequiresClause; override;
    procedure RequiresIdentifier; override;
    procedure RequiresIdentifierId; override;
    procedure ReturnType; override;
    procedure RoundClose; override;
    procedure RoundOpen; override;
    procedure SetConstructor; override;
    procedure SetElement; override;
    procedure SimpleStatement; override;
    procedure SimpleType; override;
    procedure StatementList; override;
    procedure StringConst; override;
    procedure StringConstSimple; override;
    procedure StringType; override;
    procedure StructuredType; override;
    procedure SubrangeType; override;
    procedure ThenStatement; override;
    procedure TryStatement; override;
    procedure TypeArgs; override;
    procedure TypeDeclaration; override;
    procedure TypeKind; override;
    procedure TypeParams; override;
    procedure TypeSection; override;
    procedure TypeSimple; override;
    procedure UnaryMinus; override;
    procedure UnitFile; override;
    procedure UnitName; override;
    procedure UnitId; override;
    procedure UsesClause; override;
    procedure UsedUnitName; override;
    procedure VarDeclaration; override;
    procedure VarName; override;
    procedure VarParameter; override;
    procedure VarSection; override;
    procedure VisibilityPrivate; override;
    procedure VisibilityProtected; override;
    procedure VisibilityPublic; override;
    procedure VisibilityPublished; override;
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
    class function Run(const FileName: string): TSyntaxNode; reintroduce; overload; static;
  end;

implementation

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
  FStack.Push(ntAnonymousMethod);
  try
    inherited;
  finally
    FStack.Pop;
  end;
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

procedure TPasSyntaxTreeBuilder.AsmStatement;
const
  EndLine = true;
begin
  FStack.Push(ntStatements, False).SetAttribute(aType, 'asm');
  try
    FStack.Peek.SetPositionAttributes(Lexer.PosXY);
    inherited;
    FStack.Peek.SetPositionAttributes(Lexer.PosXY, EndLine);
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
  FStack.AddChild(ntName).SetAttribute(aValue, Lexer.Token);
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
  FStack.AddChild(ntName).SetAttribute(aValue, Lexer.Token);
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
  ExpressionMethod: TExpressionMethod);
var
  RawExprNode: TSyntaxNode;
  ExprNode: TSyntaxNode;

  NodeList: TList<TSyntaxNode>;
  Node: TSyntaxNode;
  Col, Line: Integer;
begin
  Line := Lexer.PosXY.Y;
  Col := Lexer.PosXY.X;

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
      ExprNode := FStack.Push(ntExpression, False);
      try
        ExprNode.SetPositionAttributes(TokenPoint(Line, Col));
        NodeList := TList<TSyntaxNode>.Create;
        try
          for Node in RawExprNode.ChildNodes do begin
            NodeList.Add(Node);
          end;
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

procedure TPasSyntaxTreeBuilder.ClassClass;
begin
  FStack.Peek.SetAttribute(aCLASS);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.ClassField;
var
  Fields: TSyntaxNode;
  Field, TypeInfo, TypeArgs: TSyntaxNode;
begin
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

      FStack.Push(ntField, False);
      try
        FStack.AddChild(Field.Clone);
        TypeInfo := TypeInfo.Clone;
        if assigned(TypeArgs) then
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
  FStack.Peek.SetAttribute(aForward);
  inherited ClassForward;
end;

procedure TPasSyntaxTreeBuilder.ClassMethod;
begin
  FStack.Peek.SetAttribute(aClass);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.ClassMethodHeading;
begin
  FStack.Push(ntMethod);
  try
    inherited;
  finally
    FStack.Pop;
  end;
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
  FStack.Peek.SetAttribute(aTYPE, 'classof');
  inherited ClassReferenceType;
end;

procedure TPasSyntaxTreeBuilder.ClassType;
var
  classDef, child, vis: TSyntaxNode;
  i: integer;
  extracted: boolean;
begin
  FStack.Peek.SetAttribute(aTYPE, 'class');
  inherited;
  classDef := FStack.Peek;
  vis := nil;
  i := 0;
  while i < classDef.ChildNodes.Count do
  begin
    child := classDef.ChildNodes[i];
    extracted := false;
    if child.HasAttribute(Visibility) then
      vis := child
    else if assigned(vis) then
    begin
      classDef.ChildNodes.Extract(child);
      vis.ChildNodes.Add(child);
      extracted := true;
    end;
    if not extracted then
      inc(i);
  end;
end;

procedure TPasSyntaxTreeBuilder.ConstParameter;
begin
  FStack.Push(ntParameters).SetAttribute(aKIND, 'const');
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ConstructorName;
begin
  FStack.Peek.SetAttribute(aKIND, 'constructor');
  FStack.Peek.SetAttribute(aNAME, Lexer.Token);
  inherited;
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
begin
  BuildExpressionTree(
    procedure
    begin
      inherited ConstantExpression;
    end);
end;

procedure TPasSyntaxTreeBuilder.ConstantName;
begin
  FStack.AddChild(ntName).SetAttribute(aVALUE, Lexer.Token);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.ConstantValue;
begin
  FStack.Push(ntValue);
  try
    inherited ConstantValue;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ConstantValueTyped;
begin
  FStack.Push(ntValue);
  try
    inherited ConstantValueTyped;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ConstSection;
var
  ConstSect: TSyntaxNode;
  ConstList, Constant, TypeInfo, Value: TSyntaxNode;
begin
  ConstSect := TSyntaxNode.Create(ntConstants);
  try
    FStack.Push(ntConstants);

    FStack.Push(ConstSect);
    try
      inherited ConstSection;
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

        FStack.Push(ntConstant, False);
        try
          FStack.AddChild(Constant.Clone);
          if assigned(TypeInfo) then
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
  FStack := TNodeStack.Create(Self);
end;

procedure TPasSyntaxTreeBuilder.Designator;
begin
//  FStack.Push('designator');
  try
    inherited Designator;
  finally
//    FStack.Pop;
  end;
end;

destructor TPasSyntaxTreeBuilder.Destroy;
begin
  FStack.Free;
  inherited;
end;

procedure TPasSyntaxTreeBuilder.DestructorName;
begin
  FStack.Peek.SetAttribute(aKIND, 'destructor');
  FStack.Peek.SetAttribute(aNAME, Lexer.Token);
  inherited;
end;


function TPasSyntaxTreeBuilder.Directive(const Kind: string): TSyntaxNode;
begin
  //Assert(FStack.Peek.ParentNode.Typ = ntMethod);
  Result:= FStack.Peek.AddChild(ntDirective);
  Result.SetAttribute(aVALUE,Lexer.Token);
  Result.SetAttribute(aKIND, Kind);
end;

procedure TPasSyntaxTreeBuilder.DeclarationForward;
begin
  Directive(sFORWARD);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.Directive16Bit;
begin
  Directive(s16BIT);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.DirectiveBinding;
begin
  Directive(sBINDING);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.DirectiveCalling;
begin
  Directive(sCALLING);
  inherited;
end;

//Can be applied to any identifier
procedure TPasSyntaxTreeBuilder.DirectiveDeprecated;
var
  DirectiveDepr: TSyntaxNode;
begin
  FStack.Push(ntUnknown);
  try
    inherited;
  finally
    DirectiveDepr:= FStack.Pop;
    if DirectiveDepr.HasChildren then begin
      FStack.Peek.SetAttribute(aDeprecated, DirectiveDepr.ChildNodes[0].GetAttribute(aVALUE));
    end;
  end;
end;

procedure TPasSyntaxTreeBuilder.DirectiveExperimental;
begin
  inherited;
  FStack.Peek.SetAttribute(aExperimental);
end;

procedure TPasSyntaxTreeBuilder.DirectiveExternal;
var
  DirectiveExt: TSyntaxNode;
begin
  DirectiveExt:= Directive(sEXTERNAL);
  FStack.Push(DirectiveExt, false);
  try
    inherited;
  finally
    FStack.Pop;
    Assert(DirectiveExt.HasChildren);
    DirectiveExt.SetAttribute(aVALUE,DirectiveExt.ChildNodes[0].GetAttribute(aVALUE));
    DirectiveExt.DeleteChild(DirectiveExt.ChildNodes[0]);
  end;
end;

procedure TPasSyntaxTreeBuilder.DirectiveExternalTwo;
var
  PartTwo: TSyntaxNode;
  Token: string;
  ExternalDirective: TValueAttribute;
begin
  PartTwo:= FStack.Push(ntDirective);
  Token:= Lexer.Token;
  try
    inherited;
  finally
    FStack.Pop;
    if PartTwo.HasChildren then begin
      if Token = 'name' then ExternalDirective:= aName
      else if Token = 'index' then ExternalDirective:= aIndex
      else begin
        raise Exception.Create('Error in TPasSyntaxTreeBuilder.DirectiveExternalTwo Token = '+Token+' "index" or "name" expected');
      end;
      FStack.Peek.SetAttribute(ExternalDirective, PartTwo.ChildNodes[0].GetAttribute(aVALUE));
    end;
    PartTwo.Free;
  end;
end;

procedure TPasSyntaxTreeBuilder.DirectiveExternalThree;
begin
  inherited;
  if Lexer.Token = sDELAYED then
    FStack.Peek.SetAttribute(aDelayed);
end;

//The `library` directive can be applied to any identifier
procedure TPasSyntaxTreeBuilder.DirectiveLibrary;
begin
  inherited;
  FStack.Peek.SetAttribute(aLIBRARY);
end;

procedure TPasSyntaxTreeBuilder.DotOp;
begin
  FStack.AddChild(ntDot, False);
  inherited;
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
begin
  FStack.Push(ntType).SetAttribute(aNAME, sENUM);
  try
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

procedure TPasSyntaxTreeBuilder.ExceptionHandler;
begin
  FStack.Push(ntExceptionHandler);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ExportedHeading;
begin
  FStack.Push(ntMethod);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.Expression;
begin
  BuildExpressionTree(
    procedure
    begin
      inherited Expression
    end);
end;

procedure TPasSyntaxTreeBuilder.ExpressionList;
begin
  FStack.Push(ntExpressions, False);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.FieldName;
begin
  FStack.AddChild(ntName).SetAttribute(aVALUE, Lexer.Token);
  inherited;
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
  Params: TSyntaxNode;
  ParamList, Param, TypeInfo, ParamExpr: TSyntaxNode;
  ParamKind: string;
begin
  Params := TSyntaxNode.Create(ntUnknown);
  try
    FStack.Push(ntParameters);

    FStack.Push(Params);
    try
      inherited;
    finally
      FStack.Pop;
    end;

    for ParamList in Params.ChildNodes do
    begin
      TypeInfo := ParamList.FindNode(ntType);
      ParamKind := ParamList.GetAttribute(aKIND);
      ParamExpr := ParamList.FindNode(ntExpression);

      for Param in ParamList.ChildNodes do
      begin
        if Param.Typ <> ntName then
          Continue;

        FStack.Push(ntParameter, False);
        if ParamKind <> '' then
          FStack.Peek.SetAttribute(aKIND, ParamKind);

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

procedure TPasSyntaxTreeBuilder.FunctionHeading;
begin
  FStack.Peek.SetAttribute(aKIND, 'function');
  inherited;
end;

procedure TPasSyntaxTreeBuilder.FunctionMethodName;
begin
  FStack.AddChild(ntName).SetAttribute(aVALUE, Lexer.Token);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.FunctionProcedureName;
var
  ChildNode, nameNode, TypeParam: TSyntaxNode;
  FullName, TypeParams: string;
begin
  FStack.Push(ntName);
  nameNode := FStack.Peek;
  try
    inherited;
    for ChildNode in nameNode.ChildNodes do
    begin
      if ChildNode.Typ = ntTypeParams then
      begin
        TypeParams := '';

        for TypeParam in ChildNode.ChildNodes do
        begin
          if TypeParams <> '' then TypeParams := TypeParams + ',';
          TypeParams := TypeParams + TypeParam.GetAttribute(aNAME);
        end;

        FullName := FullName + '<' + TypeParams + '>';
        Continue;
      end;

      if FullName <> '' then
        FullName := FullName + '.';
      FullName := FullName + ChildNode.GetAttribute(aVALUE);
    end;
  finally
    FStack.Pop;
    FStack.Peek.SetAttribute(aNAME, FullName);
    FStack.Peek.DeleteChild(nameNode);
  end;
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
  FStack.AddChild(ntIdentifier).SetAttribute(aNAME, Lexer.Token);
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
  FStack.Push(ntImplementation);
  try
    inherited;
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
  FStack.Push(ntInterface);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.InterfaceType;
begin
  FStack.Peek.SetAttribute(aTYPE, 'interface');
  inherited;
end;

procedure TPasSyntaxTreeBuilder.LabelId;
begin
  FStack.AddChild(ntLabel).SetAttribute(aVALUE, Lexer.Token);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.MainUsedUnitStatement;
var
  NameNode, PathNode: TSyntaxNode;
begin
  FStack.Push(ntUnit);
  try
    inherited;
    NameNode := FStack.Peek.FindNode(ntUnit);
    PathNode := FStack.Peek.FindNode(ntExpression);
    if Assigned(PathNode) then
      PathNode := PathNode.FindNode(ntLiteral);

    if Assigned(NameNode) then
    begin
      FStack.Peek.SetAttribute(aNAME, NameNode.GetAttribute(aNAME));
      FStack.Peek.DeleteChild(NameNode);
    end;

    if Assigned(PathNode) then
    begin
      FStack.Peek.SetAttribute(aPATH, PathNode.GetAttribute(aVALUE));
      FStack.Peek.DeleteChild(PathNode);
    end;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.MainUsesClause;
begin
  FStack.Push(ntUses);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.MethodKind;
begin
  FStack.Peek.SetAttribute(aKIND, LowerCase(Lexer.Token));
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

procedure TPasSyntaxTreeBuilder.NewFormalParameterType;
begin
  FStack.AddChild(ntType).SetAttribute(aNAME, Lexer.Token);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.NilToken;
begin
  FStack.AddChild(ntLiteral).SetAttribute(aTYPE, 'nil');
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
  Node := FStack.AddChild(ntLiteral);
  Node.SetAttribute(aTYPE, 'numeric');
  Node.SetAttribute(aVALUE, Lexer.Token);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.ObjectNameOfMethod;
begin
  FStack.AddChild(ntName).SetAttribute(aVALUE, Lexer.Token);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.ParserMessage(Sender: TObject;
  const Typ: TMessageEventType; const Msg: string; X, Y: Integer);
begin
  if Typ = TMessageEventType.meError then
    raise EParserException.Create(Y, X, Msg);
end;

procedure TPasSyntaxTreeBuilder.OutParameter;
begin
  FStack.Push(ntParameters).SetAttribute(aKIND, 'out');
  try
    inherited;
  finally
    FStack.Pop;
  end;
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
  FStack.AddChild(ntName).SetAttribute(aVALUE, Lexer.Token);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.PointerSymbol;
begin
  FStack.AddChild(ntDeref);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.PointerType;
begin
  FStack.Peek.SetAttribute(aTYPE, 'pointer');
  inherited;
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

procedure TPasSyntaxTreeBuilder.ProceduralType;
begin
  FStack.Push(ntType).SetAttribute(aNAME, Lexer.Token);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ProcedureDeclarationSection;
begin
  FStack.Push(ntMethod);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ProceduralDirective;
var
  Directives: TSyntaxNode;
begin
  //See if we already have a `ntDirectives` child in the `ntMethod` node.
  Directives:= FStack.Peek.FindNode(ntDirectives);
  if (Directives = nil) then begin
    Directives:= FStack.Peek.AddChild(ntDirectives);
  end;
  FStack.Push(Directives);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ProcedureHeading;
begin
  FStack.Peek.SetAttribute(aKIND, 'procedure');
  inherited;
end;

procedure TPasSyntaxTreeBuilder.ProcedureProcedureName;
begin
  FStack.Peek.SetAttribute(aNAME, Lexer.Token);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.PropertyName;
begin
  FStack.Peek.SetAttribute(aNAME, Lexer.Token);
  inherited PropertyName;
end;

procedure TPasSyntaxTreeBuilder.PropertyParameterList;
begin
  FStack.Push(ntParameters);
  try
    inherited PropertyParameterList;
  finally
    FStack.Pop;
  end;
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
var
  Node: TSyntaxNode;
begin
  Node := FStack.Push(ntField);
  try
    Node.SetAttribute(aTYPE, 'name');
    Node.SetAttribute(aVALUE, Lexer.Token);
    inherited;
  finally
    FStack.Pop;
  end;
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

    FStack.AddChild(ntPackage).SetAttribute(aNAME, NodeListToString(NamesNode));
  finally
    NamesNode.Free;
  end;
end;

procedure TPasSyntaxTreeBuilder.RequiresIdentifierId;
begin
  FStack.AddChild(ntUnknown, False).SetAttribute(aNAME, Lexer.Token);
  inherited;
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

class function TPasSyntaxTreeBuilder.Run(const FileName: string): TSyntaxNode;
var
  Stream: TStringStream;
  Builder: TPasSyntaxTreeBuilder;
begin
  Stream := TStringStream.Create;
  try
    Stream.LoadFromFile(FileName);
    Builder := TPasSyntaxTreeBuilder.Create;
    try
      Builder.InitDefinesDefinedByCompiler;
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
  Result := TSyntaxNode.Create(ntUnit);
  try
    FStack.Clear;
    FStack.Push(Result);
    try
      self.OnMessage := ParserMessage;
      inherited Run('', SourceStream);
    finally
      FStack.Pop;
    end;
  except
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
    if Result <> '' then
      Result := Result + '.';
    Result := Result + NamePartNode.Attributes.Value[aNAME];
  end;
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
  RawStatement: TSyntaxNode;
  Node: TSyntaxNode;
  NodeList: TList<TSyntaxNode>;
  I, AssignIdx: Integer;
  Position: TTokenPoint;
begin
  Position := Lexer.PosXY;

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
      FStack.Push(ntAssign, False);
      try
        FStack.Peek.SetPositionAttributes(Position);

        NodeList := TList<TSyntaxNode>.Create;
        try
          AssignIdx := -1;
          for I := 0 to RawStatement.ChildNodes.Count - 1 do
          begin
            if RawStatement.ChildNodes[I].Typ = ntAssign then
            begin
              AssignIdx := I;
              Break;
            end;
            NodeList.Add(RawStatement.ChildNodes[I]);
          end;
          TExpressionTools.RawNodeListToTree(RawStatement, NodeList, FStack.AddChild(ntLHS, False));

          NodeList.Clear;

          for I := AssignIdx + 1 to RawStatement.ChildNodes.Count - 1 do
            NodeList.Add(RawStatement.ChildNodes[I]);
          TExpressionTools.RawNodeListToTree(RawStatement, NodeList, FStack.AddChild(ntRHS, False));
        finally
          NodeList.Free;
        end;
      finally
        FStack.Pop;
      end;
    end else
    begin
      FStack.Push(ntCall, False);
      try
        FStack.Peek.SetPositionAttributes(Position);

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
  FStack.Push(ntType).SetAttribute(aNAME, Lexer.Token);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.StatementList;
const
  statement_begin = false;
  statement_end = true;
begin
  FStack.Push(ntStatements, False);
  try
    FStack.Peek.SetPositionAttributes(Lexer.PosXY, statement_begin);
    inherited;
    FStack.Peek.SetPositionAttributes(Lexer.PosXY, statement_end);
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
      Str := Str + Literal.GetAttribute(aValue);
  finally
    StrConst.Free;
  end;

  Node := FStack.AddChild(ntLiteral);
  Node.SetAttribute(aTYPE, 'string');
  Node.SetAttribute(aVALUE, Str);
end;

procedure TPasSyntaxTreeBuilder.StringConstSimple;
begin
  //TODO support ptAsciiChar
  FStack.AddChild(ntLiteral).SetAttribute(aVALUE, AnsiDequotedStr(Lexer.Token, ''''));
  inherited;
end;

procedure TPasSyntaxTreeBuilder.StringType;
begin
  FStack.Push(ntType).SetAttribute(aNAME, Lexer.Token);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.StructuredType;
begin
  FStack.Push(ntType).SetAttribute(aNAME, Lexer.Token);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.SubrangeType;
begin
  FStack.Push(ntType).SetAttribute(aNAME, sSUBRANGE);
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
  FStack.Push(ntTypeDecl).SetAttribute(aNAME, Lexer.Token);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.TypeKind;
var
  compound: TSyntaxNode;
  compoundType: string;
  found: boolean;
  i: integer;
begin
  case TokenID of
    ptArray, ptFile, ptSet: begin
      compound := FStack.Push(ntType);
      case TokenID of
        ptArray: compoundType := 'ArrayOf';
        ptFile: compoundType := 'FileOf';
        ptSet: compoundType := 'SetOf';
      end;
      compound.SetAttribute(aNAME, compoundType);
      compound.SetAttribute(aCompound);
    end
    else
      compound := nil;
  end;
  try
    inherited;
  finally
    if assigned(compound) then
    begin
      FStack.Pop;
      found := false;
      for i := compound.ChildNodes.Count - 1 downto 0 do
        if compound.ChildNodes[i].Typ = ntType then
          if found then
            compound.ChildNodes.Delete(i)
          else found := true;
    end;
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
  FStack.Push(ntType).SetAttribute(aNAME, Lexer.Token);
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
  FStack.Peek.SetPositionAttributes(Lexer.PosXY);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.UnitId;
begin
  FStack.AddChild(ntUnknown, False).SetAttribute(aNAME, Lexer.Token);
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

    FStack.Peek.SetAttribute(aNAME, NodeListToString(NamesNode));
  finally
    NamesNode.Free;
  end;
end;

procedure TPasSyntaxTreeBuilder.UsedUnitName;
var
  NamesNode, UnitNode: TSyntaxNode;
  Position: TTokenPoint;
begin
  Position := Lexer.PosXY;

  NamesNode := TSyntaxNode.Create(ntUnit);
  try
    FStack.Push(NamesNode);
    try
      inherited;
    finally
      FStack.Pop;
    end;

    UnitNode := FStack.AddChild(ntUnit);
    UnitNode.SetAttribute(aNAME, NodeListToString(NamesNode));
    UnitNode.SetPositionAttributes(Position);
  finally
    NamesNode.Free;
  end;
end;

procedure TPasSyntaxTreeBuilder.UsesClause;
begin
  FStack.Push(ntUses);
  try
    inherited;
  finally
    FStack.Pop;
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

procedure TPasSyntaxTreeBuilder.VarName;
begin
  FStack.AddChild(ntName).SetAttribute(aVALUE, Lexer.Token);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.VarParameter;
begin
  FStack.Push(ntParameters).SetAttribute(aKIND, 'var');
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.VarSection;
var
  VarSect: TSyntaxNode;
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

        FStack.Push(ntVariable, False);
        try
          FStack.AddChild(Variable.Clone);
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

procedure TPasSyntaxTreeBuilder.VisibilityPrivate;
begin
  FStack.Push(ntPrivate);
  try
    FStack.Peek.SetAttribute(aPrivate);
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.VisibilityProtected;
begin
  FStack.Push(ntProtected);
  try
    FStack.Peek.SetAttribute(aProtected);
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.VisibilityPublic;
begin
  FStack.Push(ntPublic);
  try
    FStack.Peek.SetAttribute(aPublic);
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.VisibilityPublished;
begin
  FStack.Push(ntPublished);
  try
    FStack.Peek.SetAttribute(aPublished);
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



{ TNodeStack }

function TNodeStack.AddChild(Typ: TSyntaxNodeType;
  SetPositionAttributes: Boolean = true): TSyntaxNode;
begin
  Result := FStack.Peek.AddChild(TSyntaxNode.Create(Typ));

  if SetPositionAttributes then
    Result.SetPositionAttributes(FParser.Lexer.PosXY);
end;

function TNodeStack.AddChild(Node: TSyntaxNode): TSyntaxNode;
begin
  Result := FStack.Peek.AddChild(Node);
end;

procedure TNodeStack.Clear;
begin
  FStack.Clear;
end;

constructor TNodeStack.Create(Parser: TmwSimplePasPar);
begin
  FParser := Parser;
  FStack := TStack<TSyntaxNode>.Create;
  //FStack.Init;
end;

destructor TNodeStack.Destroy;
begin
  FStack.Free;
  //FStack.Clear;
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

function TNodeStack.Push(Node: TSyntaxNode; SetPositionAttributes: Boolean): TSyntaxNode;
begin
  FStack.Push(Node);
  Result := Node;

  if SetPositionAttributes then
    Result.SetPositionAttributes(FParser.Lexer.PosXY);
end;

function TNodeStack.Push(Typ: TSyntaxNodeType; SetPositionAttributes: Boolean = True): TSyntaxNode;
begin
  Result := FStack.Peek.AddChild(TSyntaxNode.Create(Typ));
  Push(Result, SetPositionAttributes);
end;

{ EParserException }

constructor EParserException.Create(Line, Col: Integer; Msg: string);
begin
  inherited Create(Msg);
  FLine := Line;
  FCol := Col;
end;

end.
