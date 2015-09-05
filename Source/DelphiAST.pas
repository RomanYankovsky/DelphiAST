unit DelphiAST;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}  

interface

uses
  SysUtils, Classes, Generics.Collections, SimpleParser,
  SimpleParser.Lexer.Types, DelphiAST.Classes, DelphiAST.Consts;

type
  ESyntaxTreeException = class(EParserException)
  strict private
    FSyntaxTree: TSyntaxNode;
  public
    constructor Create(Line, Col: Integer; Msg: string; SyntaxTree: TSyntaxNode); reintroduce;
    destructor Destroy; override;

    property SyntaxTree: TSyntaxNode read FSyntaxTree;
  end;

  TNodeStack = class
  strict private
    FParser: TmwSimplePasPar;
    FStack: TStack<TSyntaxNode>;

    function GetCount: Integer;
  public
    constructor Create(Parser: TmwSimplePasPar);
    destructor Destroy; override;

    function AddChild(Typ: TSyntaxNodeType; SetPositionAttributes: Boolean = True): TSyntaxNode; overload;
    function AddChild(Node: TSyntaxNode): TSyntaxNode; overload;
    function AddValuedChild(Typ: TSyntaxNodeType; const Value: string): TSyntaxNode;

    procedure Clear;
    function Peek: TSyntaxNode;
    function Pop: TSyntaxNode;

    function Push(Typ: TSyntaxNodeType; SetPositionAttributes: Boolean = True): TSyntaxNode; overload;
    function Push(Node: TSyntaxNode; SetPositionAttributes: Boolean = True): TSyntaxNode; overload;
    function PushCompoundSyntaxNode(Typ: TSyntaxNodeType): TSyntaxNode;
    function PushValuedNode(Typ: TSyntaxNodeType; const Value: string): TSyntaxNode;

    property Count: Integer read GetCount;
  end;

  TPasSyntaxTreeBuilder = class(TmwSimplePasPar)
  private type
    TExpressionMethod = procedure of object;
  private
    procedure BuildExpressionTree(ExpressionMethod: TExpressionMethod);
    procedure ParserMessage(Sender: TObject; const Typ: TMessageEventType; const Msg: string; X, Y: Integer);
    function NodeListToString(NamesNode: TSyntaxNode): string;
    procedure CallInheritedConstantExpression;
    procedure CallInheritedExpression;
    procedure SetCurrentCompoundNodesEndPosition;
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
    procedure ClassConstraint; override;    
    procedure ClassField; override;
    procedure ClassForward; override;
    procedure ClassFunctionHeading; override;
    procedure ClassHelper; override;
    procedure ClassMethod; override;
    procedure ClassMethodHeading; override;
    procedure ClassProcedureHeading; override;
    procedure ClassProperty; override;
    procedure ClassReferenceType; override;
    procedure ClassType; override;
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
    procedure Designator; override;
    procedure DestructorName; override;
    procedure DirectiveBinding; override;
    procedure DirectiveBindingMessage; override;
    procedure DirectiveCalling; override;
    procedure DotOp; override;
    procedure ElseStatement; override;
    procedure EmptyStatement; override;
    procedure EnumeratedType; override;
    procedure ExceptBlock; override;
    procedure ExceptionBlockElseBranch; override;
    procedure ExceptionHandler; override;
    procedure ExceptionVariable; override;
    procedure ExportedHeading; override;
    procedure ExportsClause; override;
    procedure ExportsElement; override;
    procedure Expression; override;
    procedure ExpressionList; override;
    procedure FieldName; override;
    procedure FinalizationSection; override;
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
    procedure InitializationSection; override;
    procedure InterfaceForward; override;
    procedure InterfaceGUID; override;
    procedure InterfaceSection; override;
    procedure InterfaceType; override;
    procedure LabelId; override;
    procedure MainUsesClause; override;
    procedure MainUsedUnitStatement; override;
    procedure MethodKind; override;
    procedure MultiplicativeOperator; override;
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
    procedure ProcedureProcedureName; override;
    procedure PropertyName; override;
    procedure PropertyParameterList; override;
    procedure RaiseStatement; override;
    procedure RecordConstraint; override;
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
    procedure StorageDefault; override;
    procedure StringConst; override;
    procedure StringConstSimple; override;
    procedure StringStatement; override;
    procedure StructuredType; override;
    procedure SubrangeType; override;
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
    class function Run(const FileName: string;
      InterfaceOnly: Boolean = False; IncludeHandler: IIncludeHandler = nil): TSyntaxNode; reintroduce; overload; static;
  end;

implementation

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
begin
  FStack.PushCompoundSyntaxNode(ntStatements).SetAttribute(anType, 'asm');
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
  FStack.AddValuedChild(ntName, Lexer.Token);
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
  FStack.AddValuedChild(ntName, Lexer.Token);
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
        ExprNode.Line := Line;
        ExprNode.Col  := Col;

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
  FStack.Peek.SetAttribute(anClass, 'true');
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
        FStack.Peek.Col  := Field.Col;
        FStack.Peek.Line := Field.Line;

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
  FStack.Peek.SetAttribute(anForwarded, 'true');
  inherited ClassForward;
end;

procedure TPasSyntaxTreeBuilder.ClassFunctionHeading;
begin
  FStack.Peek.SetAttribute(anKind, 'function');
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
  FStack.Peek.SetAttribute(anClass, 'true');
  inherited;
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
  FStack.Peek.SetAttribute(anKind, 'procedure');
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
  FStack.Push(ntType).SetAttribute(anType, 'classof');
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ClassType;
var
  classDef, child, vis: TSyntaxNode;
  i: Integer;
  extracted: Boolean;
begin
  FStack.Push(ntType).SetAttribute(anType, 'class');
  try
    inherited;
  finally
    classDef := FStack.Pop;
    vis := nil;
    i := 0;
    while i < classDef.ChildNodes.Count do
    begin
      child := classDef.ChildNodes[i];
      extracted := false;
      if child.HasAttribute(anVisibility) then
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
end;

procedure TPasSyntaxTreeBuilder.ConstParameter;
begin
  FStack.Push(ntParameters).SetAttribute(anKind, 'const');
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ConstructorName;
begin
  FStack.Peek.SetAttribute(anKind, 'constructor');
  FStack.Peek.SetAttribute(anName, Lexer.Token);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.CompoundStatement;
begin
  FStack.PushCompoundSyntaxNode(ntStatements);
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
  ExpressionMethod: TExpressionMethod;
begin
  ExpressionMethod := CallInheritedConstantExpression;
  BuildExpressionTree(ExpressionMethod);
end;

procedure TPasSyntaxTreeBuilder.CallInheritedConstantExpression;
begin
  inherited ConstantExpression;
end;

procedure TPasSyntaxTreeBuilder.ConstantName;
begin
  FStack.AddValuedChild(ntName, Lexer.Token);
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
          FStack.Peek.Col  := Constant.Col;
          FStack.Peek.Line := Constant.Line;

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
  FStack.Peek.SetAttribute(anKind, 'destructor');
  FStack.Peek.SetAttribute(anName, Lexer.Token);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.DirectiveBinding;
begin
  // Method bindings:
  if SameText(Lexer.Token, 'override') or SameText(Lexer.Token, 'virtual')
    or SameText(Lexer.Token, 'dynamic')
  then
    FStack.Peek.SetAttribute(anMethodBinding, Lexer.Token)
  // Other directives
  else if SameText(Lexer.Token, 'reintroduce') then
    FStack.Peek.SetAttribute(anReintroduce, 'true')
  else if SameText(Lexer.Token, 'overload') then
    FStack.Peek.SetAttribute(anOverload, 'true')
  else if SameText(Lexer.Token, 'abstract') then
    FStack.Peek.SetAttribute(anAbstract, 'true');

  inherited;
end;

procedure TPasSyntaxTreeBuilder.DirectiveBindingMessage;
begin
  FStack.Push(ntMessage);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.DirectiveCalling;
begin
  FStack.Peek.SetAttribute(anCallingConvention, Lexer.Token);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.DotOp;
begin
  FStack.AddChild(ntDot);
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
  FStack.Push(ntType).SetAttribute(anName, sENUM);
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

procedure TPasSyntaxTreeBuilder.ExceptionBlockElseBranch;
begin
  FStack.Push(ntElse);
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
  FStack.AddValuedChild(ntName, Lexer.Token);
  try
    inherited;
  finally
    FStack.Pop;
  end;
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
  FStack.Push(ntElement).SetAttribute(anName, Lexer.Token);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.Expression;
var
  ExpressionMethod: TExpressionMethod;
begin
  ExpressionMethod := CallInheritedExpression;
  BuildExpressionTree(ExpressionMethod);
end;

procedure TPasSyntaxTreeBuilder.SetCurrentCompoundNodesEndPosition;
begin
  TCompoundSyntaxNode(FStack.Peek).EndCol := Lexer.PosXY.X;
  TCompoundSyntaxNode(FStack.Peek).EndLine := Lexer.PosXY.Y;  
end;

procedure TPasSyntaxTreeBuilder.CallInheritedExpression;
begin
  inherited Expression;
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

procedure TPasSyntaxTreeBuilder.FieldName;
begin
  FStack.AddValuedChild(ntName, Lexer.Token);
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
      ParamKind := ParamList.GetAttribute(anKind);
      ParamExpr := ParamList.FindNode(ntExpression);

      for Param in ParamList.ChildNodes do
      begin
        if Param.Typ <> ntName then
          Continue;

        FStack.Push(ntParameter);
        if ParamKind <> '' then
          FStack.Peek.SetAttribute(anKind, ParamKind);

        FStack.Peek.Col  := Param.Col;
        FStack.Peek.Line := Param.Line;

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
  FStack.Peek.SetAttribute(anKind, 'function');
  inherited;
end;

procedure TPasSyntaxTreeBuilder.FunctionMethodName;
begin
  FStack.AddValuedChild(ntName, Lexer.Token);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.FunctionProcedureName;
var
  ChildNode, nameNode, TypeParam, TypeNode: TSyntaxNode;
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
          TypeNode := TypeParam.FindNode(ntType);
          if Assigned(TypeNode) then
          begin
            if TypeParams <> '' then
              TypeParams := TypeParams + ',';
            TypeParams := TypeParams + TypeNode.GetAttribute(anName);
          end;
        end;

        FullName := FullName + '<' + TypeParams + '>';
        Continue;
      end;

      if FullName <> '' then
        FullName := FullName + '.';
      FullName := FullName + TValuedSyntaxNode(ChildNode).Value;
    end;
  finally
    FStack.Pop;
    FStack.Peek.SetAttribute(anName, FullName);
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
  FStack.AddChild(ntIdentifier).SetAttribute(anName, Lexer.Token);
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
  FStack.Peek.SetAttribute(anForwarded, 'true');
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
  FStack.Push(ntType).SetAttribute(anType, 'interface');
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.LabelId;
begin
  FStack.AddValuedChild(ntLabel, Lexer.Token);
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
      FStack.Peek.SetAttribute(anName, NameNode.GetAttribute(anName));
      FStack.Peek.DeleteChild(NameNode);
    end;

    if PathNode is TValuedSyntaxNode then
    begin
      FStack.Peek.SetAttribute(anPath, TValuedSyntaxNode(PathNode).Value);
      FStack.Peek.DeleteChild(PathNode);
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
begin
  FStack.Peek.SetAttribute(anKind, LowerCase(Lexer.Token));
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

procedure TPasSyntaxTreeBuilder.NilToken;
begin
  FStack.AddChild(ntLiteral).SetAttribute(anType, 'nil');
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
  Node.SetAttribute(anType, 'numeric');
  inherited;
end;

procedure TPasSyntaxTreeBuilder.ObjectNameOfMethod;
begin
  FStack.AddValuedChild(ntName, Lexer.Token);
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
  FStack.Push(ntParameters).SetAttribute(anKind, 'out');
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
  FStack.AddValuedChild(ntName, Lexer.Token);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.PointerSymbol;
begin
  FStack.AddChild(ntDeref);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.PointerType;
begin
  FStack.Push(ntType).SetAttribute(anType, 'pointer');
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

procedure TPasSyntaxTreeBuilder.ProceduralType;
begin
  FStack.Push(ntType).SetAttribute(anName, Lexer.Token);
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
  FStack.Peek.SetAttribute(anKind, 'procedure');
  inherited;
end;

procedure TPasSyntaxTreeBuilder.ProcedureProcedureName;
begin
  FStack.Peek.SetAttribute(anName, Lexer.Token);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.PropertyName;
begin
  FStack.Peek.SetAttribute(anName, Lexer.Token);
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
  Node := FStack.PushValuedNode(ntField, Lexer.Token);
  try
    Node.SetAttribute(anType, 'name');
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

    FStack.AddChild(ntPackage).SetAttribute(anName, NodeListToString(NamesNode));
  finally
    NamesNode.Free;
  end;
end;

procedure TPasSyntaxTreeBuilder.RequiresIdentifierId;
begin
  FStack.AddChild(ntUnknown, False).SetAttribute(anName, Lexer.Token);
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

class function TPasSyntaxTreeBuilder.Run(const FileName: string;
  InterfaceOnly: Boolean; IncludeHandler: IIncludeHandler): TSyntaxNode;
var
  Stream: TStringStream;
  Builder: TPasSyntaxTreeBuilder;
begin
  Stream := TStringStream.Create;
  try
    Stream.LoadFromFile(FileName);
    Builder := TPasSyntaxTreeBuilder.Create;
    Builder.InterfaceOnly := InterfaceOnly;
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
    on E: EParserException do
      raise ESyntaxTreeException.Create(E.Line, E.Col, E.Message, Result);
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
    if Result <> '' then
      Result := Result + '.';
    Result := Result + NamePartNode.Attributes[anName];
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
  Node, LHS, RHS: TSyntaxNode;
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
        FStack.Peek.Col  := Position.X;
        FStack.Peek.Line := Position.Y;

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

          if NodeList.Count = 0 then
            raise EParserException.Create(Position.Y, Position.X, 'Illegal expression');

          LHS := FStack.AddChild(ntLHS, False);
          LHS.Col  := NodeList[0].Col;
          LHS.Line := NodeList[0].Line;
          TExpressionTools.RawNodeListToTree(RawStatement, NodeList, LHS);

          NodeList.Clear;

          for I := AssignIdx + 1 to RawStatement.ChildNodes.Count - 1 do
            NodeList.Add(RawStatement.ChildNodes[I]);

          if NodeList.Count = 0 then
            raise EParserException.Create(Position.Y, Position.X, 'Illegal expression');

          RHS := FStack.AddChild(ntRHS, False);
          RHS.Col  := NodeList[0].Col;
          RHS.Line := NodeList[0].Line;
          TExpressionTools.RawNodeListToTree(RawStatement, NodeList, RHS);
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
        FStack.Peek.Col  := Position.X;
        FStack.Peek.Line := Position.Y;

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
  FStack.Push(ntType).SetAttribute(anName, Lexer.Token);
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

  Node := FStack.AddValuedChild(ntLiteral, Str);
  Node.SetAttribute(anType, 'string');
end;

procedure TPasSyntaxTreeBuilder.StringConstSimple;
begin
  //TODO support ptAsciiChar
  FStack.AddValuedChild(ntLiteral, AnsiDequotedStr(Lexer.Token, ''''));
  inherited;
end;

procedure TPasSyntaxTreeBuilder.StringStatement;
begin
  FStack.AddChild(ntType).SetAttribute(anName, Lexer.Token);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.StructuredType;
begin
  FStack.Push(ntType).SetAttribute(anType, Lexer.Token);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.SubrangeType;
begin
  FStack.Push(ntType).SetAttribute(anName, sSUBRANGE);
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
  FStack.PushCompoundSyntaxNode(ntTypeDecl).SetAttribute(anName, Lexer.Token);
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
      InnerTypeName := InnerTypeNode.GetAttribute(anName);
      for SubNode in InnerTypeNode.ChildNodes do 
        TypeNode.AddChild(SubNode.Clone);
        
      TypeNode.DeleteChild(InnerTypeNode);
    end;       
    
    TypeName := '';
    for i := TypeNode.ChildNodes.Count - 1 downto 0 do
    begin
      SubNode := TypeNode.ChildNodes[i];
      if SubNode.Typ = ntType then
      begin
        if TypeName <> '' then
          TypeName := '.' + TypeName;
          
        TypeName := SubNode.GetAttribute(anName) + TypeName;
        TypeNode.DeleteChild(SubNode);
      end;       
    end;
    
    if TypeName <> '' then
      TypeName := '.' + TypeName;   
    TypeName := InnerTypeName + TypeName;  
      
    TypeNode.SetAttribute(anName, TypeName);
   
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
  FStack.Push(ntType).SetAttribute(anName, Lexer.Token);
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
  FStack.Peek.Col  := Lexer.PosXY.X;
  FStack.Peek.Line := Lexer.PosXY.Y;
  inherited;
end;

procedure TPasSyntaxTreeBuilder.UnitId;
begin
  FStack.AddChild(ntUnknown, False).SetAttribute(anName, Lexer.Token);
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

    FStack.Peek.SetAttribute(anName, NodeListToString(NamesNode));
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
    UnitNode.SetAttribute(anName, NodeListToString(NamesNode));
    UnitNode.Col  := Position.X;
    UnitNode.Line := Position.Y;
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
  FStack.AddValuedChild(ntName, Lexer.Token);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.VarParameter;
begin
  FStack.Push(ntParameters).SetAttribute(anKind, 'var');
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
          FStack.Peek.Col  := Variable.Col;
          FStack.Peek.Line := Variable.Line;

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
    FStack.Peek.SetAttribute(anVisibility, 'true');
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.VisibilityProtected;
begin
  FStack.Push(ntProtected);
  try
    FStack.Peek.SetAttribute(anVisibility, 'true');
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.VisibilityPublic;
begin
  FStack.Push(ntPublic);
  try
    FStack.Peek.SetAttribute(anVisibility, 'true');
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.VisibilityPublished;
begin
  FStack.Push(ntPublished);
  try
    FStack.Peek.SetAttribute(anVisibility, 'true');
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
  SetPositionAttributes: Boolean): TSyntaxNode;
begin
  Result := FStack.Peek.AddChild(TSyntaxNode.Create(Typ));

  if SetPositionAttributes then
  begin
    Result.Col  := FParser.Lexer.PosXY.X;
    Result.Line := FParser.Lexer.PosXY.Y;
  end;
end;

function TNodeStack.AddChild(Node: TSyntaxNode): TSyntaxNode;
begin
  Result := FStack.Peek.AddChild(Node);
end;

function TNodeStack.AddValuedChild(Typ: TSyntaxNodeType;
  const Value: string): TSyntaxNode;
begin
  Result := FStack.Peek.AddChild(TValuedSyntaxNode.Create(Typ));
  Result.Col  := FParser.Lexer.PosXY.X;
  Result.Line := FParser.Lexer.PosXY.Y;

  TValuedSyntaxNode(Result).Value := Value;
end;

procedure TNodeStack.Clear;
begin
  FStack.Clear;
end;

constructor TNodeStack.Create(Parser: TmwSimplePasPar);
begin
  FParser := Parser;
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

function TNodeStack.Push(Node: TSyntaxNode; SetPositionAttributes: Boolean): TSyntaxNode;
begin
  FStack.Push(Node);
  Result := Node;

  if SetPositionAttributes then
  begin
    Result.Col  := FParser.Lexer.PosXY.X;
    Result.Line := FParser.Lexer.PosXY.Y;
  end;
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

function TNodeStack.Push(Typ: TSyntaxNodeType; SetPositionAttributes: Boolean = True): TSyntaxNode;
begin
  Result := FStack.Peek.AddChild(TSyntaxNode.Create(Typ));
  Push(Result, SetPositionAttributes);
end;

{ ESyntaxTreeException }

constructor ESyntaxTreeException.Create(Line, Col: Integer; Msg: string;
  SyntaxTree: TSyntaxNode);
begin
  inherited Create(Line, Col, Msg);
  FSyntaxTree := SyntaxTree;
end;

destructor ESyntaxTreeException.Destroy;
begin
  FSyntaxTree.Free;
  inherited;
end;

end.
