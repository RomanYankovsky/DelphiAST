unit DelphiAST;

interface

uses
  System.SysUtils, System.Classes, Generics.Collections, SimpleParser,
  SimpleParser.Lexer.Types, DelphiAST.Classes;

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

    function GetCount: Integer;
  public
    constructor Create(Parser: TmwSimplePasPar);
    destructor Destroy; override;

    function AddChild(const Name: string; SetPositionAttributes: Boolean = True): TSyntaxNode; overload;
    function AddChild(const Name: TSyntaxNode): TSyntaxNode; overload;

    procedure Clear;
    function Peek: TSyntaxNode;
    procedure Pop;

    function Push(const Name: string; SetPositionAttributes: Boolean = True): TSyntaxNode; overload;
    function Push(Node: TSyntaxNode; SetPositionAttributes: Boolean = True): TSyntaxNode; overload;

    property Count: Integer read GetCount;
  end;

  TPasSyntaxTreeBuilder = class(TmwSimplePasPar)
  private
    FStack: TNodeStack;
    procedure ParserMessage(Sender: TObject; const Typ: TMessageEventType; const Msg: string; X, Y: Integer);
    function NodeListToString(NamesNode: TSyntaxNode): string;
  protected
    procedure AccessSpecifier; override;
    procedure AdditiveOperator; override;
    procedure AddressOp; override;
    procedure AlignmentParameter; override;
    procedure AnonymousMethod; override;
    procedure AsmStatement; override;
    procedure AsOp; override;
    procedure AssignOp; override;
    procedure AtExpression; override;
    procedure CaseElseStatement; override;
    procedure CaseLabel; override;
    procedure CaseSelector; override;
    procedure CaseStatement; override;
    procedure ClassField; override;
    procedure ClassForward; override;
    procedure ClassMethod; override;
    procedure ClassMethodHeading; override;
    procedure ClassProperty; override;
    procedure ClassReferenceType; override;
    procedure ClassType; override;
    procedure ConstParameter; override;
    procedure ConstantDeclaration; override;
    procedure ConstantName; override;
    procedure ConstSection; override;
    procedure ConstantValue; override;
    procedure ConstantValueTyped; override;
    procedure ConstructorName; override;
    procedure ContainsClause; override;
    procedure Designator; override;
    procedure DestructorName; override;
    procedure DirectiveBinding; override;
    procedure DotOp; override;
    procedure ElseStatement; override;
    procedure ExceptBlock; override;
    procedure ExceptionHandler; override;
    procedure ExportedHeading; override;
    procedure Expression; override;
    procedure ExpressionList; override;
    procedure FieldName; override;
    procedure FinallyBlock; override;
    procedure FormalParameterList; override;
    procedure ForStatement; override;
    procedure FunctionHeading; override;
    procedure FunctionMethodName; override;
    procedure FunctionProcedureName; override;
    procedure GotoStatement; override;
    procedure IfStatement; override;
    procedure Identifier; override;
    procedure ImplementationSection; override;
    procedure IndexOp; override;
    procedure InheritedStatement; override;
    procedure InheritedVariableReference; override;
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
    procedure ProcedureHeading; override;
    procedure ProcedureDeclarationSection; override;
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
    procedure StatementList; override;
    procedure StringConst; override;
    procedure StringConstSimple; override;
    procedure StringType; override;
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
    procedure WithStatement; override;
  public
    constructor Create;
    destructor Destroy; override;

    function Run(SourceStream: TCustomMemoryStream): TSyntaxNode; reintroduce;
  end;

implementation

uses
  DelphiAST.Consts;

{ TPasSyntaxTreeBuilder }

procedure TPasSyntaxTreeBuilder.AccessSpecifier;
begin
  FStack.Push(Lexer.Token);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.AdditiveOperator;
begin
  case TokenID of
    ptMinus: FStack.AddChild(sSUB);
    ptOr: FStack.AddChild(sOR);
    ptPlus: FStack.AddChild(sADD);
    ptXor: FStack.AddChild(sXOR);
  end;

  inherited;
end;

procedure TPasSyntaxTreeBuilder.AddressOp;
begin
  FStack.Push(sADDR);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.AlignmentParameter;
begin
  FStack.Push(sALIGNMENTPARAM);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.AnonymousMethod;
begin
  FStack.Push('anonymousmethod');
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.AsmStatement;
begin
  FStack.Push('STATEMENTS', False).SetAttribute('type', 'asm');
  try
    FStack.Peek.SetPositionAttributes(Lexer.PosXY, 'begin_line', 'begin_col');
    inherited;
    FStack.Peek.SetPositionAttributes(Lexer.PosXY, 'end_line', 'end_col');
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.AsOp;
begin
  FStack.AddChild(sAS);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.AssignOp;
begin
  FStack.AddChild(sASSIGN);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.AtExpression;
begin
  FStack.Push(Lexer.Token);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.CaseElseStatement;
begin
  FStack.Push('caseelse');
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.CaseLabel;
begin
  FStack.Push('caselabel');
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.CaseSelector;
begin
  FStack.Push('caseselector');
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.CaseStatement;
begin
  FStack.Push(Lexer.Token);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ClassField;
var
  Fields: TSyntaxNode;
  Field, TypeInfo, TypeArgs: TSyntaxNode;
begin
  Fields := TSyntaxNode.Create('fields');
  try
    FStack.Push(Fields);
    try
      inherited;
    finally
      FStack.Pop;
    end;

    TypeInfo := Fields.FindNode(UpperCase(sTYPE));
    TypeArgs := Fields.FindNode('TYPEARGS');
    for Field in Fields.ChildNodes do
    begin
      if not SameText(Field.Name, sNAME) then
        Continue;

      FStack.Push('field', False);
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
  FStack.Peek.SetAttribute('forwarded', 'true');
  inherited ClassForward;
end;

procedure TPasSyntaxTreeBuilder.ClassMethod;
begin
  FStack.Peek.SetAttribute('class', 'true');
  inherited;
end;

procedure TPasSyntaxTreeBuilder.ClassMethodHeading;
begin
  FStack.Push(sMETHOD);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ClassProperty;
begin
  FStack.Push(Lexer.Token);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ClassReferenceType;
begin
  FStack.Peek.SetAttribute(sTYPE, 'classof');
  inherited ClassReferenceType;
end;

procedure TPasSyntaxTreeBuilder.ClassType;
var
  classDef, child, vis: TSyntaxNode;
  i: integer;
  extracted: boolean;
begin
  FStack.Peek.SetAttribute(sTYPE, 'class');
  inherited;
  classDef := FStack.Peek;
  vis := nil;
  i := 0;
  while i < classDef.ChildNodes.Count do
  begin
    child := classDef.ChildNodes[i];
    extracted := false;
    if child.HasAttribute('Visibility') then
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
  FStack.Push(sPARAMETERS).SetAttribute('kind', 'const');
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ConstructorName;
begin
  FStack.Peek.SetAttribute('constructor', 'true');
  FStack.Peek.SetAttribute('name', Lexer.Token);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.ConstantDeclaration;
begin
  FStack.Push(sCONSTANT);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ConstantName;
begin
  FStack.AddChild(sNAME).SetAttribute(sVALUE, Lexer.Token);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.ConstantValue;
begin
  FStack.Push(sVALUE);
  try
    inherited ConstantValue;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ConstantValueTyped;
begin
  FStack.Push(sVALUE);
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
  ConstSect := TSyntaxNode.Create('constants');
  try
    FStack.Push(ConstSect);
    try
      inherited ConstSection;
    finally
      FStack.Pop;
    end;

    FStack.Push(sCONSTANTS);
    for ConstList in ConstSect.ChildNodes do
    begin
      TypeInfo := ConstList.FindNode(UpperCase(sTYPE));
      Value := ConstList.FindNode(UpperCase(sVALUE));
      for Constant in ConstList.ChildNodes do
      begin
        if not SameText(Constant.Name, sNAME) then
          Continue;

        FStack.Push(sCONSTANT, False);
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
  FStack.Push(sCONTAINS);
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
  FStack.Peek.SetAttribute('destructor', 'true');
  FStack.Peek.SetAttribute('name', Lexer.Token);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.DirectiveBinding;
begin
  FStack.Peek.SetAttribute(Lexer.Token, 'true');
  inherited;
end;

procedure TPasSyntaxTreeBuilder.DotOp;
begin
  FStack.AddChild(sDOT, False);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.ElseStatement;
begin
  FStack.Push(Lexer.Token);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ExceptBlock;
begin
  FStack.Push('except');
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ExceptionHandler;
begin
  FStack.Push('exceptionhandler');
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ExportedHeading;
begin
  FStack.Push(sMETHOD);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.Expression;
var
  RawExprNode: TSyntaxNode;
  ExprNode: TSyntaxNode;

  NodeList: TList<TSyntaxNode>;
  Node: TSyntaxNode;
  Col, Line: Integer;
begin
  Line := Lexer.PosXY.Y;
  Col := Lexer.PosXY.X;

  RawExprNode := TSyntaxNode.Create('expression');
  try
    FStack.Push(RawExprNode);
    try
      inherited;
    finally
      FStack.Pop;
    end;

    if RawExprNode.HasChildren then
    begin
      ExprNode := FStack.Push('EXPRESSION', False);
      try
        ExprNode.SetAttribute('line', IntToStr(Line));
        ExprNode.SetAttribute('col', IntToStr(Col));

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

procedure TPasSyntaxTreeBuilder.ExpressionList;
begin
  FStack.Push('expressions', False);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.FieldName;
begin
  FStack.AddChild(sNAME).SetAttribute(sVALUE, Lexer.Token);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.FinallyBlock;
begin
  FStack.Push('finally');
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.FormalParameterList;
var
  Params: TSyntaxNode;
  ParamList, Param, TypeInfo: TSyntaxNode;
  ParamKind: string;
  Position: TTokenPoint;
begin
  Position := Lexer.PosXY;

  Params := TSyntaxNode.Create('params');
  try
    FStack.Push(Params);
    try
      inherited;
    finally
      FStack.Pop;
    end;

    FStack.Push(sPARAMETERS, False).SetPositionAttributes(Position);
    for ParamList in Params.ChildNodes do
    begin
      TypeInfo := ParamList.FindNode(sTYPE);
      ParamKind := ParamList.GetAttribute('kind');
      for Param in ParamList.ChildNodes do
      begin
        if not SameText(Param.Name, sNAME) then
          Continue;

        FStack.Push(sPARAMETER, False);
        if ParamKind <> '' then
          FStack.Peek.SetAttribute('kind', ParamKind);

        FStack.AddChild(Param.Clone);
        if Assigned(TypeInfo) then
          FStack.AddChild(TypeInfo.Clone);

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
  FStack.Push(Lexer.Token);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.FunctionHeading;
begin
  FStack.Peek.SetAttribute('kind', 'function');
  inherited;
end;

procedure TPasSyntaxTreeBuilder.FunctionMethodName;
begin
  FStack.AddChild(sNAME).SetAttribute(sVALUE, Lexer.Token);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.FunctionProcedureName;
var
  ChildNode, nameNode: TSyntaxNode;
  FullName: string;
begin
  FStack.Push(sNAME);
  nameNode := FStack.Peek;
  try
    inherited;
    for ChildNode in nameNode.ChildNodes do
    begin
      if FullName <> '' then
        FullName := FullName + '.';
      FullName := FullName + ChildNode.GetAttribute(sVALUE);
    end;
  finally
    FStack.Pop;
    FStack.Peek.SetAttribute(sNAME, FullName);
    FStack.Peek.DeleteChild(nameNode);
  end;
end;

procedure TPasSyntaxTreeBuilder.GotoStatement;
begin
  FStack.Push(Lexer.Token);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.Identifier;
begin
  FStack.AddChild(sIDENTIFIER).SetAttribute(sNAME, Lexer.Token);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.IfStatement;
begin
  FStack.Push(Lexer.Token);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ImplementationSection;
begin
  FStack.Push(Lexer.Token);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.IndexOp;
begin
  FStack.Push(sINDEXED);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.InheritedStatement;
begin
  FStack.Push(Lexer.Token);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.InheritedVariableReference;
begin
  FStack.Push(Lexer.Token);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.InterfaceSection;
begin
  FStack.Push(Lexer.Token);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.InterfaceType;
begin
  FStack.Peek.SetAttribute(sTYPE, 'interface');
  inherited;
end;

procedure TPasSyntaxTreeBuilder.LabelId;
begin
  FStack.AddChild('label').SetAttribute(sVALUE, Lexer.Token);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.MainUsedUnitStatement;
var
  NameNode, PathNode: TSyntaxNode;
begin
  FStack.Push(sUNIT);
  try
    inherited;
    NameNode := FStack.Peek.FindNode(sUNIT);
    PathNode := FStack.Peek.FindNode(sLITERAL);

    if Assigned(NameNode) then
    begin
      FStack.Peek.SetAttribute(sNAME, NameNode.GetAttribute(sNAME));
      FStack.Peek.DeleteChild(NameNode);
    end;

    if Assigned(PathNode) then
    begin
      FStack.Peek.SetAttribute(sPATH, PathNode.GetAttribute(sVALUE));
      FStack.Peek.DeleteChild(PathNode);
    end;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.MainUsesClause;
begin
  FStack.Push(sUSES);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.MethodKind;
begin
  FStack.Peek.SetAttribute('kind', LowerCase(Lexer.Token));
  inherited;
end;

procedure TPasSyntaxTreeBuilder.MultiplicativeOperator;
begin
  case TokenID of
    ptAnd:
      begin
        FStack.AddChild(sAND);
      end;
    ptDiv:
      begin
        FStack.AddChild(sDIV);
      end;
    ptMod:
      begin
        FStack.AddChild(sMOD);
      end;
    ptShl:
      begin
        FStack.AddChild(sSHL);
      end;
    ptShr:
      begin
        FStack.AddChild(sSHR);
      end;
    ptSlash:
      begin
        FStack.AddChild(sFDIV);
      end;
    ptStar:
      begin
        FStack.AddChild(sMUL);
      end;
  end;

  inherited;
end;

procedure TPasSyntaxTreeBuilder.NewFormalParameterType;
begin
  FStack.AddChild(sTYPE).SetAttribute(sNAME, Lexer.Token);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.NilToken;
begin
  FStack.AddChild(sLITERAL).SetAttribute(sTYPE, 'nil');
  inherited;
end;

procedure TPasSyntaxTreeBuilder.NotOp;
begin
  FStack.AddChild(sNOT);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.Number;
var
  Node: TSyntaxNode;
begin
  Node := FStack.AddChild(sLITERAL);
  Node.SetAttribute(sTYPE, 'numeric');
  Node.SetAttribute(sVALUE, Lexer.Token);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.ObjectNameOfMethod;
begin
  FStack.AddChild(sNAME).SetAttribute(sVALUE, Lexer.Token);
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
  FStack.Push(sPARAMETERS).SetAttribute('kind', 'out');
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ParameterFormal;
begin
  FStack.Push(sPARAMETERS);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ParameterName;
begin
  FStack.AddChild(sNAME).SetAttribute(sVALUE, Lexer.Token);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.PointerSymbol;
begin
  FStack.AddChild(sDEREF);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.PointerType;
begin
  FStack.Peek.SetAttribute(sTYPE, 'pointer');
  inherited;
end;

procedure TPasSyntaxTreeBuilder.ProcedureDeclarationSection;
begin
  FStack.Push('method');
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ProcedureHeading;
begin
  FStack.Peek.SetAttribute('kind', 'procedure');
  inherited;
end;

procedure TPasSyntaxTreeBuilder.ProcedureProcedureName;
begin
  FStack.Peek.SetAttribute(sNAME, Lexer.Token);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.PropertyName;
begin
  FStack.Peek.SetAttribute('name', Lexer.Token);
  inherited PropertyName;
end;

procedure TPasSyntaxTreeBuilder.PropertyParameterList;
begin
  FStack.Push('Parameters');
  try
    inherited PropertyParameterList;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.RaiseStatement;
begin
  FStack.Push(Lexer.Token);
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
  Node := FStack.Push('Field');
  Node.SetAttribute(sTYPE, 'name');
  Node.SetAttribute(sVALUE, Lexer.Token);
  inherited RecordFieldConstant;
  FStack.Pop;
end;

procedure TPasSyntaxTreeBuilder.RelativeOperator;
begin
  case TokenID of
    ptAs:
      begin
        FStack.AddChild(sAS);
      end;
    ptEqual:
      begin
        FStack.AddChild(sEQUAL);
      end;
    ptGreater:
      begin
        FStack.AddChild(sGREATER);
      end;
    ptGreaterEqual:
      begin
        FStack.AddChild(sGREATEREQUAL);
      end;
    ptIn:
      begin
        FStack.AddChild(sIN);
      end;
    ptIs:
      begin
        FStack.AddChild(sIS);
      end;
    ptLower:
      begin
        FStack.AddChild(sLOWER);
      end;
    ptLowerEqual:
      begin
        FStack.AddChild(sLOWEREQUAL);
      end;
    ptNotEqual:
      begin
        FStack.AddChild(sNOTEQUAL);
      end;
  end;

  inherited;
end;

procedure TPasSyntaxTreeBuilder.RepeatStatement;
begin
  FStack.Push(sREPEAT);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.RequiresClause;
begin
  FStack.Push('requires');
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
  NamesNode := TSyntaxNode.Create('requires');
  try
    FStack.Push(NamesNode);
    try
      inherited;
    finally
      FStack.Pop;
    end;

    FStack.AddChild('package').SetAttribute(sNAME, NodeListToString(NamesNode));
  finally
    NamesNode.Free;
  end;
end;

procedure TPasSyntaxTreeBuilder.RequiresIdentifierId;
begin
  FStack.AddChild(Lexer.Token, False);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.ReturnType;
begin
  FStack.Push(sRETURNTYPE);
  try
    inherited;
  finally
    FStack.Pop
  end;
end;

procedure TPasSyntaxTreeBuilder.RoundClose;
begin
  FStack.AddChild(sROUNDCLOSE);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.RoundOpen;
begin
  FStack.AddChild(sROUNDOPEN);
  inherited;
end;

function TPasSyntaxTreeBuilder.Run(SourceStream: TCustomMemoryStream): TSyntaxNode;
begin
  Result := TSyntaxNode.Create(sUNIT);
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
    Result := Result + NamePartNode.Name;
  end;
end;

procedure TPasSyntaxTreeBuilder.SetConstructor;
begin
  FStack.Push('SET');
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.SetElement;
begin
  FStack.Push('element');
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

  RawStatement := TSyntaxNode.Create('STATEMENT');
  try
    FStack.Push(RawStatement);
    try
      inherited;
    finally
      FStack.Pop;
    end;

    if not RawStatement.HasChildren then
      Exit;

    if RawStatement.FindNode(sASSIGN) <> nil then
    begin
      FStack.Push(sASSIGN, False);
      try
        FStack.Peek.SetPositionAttributes(Position);

        NodeList := TList<TSyntaxNode>.Create;
        try
          AssignIdx := -1;
          for I := 0 to RawStatement.ChildNodes.Count - 1 do
          begin
            if RawStatement.ChildNodes[I].Name = sASSIGN then
            begin
              AssignIdx := I;
              Break;
            end;
            NodeList.Add(RawStatement.ChildNodes[I]);
          end;
          TExpressionTools.RawNodeListToTree(RawStatement, NodeList, FStack.AddChild('LHS', False));

          NodeList.Clear;

          for I := AssignIdx + 1 to RawStatement.ChildNodes.Count - 1 do
            NodeList.Add(RawStatement.ChildNodes[I]);
          TExpressionTools.RawNodeListToTree(RawStatement, NodeList, FStack.AddChild('RHS', False));
        finally
          NodeList.Free;
        end;
      finally
        FStack.Pop;
      end;
    end else
    begin
      FStack.Push(sCALL, False);
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

procedure TPasSyntaxTreeBuilder.StatementList;
begin
  FStack.Push('statements', False);
  try
    FStack.Peek.SetPositionAttributes(Lexer.PosXY, 'begin_line', 'begin_col');
    inherited;
    FStack.Peek.SetPositionAttributes(Lexer.PosXY, 'end_line', 'end_col');
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
  StrConst := TSyntaxNode.Create('StringConst');
  try
    FStack.Push(StrConst);
    try
      inherited;
    finally
      FStack.Pop;
    end;

    Str := '';
    for Literal in StrConst.ChildNodes do
      Str := Str + Literal.GetAttribute(sValue);
  finally
    StrConst.Free;
  end;

  Node := FStack.AddChild(sLITERAL);
  Node.SetAttribute(sTYPE, 'string');
  Node.SetAttribute(sVALUE, Str);
end;

procedure TPasSyntaxTreeBuilder.StringConstSimple;
begin
  //TODO support ptAsciiChar
  FStack.AddChild(sLITERAL).SetAttribute(sVALUE, Lexer.Token.DeQuotedString);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.StringType;
begin
  FStack.Push(sTYPE).SetAttribute(sNAME, Lexer.Token);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ThenStatement;
begin
  FStack.Push(Lexer.Token);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.TryStatement;
begin
  FStack.Push('try');
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.TypeArgs;
begin
  FStack.Push(sTYPEARGS);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.TypeDeclaration;
begin
  FStack.Push('TYPEDECL').SetAttribute(sNAME, Lexer.Token);
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
      compound := FStack.push(sTYPE);
      case TokenID of
        ptArray: compoundType := 'ArrayOf';
        ptFile: compoundType := 'FileOf';
        ptSet: compoundType := 'SetOf';
      end;
      compound.SetAttribute(sNAME, compoundType);
      compound.SetAttribute('compound', 'true');
    end
    else begin
      FStack.AddChild(sTYPE).SetAttribute(sNAME, Lexer.Token);
      compound := nil;
    end;
  end;
  try
    inherited;
  finally
    if assigned(compound) then
    begin
      FStack.Pop;
      found := false;
      for i := compound.ChildNodes.Count - 1 downto 0 do
        if compound.ChildNodes[i].name = sTYPE then
          if found then
            compound.ChildNodes.Delete(i)
          else found := true;
    end;
  end;
end;

procedure TPasSyntaxTreeBuilder.TypeParams;
begin
  FStack.Push('TYPEPARAMS');
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.TypeSection;
begin
  FStack.Push('TYPESECTION');
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.TypeSimple;
begin
  FStack.Push(sTYPE).SetAttribute(sNAME, Lexer.Token);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.UnaryMinus;
begin
  FStack.AddChild(sUNARYMINUS);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.UnitFile;
begin
  FStack.Peek.SetPositionAttributes(Lexer.PosXY);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.UnitId;
begin
  FStack.AddChild(Lexer.Token, False);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.UnitName;
var
  NamesNode: TSyntaxNode;
begin
  NamesNode := TSyntaxNode.Create('unit');
  try
    FStack.Push(NamesNode);
    try
      inherited;
    finally
      FStack.Pop;
    end;

    FStack.Peek.SetAttribute(sNAME, NodeListToString(NamesNode));
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

  NamesNode := TSyntaxNode.Create('unit');
  try
    FStack.Push(NamesNode);
    try
      inherited;
    finally
      FStack.Pop;
    end;

    UnitNode := FStack.AddChild(sUNIT);
    UnitNode.SetAttribute(sNAME, NodeListToString(NamesNode));
    UnitNode.SetPositionAttributes(Position);
  finally
    NamesNode.Free;
  end;
end;

procedure TPasSyntaxTreeBuilder.UsesClause;
begin
  FStack.Push(sUSES);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.VarDeclaration;
begin
  FStack.Push(sVARIABLES);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.VarName;
begin
  FStack.AddChild(sNAME).SetAttribute(sVALUE, Lexer.Token);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.VarParameter;
begin
  FStack.Push(sPARAMETERS).SetAttribute('kind', 'var');
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.VarSection;
var
  VarSect: TSyntaxNode;
  VarList, Variable, TypeInfo: TSyntaxNode;
begin
  VarSect := TSyntaxNode.Create('variables');
  try
    FStack.Push(VarSect);
    try
      inherited VarSection;
    finally
      FStack.Pop;
    end;

    FStack.Push(sVARIABLES);
    for VarList in VarSect.ChildNodes do
    begin
      TypeInfo := VarList.FindNode(UpperCase(sTYPE));
      for Variable in VarList.ChildNodes do
      begin
        if not SameText(Variable.Name, sNAME) then
          Continue;

        FStack.Push(sVARIABLE, False);
        try
          FStack.AddChild(Variable.Clone);
          FStack.AddChild(TypeInfo.Clone);
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
  FStack.Push('private');
  try
    FStack.Peek.SetAttribute('Visibility', 'True');
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.VisibilityProtected;
begin
  FStack.Push('protected');
  try
    FStack.Peek.SetAttribute('Visibility', 'True');
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.VisibilityPublic;
begin
  FStack.Push('public');
  try
    FStack.Peek.SetAttribute('Visibility', 'True');
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.VisibilityPublished;
begin
  FStack.Push('published');
  try
    FStack.Peek.SetAttribute('Visibility', 'True');
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.WhileStatement;
begin
  FStack.Push(Lexer.Token);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.WithStatement;
begin
  FStack.Push(Lexer.Token);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

{ TNodeStack }

function TNodeStack.AddChild(const Name: string;
  SetPositionAttributes: Boolean): TSyntaxNode;
begin
  Result := FStack.Peek.AddChild(TSyntaxNode.Create(Name));

  if SetPositionAttributes then
    Result.SetPositionAttributes(FParser.Lexer.PosXY);
end;

function TNodeStack.AddChild(const Name: TSyntaxNode): TSyntaxNode;
begin
  Result := FStack.Peek.AddChild(Name);
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

procedure TNodeStack.Pop;
begin
  FStack.Pop;
end;

function TNodeStack.Push(Node: TSyntaxNode; SetPositionAttributes: Boolean): TSyntaxNode;
begin
  FStack.Push(Node);
  Result := Node;

  if SetPositionAttributes then
    Result.SetPositionAttributes(FParser.Lexer.PosXY);
end;

function TNodeStack.Push(const Name: string; SetPositionAttributes: Boolean = True): TSyntaxNode;
begin
  Result := FStack.Peek.AddChild(TSyntaxNode.Create(Name));
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
