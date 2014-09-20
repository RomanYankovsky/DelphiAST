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
    procedure ClassMethod; override;
    procedure ClassMethodHeading; override;
    procedure ClassProperty; override;
    procedure ClassType; override;
    procedure ConstParameter; override;
    procedure ContainsClause; override;
    procedure ContainsIdentifier; override;
    procedure ContainsIdentifierId; override;
    procedure ContainsStatement; override;
    procedure Designator; override;
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
    procedure RaiseStatement; override;
    procedure RelativeOperator; override;
    procedure RepeatStatement; override;
    procedure RequiresClause; override;
    procedure RequiresIdentifier; override;
    procedure RequiresIdentifierId; override;
    procedure RoundClose; override;
    procedure RoundOpen; override;
    procedure SetConstructor; override;
    procedure SetElement; override;
    procedure SimpleStatement; override;
    procedure StatementList; override;
    procedure StringConst; override;
    procedure StringConstSimple; override;
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
  Field, TypeInfo: TSyntaxNode;
begin
  Fields := TSyntaxNode.Create('fields');
  try
    FStack.Push(Fields);
    try
      inherited;
    finally
      FStack.Pop;
    end;

    FStack.Push('fields');
    try
      TypeInfo := Fields.FindNode(UpperCase(sTYPE));
      for Field in Fields.ChildNodes do
      begin
        if not SameText(Field.Name, sNAME) then
          Continue;

        FStack.Push('field', False);
        try
          FStack.AddChild(Field.Clone);
          FStack.AddChild(TypeInfo.Clone);
        finally
          FStack.Pop;
        end;
      end;
    finally
      FStack.Pop;
    end;
  finally
    Fields.Free;
  end;
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

procedure TPasSyntaxTreeBuilder.ClassType;
begin
  FStack.Peek.SetAttribute(sTYPE, 'class');
  inherited;
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

procedure TPasSyntaxTreeBuilder.ContainsClause;
begin
  FStack.Push(sCONTAINS);
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.ContainsIdentifier;
var
  NamesNode: TSyntaxNode;
begin
  NamesNode := TSyntaxNode.Create(sCONTAINS);
  try
    FStack.Push(NamesNode);
    try
      inherited;
    finally
      FStack.Pop;
    end;

    FStack.AddChild(sNAME).SetAttribute(sVALUE, NodeListToString(NamesNode));
  finally
    NamesNode.Free;
  end;
end;

procedure TPasSyntaxTreeBuilder.ContainsIdentifierId;
begin
  FStack.AddChild(Lexer.Token, False);
  inherited;
end;

procedure TPasSyntaxTreeBuilder.ContainsStatement;
begin
  FStack.Push(sUNIT, False);
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
  ChildNode: TSyntaxNode;
  FullName: string;
begin
  FStack.Push(sNAME);
  try
    inherited;
    for ChildNode in FStack.Peek.ChildNodes do
    begin
      if FullName <> '' then
        FullName := FullName + '.';
      FullName := FullName + ChildNode.GetAttribute(sVALUE);
    end;
  finally
    FStack.Pop;
    FStack.Peek.SetAttribute(sNAME, FullName);
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

procedure TPasSyntaxTreeBuilder.RaiseStatement;
begin
  FStack.Push(Lexer.Token);
  try
    inherited;
  finally
    FStack.Pop;
  end;
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
  I, AssignIdx, Col, Line: Integer;
begin
  Line := Lexer.PosXY.Y;
  Col := Lexer.PosXY.X;

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
        FStack.Peek.SetAttribute('line', Line.ToString);
        FStack.Peek.SetAttribute('col', Col.ToString);

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
        FStack.Peek.SetAttribute('line', Line.ToString);
        FStack.Peek.SetAttribute('col', Col.ToString);

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
  FStack.Push('TYPEARGS');
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
begin
  FStack.AddChild(sTYPE).SetAttribute(sNAME, Lexer.Token);
  inherited;
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
  NamesNode, NamePartNode, UnitNode: TSyntaxNode;
  Name: string;
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

    Name := '';
    for NamePartNode in NamesNode.ChildNodes do
    begin
      if Name <> '' then
        Name := Name + '.';
      Name := Name + NamePartNode.Name;
    end;
  finally
    NamesNode.Free;
  end;

  UnitNode := FStack.AddChild(sUNIT);
  UnitNode.SetAttribute(sNAME, Name);
  UnitNode.SetPositionAttributes(Position);
end;

procedure TPasSyntaxTreeBuilder.UsesClause;
begin
  FStack.Push(Lexer.Token);
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
  FStack.Push(sPARAMETERS).SetAttribute('kind', 'out');
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
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.VisibilityProtected;
begin
  FStack.Push('protected');
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.VisibilityPublic;
begin
  FStack.Push('public');
  try
    inherited;
  finally
    FStack.Pop;
  end;
end;

procedure TPasSyntaxTreeBuilder.VisibilityPublished;
begin
  FStack.Push('published');
  try
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
