unit DelphiAST.Classes;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  SysUtils, Generics.Collections, SimpleParser.Lexer.Types, DelphiAST.Consts;

type
  EParserException = class(Exception)
  strict private
    FLine, FCol: Integer;
  public
    constructor Create(Line, Col: Integer; Msg: string); reintroduce;

    property Line: Integer read FLine;
    property Col: Integer read FCol;
  end;

  TSyntaxNodeClass = class of TSyntaxNode;
  TSyntaxNode = class
  private
    FCol: Integer;
    FLine: Integer;
    function GetHasChildren: Boolean;
    function GetHasAttributes: Boolean;
  protected
    FAttributes: TDictionary<TAttributeName, string>;
    FChildNodes: TObjectList<TSyntaxNode>;
    FTyp: TSyntaxNodeType;
    FParentNode: TSyntaxNode;
  public
    constructor Create(Typ: TSyntaxNodeType);
    destructor Destroy; override;

    function Clone: TSyntaxNode; virtual;

    function GetAttribute(const Key: TAttributeName): string;
    function HasAttribute(const Key: TAttributeName): boolean;
    procedure SetAttribute(const Key: TAttributeName; Value: string);

    function AddChild(Node: TSyntaxNode): TSyntaxNode; overload;
    function AddChild(Typ: TSyntaxNodeType): TSyntaxNode; overload;
    procedure DeleteChild(Node: TSyntaxNode);

    function FindNode(Typ: TSyntaxNodeType): TSyntaxNode;

    property Attributes: TDictionary<TAttributeName, string> read FAttributes;
    property ChildNodes: TObjectList<TSyntaxNode> read FChildNodes;
    property HasAttributes: Boolean read GetHasAttributes;
    property HasChildren: Boolean read GetHasChildren;
    property Typ: TSyntaxNodeType read FTyp;
    property ParentNode: TSyntaxNode read FParentNode;

    property Col: Integer read FCol write FCol;
    property Line: Integer read FLine write FLine;
  end;

  TCompoundSyntaxNode = class(TSyntaxNode)
  private
    FEndCol: Integer;
    FEndLine: Integer;
  public
    function Clone: TSyntaxNode; override;

    property EndCol: Integer read FEndCol write FEndCol;
    property EndLine: Integer read FEndLine write FEndLine;
  end;

  TValuedSyntaxNode = class(TSyntaxNode)
  private
    FValue: string;
  public
    function Clone: TSyntaxNode; override;

    property Value: string read FValue write FValue;
  end;

  TExpressionTools = class
  private
    class function CreateNodeWithParentsPosition(NodeType: TSyntaxNodeType; ParentNode: TSyntaxNode): TSyntaxNode;
  public
    class function ExprToReverseNotation(Expr: TList<TSyntaxNode>): TList<TSyntaxNode>; static;
    class procedure NodeListToTree(Expr: TList<TSyntaxNode>; Root: TSyntaxNode); static;
    class function PrepareExpr(ExprNodes: TList<TSyntaxNode>): TObjectList<TSyntaxNode>; static;
    class procedure RawNodeListToTree(RawParentNode: TSyntaxNode; RawNodeList: TList<TSyntaxNode>; NewRoot: TSyntaxNode); static;
  end;

implementation

type
  TOperatorKind = (okUnary, okBinary);
  TOperatorAssocType = (atLeft, atRight);

  TOperatorInfo = record
    Typ: TSyntaxNodeType;
    Priority: Byte;
    Kind: TOperatorKind;
    AssocType: TOperatorAssocType;
  end;

  TOperators = class
  strict private
    class var FOps: TDictionary<TSyntaxNodeType, TOperatorInfo>;
    class constructor Create;
    class destructor Destroy;
    class function GetItem(Typ: TSyntaxNodeType): TOperatorInfo; static;
  public
    class function IsOpName(Typ: TSyntaxNodeType): Boolean;
    class property Items[Typ: TSyntaxNodeType]: TOperatorInfo read GetItem; default;
  end;

  TTreeData = class
  strict private
    FNode: TSyntaxNode;
    FChild1, FChild2: TTreeData;
  public
    constructor Create(Node: TSyntaxNode);
    destructor Destroy; override;

    property Node: TSyntaxNode read FNode;
    property Child1: TTreeData read FChild1 write FChild1;
    property Child2: TTreeData read FChild2 write FChild2;
  end;

const
  OperatorsInfo: array [0..27] of TOperatorInfo =
    ((Typ: ntAddr;         Priority: 1; Kind: okUnary;  AssocType: atRight),
     (Typ: ntDeref;        Priority: 1; Kind: okUnary;  AssocType: atLeft),
     (Typ: ntGeneric;      Priority: 1; Kind: okBinary; AssocType: atRight),
     (Typ: ntIndexed;      Priority: 1; Kind: okUnary;  AssocType: atLeft),
     (Typ: ntDot;          Priority: 2; Kind: okBinary; AssocType: atRight),
     (Typ: ntCall;         Priority: 3; Kind: okBinary; AssocType: atRight),
     (Typ: ntUnaryMinus;   Priority: 5; Kind: okUnary;  AssocType: atRight),
     (Typ: ntNot;          Priority: 6; Kind: okUnary;  AssocType: atRight),
     (Typ: ntMul;          Priority: 7; Kind: okBinary; AssocType: atRight),
     (Typ: ntFDiv;         Priority: 7; Kind: okBinary; AssocType: atRight),
     (Typ: ntDiv;          Priority: 7; Kind: okBinary; AssocType: atRight),
     (Typ: ntMod;          Priority: 7; Kind: okBinary; AssocType: atRight),
     (Typ: ntAnd;          Priority: 7; Kind: okBinary; AssocType: atRight),
     (Typ: ntShl;          Priority: 7; Kind: okBinary; AssocType: atRight),
     (Typ: ntShr;          Priority: 7; Kind: okBinary; AssocType: atRight),
     (Typ: ntAs;           Priority: 7; Kind: okBinary; AssocType: atRight),
     (Typ: ntAdd;          Priority: 8; Kind: okBinary; AssocType: atRight),
     (Typ: ntSub;          Priority: 8; Kind: okBinary; AssocType: atRight),
     (Typ: ntOr;           Priority: 8; Kind: okBinary; AssocType: atRight),
     (Typ: ntXor;          Priority: 8; Kind: okBinary; AssocType: atRight),
     (Typ: ntEqual;        Priority: 9; Kind: okBinary; AssocType: atRight),
     (Typ: ntNotEqual;     Priority: 9; Kind: okBinary; AssocType: atRight),
     (Typ: ntLower;        Priority: 9; Kind: okBinary; AssocType: atRight),
     (Typ: ntGreater;      Priority: 9; Kind: okBinary; AssocType: atRight),
     (Typ: ntLowerEqual;   Priority: 9; Kind: okBinary; AssocType: atRight),
     (Typ: ntGreaterEqual; Priority: 9; Kind: okBinary; AssocType: atRight),
     (Typ: ntIn;           Priority: 9; Kind: okBinary; AssocType: atRight),
     (Typ: ntIs;           Priority: 9; Kind: okBinary; AssocType: atRight));

{ TOperators }

class constructor TOperators.Create;
var
  I: Integer;
begin
  FOps := TDictionary<TSyntaxNodeType, TOperatorInfo>.Create;

  for I := Low(OperatorsInfo) to High(OperatorsInfo) do
    FOps.Add(OperatorsInfo[I].Typ, OperatorsInfo[I]);
end;

class destructor TOperators.Destroy;
begin
  FOps.Free;
end;

class function TOperators.GetItem(Typ: TSyntaxNodeType): TOperatorInfo;
begin
  Result := FOps[Typ];
end;

class function TOperators.IsOpName(Typ: TSyntaxNodeType): Boolean;
begin
  Result := FOps.ContainsKey(Typ);
end;

function IsRoundClose(Typ: TSyntaxNodeType): Boolean; inline;
begin
  Result := Typ = ntRoundClose;
end;

function IsRoundOpen(Typ: TSyntaxNodeType): Boolean; inline;
begin
  Result := Typ = ntRoundOpen;
end;

{ TTreeData }

constructor TTreeData.Create(Node: TSyntaxNode);
begin
  inherited Create;
  FNode := Node;
  FChild1 := nil;
  FChild2 := nil;
end;

destructor TTreeData.Destroy;
begin
  FChild1.Free;
  FChild2.Free;
  inherited;
end;

class function TExpressionTools.ExprToReverseNotation(Expr: TList<TSyntaxNode>): TList<TSyntaxNode>;
var
  Stack: TStack<TSyntaxNode>;
  Node: TSyntaxNode;
begin
  Result := TList<TSyntaxNode>.Create;
  try
    Stack := TStack<TSyntaxNode>.Create;
    try
      for Node in Expr do
        if TOperators.IsOpName(Node.Typ) then
        begin
          while (Stack.Count > 0) and TOperators.IsOpName(Stack.Peek.Typ) and
            (((TOperators.Items[Node.Typ].AssocType = atLeft) and
            (TOperators.Items[Node.Typ].Priority >= TOperators.Items[Stack.Peek.Typ].Priority))
            or
            ((TOperators.Items[Node.Typ].AssocType = atRight) and
            (TOperators.Items[Node.Typ].Priority > TOperators.Items[Stack.Peek.Typ].Priority)))
          do
            Result.Add(Stack.Pop);

          Stack.Push(Node);
        end
        else if IsRoundOpen(Node.Typ) then
          Stack.Push(Node)
        else if IsRoundClose(Node.Typ) then
        begin
          while not IsRoundOpen(Stack.Peek.Typ) do
            Result.Add(Stack.Pop);
          Stack.Pop;
          if (Stack.Count > 0) and TOperators.IsOpName(Stack.Peek.Typ) then
            Result.Add(Stack.Pop);
        end else
          Result.Add(Node);

      while Stack.Count > 0 do
        Result.Add(Stack.Pop);
    finally
      Stack.Free;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

class procedure TExpressionTools.NodeListToTree(Expr: TList<TSyntaxNode>; Root: TSyntaxNode);

  procedure CopyTree(TreeData: TTreeData; Root: TSyntaxNode);
  var
    Node: TSyntaxNode;
  begin
    Node := Root.AddChild(TreeData.Node.Clone);
    if Assigned(TreeData.Child1) then
      CopyTree(TreeData.Child1, Node);
    if Assigned(TreeData.Child2) then
      CopyTree(TreeData.Child2, Node);
  end;

var
  TreeData: TTreeData;
  Stack: TStack<TTreeData>;
  Node: TSyntaxNode;
begin
  Stack := TStack<TTreeData>.Create;
  try
    for Node in Expr do
    begin
      TreeData := TTreeData.Create(Node);
      if TOperators.IsOpName(Node.Typ) then
        case TOperators.Items[Node.Typ].Kind of
          okUnary: TreeData.Child1 := Stack.Pop;
          okBinary:
            begin
              TreeData.Child2 := Stack.Pop;
              TreeData.Child1 := Stack.Pop;
            end;
        end;
      Stack.Push(TreeData);
    end;

    TreeData := Stack.Pop;

    CopyTree(TreeData, Root);

    TreeData.Free;
  finally
    Stack.Free;
  end;
end;

class function TExpressionTools.PrepareExpr(ExprNodes: TList<TSyntaxNode>): TObjectList<TSyntaxNode>;
var
  Node, PrevNode: TSyntaxNode;
begin
  Result := TObjectList<TSyntaxNode>.Create(True);
  try
    PrevNode := nil;
    for Node in ExprNodes do
    begin
      if Node.Typ = ntCall then
        Continue;

      if Assigned(PrevNode) and IsRoundOpen(Node.Typ) then
      begin
        if not TOperators.IsOpName(PrevNode.Typ) and not IsRoundOpen(PrevNode.Typ) then
          Result.Add(CreateNodeWithParentsPosition(ntCall, Node.ParentNode));

        if TOperators.IsOpName(PrevNode.Typ)
          and (TOperators.Items[PrevNode.Typ].Kind = okUnary)
          and (TOperators.Items[PrevNode.Typ].AssocType = atLeft)
        then
          Result.Add(CreateNodeWithParentsPosition(ntCall, Node.ParentNode));
      end;

      if Assigned(PrevNode) and (Node.Typ = ntTypeArgs) then
      begin
        if not TOperators.IsOpName(PrevNode.Typ) and (PrevNode.Typ <> ntTypeArgs) then
          Result.Add(CreateNodeWithParentsPosition(ntGeneric, Node.ParentNode));

        if TOperators.IsOpName(PrevNode.Typ)
          and (TOperators.Items[PrevNode.Typ].Kind = okUnary)
          and (TOperators.Items[PrevNode.Typ].AssocType = atLeft)
        then
          Result.Add(CreateNodeWithParentsPosition(ntGeneric, Node.ParentNode));
      end;

      if Node.Typ <> ntAlignmentParam then
        Result.Add(Node.Clone);
      PrevNode := Node;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

class function TExpressionTools.CreateNodeWithParentsPosition(NodeType: TSyntaxNodeType; ParentNode: TSyntaxNode): TSyntaxNode;
begin
  Result := TSyntaxNode.Create(NodeType);
  Result.Line := ParentNode.Line;
  Result.Col := ParentNode.Col; 
end;

class procedure TExpressionTools.RawNodeListToTree(RawParentNode: TSyntaxNode; RawNodeList: TList<TSyntaxNode>;
  NewRoot: TSyntaxNode);
var
  PreparedNodeList, ReverseNodeList: TList<TSyntaxNode>;
begin
  try
    PreparedNodeList := PrepareExpr(RawNodeList);
    try
      ReverseNodeList := ExprToReverseNotation(PreparedNodeList);
      try
        NodeListToTree(ReverseNodeList, NewRoot);
      finally
        ReverseNodeList.Free;
      end;
    finally
      PreparedNodeList.Free;
    end;
  except
    on E: Exception do
      raise EParserException.Create(NewRoot.Line, NewRoot.Col, E.Message);
  end;
end;

{ TSyntaxNode }

procedure TSyntaxNode.SetAttribute(const Key: TAttributeName; Value: string);
begin
  FAttributes.AddOrSetValue(Key, Value);
end;

function TSyntaxNode.AddChild(Node: TSyntaxNode): TSyntaxNode;
begin
  Assert(Assigned(Node));

  FChildNodes.Add(Node);
  Node.FParentNode := Self;

  Result := Node;
end;

function TSyntaxNode.AddChild(Typ: TSyntaxNodeType): TSyntaxNode;
begin
  Result := AddChild(TSyntaxNode.Create(Typ));
end;

function TSyntaxNode.Clone: TSyntaxNode;
var
  ChildNode: TSyntaxNode;
  Attr: TPair<TAttributeName, string>;
begin
  Result := TSyntaxNodeClass(Self.ClassType).Create(FTyp);

  for ChildNode in FChildNodes do
    Result.AddChild(ChildNode.Clone);

  for Attr in FAttributes do
    Result.SetAttribute(Attr.Key, Attr.Value);

  Result.Col := Self.Col;
  Result.Line := Self.Line;
end;

constructor TSyntaxNode.Create(Typ: TSyntaxNodeType);
begin
  inherited Create;
  FTyp := Typ;
  FAttributes := TDictionary<TAttributeName, string>.Create;
  FChildNodes := TObjectList<TSyntaxNode>.Create(True);
  FParentNode := nil;
end;

procedure TSyntaxNode.DeleteChild(Node: TSyntaxNode);
begin
  FChildNodes.Remove(Node);
end;

destructor TSyntaxNode.Destroy;
begin
  FChildNodes.Free;
  FAttributes.Free;
  inherited;
end;

function TSyntaxNode.FindNode(Typ: TSyntaxNodeType): TSyntaxNode;
var
  Node: TSyntaxNode;
begin
  Result := nil;
  for Node in FChildNodes do
    if Node.Typ = Typ then
    begin
      Result := Node;
      Break;
    end;
end;

function TSyntaxNode.GetAttribute(const Key: TAttributeName): string;
begin
  if not FAttributes.TryGetValue(Key, Result) then
    Result := '';
end;

function TSyntaxNode.GetHasAttributes: Boolean;
begin
  Result := FAttributes.Count > 0;
end;

function TSyntaxNode.GetHasChildren: Boolean;
begin
  Result := FChildNodes.Count > 0;
end;

function TSyntaxNode.HasAttribute(const Key: TAttributeName): boolean;
begin
  result := FAttributes.ContainsKey(key);
end;

{ TCompoundSyntaxNode }

function TCompoundSyntaxNode.Clone: TSyntaxNode;
begin
  Result := inherited;

  TCompoundSyntaxNode(Result).EndLine := Self.EndLine;
  TCompoundSyntaxNode(Result).EndCol := Self.EndCol;
end;

{ TValuedSyntaxNode }

function TValuedSyntaxNode.Clone: TSyntaxNode;
begin
  Result := inherited;

  TValuedSyntaxNode(Result).Value := Self.Value;
end;

{ EParserException }

constructor EParserException.Create(Line, Col: Integer; Msg: string);
begin
  inherited Create(Msg);
  FLine := Line;
  FCol := Col;
end;

end.
