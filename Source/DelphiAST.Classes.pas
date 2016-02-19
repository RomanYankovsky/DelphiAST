unit DelphiAST.Classes;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  SysUtils, Generics.Collections, SimpleParser.Lexer.Types, DelphiAST.Consts;

type
  EParserException = class(Exception)
  strict private
    FFileName: string;
    FLine, FCol: Integer;
  public
    constructor Create(Line, Col: Integer; const FileName, Msg: string); reintroduce;

    property FileName: string read FFileName;
    property Line: Integer read FLine;
    property Col: Integer read FCol;
  end;
  
  TAttributeEntry = TPair<TAttributeName, string>;
  PAttributeEntry = ^TAttributeEntry;

  TSyntaxNodeClass = class of TSyntaxNode;
  TSyntaxNode = class
  private
    FCol: Integer;
    FLine: Integer;
    FFileName: string;
    function GetHasChildren: Boolean;
    function GetHasAttributes: Boolean;
    function TryGetAttributeEntry(const Key: TAttributeName; var AttributeEntry: PAttributeEntry): boolean;
  protected
    FAttributes: TArray<TAttributeEntry>;
    FChildNodes: TArray<TSyntaxNode>;
    FTyp: TSyntaxNodeType;
    FParentNode: TSyntaxNode;
  public
    constructor Create(Typ: TSyntaxNodeType);
    destructor Destroy; override;

    function Clone: TSyntaxNode; virtual;

    function GetAttribute(const Key: TAttributeName): string;
    function HasAttribute(const Key: TAttributeName): Boolean;
    procedure SetAttribute(const Key: TAttributeName; const Value: string);
    procedure ClearAttributes;

    function AddChild(Node: TSyntaxNode): TSyntaxNode; overload;
    function AddChild(Typ: TSyntaxNodeType): TSyntaxNode; overload;
    procedure DeleteChild(Node: TSyntaxNode);
    procedure ExtractChild(Node: TSyntaxNode);

    function FindNode(Typ: TSyntaxNodeType): TSyntaxNode;

    property Attributes: TArray<TAttributeEntry> read FAttributes;
    property ChildNodes: TArray<TSyntaxNode> read FChildNodes;
    property FileName: string read FFileName write FFileName;
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

  TCommentNode = class(TSyntaxNode)
  private
    FText: string;
  public
    function Clone: TSyntaxNode; override;

    property Text: string read FText write FText;
  end;

  TExpressionTools = class
  private
    class function CreateNodeWithParentsPosition(NodeType: TSyntaxNodeType; ParentNode: TSyntaxNode): TSyntaxNode;
  public
    class function ExprToReverseNotation(Expr: TList<TSyntaxNode>): TList<TSyntaxNode>; static;
    class procedure NodeListToTree(Expr: TList<TSyntaxNode>; Root: TSyntaxNode); static;
    class function PrepareExpr(ExprNodes: TList<TSyntaxNode>): TList<TSyntaxNode>; static;
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

          // RoundOpen and RoundClose nodes are not needed anymore
          Stack.Pop.Free;
          Node.Free;

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
var
  Stack: TStack<TSyntaxNode>;
  Node, SecondNode: TSyntaxNode;
begin
  Stack := TStack<TSyntaxNode>.Create;
  try
    for Node in Expr do
    begin
      if TOperators.IsOpName(Node.Typ) then
        case TOperators.Items[Node.Typ].Kind of
          okUnary: Node.AddChild(Stack.Pop);
          okBinary:
            begin
              SecondNode := Stack.Pop;
              Node.AddChild(Stack.Pop);
              Node.AddChild(SecondNode);
            end;
        end;
      Stack.Push(Node);
    end;

    Root.AddChild(Stack.Pop);

    Assert(Stack.Count = 0);
  finally
    Stack.Free;
  end;
end;

class function TExpressionTools.PrepareExpr(ExprNodes: TList<TSyntaxNode>): TList<TSyntaxNode>;
var
  Node, PrevNode: TSyntaxNode;
begin
  Result := TList<TSyntaxNode>.Create;
  try
    Result.Capacity := ExprNodes.Count * 2;

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
  Result.FileName := ParentNode.FileName;
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
      raise EParserException.Create(NewRoot.Line, NewRoot.Col, NewRoot.FileName, E.Message);
  end;
end;

{ TSyntaxNode }

procedure TSyntaxNode.SetAttribute(const Key: TAttributeName; const Value: string);
var
  AttributeEntry: PAttributeEntry;
  NewAttributeEntry: TAttributeEntry;
begin
  if TryGetAttributeEntry(Key, AttributeEntry) then
    AttributeEntry^.Value := Value
  else
  begin
    NewAttributeEntry.Key := Key;
    NewAttributeEntry.Value := Value;
    SetLength(FAttributes, Length(FAttributes) + 1);
    FAttributes[Length(FAttributes) - 1] := NewAttributeEntry;
  end;
end;

function TSyntaxNode.TryGetAttributeEntry(const Key: TAttributeName; var AttributeEntry: PAttributeEntry): boolean;
var
  i: integer;
begin
  for i := 0 to Length(FAttributes) - 1 do
    if FAttributes[i].Key = Key then
    begin
      AttributeEntry := @FAttributes[i];
      Exit(true);
    end;

  Exit(false);
end;

function TSyntaxNode.AddChild(Node: TSyntaxNode): TSyntaxNode;
begin
  Assert(Assigned(Node));

  SetLength(FChildNodes, Length(FChildNodes) + 1);
  FChildNodes[Length(FChildNodes) - 1] := Node;

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
  Result.FileName := Self.FileName;
end;

constructor TSyntaxNode.Create(Typ: TSyntaxNodeType);
begin
  inherited Create;
  FTyp := Typ;
  SetLength(FAttributes, 0);
  SetLength(FChildNodes, 0);
  FParentNode := nil;
end;

procedure TSyntaxNode.ExtractChild(Node: TSyntaxNode);
var
  NodeIndex, i: integer;
begin
  NodeIndex := -1;
  for i := 0 to Length(FChildNodes) - 1 do
    if FChildNodes[i] = Node then
    begin
      NodeIndex := i;
      break;
    end;

  if NodeIndex >= 0 then
  begin
    if NodeIndex < High(FChildNodes) then
      Move(FChildNodes[NodeIndex + 1], FChildNodes[NodeIndex], SizeOf(FChildNodes[0]) * (Length(FChildNodes) - NodeIndex - 1));
    SetLength(FChildNodes, Length(FChildNodes) - 1);
  end;   
end;

procedure TSyntaxNode.DeleteChild(Node: TSyntaxNode);
begin
  ExtractChild(Node);
  Node.Free;
end;

destructor TSyntaxNode.Destroy;
var
  i: integer;
begin
  for i := 0 to Length(FChildNodes) - 1 do
    FChildNodes[i].Free;
  SetLength(FChildNodes, 0);

  SetLength(FAttributes, 0);  
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
var
  AttributeEntry: PAttributeEntry;
begin
  if TryGetAttributeEntry(Key, AttributeEntry) then
    Result := AttributeEntry.Value
  else
    Result := '';
end;

function TSyntaxNode.GetHasAttributes: Boolean;
begin
  Result := Length(FAttributes) > 0;
end;

function TSyntaxNode.GetHasChildren: Boolean;
begin
  Result := Length(FChildNodes) > 0;
end;

function TSyntaxNode.HasAttribute(const Key: TAttributeName): Boolean;
var
  AttributeEntry: PAttributeEntry;
begin
  Result := TryGetAttributeEntry(Key, AttributeEntry);
end;

procedure TSyntaxNode.ClearAttributes;
begin
  SetLength(FAttributes, 0);
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

{ TCommentNode }

function TCommentNode.Clone: TSyntaxNode;
begin
  Result := inherited;

  TCommentNode(Result).Text := Self.Text;
end;

{ EParserException }

constructor EParserException.Create(Line, Col: Integer; const FileName, Msg: string);
begin
  inherited Create(Msg);
  FFileName := FileName;
  FLine := Line;
  FCol := Col;
end;

end.
