unit DelphiAST.Classes;

interface

uses
  Generics.Collections, SimpleParser.Lexer.Types, DelphiAST.Consts;

type
  TSyntaxNode = class
  private
    function GetHasChildren: Boolean;
    function GetHasAttributes: Boolean;
  protected
    //FAttributes: TDictionary<string, string>;
    FAttributes: TAttributeRec;
    FChildNodes: TObjectList<TSyntaxNode>;
    FTyp: TSyntaxNodeType;
    FParentNode: TSyntaxNode;
  public
    constructor Create(Typ: TSyntaxNodeType);
    destructor Destroy; override;

    function Clone: TSyntaxNode;

    //function GetAttribute(const Key: string): string;
    function GetAttribute(const Key: TValueAttribute): TExtAttribute;
    function HasAttribute(const Key: TAttribute): boolean; overload;
    function HasAttribute(const Keys: TAttributes): boolean; overload;
    procedure SetAttribute(const Key: TValueAttribute; Value: string); overload;
    procedure SetAttribute(const Key: TSimpleAttribute); overload;

    function AddChild(Node: TSyntaxNode): TSyntaxNode; overload;
    function AddChild(Typ: TSyntaxNodeType): TSyntaxNode; overload;
    procedure DeleteChild(Node: TSyntaxNode);

    function FindNode(Typ: TSyntaxNodeType; StartFrom: TSyntaxNode = nil): TSyntaxNode;
    function NextNode(const PrevNode: TSyntaxNode): TSyntaxNode;
    procedure SetPositionAttributes(PosXY: TTokenPoint; LineEnd: boolean = false);

    //property Attributes: TDictionary<string, string> read FAttributes;
    property Attributes: TAttributeRec read FAttributes;
    property ChildNodes: TObjectList<TSyntaxNode> read FChildNodes;
    property HasAttributes: Boolean read GetHasAttributes;
    property HasChildren: Boolean read GetHasChildren;
    property Typ: TSyntaxNodeType read FTyp;
    property ParentNode: TSyntaxNode read FParentNode;
  end;

  TExpressionTools = class
  public
    class function ExprToReverseNotation(Expr: TList<TSyntaxNode>): TList<TSyntaxNode>; static;
    class procedure NodeListToTree(Expr: TList<TSyntaxNode>; Root: TSyntaxNode); static;
    class function PrepareExpr(ExprNodes: TList<TSyntaxNode>): TObjectList<TSyntaxNode>; static;
    class procedure RawNodeListToTree(RawParentNode: TSyntaxNode; RawNodeList: TList<TSyntaxNode>; NewRoot: TSyntaxNode); static;
  end;

implementation

uses
  SysUtils;

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
          Result.Add(TSyntaxNode.Create(ntCall));

        if TOperators.IsOpName(PrevNode.Typ)
          and (TOperators.Items[PrevNode.Typ].Kind = okUnary)
          and (TOperators.Items[PrevNode.Typ].AssocType = atLeft)
        then
          Result.Add(TSyntaxNode.Create(ntCall));
      end;

      if Assigned(PrevNode) and (Node.Typ = ntTypeArgs) then
      begin
        if not TOperators.IsOpName(PrevNode.Typ) and (PrevNode.Typ <> ntTypeArgs) then
          Result.Add(TSyntaxNode.Create(ntGeneric));

        if TOperators.IsOpName(PrevNode.Typ)
          and (TOperators.Items[PrevNode.Typ].Kind = okUnary)
          and (TOperators.Items[PrevNode.Typ].AssocType = atLeft)
        then
          Result.Add(TSyntaxNode.Create(ntGeneric));
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

class procedure TExpressionTools.RawNodeListToTree(RawParentNode: TSyntaxNode; RawNodeList: TList<TSyntaxNode>;
  NewRoot: TSyntaxNode);
var
  PreparedNodeList, ReverseNodeList: TList<TSyntaxNode>;
begin
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
end;

{ TSyntaxNode }

procedure TSyntaxNode.SetAttribute(const Key: TValueAttribute; Value: string);
begin
  FAttributes:= FAttributes + TExtAttribute.Create(Key, Value);
end;

procedure TSyntaxNode.SetAttribute(const Key: TSimpleAttribute);
begin
  FAttributes:= FAttributes + Key;
end;

procedure TSyntaxNode.SetPositionAttributes(PosXY: TTokenPoint; LineEnd: boolean = false);
begin
  if LineEnd then begin
    FAttributes.EndLine:= PosXY.X;
    FAttributes.EndCol:= PosXY.Y;
  end else begin
    FAttributes.Line:= PosXY.X;
    FAttributes.Col:= PosXY.Y;
  end;
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
  Attr: TPair<string, string>;
begin
  Result := TSyntaxNode.Create(FTyp);

  for ChildNode in FChildNodes do
    Result.AddChild(ChildNode.Clone);

  Result.FAttributes:= FAttributes;
  //for Attr in FAttributes do
  //  Result.SetAttribute(Attr.Key, Attr.Value);
end;

constructor TSyntaxNode.Create(Typ: TSyntaxNodeType);
begin
  inherited Create;
  FTyp := Typ;
  //FAttributes := TDictionary<string, string>.Create;
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
  //FAttributes.Free;
  inherited;
end;

function TSyntaxNode.FindNode(Typ: TSyntaxNodeType; StartFrom: TSyntaxNode = nil): TSyntaxNode;
var
  StartIndex: integer;
  i: integer;
  Node: TSyntaxNode;
begin
  Result := nil;
  if (StartFrom = nil) then StartIndex := -1
  else StartIndex := FChildNodes.IndexOf(StartFrom);
  for i := StartIndex + 1 to FChildNodes.Count-1 do begin
    Node := FChildNodes[i];
    if (Node.Typ = Typ) then Exit(Node);
  end;
end;

function TSyntaxNode.NextNode(const PrevNode: TSyntaxNode): TSyntaxNode;
var
  Index: integer;
begin
  Index:= FChildNodes.IndexOf(PrevNode) + 1;
  if (Index = FChildNodes.Count) then exit(nil)
  else Result:= FChildNodes[Index];
end;

//function TSyntaxNode.GetAttribute(const Key: string): string;
//begin
//  if not FAttributes.TryGetValue(Key, Result) then
//    Result := '';
//end;

function TSyntaxNode.GetAttribute(const Key: TValueAttribute): TExtAttribute;
begin
  if Key in FAttributes then begin
    Result:= FAttributes.Data[Key];
  end;
end;

function TSyntaxNode.GetHasAttributes: Boolean;
begin
  Result := FAttributes.IsEmpty;
end;

function TSyntaxNode.GetHasChildren: Boolean;
begin
  Result := FChildNodes.Count > 0;
end;

function TSyntaxNode.HasAttribute(const Key: TAttribute): boolean;
begin
  Result := Key in FAttributes;
end;

function TSyntaxNode.HasAttribute(const Keys: TAttributes): boolean;
var
  A: TAttribute;
begin
  for A in Keys do begin
    if A in FAttributes then Exit(true);
  end;
  Result:= false;
end;

end.
