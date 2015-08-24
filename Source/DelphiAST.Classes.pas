unit DelphiAST.Classes;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  Generics.Collections, SimpleParser.Lexer.Types, DelphiAST.Consts,
  Generics.Defaults;

type
  TSyntaxNodeClass = class of TSyntaxNode;
  TStringCache = class;
  TStringCacheDictionary<TKey> = class;
  TSyntaxNode = class
  private
    FCol: Integer;
    FLine: Integer;
    function GetHasChildren: Boolean;
    function GetHasAttributes: Boolean;
  protected
    FAttributes: TStringCacheDictionary<TAttributeName>;
    FChildNodes: TObjectList<TSyntaxNode>;
    FTyp: TSyntaxNodeType;
    FParentNode: TSyntaxNode;
  public
    constructor Create(Typ: TSyntaxNodeType); virtual;
    destructor Destroy; override;

    function Clone: TSyntaxNode; virtual;

    function GetAttribute(const Key: TAttributeName): string;
    function HasAttribute(const Key: TAttributeName): boolean;
    procedure SetAttribute(const Key: TAttributeName; Value: string);

    function AddChild(Node: TSyntaxNode): TSyntaxNode; overload;
    function AddChild(Typ: TSyntaxNodeType): TSyntaxNode; overload;
    procedure DeleteChild(Node: TSyntaxNode);

    function FindNode(Typ: TSyntaxNodeType): TSyntaxNode;

    property Attributes: TStringCacheDictionary<TAttributeName> read FAttributes;
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
    FValue: NativeUInt;
    function GetValue : string;
    procedure SetValue(AValue : string);
  public
    constructor Create(Typ: TSyntaxNodeType); override;
    function Clone: TSyntaxNode; override;

    property Value: string read GetValue write SetValue;
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

  TStringCache = class
  type
    TStringRec = class
    strict private
      FValue : string;
      FUsageCount : NativeUInt;
    public
      constructor Create(const AValue : string);
      procedure IncUsageCount;
      property UsageCount : NativeUInt read FUsageCount;
      property Value : string read FValue;
    end;
  private
    type
      TStringRecValueEqualityComparer = class(TEqualityComparer<TStringRec>)
      private
        FStringComparer : IEqualityComparer<string>;
      public
        constructor Create();
        function Equals(const Left, Right: TStringRec): Boolean; overload; override;
        function GetHashCode(const Value: TStringRec): Integer; overload; override;
      end;
      TStringRecUsageComparer = class(TInterfacedObject, IComparer<TStringRec>)
        function Compare(const Left, Right: TStringRec): Integer;
      end;
  strict private
    FStringToId : TDictionary<string, NativeUInt>;
    FRefCount : NativeUInt;
    FIsPersistent : Boolean;

    class var FInstance : TStringCache;
    class constructor ClassCreate;
    class destructor ClassDestroy;
  private
    FIdToString : TList<TStringRec>; // ID is index
  public
    constructor Create;
    destructor Destroy; override;

    function Add(const Value : string) : NativeUInt;
    function AddAndGet(const P : PChar; const Length : Integer) : string;
    function Get(const ID : NativeUInt) : string;
    procedure Clear;
    procedure ByUsage(InOrder : TList<TStringRec>);

    procedure IncRef;
    procedure DecRef;

    property Persistent : Boolean read FIsPersistent write FIsPersistent;
    class property Instance : TStringCache read FInstance;
  end;

  TStringCacheDictionary<TKey> = class(TEnumerable<TPair<TKey, string>>)
  strict private
    type
      TKeyStringEnumerator = class(TEnumerator<TPair<TKey,string>>)
      private
        FDictionary: TStringCacheDictionary<TKey>;
        FInternalEnum : TDictionary<TKey, NativeUInt>.TPairEnumerator;
      protected
        function DoGetCurrent: TPair<TKey, string>; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(const ADictionary: TStringCacheDictionary<TKey>);
        destructor Destroy; override;
      end;
  private
    FKeyToId : TDictionary<TKey, NativeUInt>;

    function GetItem(const Key: TKey): string;
    procedure SetItem(const Key: TKey; const Value: string);
    function GetCount : Integer;
  protected
    function DoGetEnumerator: TEnumerator<TPair<TKey, string>>; override;
  public
    constructor Create;
    destructor Destroy; override;

    function TryGetValue(const Key: TKey; out Value: string): Boolean;
    procedure AddOrSetValue(const Key: TKey; const Value: string);
    function ContainsKey(const Key: TKey): Boolean;
    function ToArray: TArray<TPair<TKey, string>>; override;

    property Items[const Key: TKey]: string read GetItem write SetItem; default;
    property Count: Integer read GetCount;
  end;

implementation

uses
  SysUtils, Types;

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
  FAttributes := TStringCacheDictionary<TAttributeName>.Create;
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

constructor TValuedSyntaxNode.Create(Typ: TSyntaxNodeType);
begin
  inherited;
  FValue := 0; // String cache item 0 is an empty string
end;

function TValuedSyntaxNode.Clone: TSyntaxNode;
begin
  Result := inherited;

  TValuedSyntaxNode(Result).Value := Self.Value;
end;

function TValuedSyntaxNode.GetValue : string;
begin
  Result := TStringCache.Instance.Get(FValue);
end;

procedure TValuedSyntaxNode.SetValue(AValue : string);
begin
  FValue := TStringCache.Instance.Add(AValue);
end;

{ TStringCache.TStringRecValueEqualityComparer }

constructor TStringCache.TStringRecValueEqualityComparer.Create;
begin
  inherited Create();
  FStringComparer := TEqualityComparer<string>.Default;
end;

function TStringCache.TStringRecValueEqualityComparer.Equals(const Left,
  Right: TStringRec): Boolean;
begin
  // Compare by the string it holds only
  Result := FStringComparer.Equals(Left.Value, Right.Value);
end;

function TStringCache.TStringRecValueEqualityComparer.GetHashCode(
  const Value: TStringRec): Integer;
begin
  // Compare by the string it holds only
  Result := FStringComparer.GetHashCode(Value.Value);
end;

{ TStringCache.TStringRecUsageComparer }

function TStringCache.TStringRecUsageComparer.Compare(const Left,
  Right: TStringRec): Integer;
begin
  if Left.UsageCount < Right.UsageCount then
    Exit(LessThanValue)
  else if Left.UsageCount > Right.UsageCount then
    Exit(GreaterThanValue)
  else // Usage is the same, sort by string
    Exit(TComparer<string>.Default.Compare(Left.Value, Right.Value));
end;

{ TStringCache }

class constructor TStringCache.ClassCreate;
begin
  FInstance := TStringCache.Create;
end;

class destructor TStringCache.ClassDestroy;
begin
  FInstance.Free;
end;

constructor TStringCache.Create;
begin
  inherited;
  FRefCount := 0;
  FIsPersistent := false; // Clear the cache when no longer needed
  FStringToId := TDictionary<string, NativeUInt>.Create;
  FIdToString := TList<TStringRec>.Create;

  Add(''); // Empty string is always item 0
end;

destructor TStringCache.Destroy;
begin
  assert(FRefCount = 0, 'String cache destroyed with live objects still relying on it');
  Clear;
  FStringToId.Free;
  FIdToString.Free;
  inherited;
end;

function TStringCache.Add(const Value: string): NativeUInt;
begin
  Result := 0;

  if FStringToId.TryGetValue(Value, Result) then
    // Already exists. Increment the usage count of the existing one, and return
    FIdToString[Result].IncUsageCount
  else
  begin
    // Item does not yet exist
    Result := FIdToString.Add(TStringRec.Create(Value));
    FStringToId.Add(Value, Result);
  end;
end;

function TStringCache.AddAndGet(const P : PChar; const Length : Integer) : string;
var
  SearchStr : string;
begin
  SetString(SearchStr, P, Length);
  Result := Get(Add(SearchStr));
end;

function TStringCache.Get(const ID: NativeUInt): string;
begin
  if ID < NativeUInt(FIdToString.Count) then
    Exit(FIdToString[ID].Value)
  else
    raise Exception.Create(Format('String cache entry with ID %d does not exist', [ID]));
end;

procedure TStringCache.Clear;
var
  I : Integer;
begin
  if FRefCount <> 0 then
    raise Exception.Create(Format('Clearing the string cache while objects still rely on it (%d)', [FRefCount]));

  // One instance of TStringRec, but stored in two lists. Free from only one
  for I := 0 to Pred(FIdToString.Count) do
    FIdToString[I].Free;

  FStringToId.Clear;
  FIdToString.Clear;
end;

procedure TStringCache.ByUsage(InOrder: TList<TStringRec>);
begin
  InOrder.InsertRange(0, FIdToString);
  InOrder.Sort(TStringCache.TStringRecUsageComparer.Create);
end;

procedure TStringCache.IncRef;
begin
  // Keep a count of how many objects are using the string cache. This lets it
  // clear itself when the last one is freed - ie, free all the strings when
  // they are no longer needed. (The alternative, controlled by Persistent,
  // is to keep them - ie make the cache persistent over multiple runs - useful
  // for parsing the same or similar files over and over.)
  Inc(FRefCount);
end;

procedure TStringCache.DecRef;
begin
  if FRefCount = 0 then
    raise Exception.Create('String cache refcount cannot be decremented below zero');
  Dec(FRefCount);

  // Unless want to keep the strings around for next parse, clear now nothing is
  // using any of them.
  if (FRefCount = 0) and (not FIsPersistent) then
    Clear;
end;

{ TStringCache.TStringRec }

constructor TStringCache.TStringRec.Create(const AValue: string);
begin
  inherited Create;
  FValue := AValue;
  FUsageCount := 1;
end;

procedure TStringCache.TStringRec.IncUsageCount;
begin
  Inc(FUsageCount);
end;

{ TStringCacheDictionary<TKey> }

constructor TStringCacheDictionary<TKey>.Create;
begin
  inherited;
  FKeyToId := TDictionary<TKey, NativeUInt>.Create;
  TStringCache.Instance.IncRef; // Uses the cache
end;

destructor TStringCacheDictionary<TKey>.Destroy;
begin
  FKeyToId.Free;
  TStringCache.Instance.DecRef;
  inherited;
end;

function TStringCacheDictionary<TKey>.GetItem(const Key: TKey): string;
var
  ID : NativeUInt;
begin
  if FKeyToId.TryGetValue(Key, ID) then
    Result := TStringCache.Instance.Get(ID)
  else
    Result := '';
end;

procedure TStringCacheDictionary<TKey>.SetItem(const Key: TKey; const Value: string);
begin
  FKeyToId.AddOrSetValue(Key, TStringCache.Instance.Add(Value));
end;

function TStringCacheDictionary<TKey>.GetCount : Integer;
begin
  Result := FKeyToId.Count;
end;

function TStringCacheDictionary<TKey>.TryGetValue(const Key: TKey; out Value: string): Boolean;
var
  ID : NativeUInt;
begin
  Result := FKeyToId.TryGetValue(Key, ID);
  if Result then
    Value := TStringCache.Instance.Get(ID);
end;

procedure TStringCacheDictionary<TKey>.AddOrSetValue(const Key: TKey; const Value: string);
begin
  SetItem(Key, Value);
end;

function TStringCacheDictionary<TKey>.ContainsKey(const Key: TKey): Boolean;
begin
  Result := FKeyToId.ContainsKey(Key);
end;

function TStringCacheDictionary<TKey>.ToArray: TArray<TPair<TKey, string>>;
begin
  Result := ToArrayImpl(Count);
end;

function TStringCacheDictionary<TKey>.DoGetEnumerator: TEnumerator<TPair<TKey, string>>;
begin
  Result := TKeyStringEnumerator.Create(Self);
end;

{ TStringCacheDictionary<TKey>.TKeyStringEnumerator }

constructor TStringCacheDictionary<TKey>.TKeyStringEnumerator.Create(const ADictionary: TStringCacheDictionary<TKey>);
begin
  inherited Create();
  FDictionary := ADictionary;
  FInternalEnum := FDictionary.FKeyToId.GetEnumerator;
end;

destructor TStringCacheDictionary<TKey>.TKeyStringEnumerator.Destroy;
begin
  FInternalEnum.Free;
  inherited;
end;

function TStringCacheDictionary<TKey>.TKeyStringEnumerator.DoGetCurrent: TPair<TKey, string>;
var
  Pair : TPair<TKey, NativeUInt>;
begin
  // Wrap the owner dictionary's internal FKeyToId enumerator, converting ID to string
  Pair := FInternalEnum.Current;
  Result := TPair<TKey, string>.Create(Pair.Key, FDictionary.Items[Pair.Key]);
end;

function TStringCacheDictionary<TKey>.TKeyStringEnumerator.DoMoveNext: Boolean;
begin
  Result := FInternalEnum.MoveNext;
end;

end.
