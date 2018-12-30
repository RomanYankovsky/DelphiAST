unit DelphiAST.Classes;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

// Define this to use a memory pool for node instances
{$define USEBULKALLOCATOR}

interface

uses
  SysUtils, Generics.Collections, SimpleParser.Lexer.Types, DelphiAST.Consts
  {$ifdef USESTRINGCACHE}, SimpleParser.StringCache{$endif}
  {$ifdef USEBULKALLOCATOR}, SimpleParser.ObjectAllocator{$endif}
  ;

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

  {$ifdef USESTRINGCACHE}
    TAttributeEntryValue = TStringId;
  {$else}
    TAttributeEntryValue = string;
  {$endif}
  
  TAttributeEntry = TPair<TAttributeName, TAttributeEntryValue>;
  PAttributeEntry = ^TAttributeEntry;

  TSyntaxNodeClass = class of TSyntaxNode;
  TSyntaxNode = class
  {$ifdef USEBULKALLOCATOR}
  strict private
    class var FAllocator : TAllocator<TSyntaxNode>;
    class constructor ClassCreate;
    class destructor ClassDestroy;
  public
    class function NewInstance: TObject {$IFDEF AUTOREFCOUNT} unsafe {$ENDIF}; override;
    procedure FreeInstance; override;
  {$endif}
  private
    FCol: Integer;
    FLine: Integer;
    FFileName: string;
    function GetHasChildren: Boolean;
    function GetHasAttributes: Boolean;
    function TryGetAttributeEntry(const Key: TAttributeName; var AttributeEntry: PAttributeEntry): boolean;
    procedure SetAttributeInternal(const Key: TAttributeName; const Value: TAttributeEntryValue);
    {$ifdef USESTRINGCACHE}procedure SetAttribute(const Key: TAttributeName; const Value: TStringId); overload;{$endif}
  protected
    FAttributes: TArray<TAttributeEntry>;
    FChildNodes: TArray<TSyntaxNode>;
    FTyp: TSyntaxNodeType;
    FParentNode: TSyntaxNode;
  public
    constructor Create(Typ: TSyntaxNodeType);
    destructor Destroy; override;

    function Clone: TSyntaxNode; virtual;
    procedure AssignPositionFrom(const Node: TSyntaxNode);
    
    function GetAttribute(const Key: TAttributeName): string;
    function HasAttribute(const Key: TAttributeName): Boolean;
    procedure SetAttribute(const Key: TAttributeName; const Value: string); {$ifdef USESTRINGCACHE}overload;{$endif}
    procedure ClearAttributes;

    function AddChild(Node: TSyntaxNode): TSyntaxNode; overload;
    function AddChild(Typ: TSyntaxNodeType): TSyntaxNode; overload;
    procedure DeleteChild(Node: TSyntaxNode);
    procedure ExtractChild(Node: TSyntaxNode);

    function FindNode(Typ: TSyntaxNodeType): TSyntaxNode;

    property Attributes: TArray<TAttributeEntry> read FAttributes;
    property ChildNodes: TArray<TSyntaxNode> read FChildNodes;
    property HasAttributes: Boolean read GetHasAttributes;
    property HasChildren: Boolean read GetHasChildren;
    property Typ: TSyntaxNodeType read FTyp;
    property ParentNode: TSyntaxNode read FParentNode;

    property Col: Integer read FCol write FCol;
    property Line: Integer read FLine write FLine;
    property FileName: string read FFileName write FFileName;
  end;

  TCompoundSyntaxNode = class(TSyntaxNode)
  {$ifdef USEBULKALLOCATOR}
  strict private
    class var FAllocator : TAllocator<TCompoundSyntaxNode>;
    class constructor ClassCreate;
    class destructor ClassDestroy;
  public
    class function NewInstance: TObject {$IFDEF AUTOREFCOUNT} unsafe {$ENDIF}; override;
    procedure FreeInstance; override;
  {$endif}
  private
    FEndCol: Integer;
    FEndLine: Integer;
  public
    function Clone: TSyntaxNode; override;

    property EndCol: Integer read FEndCol write FEndCol;
    property EndLine: Integer read FEndLine write FEndLine;
  end;

  TValuedSyntaxNode = class(TSyntaxNode)
  {$ifdef USEBULKALLOCATOR}
  strict private
    class var FAllocator : TAllocator<TValuedSyntaxNode>;
    class constructor ClassCreate;
    class destructor ClassDestroy;
  public
    class function NewInstance: TObject {$IFDEF AUTOREFCOUNT} unsafe {$ENDIF}; override;
    procedure FreeInstance; override;
  {$endif}
  private
    FValue: {$ifdef USESTRINGCACHE}TStringId{$else}string{$endif};
    function GetValue: string;
    procedure SetValue(const Value: string);
  public
    function Clone: TSyntaxNode; override;

    property Value: string read GetValue write SetValue;
  end;

  TCommentNode = class(TSyntaxNode)
  {$ifdef USEBULKALLOCATOR}
  strict private
    class var FAllocator : TAllocator<TCommentNode>;
    class constructor ClassCreate;
    class destructor ClassDestroy;
  public
    class function NewInstance: TObject {$IFDEF AUTOREFCOUNT} unsafe {$ENDIF}; override;
    procedure FreeInstance; override;
  {$endif}
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

class function TOperators.GetItem(Typ: TSyntaxNodeType): TOperatorInfo;
var
  i: Integer;
begin
  for i := 0 to High(OperatorsInfo) do
    if OperatorsInfo[i].Typ = Typ then
      Exit(OperatorsInfo[i]);
end;

class function TOperators.IsOpName(Typ: TSyntaxNodeType): Boolean;
var
  i: Integer;
begin
  for i := 0 to High(OperatorsInfo) do
    if OperatorsInfo[i].Typ = Typ then
      Exit(True);
  Result := False;
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
  Result.AssignPositionFrom(ParentNode);
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

{$ifdef USEBULKALLOCATOR}
  class constructor TSyntaxNode.ClassCreate;
  begin
    FAllocator := TAllocator<TSyntaxNode>.Create;
  end;

  class destructor TSyntaxNode.ClassDestroy;
  begin
    FAllocator.Free;
  end;

  class function TSyntaxNode.NewInstance: TObject;
  begin
    Result := InitInstance(FAllocator.New);
  end;

  procedure TSyntaxNode.FreeInstance;
  begin
    CleanupInstance;
    FAllocator.Return(Self);
  end;
{$endif}

procedure TSyntaxNode.SetAttribute(const Key: TAttributeName; const Value: string);
{$ifdef USESTRINGCACHE}
  var
    NewValue : TAttributeEntryValue;
{$endif}
begin
  {$ifdef USESTRINGCACHE}
    NewValue := TStringCache.Instance.Add(Value);
    SetAttributeInternal(Key, NewValue);
  {$else}
    SetAttributeInternal(Key, Value);
  {$endif}
end;

{$ifdef USESTRINGCACHE}
  procedure TSyntaxNode.SetAttribute(const Key: TAttributeName; const Value: TStringId);
  begin
    SetAttributeInternal(Key, Value);
  end;
{$endif}

procedure TSyntaxNode.SetAttributeInternal(const Key: TAttributeName; const Value: TAttributeEntryValue);
var
  AttributeEntry: PAttributeEntry;
  len: Integer;
begin
  if not TryGetAttributeEntry(Key, AttributeEntry) then
  begin
    len := Length(FAttributes);
    SetLength(FAttributes, len + 1);
    AttributeEntry := @FAttributes[len];
    AttributeEntry^.Key := Key;
  end;
  AttributeEntry^.Value := Value;
end;

function TSyntaxNode.TryGetAttributeEntry(const Key: TAttributeName; var AttributeEntry: PAttributeEntry): boolean;
var
  i: integer;
begin
  for i := 0 to High(FAttributes) do
    if FAttributes[i].Key = Key then
    begin
      AttributeEntry := @FAttributes[i];
      Exit(True);
    end;

  Result := False;
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
  i: Integer;
begin
  Result := TSyntaxNodeClass(Self.ClassType).Create(FTyp);

  SetLength(Result.FChildNodes, Length(FChildNodes));
  for i := 0 to High(FChildNodes) do
  begin
    Result.FChildNodes[i] := FChildNodes[i].Clone;
    Result.FChildNodes[i].FParentNode := Result;
  end;

  Result.FAttributes := Copy(FAttributes);
  Result.AssignPositionFrom(Self);
end;

constructor TSyntaxNode.Create(Typ: TSyntaxNodeType);
begin
  inherited Create;
  FTyp := Typ;
  SetLength(FAttributes, 0);
  SetLength(FChildNodes, 0);
  FParentNode := nil;

  {$ifdef USESTRINGCACHE}TStringCache.Instance.IncRef;{$endif}
end;

procedure TSyntaxNode.ExtractChild(Node: TSyntaxNode);
var
  i: integer;
begin
  for i := 0 to High(FChildNodes) do
    if FChildNodes[i] = Node then
    begin
      if i < High(FChildNodes) then
        Move(FChildNodes[i + 1], FChildNodes[i], SizeOf(TSyntaxNode) * (Length(FChildNodes) - i - 1));
      SetLength(FChildNodes, High(FChildNodes));
      Break;
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
  {$ifdef USESTRINGCACHE}TStringCache.Instance.DecRef;{$endif}

  for i := 0 to Length(FChildNodes) - 1 do
    FChildNodes[i].Free;
  SetLength(FChildNodes, 0);

  SetLength(FAttributes, 0);

  inherited;
end;

function TSyntaxNode.FindNode(Typ: TSyntaxNodeType): TSyntaxNode;
var
  i: Integer;
begin
  for i := 0 to High(FChildNodes) do
    if FChildNodes[i].Typ = Typ then
      Exit(FChildNodes[i]);
  Result := nil;
end;

function TSyntaxNode.GetAttribute(const Key: TAttributeName): string;
var
  AttributeEntry: PAttributeEntry;
begin
  if TryGetAttributeEntry(Key, AttributeEntry) then
    {$ifdef USESTRINGCACHE}
      Result := TStringCache.Instance.Get(AttributeEntry^.Value)
    {$else}
      Result := AttributeEntry^.Value
    {$endif}
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

procedure TSyntaxNode.AssignPositionFrom(const Node: TSyntaxNode);
begin
  FCol := Node.Col;
  FLine := Node.Line;
  FFileName := Node.FileName;
end;

{ TCompoundSyntaxNode }

{$ifdef USEBULKALLOCATOR}
  class constructor TCompoundSyntaxNode.ClassCreate;
  begin
    FAllocator := TAllocator<TCompoundSyntaxNode>.Create;
  end;

  class destructor TCompoundSyntaxNode.ClassDestroy;
  begin
    FAllocator.Free;
  end;

  class function TCompoundSyntaxNode.NewInstance: TObject;
  begin
    Result := InitInstance(FAllocator.New);
  end;

  procedure TCompoundSyntaxNode.FreeInstance;
  begin
    CleanupInstance;
    FAllocator.Return(Self);
  end;
{$endif}

function TCompoundSyntaxNode.Clone: TSyntaxNode;
begin
  Result := inherited;

  TCompoundSyntaxNode(Result).EndLine := Self.EndLine;
  TCompoundSyntaxNode(Result).EndCol := Self.EndCol;
end;

{ TValuedSyntaxNode }

{$ifdef USEBULKALLOCATOR}
  class constructor TValuedSyntaxNode.ClassCreate;
  begin
    FAllocator := TAllocator<TValuedSyntaxNode>.Create;
  end;

  class destructor TValuedSyntaxNode.ClassDestroy;
  begin
    FAllocator.Free;
  end;

  class function TValuedSyntaxNode.NewInstance: TObject;
  begin
    Result := InitInstance(FAllocator.New);
  end;

  procedure TValuedSyntaxNode.FreeInstance;
  begin
    CleanupInstance;
    FAllocator.Return(Self);
  end;
{$endif}

function TValuedSyntaxNode.Clone: TSyntaxNode;
begin
  Result := inherited;

  TValuedSyntaxNode(Result).Value := Self.Value;
end;

function TValuedSyntaxNode.GetValue: string;
begin
  {$ifdef USESTRINGCACHE}
    Result := TStringCache.Instance.Get(FValue);
  {$else}
    Result := FValue;
  {$endif}
end;

procedure TValuedSyntaxNode.SetValue(const Value: string);
begin
  {$ifdef USESTRINGCACHE}
    FValue := TStringCache.Instance.Add(Value);
  {$else}
    FValue := Value;
  {$endif}
end;

{ TCommentNode }

{$ifdef USEBULKALLOCATOR}
  class constructor TCommentNode.ClassCreate;
  begin
    FAllocator := TAllocator<TCommentNode>.Create;
  end;

  class destructor TCommentNode.ClassDestroy;
  begin
    FAllocator.Free;
  end;

  class function TCommentNode.NewInstance: TObject;
  begin
    Result := InitInstance(FAllocator.New);
  end;

  procedure TCommentNode.FreeInstance;
  begin
    CleanupInstance;
    FAllocator.Return(Self);
  end;
{$endif}

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
