unit DelphiAST.Classes;

interface

uses
  Generics.Collections, SimpleParser.Lexer.Types;

type
  TSyntaxNode = class
  private
    function GetHasChildren: Boolean;
    function GetHasAttributes: Boolean;
  protected
    FAttributes: TDictionary<string, string>;
    FChildNodes: TObjectList<TSyntaxNode>;
    FName: string;
  public
    constructor Create(const Name: string);
    destructor Destroy; override;

    function Clone: TSyntaxNode;

    function GetAttribute(const Key: string): string;
    function HasAttribute(const Key: string): boolean;
    procedure SetAttribute(const Key: string; Value: string);

    function AddChild(Node: TSyntaxNode): TSyntaxNode; overload;
    function AddChild(Name: string): TSyntaxNode; overload;
    procedure DeleteChild(Node: TSyntaxNode);

    function FindNode(const Name: string): TSyntaxNode;
    procedure SetPositionAttributes(PosXY: TTokenPoint; const LineStr: string = 'line'; const ColStr: string = 'col');

    property Attributes: TDictionary<string, string> read FAttributes;
    property ChildNodes: TObjectList<TSyntaxNode> read FChildNodes;
    property HasAttributes: Boolean read GetHasAttributes;
    property HasChildren: Boolean read GetHasChildren;
    property Name: string read FName;
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
  System.SysUtils, DelphiAST.Consts;

type
  TOperatorKind = (okUnary, okBinary);
  TOperatorAssocType = (atLeft, atRight);

  TOperatorInfo = record
    Name: string;
    Priority: Byte;
    Kind: TOperatorKind;
    AssocType: TOperatorAssocType;
  end;

  TOperators = class
  strict private
    class var FOps: TDictionary<string, TOperatorInfo>;
    class constructor Create;
    class destructor Destroy;
    class function GetItem(const Name: string): TOperatorInfo; static;
  public
    class function IsOpName(const Name: string): Boolean;
    class property Items[const Name: string]: TOperatorInfo read GetItem; default;
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
    ((Name: sADDR;         Priority: 1; Kind: okUnary;  AssocType: atRight),
     (Name: sDEREF;        Priority: 1; Kind: okUnary;  AssocType: atLeft),
     (Name: sGENERIC;      Priority: 1; Kind: okBinary; AssocType: atRight),
     (Name: sDOT;          Priority: 1; Kind: okBinary; AssocType: atRight),
     (Name: sCALL;         Priority: 3; Kind: okBinary; AssocType: atRight),
     (Name: sINDEXED;      Priority: 4; Kind: okUnary;  AssocType: atLeft),
     (Name: sUNARYMINUS;   Priority: 5; Kind: okUnary;  AssocType: atRight),
     (Name: sNOT;          Priority: 6; Kind: okUnary;  AssocType: atRight),
     (Name: sMUL;          Priority: 7; Kind: okBinary; AssocType: atRight),
     (Name: sFDIV;         Priority: 7; Kind: okBinary; AssocType: atRight),
     (Name: sDIV;          Priority: 7; Kind: okBinary; AssocType: atRight),
     (Name: sMOD;          Priority: 7; Kind: okBinary; AssocType: atRight),
     (Name: sAND;          Priority: 7; Kind: okBinary; AssocType: atRight),
     (Name: sSHL;          Priority: 7; Kind: okBinary; AssocType: atRight),
     (Name: sSHR;          Priority: 7; Kind: okBinary; AssocType: atRight),
     (Name: sAS;           Priority: 7; Kind: okBinary; AssocType: atRight),
     (Name: sADD;          Priority: 8; Kind: okBinary; AssocType: atRight),
     (Name: sSUB;          Priority: 8; Kind: okBinary; AssocType: atRight),
     (Name: sOR;           Priority: 8; Kind: okBinary; AssocType: atRight),
     (Name: sXOR;          Priority: 8; Kind: okBinary; AssocType: atRight),
     (Name: sEQUAL;        Priority: 9; Kind: okBinary; AssocType: atRight),
     (Name: sNOTEQUAL;     Priority: 9; Kind: okBinary; AssocType: atRight),
     (Name: sLOWER;        Priority: 9; Kind: okBinary; AssocType: atRight),
     (Name: sGREATER;      Priority: 9; Kind: okBinary; AssocType: atRight),
     (Name: sLOWEREQUAL;   Priority: 9; Kind: okBinary; AssocType: atRight),
     (Name: sGREATEREQUAL; Priority: 9; Kind: okBinary; AssocType: atRight),
     (Name: sIN;           Priority: 9; Kind: okBinary; AssocType: atRight),
     (Name: sIS;           Priority: 9; Kind: okBinary; AssocType: atRight));

{ TOperators }

class constructor TOperators.Create;
var
  I: Integer;
begin
  FOps := TDictionary<string, TOperatorInfo>.Create;

  for I := Low(OperatorsInfo) to High(OperatorsInfo) do
    FOps.Add(UpperCase(OperatorsInfo[I].Name), OperatorsInfo[I]);
end;

class destructor TOperators.Destroy;
begin
  FOps.Free;
end;

class function TOperators.GetItem(const Name: string): TOperatorInfo;
begin
  Result := FOps[UpperCase(Name)];
end;

class function TOperators.IsOpName(const Name: string): Boolean;
begin
  Result := FOps.ContainsKey(UpperCase(Name));
end;

function IsRoundClose(const Name: string): Boolean; inline;
begin
  Result := SameText(Name, sROUNDCLOSE);
end;

function IsRoundOpen(const Name: string): Boolean; inline;
begin
  Result := SameText(Name, sROUNDOPEN);
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
        if TOperators.IsOpName(Node.Name) then
        begin
          while (Stack.Count > 0) and TOperators.IsOpName(Stack.Peek.Name) and
            (((TOperators[Node.Name].AssocType = atLeft) and
            (TOperators[Node.Name].Priority >= TOperators[Stack.Peek.Name].Priority))
            or
            ((TOperators[Node.Name].AssocType = atRight) and
            (TOperators[Node.Name].Priority > TOperators[Stack.Peek.Name].Priority)))
          do
            Result.Add(Stack.Pop);

          Stack.Push(Node);
        end
        else if IsRoundOpen(Node.Name) then
          Stack.Push(Node)
        else if IsRoundClose(Node.Name) then
        begin
          while not IsRoundOpen(Stack.Peek.Name) do
            Result.Add(Stack.Pop);
          Stack.Pop;
          if (Stack.Count > 0) and TOperators.IsOpName(Stack.Peek.Name) then
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
      if TOperators.IsOpName(Node.Name) then
        case TOperators[Node.Name].Kind of
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
      if SameText(Node.Name, sCALL) then
        Continue;

      if Assigned(PrevNode) and IsRoundOpen(Node.Name) then
      begin
        if not TOperators.IsOpName(PrevNode.Name) and not IsRoundOpen(PrevNode.Name) then
          Result.Add(TSyntaxNode.Create(sCALL));

        if TOperators.IsOpName(PrevNode.Name)
          and (TOperators[PrevNode.Name].Kind = okUnary)
          and (TOperators[PrevNode.Name].AssocType = atLeft)
        then
          Result.Add(TSyntaxNode.Create(sCALL));
      end;

      if Assigned(PrevNode) and (Node.Name = sTYPEARGS) then
      begin
        if not TOperators.IsOpName(PrevNode.Name) and (PrevNode.Name <> sTYPEARGS) then
          Result.Add(TSyntaxNode.Create(sGENERIC));

        if TOperators.IsOpName(PrevNode.Name)
          and (TOperators[PrevNode.Name].Kind = okUnary)
          and (TOperators[PrevNode.Name].AssocType = atLeft)
        then
          Result.Add(TSyntaxNode.Create(sGENERIC));
      end;

      if Node.Name <> sALIGNMENTPARAM then
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

procedure TSyntaxNode.SetAttribute(const Key: string; Value: string);
begin
  FAttributes.AddOrSetValue(Key, Value);
end;

procedure TSyntaxNode.SetPositionAttributes(PosXY: TTokenPoint; const LineStr, ColStr: string);
begin
  SetAttribute(LineStr, IntToStr(PosXY.Y));
  SetAttribute(ColStr, IntToStr(PosXY.X))
end;

function TSyntaxNode.AddChild(Node: TSyntaxNode): TSyntaxNode;
begin
  Assert(Assigned(Node));

  FChildNodes.Add(Node);
  Result := Node;
end;

function TSyntaxNode.AddChild(Name: string): TSyntaxNode;
begin
  Result := AddChild(TSyntaxNode.Create(Name));
end;

function TSyntaxNode.Clone: TSyntaxNode;
var
  ChildNode: TSyntaxNode;
  Attr: TPair<string, string>;
begin
  Result := TSyntaxNode.Create(FName);

  for ChildNode in FChildNodes do
    Result.AddChild(ChildNode.Clone);

  for Attr in FAttributes do
    Result.SetAttribute(Attr.Key, Attr.Value);
end;

constructor TSyntaxNode.Create(const Name: string);
begin
  inherited Create;
  FName := Name;
  FAttributes := TDictionary<string, string>.Create;
  FChildNodes := TObjectList<TSyntaxNode>.Create(True);
end;

procedure TSyntaxNode.DeleteChild(Node: TSyntaxNode);
begin
  FChildNodes.Remove(node);
end;

destructor TSyntaxNode.Destroy;
begin
  FChildNodes.Free;
  FAttributes.Free;
  inherited;
end;

function TSyntaxNode.FindNode(const Name: string): TSyntaxNode;
var
  Node: TSyntaxNode;
begin
  Result := nil;
  for Node in FChildNodes do
    if SameText(Node.Name, Name) then
    begin
      Result := Node;
      Break;
    end;
end;

function TSyntaxNode.GetAttribute(const Key: string): string;
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

function TSyntaxNode.HasAttribute(const Key: string): boolean;
begin
  result := FAttributes.ContainsKey(key);
end;

end.
