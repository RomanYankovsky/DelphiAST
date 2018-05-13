unit DelphiAST.SymbolTable;

interface

uses
  System.Generics.Defaults, System.Generics.Collections, DelphiAST.MSXML2, System.Classes;

type
  IDelphiASTSymbol = interface
    ['{53BEC7AC-1BC8-4C15-9621-6C86F220CFB8}']
    function GetFullyQualifiedName: string;
    function GetLine: Integer;
    function GetColumn: Integer;
    function GetSymbolType: string;
    function GetValueType: string;
    function GetXmlNode: IXMLDOMNode;
    function GetNextVersion: IDelphiASTSymbol;

    property FullyQualifiedName: string read GetFullyQualifiedName;
    property Line: Integer read GetLine;
    property Column: Integer read GetColumn;
    property SymbolType: string read GetSymbolType;
    property ValueType: string read GetValueType;
    property XmlNode: IXMLDOMNode read GetXmlNode;
    property NextVersion: IDelphiASTSymbol read GetNextVersion;
  end;

  TSymbol = class(TSingletonImplementation, IDelphiASTSymbol)
  private
    FFullyQualifiedName: string; // Fully qualified name
    FLine: Integer;
    FColumn: Integer;
    FSymbolType: string;
    FValueType: string;
    FValueTypeSymbol: TSymbol;
    FAncestorType: string;
    FAncestorTypeSymbol: TSymbol;
    FScopeSymbol: TSymbol;
    FUnitSection: string;
    FNextVersion: TSymbol;
    FXmlNode: IXMLDOMNode;
  public
    constructor Create(
      const FullyQualifiedName, SymbolType, ValueType, AncestorType, UnitSection: string;
      Node: IXMLDOMNode;
      DeclLine, DeclColumn: Integer);
    destructor Destroy; override;

    function GetFullyQualifiedName: string;
    function GetLine: Integer;
    function GetColumn: Integer;
    function GetSymbolType: string;
    function GetValueType: string;
    function GetValueTypeSymbol: IDelphiASTSymbol;
    function GetXmlNode: IXMLDOMNode;
    function GetNextVersion: IDelphiASTSymbol;

    property FullyQualifiedName: string read FFullyQualifiedName;
    property Line: Integer read FLine;
    property Column: Integer read FColumn;
    property SymbolType: string read FSymbolType;
    property ValueType: string read FValueType;
    property AncestorType: string read FAncestorType;
    property AncestorTypeSymbol: TSymbol read FAncestorTypeSymbol;
    property ValueTypeSymbol: TSymbol read FValueTypeSymbol;
    property ScopeSymbol: TSymbol read FScopeSymbol write FScopeSymbol;
    property UnitSection: string read FUnitSection;
    property XmlNode: IXMLDOMNode read FXmlNode;
  end;

  TUnitInfo = class
  private
    FImplementationUses: TStringList;
    FInterfaceUses: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    property ImplementationUses: TStringList read FImplementationUses;
    property InterfaceUses: TStringList read FInterfaceUses;
  end;

  TSymbolTable = class(TSingletonImplementation)
  private
    FSymbols: TObjectDictionary<string, TSymbol>;
    FUnits: TObjectDictionary<string, TUnitInfo>;

    function GetSymbolNameFromNode(const SymbolNode: IXMLDOMNode): string;

    procedure AddSymbol(
      const FullyQualifiedName, SymbolKind, SymbolType, AncestorType, UnitSection: string;
      Node: IXMLDOMNode;
      Line, Column: Integer);

    function DecomposeSymbolPath(const SymbolPath: string): TArray<string>;
    function ExtractSymbolNames(const SymbolPath: string): TArray<string>;
  public
    constructor Create;
    destructor Destroy; override;

    function LookupSymbol(const Scopes: TArray<string>; const SymbolName: string): TSymbol;
    function ResolveSymbol(const SymbolEntryNode: IXMLDOMNode; SymbolName: PWideChar): IDelphiASTSymbol;

    procedure Clear;
    procedure ProcessUnit(const FileName: string; const XMLDoc: IXMLDOMDocument2);
    procedure PostProcess;

    property Symbols: TObjectDictionary<string, TSymbol> read FSymbols;
  end;

implementation

uses
  System.IOUtils, System.SysUtils, System.StrUtils;

function ExtractSymbolName(const SymbolFQN: string): string;
var
  IdxBegin, Len: Integer;
begin
  Len := Length(SymbolFQN);

  if Len < 1 then
    Exit('');

  IdxBegin := Len;
  while (SymbolFQN[IdxBegin] <> '.') and (IdxBegin > 0) do
    IdxBegin := IdxBegin - 1;

  Result := Copy(SymbolFQN, IdxBegin + 1, Len - IdxBegin);
end;

function ExtractParentFQN(const SymbolFQN: string): string;
var
  IdxBegin, Len: Integer;
begin
  Len := Length(SymbolFQN);

  if Len < 1 then
    Exit('');

  IdxBegin := Len;
  while (SymbolFQN[IdxBegin] <> '.') and (IdxBegin > 0) do
    IdxBegin := IdxBegin - 1;

  if IdxBegin < 2 then
    Result := ''
  else
    Result := Copy(SymbolFQN, 1, IdxBegin - 1);
end;

function ExtractUnitName(const SymbolFQN: string): string;
var
  DotPos: Integer;
begin
  DotPos := Pos('.', SymbolFQN);

  if DotPos > 0 then
    Result := Copy(SymbolFQN, 1, DotPos - 1)
  else
    Result := SymbolFQN;
end;

{ TSymbolTable }

procedure TSymbolTable.AddSymbol(
  const FullyQualifiedName, SymbolKind, SymbolType, AncestorType, UnitSection: string;
  Node: IXMLDOMNode;
  Line, Column: Integer);
var
  DupSymbol: TSymbol;
  NewSymbol: TSymbol;
begin
  NewSymbol := TSymbol.Create(FullyQualifiedName, SymbolKind, SymbolType, AncestorType, UnitSection, Node, Line, Column);

  if FSymbols.TryGetValue(FullyQualifiedName, DupSymbol) then
  begin

    while Assigned(DupSymbol.FNextVersion) do
      DupSymbol := DupSymbol.FNextVersion;

    DupSymbol.FNextVersion := NewSymbol;
  end else
    FSymbols.Add(FullyQualifiedName, NewSymbol);
end;

procedure TSymbolTable.Clear;
begin
  FSymbols.Clear;
  FUnits.Clear;
end;

constructor TSymbolTable.Create;
begin
  FSymbols := TObjectDictionary<string, TSymbol>.Create([doOwnsValues]);
  FUnits := TObjectDictionary<string, TUnitInfo>.Create([doOwnsValues]);
end;

destructor TSymbolTable.Destroy;
begin
  FSymbols.Free;
  FUnits.Free;

  inherited;
end;


procedure TSymbolTable.PostProcess;
var
  Symbol: TSymbol;
  ScopeSymbolFQN: string;
begin
  for Symbol in FSymbols.Values do
  begin
    // Retrieving ScopeSymbol
    ScopeSymbolFQN := ExtractParentFQN(Symbol.FullyQualifiedName);
    FSymbols.TryGetValue(ScopeSymbolFQN, Symbol.FScopeSymbol);

    // Retrieving ValueTypeSymbol
    if Symbol.SymbolType <> 'typedecl' then
      Symbol.FValueTypeSymbol := LookupSymbol(ExtractSymbolNames(ScopeSymbolFQN), Symbol.ValueType)
    else
      // Retrieving AncestorTypeSymbol
      Symbol.FAncestorTypeSymbol := LookupSymbol(ExtractSymbolNames(ScopeSymbolFQN), Symbol.AncestorType);
  end;
end;

procedure TSymbolTable.ProcessUnit(const FileName: string; const XMLDoc: IXMLDOMDocument2);
var
  SymbolNodes: IXMLDOMNodeList;
  I: Integer;

  procedure ProcessSymbolNode(SymbolNode: IXMLDOMNode);
  var
    Ancestors: IXMLDOMNodeList;
    SymbolNodeName,
    SymbolFQN, // Fully Qualified Name
    SymbolKind,
    SymbolType,
    SymbolAncestorType,
    SymbolUnitSection: string;
    NameNode,
    UnitSection,
    ResolutionNode,
    SymbolTypeNode,
    LineNode,
    ColNode: IXMLDOMNode;
    HasNameAttribute: Boolean;
    J: Integer;
    Line, Col: Integer;
  begin
    SymbolNodeName := LowerCase(SymbolNode.nodeName);

    HasNameAttribute :=
      (SymbolNodeName = 'typedecl') or
      (SymbolNodeName = 'method') or
      (SymbolNodeName = 'unit') or
      (SymbolNodeName = 'property');

    // evaluating Fully Qualified Name of the symbol

    Ancestors := SymbolNode.SelectNodes('ancestor::node()[@name]');

    if not HasNameAttribute then
    begin
      NameNode := SymbolNode.selectSingleNode('./NAME');
      SymbolFQN := NameNode.attributes.getNamedItem('value').nodeValue;
    end else begin
      ResolutionNode := SymbolNode.selectSingleNode('./RESOLUTIONCLAUSE');

      if not Assigned(ResolutionNode) then
        SymbolFQN := SymbolNode.attributes.getNamedItem('name').nodeValue
      else begin
        SymbolFQN := ExtractSymbolName(ResolutionNode.attributes.getNamedItem('name').nodeValue);
      end;
    end;

    for J := Ancestors.length - 1 downto 0 do
    begin
      SymbolFQN := Ancestors[J].attributes.getNamedItem('name').nodeValue + '.' + SymbolFQN;
    end;

    SymbolFQN := LowerCase(SymbolFQN);

    SymbolKind := SymbolNodeName;

    LineNode := SymbolNode.attributes.getNamedItem('begin_line');
    if Assigned(LineNode) then
      ColNode := SymbolNode.attributes.getNamedItem('begin_col')
    else begin
      LineNode := SymbolNode.attributes.getNamedItem('line');
      if Assigned(LineNode) then
        ColNode := SymbolNode.attributes.getNamedItem('col')
      else begin
        LineNode := NameNode.attributes.getNamedItem('line');
        if Assigned(LineNode) then
          ColNode := NameNode.attributes.getNamedItem('col');
      end;
    end;

    if Assigned(LineNode) then
      Line := LineNode.nodeValue
    else
      Line := -1;

    if Assigned(ColNode) then
      Col := ColNode.nodeValue
    else
      Col := -1;

    if SymbolKind = 'typedecl' then
    begin
      SymbolTypeNode := SymbolNode.selectSingleNode('./TYPE/@type');
      if Assigned(SymbolTypeNode) then
        SymbolType := LowerCase(SymbolTypeNode.nodeValue);

      SymbolTypeNode := SymbolNode.selectSingleNode('./TYPE/TYPE[1]/@name');
      if Assigned(SymbolTypeNode) then
        SymbolAncestorType := LowerCase(SymbolTypeNode.nodeValue);
    end
    else begin
      SymbolAncestorType := '';
      SymbolTypeNode := SymbolNode.selectSingleNode('./TYPE/@name');
      if Assigned(SymbolTypeNode) then
        SymbolType := LowerCase(SymbolTypeNode.nodeValue)
      else
        SymbolType := '';
    end;

    UnitSection := SymbolNode.selectSingleNode('ancestor::INTERFACE');
    if Assigned(UnitSection) then
      SymbolUnitSection := 'interface'
    else
      SymbolUnitSection := 'implementation';

    AddSymbol(SymbolFQN, SymbolKind, SymbolType, SymbolAncestorType, SymbolUnitSection, SymbolNode, Line, Col);
  end;

  procedure RetrieveUnitInfo(const UnitFileName: string);
  var
    UnitInfo: TUnitInfo;
    UsesNodes: IXMLDOMNodeList;
    I: Integer;
  begin
    //Filling up the unit info
    UnitInfo := TUnitInfo.Create;
    FUnits.Add(UnitFileName, UnitInfo);

    //Uses list of INTERFACE section
    UsesNodes := XMLDoc.DocumentElement.SelectNodes('/UNIT/INTERFACE/USES/UNIT/@name');

    if Assigned(UsesNodes) then
      for I := 0 to UsesNodes.length - 1 do
        UnitInfo.InterfaceUses.Add(LowerCase(UsesNodes[I].text));

    //Uses list of IMPLEMENTATION sections
    UsesNodes := XMLDoc.DocumentElement.SelectNodes('/UNIT/IMPLEMENTATION/USES/UNIT/@name');

    if Assigned(UsesNodes) then
      for I := 0 to UsesNodes.length - 1 do
        UnitInfo.ImplementationUses.Add(LowerCase(UsesNodes[I].text));
  end;

begin
  RetrieveUnitInfo(LowerCase(TPath.GetFileNameWithoutExtension(FileName)));

  SymbolNodes := XMLDoc.DocumentElement.SelectNodes(
    '/UNIT'            + ' | ' +
    '/UNIT//TYPEDECL'  + ' | ' +
    '/UNIT//TYPEDECL/TYPE//FIELD   '  + ' | ' +
    '/UNIT//PROPERTY'  + ' | ' +
    '/UNIT//VARIABLE'  + ' | ' +
    '/UNIT//CONSTANT'  + ' | ' +
    '/UNIT//PARAMETER' + ' | ' +
    '/UNIT/INTERFACE//METHOD'
  );

  for I := 0 to SymbolNodes.length - 1 do
  begin
    ProcessSymbolNode(SymbolNodes[I]);
  end;
{
  SymbolNodes := XMLDoc.DocumentElement.SelectNodes(
    '/UNIT/IMPLEMENTATION//METHOD'
  );

  for I := 0 to SymbolNodes.length - 1 do
  begin
    ProcessSymbolNode(SymbolNodes[I]);
  end;
}


end;

function TSymbolTable.DecomposeSymbolPath(const SymbolPath: string): TArray<string>;
var
  IdxDelimPos, IdxNameBegin: Integer;
  PrevName: string;
begin
  if SymbolPath = '' then
    Exit(nil);

  IdxNameBegin := 1;
  IdxDelimPos := 0;

  PrevName := '';

  while (IdxDelimPos <= Length(SymbolPath)) do
  begin
    IdxDelimPos := PosEx('.', SymbolPath, IdxNameBegin);

    if (IdxDelimPos < 1) then
      IdxDelimPos := Length(SymbolPath) + 1;

    if PrevName <> 'self' then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := Copy(SymbolPath, IdxNameBegin, IdxDelimPos - IdxNameBegin);
    end else
      Result[High(Result)] := PrevName + '.' + Copy(SymbolPath, IdxNameBegin, IdxDelimPos - IdxNameBegin);

    PrevName := Result[High(Result)];

    IdxNameBegin := IdxDelimPos + 1;
  end;
end;

function TSymbolTable.ExtractSymbolNames(const SymbolPath: string): TArray<string>;
var
  IdxDelimPos: Integer;
begin
  if SymbolPath = '' then
    Exit(nil);

  IdxDelimPos := 0;

  while (IdxDelimPos <= Length(SymbolPath)) do
  begin
    IdxDelimPos := PosEx('.', SymbolPath, IdxDelimPos + 1);

    if (IdxDelimPos < 1) then
      IdxDelimPos := Length(SymbolPath) + 1;

    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := Copy(SymbolPath, 1, IdxDelimPos - 1);
  end;
end;

function TSymbolTable.GetSymbolNameFromNode(const SymbolNode: IXMLDOMNode): string;

  procedure AppendResult(const IdentifierName: string);
  begin
    if Result = '' then
      Result := IdentifierName
    else
      Result := Result + '.' + IdentifierName;
  end;

  procedure ProcessNode(const Node: IXMLDOMNode);
  var
    NodeName: string;
    Nodes: IXMLDOMNodeList;
    IdentifierNode: IXMLDOMNode;
    I: Integer;
  begin
    NodeName := Node.nodeName;

    if NodeName = 'AS' then
    begin
      IdentifierNode := Node.selectSingleNode('./IDENTIFIER[last()]');

      if not Assigned(IdentifierNode) then
        IdentifierNode := Node.selectSingleNode('./TYPE[last()]');

      if Assigned(IdentifierNode) then
      begin
        AppendResult(IdentifierNode.attributes.getNamedItem('name').nodeValue);
        Exit;
      end;
    end
    else
    if NodeName = 'IDENTIFIER' then
    begin
      AppendResult(Node.attributes.getNamedItem('name').nodeValue);
    end
    else
    if NodeName = 'EXPRESSIONS' then
      Exit;

    Nodes := Node.childNodes;

    if Assigned(Nodes) then
      for I := 0 to Nodes.length - 1 do
        ProcessNode(Nodes[I]);
  end;

begin
  Result := '';

  ProcessNode(SymbolNode);

  Result := LowerCase(Result);
end;

function TSymbolTable.LookupSymbol(const Scopes: TArray<string>; const SymbolName: string): TSymbol;
var
  I: Integer;
  SymbolFQN: string;
  UnitInfo: TUnitInfo;
  ScopeSymbol: TSymbol;

  function OmitSelfReference(const SymbolNameWithSelf: string): string;
  const
    SELF_REFERENCE = 'self.';
  begin
    Result := Copy(SymbolNameWithSelf, Length(SELF_REFERENCE) + 1, Length(SymbolNameWithSelf) - Length(SELF_REFERENCE));
  end;
begin
  Result := nil;

  if SymbolName = '' then
    Exit;

  if Pos('self.', SymbolName) <> 1 then
  begin
    // Name without SELF reference
    for I := High(Scopes) downto Low(Scopes) do
    begin
      SymbolFQN := Scopes[I] + '.' + SymbolName;
      if FSymbols.TryGetValue(SymbolFQN, Result) then
        Exit;
    end
  end
  else begin
    // Name with SELF reference (self.)
    for I := High(Scopes) downto Low(Scopes) do
    begin
      if FSymbols.TryGetValue(Scopes[I], ScopeSymbol) then
      begin
        if (ScopeSymbol.ValueType = 'class') or (ScopeSymbol.ValueType = 'object') then
        begin
          FSymbols.TryGetValue(Scopes[I] + '.' + OmitSelfReference(SymbolName), Result);
          Exit;
        end;
      end;
    end;
  end;

  UnitInfo := nil;
  //Search in units from Uses section
  FUnits.TryGetValue(Scopes[Low(Scopes)], UnitInfo);

  if Assigned(UnitInfo) then
  begin
    //Interfaces section
    for I := 0 to UnitInfo.InterfaceUses.Count - 1 do
    begin
      SymbolFQN := UnitInfo.InterfaceUses[I] + '.' + SymbolName;
      if FSymbols.TryGetValue(SymbolFQN, Result) then
        Exit;
    end;

    //Implementation section
    for I := 0 to UnitInfo.ImplementationUses.Count - 1 do
    begin
      SymbolFQN := UnitInfo.ImplementationUses[I] + '.' + SymbolName;
      if FSymbols.TryGetValue(SymbolFQN, Result) then
        Exit;
    end;
  end;

end;

function TSymbolTable.ResolveSymbol(const SymbolEntryNode: IXMLDOMNode;
  SymbolName: PWideChar): IDelphiASTSymbol;
var
  AncestorNodes: IXMLDOMNodeList;
  SymbolEntryScope: string;
  I: Integer;
  Symbol, ConcatSymbol, WithExprSymbol: TSymbol;
  SymbolNames, Scopes: TArray<string>;
  LocalSymbolName: string;
  HasSelfReference: Boolean;
  NameAttributeNode: IXMLDOMNode;
begin
  if SymbolName = nil then
  begin
    SymbolName := PWideChar(GetSymbolNameFromNode(SymbolEntryNode));
  end;

  LocalSymbolName := LowerCase(SymbolName);

  HasSelfReference := Pos('self.', LocalSymbolName) = 1;

  AncestorNodes := SymbolEntryNode.SelectNodes('ancestor::node()[@name]');

  SymbolEntryScope := '';

  for I := AncestorNodes.length - 1 downto 0 do
  begin
    NameAttributeNode := AncestorNodes[I].attributes.getNamedItem('name');

    if SymbolEntryScope = '' then
      SymbolEntryScope := NameAttributeNode.nodeValue
    else
      SymbolEntryScope := NameAttributeNode.nodeValue + '.' + SymbolEntryScope;
  end;

  Scopes := ExtractSymbolNames(LowerCase(SymbolEntryScope));

  // If SELF reference is not used we have to take into account WITH scopes
  if not HasSelfReference then begin
    AncestorNodes := SymbolEntryNode.SelectNodes('ancestor::WITH/EXPRESSIONS/EXPRESSION');
    for I := 0 to AncestorNodes.length - 1 do
    begin
      WithExprSymbol := LookupSymbol(Scopes, GetSymbolNameFromNode(AncestorNodes[I]));
      if Assigned(WithExprSymbol) then
      begin
        SetLength(Scopes, Length(Scopes) + 1);
        if Assigned(WithExprSymbol.ValueTypeSymbol) then
          Scopes[High(Scopes)] := WithExprSymbol.ValueTypeSymbol.FullyQualifiedName
        else
          Scopes[High(Scopes)] := WithExprSymbol.FullyQualifiedName;
      end;
    end;
  end;

  Symbol := LookupSymbol(Scopes, LocalSymbolName);

  if Assigned(Symbol) then
    Exit(Symbol);

  SymbolNames := DecomposeSymbolPath(LowerCase(LocalSymbolName));

  for I := Low(SymbolNames) to High(SymbolNames) do
  begin
    ConcatSymbol := nil;

    if not Assigned(Symbol) then
      Symbol := LookupSymbol(Scopes, SymbolNames[I])
    else begin
      while Assigned(Symbol) and not Assigned(ConcatSymbol) do
      begin
        if not FSymbols.TryGetValue(Symbol.FullyQualifiedName + '.' + SymbolNames[I], ConcatSymbol) then
        begin
          if Symbol.SymbolType <> 'typedecl' then
            Symbol := Symbol.ValueTypeSymbol
          else
            Symbol := Symbol.AncestorTypeSymbol;
        end else
          Symbol := ConcatSymbol;
      end;
    end;

    if not Assigned(Symbol) then
      Exit(nil);
  end;

  Result := Symbol;
end;

{ TSymbol }

constructor TSymbol.Create(
  const FullyQualifiedName, SymbolType, ValueType, AncestorType, UnitSection: string;
  Node: IXMLDOMNode;
  DeclLine, DeclColumn: Integer);
begin
  FFullyQualifiedName := FullyQualifiedName;
  FValueType := ValueType;
  FSymbolType:= SymbolType;
  FAncestorType := AncestorType;
  FUnitSection := UnitSection;
  FXmlNode := Node;
  FColumn := DeclColumn;
  FLine := DeclLine;
end;

destructor TSymbol.Destroy;
begin
  FreeAndNil(FNextVersion);

  inherited;
end;

function TSymbol.GetColumn: Integer;
begin
  Result := FColumn;
end;

function TSymbol.GetLine: Integer;
begin
  Result := FLine;
end;

function TSymbol.GetNextVersion: IDelphiASTSymbol;
begin
  Result := FNextVersion;
end;

function TSymbol.GetFullyQualifiedName: string;
begin
  Result := FFullyQualifiedName;
end;

function TSymbol.GetSymbolType: string;
begin
  Result := FSymbolType;
end;

function TSymbol.GetValueType: string;
begin
  Result := FValueType;
end;

function TSymbol.GetValueTypeSymbol: IDelphiASTSymbol;
begin
  Result := FValueTypeSymbol;
end;

function TSymbol.GetXmlNode: IXMLDOMNode;
begin
  Result := FXmlNode;
end;

{ TUnitInfo }

constructor TUnitInfo.Create;
begin
  FImplementationUses := TStringList.Create;
  FInterfaceUses := TStringList.Create;
end;

destructor TUnitInfo.Destroy;
begin
  FImplementationUses.Free;
  FInterfaceUses.Free;
  inherited;
end;

end.
