unit DelphiAST.Serialize.Binary;

interface

uses
  Classes,
  System.Generics.Collections,
  DelphiAST.Consts,
  DelphiAST.Classes;

type
  TNodeClass = (ntSyntax, ntCompound, ntValued, ntComment);

  TBinarySerializer = class
  strict private
    FStream     : TStream;
    FStringList : TStringList;
    FStringTable: TDictionary<string,integer>;
  strict protected
    function CheckSignature: boolean;
    function CheckVersion: boolean;
    function CreateNode(nodeClass: TNodeClass; nodeType: TSyntaxNodeType): TSyntaxNode;
    function ReadNode(var node: TSyntaxNode): boolean;
    function ReadNumber(var num: cardinal): Boolean;
    function ReadString(var str: string): boolean;
    procedure WriteNode(Node: TSyntaxNode);
    procedure WriteNumber(Num: cardinal);
    procedure WriteString(const S: string);
  public
    function Read(Stream: TStream; var Root: TSyntaxNode): boolean;
    procedure Write(Stream: TStream; Root: TSyntaxNode);
  end;

implementation

var
  CSignature: AnsiString = 'DAST binary file'#26;

function TBinarySerializer.CheckSignature: boolean;
var
  sig: AnsiString;
begin
  SetLength(sig, Length(CSignature));
  Result := (FStream.Read(sig[1], Length(CSignature)) = Length(CSignature))
        and (sig = CSignature);
end;

function TBinarySerializer.CheckVersion: boolean;
var
  version: Integer;
begin
  Result := (FStream.Read(version, 4) = 4)
        and ((version AND $FFFF0000) = $01000000);
end;

function TBinarySerializer.CreateNode(nodeClass: TNodeClass; nodeType: TSyntaxNodeType):
  TSyntaxNode;
begin
  case nodeClass of
    ntSyntax:   Result := TSyntaxNode.Create(nodeType);
    ntCompound: Result := TCompoundSyntaxNode.Create(nodeType);
    ntValued:   Result := TValuedSyntaxNode.Create(nodeType);
    ntComment:  Result := TCommentNode.Create(nodeType);
  end;
end;

function TBinarySerializer.Read(Stream: TStream; var Root: TSyntaxNode): boolean;
var
  node: TSyntaxNode;
begin
  Result := false;
  FStringList := TStringList.Create;
  try
    FStream := Stream;
    if not CheckSignature then
      Exit;
    if not CheckVersion then
      Exit;
    if not ReadNode(node) then
      Exit;
    Root := node;
  finally FStringList.Free; end;
  Result := true;
end;

function TBinarySerializer.ReadNode(var node: TSyntaxNode): boolean;
var
  childNode: TSyntaxNode;
  i        : Integer;
  nodeClass: TNodeClass;
  num      : cardinal;
  numSub: cardinal;
  str      : string;
begin
  Result := false;
  node := nil;
  if (not ReadNumber(num)) or (num > Ord(High(TNodeClass))) then
    Exit;
  nodeClass := TNodeClass(num);
  if (not ReadNumber(num)) or (num > Ord(High(TSyntaxNodeType))) then
    Exit;
  node := CreateNode(nodeClass, TSyntaxNodeType(num));
  try

    if (not ReadNumber(num)) or (num > High(integer)) then
      Exit;
    Node.Col := num;
    if (not ReadNumber(num)) or (num > High(integer)) then
      Exit;
    Node.Line := num;

    case nodeClass of
      ntCompound:
        begin
          if (not ReadNumber(num)) or (num > High(integer)) then
            Exit;
          TCompoundSyntaxNode(Node).EndCol := num;
          if (not ReadNumber(num)) or (num > High(integer)) then
            Exit;
          TCompoundSyntaxNode(Node).EndLine := num;
        end;
      ntValued:
        begin
          if not ReadString(str) then
            Exit;
          TValuedSyntaxNode(Node).Value := str;
        end;
      ntComment:
        begin
          if not ReadString(str) then
            Exit;
          TCommentNode(Node).Text := str;
        end;
    end;

    if not ReadNumber(numSub) then
      Exit;
    for i := 1 to numSub do begin
      if (not ReadNumber(num)) or (num > Ord(High(TAttributeName))) then
        Exit;
      if not ReadString(str) then
        Exit;
      Node.SetAttribute(TAttributeName(num), str);
    end;

    if not ReadNumber(numSub) then
      Exit;
    for i := 1 to numSub do begin
      if not ReadNode(childNode) then
        Exit;
      Node.AddChild(childNode);
    end;

    Result := true;
  finally
    if not Result then begin
      node.Free;
      node := nil;
    end;
  end;
end;

function TBinarySerializer.ReadNumber(var num: cardinal): Boolean;
var
  lowPart: byte;
  shift  : Integer;
begin
  Result := false;
  shift := 0;
  num := 0;
  repeat
    if FStream.Read(lowPart, 1) <> 1 then
      Exit;
    num := num OR ((lowPart AND $7F) SHL shift);
    Inc(shift, 7);
  until (lowPart AND $80) = 0;
  Result := true;
end;

function TBinarySerializer.ReadString(var str: string): boolean;
var
  id: integer;
  len: cardinal;
  u8: UTF8String;
begin
  Result := false;
  if not ReadNumber(len) then
    Exit;
  if (len SHR 24) = $FF then begin
    id := len AND $00FFFFFF;
    if id >= FStringList.Count then
      Exit;
    str := FStringList[id];
  end
  else begin
    SetLength(u8, len);
    if len > 0 then
      if FStream.Read(u8[1], len) <> len then
        Exit;
    str := UTF8ToUnicodeString(u8);
    FStringList.Add(str);
  end;
  Result := true;
end;

procedure TBinarySerializer.Write(Stream: TStream; Root: TSyntaxNode);
var
  version: Integer;
begin
  FStringTable := TDictionary<string,integer>.Create;
  try
    FStream := Stream;
    FStream.Write(CSignature[1], Length(CSignature));
    version := $01000000;
    FStream.Write(version, 4);
    WriteNode(Root);
  finally FStringTable.Free; end;
end;

procedure TBinarySerializer.WriteNode(Node: TSyntaxNode);
var
  attr     : TAttributeEntry;
  childNode: TSyntaxNode;
  nodeClass: TNodeClass;
begin
  if Node is TCompoundSyntaxNode then
    nodeClass := ntCompound
  else if Node is TValuedSyntaxNode then
    nodeClass := ntValued
  else if Node is TCommentNode then
    nodeClass := ntComment
  else
    nodeClass := ntSyntax;

  WriteNumber(Ord(nodeClass));
  WriteNumber(Ord(Node.Typ));
  WriteNumber(Node.Col);
  WriteNumber(Node.Line);

  case nodeClass of
    ntCompound:
      begin
        WriteNumber(TCompoundSyntaxNode(Node).EndCol);
        WriteNumber(TCompoundSyntaxNode(Node).EndLine);
      end;
    ntValued:
      WriteString(TValuedSyntaxNode(Node).Value);
    ntComment:
      WriteString(TCommentNode(Node).Text);
  end;

  WriteNumber(Length(Node.Attributes));
  for attr in Node.Attributes do begin // causes dynamic array assignment, yuck
    WriteNumber(Ord(attr.Key));
    WriteString(attr.Value);
  end;

  WriteNumber(Length(Node.ChildNodes));
  for childNode in Node.ChildNodes do // causes dynamic array assignment, yuck
    WriteNode(childNode);
end;

procedure TBinarySerializer.WriteNumber(Num: cardinal);
var
  lowPart: byte;
begin
  repeat
    lowPart := Num AND $7F;
    Num := Num SHR 7;
    if Num <> 0 then
      lowPart := lowPart OR $80;
    FStream.Write(lowPart, 1);
  until Num = 0;
end;

procedure TBinarySerializer.WriteString(const S: string);
var
  i: Integer;
  id: integer;
  u8: UTF8String;
begin
  if (Length(S) > 4) and FStringTable.TryGetValue(S, id) then
    WriteNumber(cardinal(id) OR $FF000000)
  else begin
    if Length(S) > 4 then
      FStringTable.Add(S, FStringTable.Count + 1);
    u8 := UTF8Encode(s);
    i := Length(u8);
    WriteNumber(i);
    if i > 0 then
      FStream.Write(@(u8[1]), i);
  end;
end;

end.
