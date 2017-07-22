unit DelphiAST.Serialize.Binary;

interface

uses
  Classes,
  System.Generics.Collections,
  DelphiAST.Classes;

type
  TBinarySerializer = class
  strict private
    FStream     : TStream;
    FStringTable: TDictionary<string,integer>;
  strict protected
    procedure WriteNode(Node: TSyntaxNode);
    procedure WriteNumber(Num: cardinal);
    procedure WriteString(const S: string);
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Write(Root: TSyntaxNode; Stream: TStream);
  end;

implementation

var
  CSignature: AnsiString = 'DAST binary file'#26;

type
  TNodeClass = (ntSyntax, ntCompound, ntValued, ntComment);

constructor TBinarySerializer.Create;
begin
  inherited Create;
  FStringTable := TDictionary<string,integer>.Create;
end;

destructor TBinarySerializer.Destroy;
begin
  FStringTable.Free;
  inherited Destroy;
end;

procedure TBinarySerializer.Write(Root: TSyntaxNode; Stream: TStream);
var
  version: Integer;
begin
  FStream := Stream;
  FStream.Write(CSignature[1], Length(CSignature));
  version := $01000000;
  FStream.Write(version, 4);
  WriteNode(Root);
end;

procedure TBinarySerializer.WriteNode(Node: TSyntaxNode);
var
  attr     : TAttributeEntry;
  b        : byte;
  childNode: TSyntaxNode;
  i        : Integer;
  nodeClass: TNodeClass;
  w        : word;
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
    WriteNumber(id OR $FF000000)
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
