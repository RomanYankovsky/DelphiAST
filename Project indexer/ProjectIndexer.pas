unit ProjectIndexer;

interface

uses
  System.Classes, System.Generics.Collections,
  DelphiAST, DelphiAST.Classes, DelphiAST.Consts;

type
  TProjectIndexer = class
  strict private
    FParsedUnits: TDictionary<string,TSyntaxNode>;
  strict protected
    procedure AppendUnits(usesNode: TSyntaxNode; unitList: TStrings);
    procedure BuildUsesList(unitNode: TSyntaxNode; isProject: boolean; unitList: TStringList);
    function FindType(node: TSyntaxNode; nodeType: TSyntaxNodeType): TSyntaxNode;
    procedure ParseUnit(const fileName: string; isProject: boolean);
  public
    procedure Index(const fileName: string);
  end;

implementation

uses
  System.SysUtils;

{ TProjectIndexer }

procedure TProjectIndexer.AppendUnits(usesNode: TSyntaxNode; unitList: TStrings);
var
  childNode: TSyntaxNode;
begin
  for childNode in usesNode.ChildNodes do
    if childNode.Typ = ntUnit then
      unitList.Add(childNode.GetAttribute(anName)); // 'path' is currently ignored
end;

procedure TProjectIndexer.BuildUsesList(unitNode: TSyntaxNode; isProject: boolean;
  unitList: TStringList);
var
  implNode: TSyntaxNode;
  intfNode: TSyntaxNode;
  usesNode: TSyntaxNode;
begin
  if isProject then begin
    usesNode := FindType(unitNode, ntUses);
    if assigned(usesNode) then
      AppendUnits(usesNode, unitList);
  end
  else begin
    intfNode := FindType(unitNode, ntInterface);
    if assigned(intfNode) then begin
      usesNode := FindType(intfNode, ntUses);
      if assigned(usesNode) then
        AppendUnits(usesNode, unitList);
    end;
    implNode := FindType(unitNode, ntImplementation);
    if assigned(implNode) then begin
      usesNode := FindType(implNode, ntUses);
      if assigned(usesNode) then
        AppendUnits(usesNode, unitList);
    end;
  end;
end;

function TProjectIndexer.FindType(node: TSyntaxNode; nodeType: TSyntaxNodeType):
  TSyntaxNode;
begin
  if node.Typ = nodeType then
    Exit(node)
  else
    Result := node.FindNode(nodeType);
end;

procedure TProjectIndexer.Index(const fileName: string);
begin
  FParsedUnits := TDictionary<string,TSyntaxNode>.Create;
  ParseUnit(fileName, true);
  FParsedUnits.Free;
end;

procedure TProjectIndexer.ParseUnit(const fileName: string; isProject: boolean);
var
  syntaxTree: TSyntaxNode;
  unitList  : TStringList;
  unitNode  : TSyntaxNode;
begin
  syntaxTree := TPasSyntaxTreeBuilder.Run(fileName, false);
  unitNode := FindType(syntaxTree, ntUnit);
  if assigned(unitNode) then begin
    unitList := TStringList.Create;
    try
      BuildUsesList(unitNode, isProject, unitList);

    finally FreeAndNil(unitList); end;
  end;
end;

end.
