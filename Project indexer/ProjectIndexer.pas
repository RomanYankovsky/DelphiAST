unit ProjectIndexer;

interface

uses
  System.Classes, System.Generics.Collections,
  DelphiAST, DelphiAST.Classes, DelphiAST.Consts;

type
  TProjectIndexer = class
  strict private
    FParsedUnits  : TObjectDictionary<string,TSyntaxNode>;
    FProjectFolder: string;
    FSearchPath   : string;
    FSearchPaths  : TStringList;
  strict protected
    procedure AppendUnits(usesNode: TSyntaxNode; unitList: TStrings);
    procedure BuildUsesList(unitNode: TSyntaxNode; isProject: boolean; unitList: TStringList);
    function  FindType(node: TSyntaxNode; nodeType: TSyntaxNodeType): TSyntaxNode;
    procedure ParseUnit(const unitName: string; const fileName: string; isProject: boolean);
    function  ResolveUnit(const unitName: string; var unitPath: string): boolean;
    procedure SetSearchPath(const value: string);
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Index(const fileName: string);
    property SearchPath: string read FSearchPath write SetSearchPath;
  end;

implementation

uses
  Windows,
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

constructor TProjectIndexer.Create;
begin
AllocConsole;
  inherited Create;
  FSearchPaths := TStringList.Create;
  FSearchPaths.Delimiter := ';';
  FSearchPaths.StrictDelimiter := true;
end;

destructor TProjectIndexer.Destroy;
begin
  FreeAndNil(FSearchPaths);
  inherited;
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
  FParsedUnits := TObjectDictionary<string,TSyntaxNode>.Create([doOwnsValues]);
  FProjectFolder := IncludeTrailingPathDelimiter(ExtractFilePath(fileName));
  ParseUnit(ChangeFileExt(ExtractFileName(fileName), ''), fileName, true);
  FreeAndNil(FParsedUnits);
end;

procedure TProjectIndexer.ParseUnit(const unitName: string; const fileName: string; isProject: boolean);
var
  syntaxTree  : TSyntaxNode;
  thisUnitName: string;
  unitList    : TStringList;
  unitNode    : TSyntaxNode;
  usesName    : string;
  usesPath    : string;
begin
  try
    syntaxTree := TPasSyntaxTreeBuilder.Run(fileName, false);
  except
    on E: ESyntaxTreeException do begin
      syntaxTree := nil;
      Writeln(unitName, ' @ ', E.Line, ',', E.Col, ': ', E.Message);
    end;
  end;

  FParsedUnits.Add(unitName, syntaxTree);

  if not assigned(syntaxTree) then
    Exit;
  unitNode := FindType(syntaxTree, ntUnit);
  if not assigned(unitNode) then
    Exit;

  unitList := TStringList.Create;
  try
    BuildUsesList(unitNode, isProject, unitList);
    for usesName in unitList do
      if not FParsedUnits.ContainsKey(usesName) then
        if ResolveUnit(usesName, usesPath) then
          ParseUnit(usesName, usesPath, false)
        else
          FParsedUnits.Add(usesName, nil)
  finally FreeAndNil(unitList); end;
end;

function TProjectIndexer.ResolveUnit(const unitName: string; var unitPath: string):
  boolean;
var
  searchPath : string;
  unitNamePas: string;
begin
  Result := false;
  unitNamePas := unitName + '.pas';
  unitPath := FProjectFolder + unitNamePas;
  if FileExists(unitPath) then
    Exit(true);
  for searchPath in FSearchPaths do begin
    unitPath := searchPath + unitNamePas;
    if FileExists(unitPath) then
      Exit(true);
  end;
end;

procedure TProjectIndexer.SetSearchPath(const value: string);
var
  iPath: integer;
begin
  FSearchPath := value;
  FSearchPaths.DelimitedText := value;
  for iPath := 0 to FSearchPaths.Count - 1 do
    FSearchPaths[iPath] := IncludeTrailingPathDelimiter(FSearchPaths[iPath]);
end;

end.
