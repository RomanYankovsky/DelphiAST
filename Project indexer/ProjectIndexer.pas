unit ProjectIndexer;

interface

uses
  System.Classes, System.Generics.Collections,
  SimpleParser.Lexer.Types,
  DelphiAST, DelphiAST.Classes, DelphiAST.Consts;

type
  TProjectIndexer = class
  strict private type
    TIncludeHandler = class(TInterfacedObject, IIncludeHandler)
    strict private
      FUnitFileFolder: string;
      [weak] FIndexer: TProjectIndexer;
    public
      function  GetIncludeFileContent(const fileName: string): string;
      constructor Create(indexer: TProjectIndexer; const currentFile: string);
    end;
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
  protected
    function  FindFile(const fileName: string; relativeToFolder: string; var filePath: string): boolean;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Index(const fileName: string);
    property SearchPath: string read FSearchPath write SetSearchPath;
  end;

implementation

uses
  System.SysUtils,
  GpStreams;

{ TProjectIndexer }

procedure TProjectIndexer.AppendUnits(usesNode: TSyntaxNode; unitList: TStrings);
var
  childNode: TSyntaxNode;
begin
  for childNode in usesNode.ChildNodes do
    if childNode.Typ = ntUnit then
      unitList.Add(childNode.GetAttribute(anName)); // TODO 1 -oPrimoz Gabrijelcic : 'path' is currently ignored
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

function TProjectIndexer.FindFile(const fileName: string; relativeToFolder: string;
  var filePath: string): boolean;
var
  fName     : string;
  searchPath: string;
begin
  Result := false;
  fName := fileName.DeQuotedString;

//  if SameText(fName, 'msdefine.inc') then
//    fName := fName;

  if relativeToFolder <> '' then begin
    filePath := relativeToFolder + fName;
    if FileExists(filePath) then
      Exit(true);
  end;

  filePath := FProjectFolder + fName;
  if FileExists(filePath) then
    Exit(true);

  for searchPath in FSearchPaths do begin
    filePath := searchPath + fName;
    if FileExists(filePath) then
      Exit(true);
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
    syntaxTree := TPasSyntaxTreeBuilder.Run(fileName, false, TIncludeHandler.Create(Self, fileName));
  except
    on E: ESyntaxTreeException do begin
      syntaxTree := nil;
      Writeln(unitName, ' @ ', E.Line, ',', E.Col, ': ', E.Message); // TODO 1 -oPrimoz Gabrijelcic : Remove debugging code
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
begin
  Result := FindFile(unitName + '.pas', '', unitPath);
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

{ TProjectIndexer.TIncludeHandler }

constructor TProjectIndexer.TIncludeHandler.Create(indexer: TProjectIndexer;
  const currentFile: string);
begin
  inherited Create;
  FIndexer := indexer;
  FUnitFileFolder := IncludeTrailingPathDelimiter(ExtractFilePath(currentFile));
end;

function TProjectIndexer.TIncludeHandler.GetIncludeFileContent(
  const fileName: string): string;
var
  data    : AnsiString;
  filePath: string;
begin
  if not (FIndexer.FindFile(fileName, FUnitFileFolder, filePath)
          and ReadFromFile(filePath, data))
  then
    Exit('');

  if (Length(data) >= 3) and (Ord(data[1]) = $EF) and (Ord(data[2]) = $BB) and (Ord(data[3]) = $BF) then begin
    Delete(data, 1, 3);
    Result := UTF8ToUnicodeString(data);
  end
  else
    Result := data;
end;

end.
