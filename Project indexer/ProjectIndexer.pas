unit ProjectIndexer;

interface

uses
  System.Classes, System.Generics.Collections,
  SimpleParser.Lexer.Types,
  DelphiAST, DelphiAST.Classes, DelphiAST.Consts;

type
  TProjectIndexer = class
  strict private type
    TIncludeCache = TDictionary<string,string>;
    TIncludeHandler = class(TInterfacedObject, IIncludeHandler)
    strict private
      [weak] FIncludeCache: TIncludeCache;
      [weak] FIndexer     : TProjectIndexer;
      FUnitFile           : string;
      FUnitFileFolder     : string;
    public
      function  GetIncludeFileContent(const fileName: string): string;
      constructor Create(indexer: TProjectIndexer; includeCache: TIncludeCache; const currentFile: string);
    end;
  strict private
    FDefines      : string;
    FDefinesList  : TStringList;
    FIncludeCache : TIncludeCache;
    FParsedUnits  : TObjectDictionary<string,TSyntaxNode>;
    FProjectFolder: string;
    FSearchPath   : string;
    FSearchPaths  : TStringList;
    FUseDefinesDefinedByCompiler: boolean;
  strict protected
    procedure AppendUnits(usesNode: TSyntaxNode; unitList: TStrings);
    procedure BuildUsesList(unitNode: TSyntaxNode; isProject: boolean; unitList: TStringList);
    function  FindType(node: TSyntaxNode; nodeType: TSyntaxNodeType): TSyntaxNode;
    procedure ParseUnit(const unitName: string; const fileName: string; isProject: boolean);
    function  ResolveUnit(const unitName: string; var unitPath: string): boolean;
    procedure SetDefines(const value: string);
    procedure SetSearchPath(const value: string);
  protected
    function  FindFile(const fileName: string; relativeToFolder: string; var filePath: string): boolean;
    class function SafeOpenFileStream(const fileName: string; var fileStream: TStringStream;
      var errorMsg: string): boolean;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Index(const fileName: string);
    property Defines: string read FDefines write SetDefines;
    property SearchPath: string read FSearchPath write SetSearchPath;
    property UseDefinesDefinedByCompiler: boolean read FUseDefinesDefinedByCompiler write
      FUseDefinesDefinedByCompiler default true;
  end;

implementation

uses
  System.SysUtils,
  SimpleParser;

{ TProjectIndexer }

procedure TProjectIndexer.AppendUnits(usesNode: TSyntaxNode; unitList: TStrings);
var
  childNode: TSyntaxNode;
begin
  for childNode in usesNode.ChildNodes do
    if childNode.Typ = ntUnit then
      unitList.Add(childNode.GetAttribute(anName)); // TODO 1 -oPrimoz Gabrijelcic : !!! 'path' is currently ignored
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
  FUseDefinesDefinedByCompiler := true;
  FSearchPaths := TStringList.Create;
  FSearchPaths.Delimiter := ';';
  FSearchPaths.StrictDelimiter := true;
  FDefinesList := TStringList.Create;
  FDefinesList.Delimiter := ';';
  FDefinesList.StrictDelimiter := true;
  FParsedUnits := TObjectDictionary<string,TSyntaxNode>.Create([doOwnsValues]);
end;

destructor TProjectIndexer.Destroy;
begin
  FreeAndNil(FDefinesList);
  FreeAndNil(FParsedUnits);
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

  if not SameText(ExtractFileExt(fileName), '.pas') then
    Result := FindFile(fileName + '.pas', relativeToFolder, filePath);
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
  FParsedUnits.Clear;
  FProjectFolder := IncludeTrailingPathDelimiter(ExtractFilePath(fileName));
  FIncludeCache := TIncludeCache.Create;
  try
    ParseUnit(ChangeFileExt(ExtractFileName(fileName), ''), fileName, true);
  finally FreeAndNil(FIncludeCache); end;
end;

procedure TProjectIndexer.ParseUnit(const unitName: string; const fileName: string; isProject: boolean);
var
  builder   : TPasSyntaxTreeBuilder;
  define: string;
  errorMsg  : string;
  fileStream: TStringStream;
  syntaxTree: TSyntaxNode;
  unitList  : TStringList;
  unitNode  : TSyntaxNode;
  usesName  : string;
  usesPath  : string;
begin
  syntaxTree := nil;

  if not SafeOpenFileStream(fileName, fileStream, errorMsg) then
    Writeln('Failed to open ', fileName, '. ', errorMsg) // TODO 1 -oPrimoz Gabrijelcic : Remove debugging code
  else try
    builder := TPasSyntaxTreeBuilder.Create;
    try
      builder.IncludeHandler := TIncludeHandler.Create(Self, FIncludeCache, fileName);
      if UseDefinesDefinedByCompiler then
        builder.InitDefinesDefinedByCompiler;
      for define in FDefinesList do
        TmwSimplePasPar(builder).Lexer.AddDefine(define);
      try
        syntaxTree := builder.Run(fileStream);
      except
        on E: ESyntaxTreeException do begin
          Writeln(unitName, ' @ ', E.Line, ',', E.Col, ': ', E.Message); // TODO 1 -oPrimoz Gabrijelcic : Remove debugging code
        end;
      end;
    finally FreeAndNil(builder); end;
  finally FreeAndNil(fileStream); end;

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

class function TProjectIndexer.SafeOpenFileStream(const fileName: string; var fileStream:
  TStringStream; var errorMsg: string): boolean;
var
  buf       : TBytes;
  encoding  : TEncoding;
  readStream: TStream;
begin
  Result := true;
  try
    readStream := TFileStream.Create(fileName, fmOpenRead);
  except
    on E: EFCreateError do begin
      errorMsg := E.Message;
      Result := false;
    end;
    on E: EFOpenError do begin
      errorMsg := E.Message;
      Result := false;
    end;
  end;

  if Result then try
    SetLength(buf, 4);
    SetLength(buf, readStream.Read(buf[0], Length(buf)));
    encoding := nil;
    readStream.Position := TEncoding.GetBufferEncoding(buf, encoding);
    fileStream := TStringStream.Create('', encoding);
    fileStream.CopyFrom(readStream, readStream.Size - readStream.Position);
  finally FreeAndNil(readStream); end;
end;

procedure TProjectIndexer.SetDefines(const value: string);
begin
  FDefines := value;
  FDefinesList.DelimitedText := value;
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
  includeCache: TIncludeCache; const currentFile: string);
begin
  inherited Create;
  FIndexer := indexer;
  FIncludeCache := includeCache;
  FUnitFileFolder := IncludeTrailingPathDelimiter(ExtractFilePath(currentFile));
  FUnitFile := ChangeFileExt(ExtractFileName(currentFile), '');
end;

function TProjectIndexer.TIncludeHandler.GetIncludeFileContent(
  const fileName: string): string;
var
  errorMsg  : string;
  filePath  : string;
  fileStream: TStringStream;
  fName: string;
  key       : string;
begin
  if fileName.StartsWith('*.') then
    fName := FUnitFile + fileName.Remove(0 {0-based}, 1)
  else if fileName.Contains('*') then
    fName := fileName.Replace('*', '', [rfReplaceAll])
  else
    fName := fileName;

  key := fName + '#13' + FUnitFileFolder;

  if FIncludeCache.TryGetValue(key, Result) then
    Exit;

  // TODO 1 -oPrimoz Gabrijelcic : ??? Also check in project folder?
  if not FIndexer.FindFile(fName, FUnitFileFolder, filePath) then begin
    Writeln('Include file ', fName, ' not found from unit folder ', FUnitFileFolder); // TODO 1 -oPrimoz Gabrijelcic : Remove debugging code
    FIncludeCache.Add(key, '');
    Exit('');
  end;

  if FIncludeCache.TryGetValue(filePath, Result) then
    Exit;

  if not TProjectIndexer.SafeOpenFileStream(filePath, fileStream, errorMsg) then begin
    Writeln('Failed to open ', filePath, '. ', errorMsg); // TODO 1 -oPrimoz Gabrijelcic : Remove debugging code
    Result := ''
  end
  else try
    Result := fileStream.DataString;
  finally FreeAndNil(fileStream); end;

  FIncludeCache.Add(filePath, Result);
end;

end.
