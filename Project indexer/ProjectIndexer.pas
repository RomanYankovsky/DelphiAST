unit ProjectIndexer;

interface

uses
  System.Classes, System.Generics.Defaults, System.Generics.Collections,
  SimpleParser.Lexer.Types,
  DelphiAST, DelphiAST.Classes, DelphiAST.Consts;

type
  TProjectIndexer = class
  strict private type
    TParsedUnitsCache = TObjectDictionary<string,TSyntaxNode>;
    TUnitPathsCache = TDictionary<string,string>;
  public type
    TOption = (piUseDefinesDefinedByCompiler);
    TOptions = set of TOption;
    TUnitParsedEvent = procedure (Sender: TObject; const unitName: string; const fileName: string;
      var syntaxTree: TSyntaxNode; var abort: boolean) of object;

    TUnitInfo = record
      Name: string;
      Path: string;
      SyntaxTree: TSyntaxNode;
      HasError: boolean;
      ErrorInfo: record
        Line: integer;
        Col: integer;
        Error: string;
      end;
    end;

    TParsedUnits = class
    strict private
      FInfo: TList<TUnitInfo>;
    strict protected
      function  GetInfo(idx: integer): TUnitInfo; inline;
    protected
      procedure Initialize(parsedUnits: TParsedUnitsCache; unitPaths: TUnitPathsCache);
    public
      constructor Create;
      destructor Destroy; override;
      function  Count: integer; inline;
      function  GetEnumerator: TEnumerator<TUnitInfo>; inline;
      function  ToArray: TArray<TUnitInfo>; inline;
      property Info[idx: integer]: TUnitInfo read GetInfo; default;
    end;

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
    FAborting       : boolean;
    FDefines        : string;
    FDefinesList    : TStringList;
    FIncludeCache   : TIncludeCache;
    FOnUnitParsed   : TUnitParsedEvent;
    FOptions        : TOptions;
    FParsedUnits    : TParsedUnitsCache;
    FParsedUnitsInfo: TParsedUnits;
    FProjectFolder  : string;
    FSearchPath     : string;
    FSearchPaths    : TStringList;
    FUnitPaths      : TUnitPathsCache;
  strict protected
    procedure AppendUnits(usesNode: TSyntaxNode; const filePath: string; unitList: TStrings);
    procedure BuildUsesList(unitNode: TSyntaxNode; const fileName: string; isProject: boolean;
      unitList: TStringList);
    function  FindType(node: TSyntaxNode; nodeType: TSyntaxNodeType): TSyntaxNode;
    procedure ParseUnit(const unitName: string; const fileName: string; isProject: boolean);
    procedure PrepareSearchPath;
    procedure PrepareDefines;
  protected
    function  FindFile(const fileName: string; relativeToFolder: string; var filePath: string): boolean;
    class function SafeOpenFileStream(const fileName: string; var fileStream: TStringStream;
      var errorMsg: string): boolean;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Index(const fileName: string);
    property Defines: string read FDefines write FDefines;
    property Options: TOptions read FOptions write FOptions default [piUseDefinesDefinedByCompiler];
    property ParsedUnits: TParsedUnits read FParsedUnitsInfo;
//    property IncludeFiles:
//    property Problems
//    property NotFoundUnits
    property SearchPath: string read FSearchPath write FSearchPath;
    property OnUnitParsed: TUnitParsedEvent read FOnUnitParsed write FOnUnitParsed;
  end;

implementation

uses
  System.SysUtils,
  SimpleParser;

{ TProjectIndexer.TParsedUnits }

constructor TProjectIndexer.TParsedUnits.Create;
begin
  inherited Create;
  FInfo := TList<TUnitInfo>.Create;
end;

destructor TProjectIndexer.TParsedUnits.Destroy;
begin
  FreeAndNil(FInfo);
  inherited;
end;

function TProjectIndexer.TParsedUnits.Count: integer;
begin
  Result := FInfo.Count;
end;

function TProjectIndexer.TParsedUnits.GetEnumerator: TEnumerator<TUnitInfo>;
begin
  Result := FInfo.GetEnumerator;
end;

function TProjectIndexer.TParsedUnits.GetInfo(idx: integer): TUnitInfo;
begin
  Result := FInfo[idx];
end;

procedure TProjectIndexer.TParsedUnits.Initialize(parsedUnits: TParsedUnitsCache;
  unitPaths: TUnitPathsCache);
var
  info    : TUnitInfo;
  kv      : TPair<string,TSyntaxNode>;
  parsed  : TArray<TPair<string,TSyntaxNode>>;
  unitPath: string;
begin
  FInfo.Clear;
  FInfo.Capacity := parsedUnits.Count;

  parsed := parsedUnits.ToArray;
  TArray.Sort<TPair<string,TSyntaxNode>>(parsed,
    TComparer<TPair<string,TSyntaxNode>>.Construct(
      function(const Left, Right: TPair<string,TSyntaxNode>): integer
      begin
        Result := TOrdinalIStringComparer(TIStringComparer.Ordinal).Compare(Left.Key, Right.Key);
      end));

  for kv in parsed do begin
    if not assigned(kv.Value) then
      continue; //for kv
    info.Name := kv.Key;
    info.SyntaxTree := kv.Value;
    if not (unitPaths.TryGetValue(kv.Key + '.pas', unitPath)
            or unitPaths.TryGetValue(kv.Key + '.dpr', unitPath))
    then
      unitPath := '';
    info.Path := unitPath;
    info.HasError := false; // TODO 1 -oPrimoz Gabrijelcic : fix that
    FInfo.Add(info);
  end;
  FInfo.TrimExcess;
end;

function TProjectIndexer.TParsedUnits.ToArray: TArray<TUnitInfo>;
begin
  Result := FInfo.ToArray;
end; { TParsedUnits.ToArray }

{ TProjectIndexer }

procedure TProjectIndexer.AppendUnits(usesNode: TSyntaxNode; const filePath: string;
  unitList: TStrings);
var
  childNode: TSyntaxNode;
  unitName : string;
  unitPath : string;
begin
  for childNode in usesNode.ChildNodes do
    if childNode.Typ = ntUnit then begin
      unitName := childNode.GetAttribute(anName);
      unitList.Add(unitName);
      if not FUnitPaths.ContainsKey(unitName) then begin
        unitPath := childNode.GetAttribute(anPath);
        if unitPath <> '' then begin
          if IsRelativePath(unitPath) then
            unitPath := filePath + unitPath;
          FUnitPaths.Add(unitName + '.pas', unitPath);
        end;
      end;
    end;
end;

procedure TProjectIndexer.BuildUsesList(unitNode: TSyntaxNode; const fileName: string;
  isProject: boolean; unitList: TStringList);
var
  fileFolder: string;
  implNode  : TSyntaxNode;
  intfNode  : TSyntaxNode;
  usesNode  : TSyntaxNode;
begin
  fileFolder := IncludeTrailingPathDelimiter(ExtractFilePath(fileName));
  if isProject then begin
    usesNode := FindType(unitNode, ntUses);
    if assigned(usesNode) then
      AppendUnits(usesNode, fileFolder, unitList);
  end
  else begin
    intfNode := FindType(unitNode, ntInterface);
    if assigned(intfNode) then begin
      usesNode := FindType(intfNode, ntUses);
      if assigned(usesNode) then
        AppendUnits(usesNode, fileFolder, unitList);
    end;
    implNode := FindType(unitNode, ntImplementation);
    if assigned(implNode) then begin
      usesNode := FindType(implNode, ntUses);
      if assigned(usesNode) then
        AppendUnits(usesNode, fileFolder, unitList);
    end;
  end;
end;

constructor TProjectIndexer.Create;
begin
  inherited Create;
  FOptions := [piUseDefinesDefinedByCompiler];
  FSearchPaths := TStringList.Create;
  FSearchPaths.Delimiter := ';';
  FSearchPaths.StrictDelimiter := true;
  FDefinesList := TStringList.Create;
  FDefinesList.Delimiter := ';';
  FDefinesList.StrictDelimiter := true;
  FParsedUnits := TParsedUnitsCache.Create;
  FParsedUnitsInfo := TParsedUnits.Create;
end;

destructor TProjectIndexer.Destroy;
begin
  FreeAndNil(FParsedUnitsInfo);
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

  function AddToUnitPaths: boolean;
  begin
    FUnitPaths.Add(fName, filePath);
    Result := true;
  end;

begin
  Result := false;
  fName := fileName.DeQuotedString;

  if FUnitPaths.TryGetValue(fName, filePath) then
    Exit(true);

  if relativeToFolder <> '' then begin
    filePath := relativeToFolder + fName;
    if FileExists(filePath) then
      Exit(AddToUnitPaths);
  end;

  filePath := FProjectFolder + fName;
  if FileExists(filePath) then
    Exit(AddToUnitPaths);

  for searchPath in FSearchPaths do begin
    filePath := searchPath + fName;
    if FileExists(filePath) then
      Exit(AddToUnitPaths);
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
var
  projectName: string;
begin
  FAborting := false;
  FParsedUnits.Clear;
  FProjectFolder := IncludeTrailingPathDelimiter(ExtractFilePath(fileName));
  FIncludeCache := TIncludeCache.Create;
  try
    FUnitPaths := TUnitPathsCache.Create;;
    try
      PrepareDefines;
      PrepareSearchPath;
      projectName := ChangeFileExt(ExtractFileName(fileName), '');
      FUnitPaths.Add(projectName + '.dpr', fileName);
      ParseUnit(projectName, fileName, true);
      FParsedUnitsInfo.Initialize(FParsedUnits, FUnitPaths);
    finally FreeAndNil(FUnitPaths); end;
  finally FreeAndNil(FIncludeCache); end;
end;

procedure TProjectIndexer.ParseUnit(const unitName: string; const fileName: string; isProject: boolean);
var
  abort     : boolean;
  builder   : TPasSyntaxTreeBuilder;
  define    : string;
  errorMsg  : string;
  fileStream: TStringStream;
  syntaxTree: TSyntaxNode;
  unitList  : TStringList;
  unitNode  : TSyntaxNode;
  usesName  : string;
  usesPath  : string;
begin
  if FAborting then
    Exit;

  syntaxTree := nil;

  if not SafeOpenFileStream(fileName, fileStream, errorMsg) then
    Writeln('Failed to open ', fileName, '. ', errorMsg) // TODO 1 -oPrimoz Gabrijelcic : Remove debugging code
  else try
    builder := TPasSyntaxTreeBuilder.Create;
    try
      builder.IncludeHandler := TIncludeHandler.Create(Self, FIncludeCache, fileName);
      if piUseDefinesDefinedByCompiler in Options then
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
    BuildUsesList(unitNode, fileName, isProject, unitList);

    if assigned(OnUnitParsed) then begin
      abort := false;
      OnUnitParsed(Self, unitName, fileName, syntaxTree, abort);
      if abort then
        FAborting := true;
    end;

    for usesName in unitList do begin
      if FAborting then
        Exit;
      if not FParsedUnits.ContainsKey(usesName) then
        if FindFile(usesName + '.pas', '', usesPath) then
          ParseUnit(usesName, usesPath, false)
        else
          FParsedUnits.Add(usesName, nil);
    end;
  finally FreeAndNil(unitList); end;
end;

procedure TProjectIndexer.PrepareSearchPath;
var
  iPath: integer;
  sPath: string;
begin
  FSearchPaths.DelimitedText := SearchPath;
  for iPath := 0 to FSearchPaths.Count - 1 do begin
    sPath := FSearchPaths[iPath];
    if IsRelativePath(sPath) then
      sPath := FProjectFolder + sPath;
    FSearchPaths[iPath] := IncludeTrailingPathDelimiter(sPath);
  end;
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

procedure TProjectIndexer.PrepareDefines;
begin
  FDefinesList.DelimitedText := FDefines;
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
