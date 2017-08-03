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

    TIncludeInfo = record
      FileName: string;
      Content : string;
    end;

    TIncludeCache = TDictionary<string,TIncludeInfo>;
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
  public type
    TOption = (piUseDefinesDefinedByCompiler);
    TOptions = set of TOption;
    TGetUnitSyntaxEvent = procedure (Sender: TObject; const fileName: string;
      var syntaxTree: TSyntaxNode; var doParseUnit, doAbort: boolean) of object;
    TUnitParsedEvent = procedure (Sender: TObject; const unitName: string; const fileName: string;
      var syntaxTree: TSyntaxNode; syntaxTreeFromParser: boolean; var doAbort: boolean) of object;

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
      destructor  Destroy; override;
      function  Count: integer; inline;
      function  GetEnumerator: TEnumerator<TUnitInfo>; inline;
      function  ToArray: TArray<TUnitInfo>; inline;
      property Info[idx: integer]: TUnitInfo read GetInfo; default;
    end;

    TIncludeFileInfo = record
      Name: string;
      Path: string;
    end;

    TIncludeFiles = class
    strict private
      FInfo: TList<TIncludeFileInfo>;
    strict protected
      function  GetInfo(idx: integer): TIncludeFileInfo; inline;
    protected
      procedure Initialize(includeCache: TIncludeCache);
    public
      constructor Create;
      destructor  Destroy; override;
      function  Count: integer; inline;
      function  GetEnumerator: TEnumerator<TIncludeFileInfo>; inline;
      function  ToArray: TArray<TIncludeFileInfo>; inline;
      property Info[idx: integer]: TIncludeFileInfo read GetInfo; default;
    end;

  strict private
    FAborting       : boolean;
    FDefines        : string;
    FDefinesList    : TStringList;
    FIncludeCache   : TIncludeCache;
    FIncludeFiles   : TIncludeFiles;
    FOnGetUnitSyntax: TGetUnitSyntaxEvent;
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
    procedure GetUnitSyntax(const fileName: string; var syntaxTree: TSyntaxNode; var
      doParseUnit: boolean);
    procedure NotifyUnitParsed(const unitName, fileName: string; var syntaxTree: TSyntaxNode;
      syntaxTreeFromParser: boolean);
    procedure ParseUnit(const unitName: string; const fileName: string; isProject: boolean);
    procedure PrepareSearchPath;
    procedure PrepareDefines;
    procedure RunParserOnUnit(const fileName: string; var syntaxTree: TSyntaxNode);
    procedure ScanUsedUnits(const fileName: string; isProject: boolean;
      syntaxTree: TSyntaxNode; syntaxTreeFromParser: boolean);
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
    property IncludeFiles: TIncludeFiles read FIncludeFiles;
//    property Problems
//    property NotFoundUnits
    property SearchPath: string read FSearchPath write FSearchPath;
    property OnGetUnitSyntax: TGetUnitSyntaxEvent read FOnGetUnitSyntax write FOnGetUnitSyntax;
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

{ TProjectIndexer.TIncludeFiles }

constructor TProjectIndexer.TIncludeFiles.Create;
begin
  inherited Create;
  FInfo := TList<TIncludeFileInfo>.Create;
end;

destructor TProjectIndexer.TIncludeFiles.Destroy;
begin
  FreeAndNil(FInfo);
  inherited;
end;

function TProjectIndexer.TIncludeFiles.Count: integer;
begin
  Result := FInfo.Count;
end;

function TProjectIndexer.TIncludeFiles.GetEnumerator: TEnumerator<TIncludeFileInfo>;
begin
  Result := FInfo.GetEnumerator;
end;

function TProjectIndexer.TIncludeFiles.GetInfo(idx: integer): TIncludeFileInfo;
begin
  Result := FInfo[idx];
end;

procedure TProjectIndexer.TIncludeFiles.Initialize(includeCache: TIncludeCache);
var
  info: TIncludeFileInfo;
  kv  : TPair<string,TIncludeInfo>;
  p   : integer;
begin
  FInfo.Clear;
  for kv in includeCache do begin
    p := Pos(#13, kv.Key);
    if p = 0 then
      continue; //for kv

    info.Name := Copy(kv.Key, 1, p-1);
    info.Path := kv.Value.FileName;
    FInfo.Add(info);
  end;

  FInfo.Sort;
end;

function TProjectIndexer.TIncludeFiles.ToArray: TArray<TIncludeFileInfo>;
begin
  Result := FInfo.ToArray;
end;

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
  FParsedUnits := TParsedUnitsCache.Create([doOwnsValues]);
  FParsedUnitsInfo := TParsedUnits.Create;
  FIncludeFiles := TIncludeFiles.Create;
end;

destructor TProjectIndexer.Destroy;
begin
  FreeAndNil(FIncludeFiles);
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
    filePath := ExpandFileName(relativeToFolder + fName);
    if FileExists(filePath) then
      Exit(AddToUnitPaths);
  end;

  filePath := ExpandFileName(FProjectFolder + fName);
  if FileExists(filePath) then
    Exit(AddToUnitPaths);

  for searchPath in FSearchPaths do begin
    filePath := ExpandFileName(searchPath + fName);
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

procedure TProjectIndexer.GetUnitSyntax(const fileName: string; var syntaxTree:
  TSyntaxNode; var doParseUnit: boolean);
var
  doAbort: boolean;
begin
  doAbort := false;
  doParseUnit := true;
  syntaxTree := nil;
  if assigned(OnGetUnitSyntax) then begin
    OnGetUnitSyntax(Self, fileName, syntaxTree, doParseUnit, doAbort);
    if doAbort then
      FAborting := true;
  end;
end;

procedure TProjectIndexer.Index(const fileName: string);
var
  filePath   : string;
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
      filePath := ExpandFileName(fileName);
      projectName := ChangeFileExt(ExtractFileName(fileName), '');
      FUnitPaths.Add(projectName + '.dpr', fileName);
      ParseUnit(projectName, filePath, true);
      FParsedUnitsInfo.Initialize(FParsedUnits, FUnitPaths);
      FIncludeFiles.Initialize(FIncludeCache);
    finally FreeAndNil(FUnitPaths); end;
  finally FreeAndNil(FIncludeCache); end;
end;

procedure TProjectIndexer.NotifyUnitParsed(const unitName, fileName: string; var
  syntaxTree: TSyntaxNode; syntaxTreeFromParser: boolean);
var
  doAbort: boolean;
begin
  if assigned(OnUnitParsed) then begin
    doAbort := false;
    OnUnitParsed(Self, unitName, fileName, syntaxTree, syntaxTreeFromParser, doAbort);
    if doAbort then
      FAborting := true;
  end;
end;

procedure TProjectIndexer.ParseUnit(const unitName: string; const fileName: string; isProject: boolean);
var
  doParseUnit: boolean;
  syntaxTree : TSyntaxNode;
begin
  if FAborting then
    Exit;

  GetUnitSyntax(fileName, syntaxTree, doParseUnit);

  if FAborting then
    Exit;

  if doParseUnit then
    RunParserOnUnit(fileName, syntaxTree);

  FParsedUnits.Add(unitName, syntaxTree);

  if (not FAborting) and assigned(syntaxTree) then
    ScanUsedUnits(fileName, isProject, syntaxTree, doParseUnit);
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

procedure TProjectIndexer.RunParserOnUnit(const fileName: string; var syntaxTree:
  TSyntaxNode);
var
  builder   : TPasSyntaxTreeBuilder;
  define    : string;
  errorMsg  : string;
  fileStream: TStringStream;
begin
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
end; { TProjectIndexer.RunParserOnUnit }

procedure TProjectIndexer.ScanUsedUnits(const fileName: string; isProject: boolean;
  syntaxTree: TSyntaxNode; syntaxTreeFromParser: boolean);
var
  unitList: TStringList;
  unitNode: TSyntaxNode;
  usesName: string;
  usesPath: string;
begin
  unitNode := FindType(syntaxTree, ntUnit);
  if not assigned(unitNode) then
    Exit;

  unitList := TStringList.Create;
  try
    BuildUsesList(unitNode, fileName, isProject, unitList);

    NotifyUnitParsed(unitName, fileName, syntaxTree, syntaxTreeFromParser);

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
  errorMsg   : string;
  filePath   : string;
  fileStream : TStringStream;
  fName      : string;
  includeInfo: TIncludeInfo;
  key        : string;
begin
  if fileName.StartsWith('*.') then
    fName := FUnitFile + fileName.Remove(0 {0-based}, 1)
  else if fileName.Contains('*') then
    fName := fileName.Replace('*', '', [rfReplaceAll])
  else
    fName := fileName;

  key := fName + #13 + FUnitFileFolder;
  if FIncludeCache.TryGetValue(key, includeInfo) then begin
    Result := includeInfo.Content;
    Exit;
  end;

  if not FIndexer.FindFile(fName, FUnitFileFolder, filePath) then begin
    Writeln('Include file ', fName, ' not found from unit folder ', FUnitFileFolder); // TODO 1 -oPrimoz Gabrijelcic : Remove debugging code
    includeInfo.FileName := '';
    includeInfo.Content := '';
    FIncludeCache.Add(key, includeInfo);
    Exit('');
  end;

  if FIncludeCache.TryGetValue(filePath, includeInfo) then begin
    Result := includeInfo.Content;
    Exit;
  end;

  if not TProjectIndexer.SafeOpenFileStream(filePath, fileStream, errorMsg) then begin
    Writeln('Failed to open ', filePath, '. ', errorMsg); // TODO 1 -oPrimoz Gabrijelcic : Remove debugging code
    Result := ''
  end
  else try
    Result := fileStream.DataString;
  finally FreeAndNil(fileStream); end;

  includeInfo.FileName := filePath;
  includeInfo.Content := Result;
  FIncludeCache.Add(fName + #13 + FUnitFileFolder, includeInfo);
  includeInfo.FileName := '';
  FIncludeCache.Add(filePath, includeInfo);
end;

end.
