unit DelphiAST.SimpleParserEx;

interface

uses
  SysUtils, Generics.Collections, SimpleParser, SimpleParser.Lexer.Types, SimpleParser.Lexer;

type
  TStringEvent = procedure(var s: string) of object;

  TPasLexer = class
  private
    FLexer: TmwPasLex;
    FOnHandleString: TStringEvent;
    function GetToken: string; inline;
    function GetPosXY: TTokenPoint; inline;
    function GetFileName: string;
  public
    constructor Create(const ALexer: TmwPasLex; AOnHandleString: TStringEvent);

    property FileName: string read GetFileName;
    property PosXY: TTokenPoint read GetPosXY;
    property Token: string read GetToken;
    property Lexer: TmwPasLex read FLexer;
  end;

  TmwSimplePasParEx = class(TmwSimplePasPar)
  strict protected type
    TNameListStack = class;
    TNameList = class
    strict private type
      TNameItem = class
      strict private
        FTokenPoint: TTokenPoint;
        FTokenPos: Integer;
        FNextTokenPos: Integer;
        FEndNameCalled: Boolean;
        function GetTokenLen: Integer; inline;
      public
        constructor Create(const ATokenPoint: TTokenPoint;
          const ATokenPos: Integer);
        property TokenPoint: TTokenPoint read FTokenPoint;
        property TokenPos: Integer read FTokenPos;
        property NextTokenPos: Integer read FNextTokenPos write FNextTokenPos;
        property TokenLen: Integer read GetTokenLen;
        property EndNameCalled: Boolean read FEndNameCalled write FEndNameCalled;
      end;
    strict private
      FParser: TmwSimplePasParEx;
      FNameItems: TObjectList<TNameItem>;
      function GetItems(const Index: Integer): TNameItem; inline;
      function GetLastItem: TNameItem; inline;
      function GetNames(const Index: Integer): string;
      function GetLastName: string;
      function GetCount: Integer; inline;
      function GetLexer: TmwPasLex; inline;
      property Lexer: TmwPasLex read GetLexer;
    public
      constructor Create(const AParser: TmwSimplePasParEx);
      destructor Destroy; override;
      procedure BeginName;
      procedure EndName;
      property Items[const Index: Integer]: TNameItem read GetItems;
      property LastItem: TNameItem read GetLastItem;
      property Names[const Index: Integer]: string read GetNames; default;
      property LastName: string read GetLastName;
      property Count: Integer read GetCount;
    end;
    TNameListStack = class
    strict private
      FParser: TmwSimplePasParEx;
      FNameListStack: TObjectStack<TNameList>;
    public
      constructor Create(const AParser: TmwSimplePasParEx);
      destructor Destroy; override;
      procedure PushNames; inline;
      procedure PopNames; inline;
      function ExtractNames: TNameList; inline;
      function PeekNames: TNameList; inline;
    end;
  strict private
    FNameListStack: TNameListStack;
    FPreviousNames: TNameList;
    FLexer: TPasLexer;
    FLowerCaseNames: Boolean;
    FOnHandleString: TStringEvent;
    function GetCurrentNames: TNameList; inline;
  strict protected
    procedure DoHandleString(var AString: string); inline;
    procedure PushNames; inline;
    procedure PopNames; inline;
    function PeekNames: TNameList; inline;
    procedure BeginName;
    procedure EndName;
    property CurrentNames: TNameList read GetCurrentNames;
    property PreviousNames: TNameList read FPreviousNames;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Lexer: TPasLexer read FLexer;
    property LowerCaseNames: Boolean read FLowerCaseNames write FLowerCaseNames;
    property OnHandleString: TStringEvent read FOnHandleString write FOnHandleString;
  end;

implementation

{ TPasLexer }

constructor TPasLexer.Create(const ALexer: TmwPasLex; AOnHandleString: TStringEvent);
begin
  inherited Create;
  FLexer := ALexer;
  FOnHandleString := AOnHandleString;
end;

function TPasLexer.GetFileName: string;
begin
  Result := FLexer.Buffer.FileName;
end;

function TPasLexer.GetPosXY: TTokenPoint;
begin
  Result := FLexer.PosXY;
end;

function TPasLexer.GetToken: string;
begin
  Result := FLexer.Token;
  FOnHandleString(Result);
end;

{ TPasNamesBuilder.TNamesList.TNameItem }

constructor TmwSimplePasParEx.TNameList.TNameItem.Create(
  const ATokenPoint: TTokenPoint; const ATokenPos: Integer);
begin
  FTokenPoint := ATokenPoint;
  FTokenPos := ATokenPos;
  FNextTokenPos := ATokenPos;
end;

function TmwSimplePasParEx.TNameList.TNameItem.GetTokenLen: Integer;
begin
  Result := FNextTokenPos - FTokenPos;
end;

{ TPasNamesBuilder.TNamesList }

constructor TmwSimplePasParEx.TNameList.Create(const AParser: TmwSimplePasParEx);
begin
  FParser := AParser;
  FNameItems := TObjectList<TNameItem>.Create(True);
end;

destructor TmwSimplePasParEx.TNameList.Destroy;
begin
  FNameItems.Free;
  inherited;
end;

function TmwSimplePasParEx.TNameList.GetLexer: TmwPasLex;
begin
  Result := FParser.Lexer.Lexer;
end;

procedure TmwSimplePasParEx.TNameList.BeginName;
begin
  FNameItems.Add(TNameItem.Create(Lexer.PosXY, Lexer.TokenPos));
end;

procedure TmwSimplePasParEx.TNameList.EndName;
begin
  with FNameItems.Last do
  begin
    NextTokenPos := Lexer.TokenPos;
    EndNameCalled := True;
  end;
end;

function TmwSimplePasParEx.TNameList.GetItems(const Index: Integer): TNameItem;
begin
  Result := FNameItems[Index];
end;

function TmwSimplePasParEx.TNameList.GetLastItem: TNameItem;
begin
  Result := FNameItems.Last;
end;

function TmwSimplePasParEx.TNameList.GetNames(const Index: Integer): string;
var
  NameItem: TNameItem;
begin
  NameItem := Items[Index];
  SetString(Result, Lexer.Buffer.Buf + NameItem.TokenPos, NameItem.TokenLen);
  Result := Trim(Result);
  if FParser.LowerCaseNames then
    Result := LowerCase(Result);
  FParser.DoHandleString(Result);
end;

function TmwSimplePasParEx.TNameList.GetLastName: string;
begin
  Result := Names[Count - 1];
end;

function TmwSimplePasParEx.TNameList.GetCount: Integer;
begin
  Result := FNameItems.Count;
end;

{ TPasNamesBuilder.TNameListStack }

constructor TmwSimplePasParEx.TNameListStack.Create(
  const AParser: TmwSimplePasParEx);
begin
  FParser := AParser;
  FNameListStack := TObjectStack<TNameList>.Create(True);
end;

destructor TmwSimplePasParEx.TNameListStack.Destroy;
begin
  FNameListStack.Free;
  inherited;
end;

procedure TmwSimplePasParEx.TNameListStack.PushNames;
begin
  FNameListStack.Push(TNameList.Create(FParser));
end;

procedure TmwSimplePasParEx.TNameListStack.PopNames;
begin
  FNameListStack.Pop;
end;

function TmwSimplePasParEx.TNameListStack.ExtractNames: TNameList;
begin
  Result := FNameListStack.Extract;
end;

function TmwSimplePasParEx.TNameListStack.PeekNames: TNameList;
begin
  Result := FNameListStack.Peek;
end;

{ TmwSimplePasParEx }

constructor TmwSimplePasParEx.Create;
begin
  inherited;
  FNameListStack := TNameListStack.Create(Self);
  FNameListStack.PushNames;
  FPreviousNames := TNameList.Create(Self);
  FLexer := TPasLexer.Create(inherited Lexer, DoHandleString);
  FLowerCaseNames := True;
end;

destructor TmwSimplePasParEx.Destroy;
begin
  FLexer.Free;
  FPreviousNames.Free;
  FNameListStack.PopNames;
  FNameListStack.Free;
  inherited;
end;

procedure TmwSimplePasParEx.PushNames;
begin
  FNameListStack.PushNames;
end;

procedure TmwSimplePasParEx.PopNames;
begin
  FPreviousNames.Free;
  FPreviousNames := FNameListStack.ExtractNames;
end;

function TmwSimplePasParEx.PeekNames: TNameList;
begin
  Result := FNameListStack.PeekNames;
end;

procedure TmwSimplePasParEx.BeginName;
var
  NameList: TNameList;
begin
  NameList := FNameListStack.PeekNames;
  if (NameList.Count > 0) and not NameList.LastItem.EndNameCalled then
  begin
    FNameListStack.PushNames;
    NameList := FNameListStack.PeekNames;
  end;
  NameList.BeginName;
end;

procedure TmwSimplePasParEx.EndName;
var
  NameList: TNameList;
begin
  NameList := FNameListStack.PeekNames;
  if NameList.LastItem.EndNameCalled then
  begin
    FNameListStack.PopNames;
    NameList := FNameListStack.PeekNames;
  end;
  NameList.EndName;
end;

procedure TmwSimplePasParEx.DoHandleString(var AString: string);
begin
  if Assigned(FOnHandleString) then
    FOnHandleString(AString);
end;

function TmwSimplePasParEx.GetCurrentNames: TNameList;
begin
  Result := PeekNames;
end;

end.
