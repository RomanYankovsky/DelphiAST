unit SimpleParser.StringCache;

interface

uses
  System.Generics.Defaults, System.Generics.Collections;

type
  TStringId = type NativeInt;

  TStringCache = class
  type
    TStringRec = class
    strict private
      FValue : string;
      FUsageCount : NativeUInt;
    public
      constructor Create(const AValue : string);
      procedure IncUsageCount;
      property UsageCount : NativeUInt read FUsageCount;
      property Value : string read FValue;
    end;
  private
    type
      TStringRecValueEqualityComparer = class(TEqualityComparer<TStringRec>)
      private
        FStringComparer : IEqualityComparer<string>;
      public
        constructor Create();
        function Equals(const Left, Right: TStringRec): Boolean; overload; override;
        function GetHashCode(const Value: TStringRec): Integer; overload; override;
      end;
      TStringRecUsageComparer = class(TInterfacedObject, IComparer<TStringRec>)
        function Compare(const Left, Right: TStringRec): Integer;
      end;
  strict private
    FStringToId : TDictionary<TStringRec, TStringId>;
    FRefCount : NativeInt;
    FIsPersistent : Boolean;

    class var FInstance : TStringCache;
    class constructor ClassCreate;
    class destructor ClassDestroy;
  private
    FIdToString : TList<TStringRec>; // ID is index
  public
    constructor Create;
    destructor Destroy; override;

    function Add(const Value : string) : TStringId;
    function AddAndGet(const P : PChar; const Length : Integer) : string;
    function Get(const ID : TStringId) : string;
    procedure Clear;
    procedure ByUsage(InOrder : TList<TStringRec>);

    procedure IncRef;
    procedure DecRef;

    property Persistent : Boolean read FIsPersistent write FIsPersistent;
    class property Instance : TStringCache read FInstance;
  end;

implementation

uses
  SysUtils, Types;

{ TStringCache.TStringRecValueEqualityComparer }

constructor TStringCache.TStringRecValueEqualityComparer.Create;
begin
  inherited Create();
  FStringComparer := TEqualityComparer<string>.Default;
end;

function TStringCache.TStringRecValueEqualityComparer.Equals(const Left,
  Right: TStringRec): Boolean;
begin
  // Compare by the string it holds only
  Result := FStringComparer.Equals(Left.Value, Right.Value);
end;

function TStringCache.TStringRecValueEqualityComparer.GetHashCode(
  const Value: TStringRec): Integer;
begin
  // Compare by the string it holds only
  Result := FStringComparer.GetHashCode(Value.Value);
end;

{ TStringCache.TStringRecUsageComparer }

function TStringCache.TStringRecUsageComparer.Compare(const Left,
  Right: TStringRec): Integer;
begin
  if Left.UsageCount < Right.UsageCount then
    Exit(LessThanValue)
  else if Left.UsageCount > Right.UsageCount then
    Exit(GreaterThanValue)
  else // Usage is the same, sort by string
    Exit(TComparer<string>.Default.Compare(Left.Value, Right.Value));
end;

{ TStringCache }

class constructor TStringCache.ClassCreate;
begin
  FInstance := TStringCache.Create;
end;

class destructor TStringCache.ClassDestroy;
begin
  FInstance.Free;
end;

constructor TStringCache.Create;
begin
  inherited;
  FRefCount := 0;
  FIsPersistent := false; // Clear the cache when no longer needed
  FStringToId := TDictionary<TStringRec, TStringId>.Create(
    TStringCache.TStringRecValueEqualityComparer.Create);
  FIdToString := TList<TStringRec>.Create;

  Add(''); // Empty string is always item 0
end;

destructor TStringCache.Destroy;
begin
  assert(FRefCount = 0, 'String cache destroyed with live objects still relying on it');
  Clear;
  FStringToId.Free;
  FIdToString.Free;
  inherited;
end;

function TStringCache.Add(const Value: string): TStringId;
var
  Item : TStringRec;
begin
  Result := 0;
  Item := TStringRec.Create(Value);

  if FStringToId.TryGetValue(Item, Result) then begin
    // Already exists. Increment the usage count of the existing one, and return
    FIdToString[Result].IncUsageCount;
    Item.Free; // Already exists, Item was search key only
    Exit;
  end;

  // Item does not yet exist
  Result := FIdToString.Add(Item);
  FStringToId.Add(Item, Result);
end;

function TStringCache.AddAndGet(const P : PChar; const Length : Integer) : string;
var
  SearchStr : string;
begin
  SetString(SearchStr, P, Length);
  Result := Get(Add(SearchStr));
end;

function TStringCache.Get(const ID: TStringId): string;
begin
  if ID < FIdToString.Count then
    Exit(FIdToString[ID].Value)
  else
    raise Exception.Create(Format('String cache entry with ID %d does not exist', [ID]));
end;

procedure TStringCache.Clear;
var
  I : Integer;
begin
  if FRefCount <> 0 then
    raise Exception.Create(Format('Clearing the string cache while objects still rely on it (%d)', [FRefCount]));

  // One instance of TStringRec, but stored in two lists. Free from only one
  for I := 0 to Pred(FIdToString.Count) do
    FIdToString[I].Free;

  FStringToId.Clear;
  FIdToString.Clear;
end;

procedure TStringCache.ByUsage(InOrder: TList<TStringRec>);
begin
  InOrder.InsertRange(0, FIdToString);
  InOrder.Sort(TStringCache.TStringRecUsageComparer.Create);
end;

procedure TStringCache.IncRef;
begin
  // Keep a count of how many objects are using the string cache. This lets it
  // clear itself when the last one is freed - ie, free all the strings when
  // they are no longer needed. (The alternative, controlled by Persistent,
  // is to keep them - ie make the cache persistent over multiple runs - useful
  // for parsing the same or similar files over and over.)
  Inc(FRefCount);
end;

procedure TStringCache.DecRef;
begin
  if FRefCount = 0 then
    raise Exception.Create('String cache refcount cannot be decremented below zero');
  Dec(FRefCount);

  // Unless want to keep the strings around for next parse, clear now nothing is
  // using any of them.
  if (FRefCount = 0) and (not FIsPersistent) then
    Clear;
end;

{ TStringCache.TStringRec }

constructor TStringCache.TStringRec.Create(const AValue: string);
begin
  inherited Create;
  FValue := AValue;
  FUsageCount := 1;
end;

procedure TStringCache.TStringRec.IncUsageCount;
begin
  Inc(FUsageCount);
end;

end.
