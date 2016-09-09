unit SimpleParser.StringCache;

{
  String cache: provides a global class to keep unique string instances, which
  are then referred to by an ID. There are methods to then get a string given
  an ID. This can greatly reduce the number of strings in memory, since all
  strings with the same content will be the same actual string, stored in the
  cache.

  Originally written by David Millington: vintagedave@gmail.com or dave@parnassus.co
  Code donated to the DelphiAST project, April 2016.
}

interface

uses
  System.Generics.Defaults, System.Generics.Collections, SyncObjs;

// Use STRINGCACHE_THREADSAFE to ensure one instance can be accessed by multiple
// threads at once. This prevents clearing - it keeps all added elements for the
// life of the instance (life of the program if using TStringCache.Instance)
// and locks around adding / getting items.
// This is one by default
{$define STRINGCACHE_THREADSAFE}

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
    {$ifdef STRINGCACHE_THREADSAFE}
      FLock : TCriticalSection;
    {$else}
      // If threadsafe, always persistent, so only allow it to be changed when not threadsafe
      FIsPersistent : Boolean;
    {$endif}

    class var FInstance : TStringCache;
    class constructor ClassCreate;
    class destructor ClassDestroy;

    procedure Lock; inline;
    procedure Unlock; inline;
  private
    FIdToString : TList<TStringRec>;
    function GetIsPersistent: Boolean;
    procedure SetIsPersistent(const Value: Boolean); // ID is index
  public
    constructor Create;
    destructor Destroy; override;

    function Add(const Value : string) : TStringId;
    function AddAndGet(const P : PChar; const Length : Integer) : string;
    function Get(const ID : TStringId) : string;
    procedure Clear(const OnDestruction : Boolean = false);
    procedure ByUsage(InOrder : TList<TStringRec>);

    procedure IncRef;
    procedure DecRef;

    property Persistent : Boolean read GetIsPersistent write SetIsPersistent;
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
  {$ifdef STRINGCACHE_THREADSAFE}
    FLock := TCriticalSection.Create;
  {$else}
    FIsPersistent := false; // Clear the cache when no longer needed
  {$endif}
  FStringToId := TDictionary<TStringRec, TStringId>.Create(
    TStringCache.TStringRecValueEqualityComparer.Create);
  FIdToString := TList<TStringRec>.Create;

  Add(''); // Empty string is always item 0
end;

destructor TStringCache.Destroy;
begin
  assert(FRefCount = 0, 'String cache destroyed with live objects still relying on it');
  Clear(true);
  FStringToId.Free;
  FIdToString.Free;
  {$ifdef STRINGCACHE_THREADSAFE}
    FLock.Free;
  {$endif}
  inherited;
end;

function TStringCache.Add(const Value: string): TStringId;
var
  Item : TStringRec;
begin
  Result := 0;
  Item := TStringRec.Create(Value);

  Lock;
  try
    if FStringToId.TryGetValue(Item, Result) then begin
      // Already exists. Increment the usage count of the existing one, and return
      FIdToString[Result].IncUsageCount;
      Item.Free; // Already exists, Item was search key only
      Exit;
    end;

    // Item does not yet exist
    Result := FIdToString.Add(Item);
    FStringToId.Add(Item, Result);
  finally
    Unlock;
  end;
end;

function TStringCache.AddAndGet(const P : PChar; const Length : Integer) : string;
var
  SearchStr : string;
begin
  SetString(SearchStr, P, Length);

  Lock; // Will enter in Get and Add too, but a CS can be entered multiple times
  try
    Result := Get(Add(SearchStr));
  finally
    Unlock;
  end;
end;

function TStringCache.Get(const ID: TStringId): string;
begin
  Lock;
  try
    if ID < FIdToString.Count then
      Exit(FIdToString[ID].Value)
    else
      raise Exception.Create(Format('String cache entry with ID %d does not exist', [ID]));
  finally
    Unlock;
  end;
end;

procedure TStringCache.Clear(const OnDestruction : Boolean);
var
  I : Integer;
begin
  // This doesn't need a lock. When threadsafe, never cleared except on destruction

  if FRefCount <> 0 then
    raise Exception.Create(Format('Clearing the string cache while objects still rely on it (%d)', [FRefCount]));

  // One instance of TStringRec, but stored in two lists. Free from only one
  for I := 0 to Pred(FIdToString.Count) do
    FIdToString[I].Free;

  FStringToId.Clear;
  FIdToString.Clear;

  if not OnDestruction then begin
    // Add emtpy string - it's always item 0 - unless this is being called as
    // part of destruction
    Add('');
    assert(Get(0) = '');
  end;
end;

procedure TStringCache.ByUsage(InOrder: TList<TStringRec>);
begin
  Lock;
  try
    InOrder.InsertRange(0, FIdToString);
    InOrder.Sort(TStringCache.TStringRecUsageComparer.Create);
  finally
    Unlock;
  end;
end;

function TStringCache.GetIsPersistent: Boolean;
begin
  {$ifdef STRINGCACHE_THREADSAFE}
    Result := true; // Never clears
  {$else}
    Result := FIsPersistent;
  {$endif}
end;

procedure TStringCache.SetIsPersistent(const Value: Boolean);
begin
  // If threadsafe, always persistent (never clears) so don't set anything
  {$ifndef STRINGCACHE_THREADSAFE}
    FIsPersistent := Value;
  {$endif}
end;

procedure TStringCache.IncRef;
begin
  // Keep a count of how many objects are using the string cache. This lets it
  // clear itself when the last one is freed - ie, free all the strings when
  // they are no longer needed. (The alternative, controlled by Persistent,
  // is to keep them - ie make the cache persistent over multiple runs - useful
  // for parsing the same or similar files over and over.)
  AtomicIncrement(FRefCount);
end;

procedure TStringCache.DecRef;
begin
  if AtomicDecrement(FRefCount) < 0 then
    raise Exception.Create('String cache refcount cannot be decremented below zero');

  // When threadsafe, synchronizing clearing while ensuring the refcount is 0
  // (ie an addref dosn't occur while clearing) is hard without locking around
  // IncRef and DecRef, which is expensive. So just don't clear.
  {$ifndef STRINGCACHE_THREADSAFE}
    // Unless want to keep the strings around for next parse, clear now nothing is
    // using any of them.
    if (FRefCount = 0) and (not Persistent) then
      Clear;
  {$endif}
end;

procedure TStringCache.Lock;
begin
  // If not threadsafe, nothing to do here
  {$ifdef STRINGCACHE_THREADSAFE}
    FLock.Acquire;
  {$endif}
end;

procedure TStringCache.Unlock;
begin
  // If not threadsafe, nothing to do here
  {$ifdef STRINGCACHE_THREADSAFE}
    FLock.Release;
  {$endif}
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
