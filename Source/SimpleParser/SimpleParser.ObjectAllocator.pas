unit SimpleParser.ObjectAllocator;

{
  Object bulk allocator, to assist in preventing memory fragmentation over time.
  Allocates large chunks of memory, and classes can implement NewInstance to
  get a piece of it. Freeing should similarly return the memory back to the pool.
  In this implementation, memory is never returned to the OS until the process
  shuts down - if objects are allocated and freed frequently enough that a pool
  prevents fragmentation, keeping the memory allocated for the subsequent times
  round is a good strategy. Some bookkeeping could track when an entire OS
  allocation is unused and return it.

  Originally written by David Millington: vintagedave@gmail.com or dave@parnassus.co
  See https://parnassus.co/custom-object-memory-allocation-in-delphi-bypassing-fastmm-for-fun-and-profit/
  for an overview.
  Code donated to the DelphiAST project, April 2016.
}

interface

uses
  System.Generics.Collections;

{.$define COUNT_ALLOCATIONS}

type
  TAllocator<T : class> = class
  private
    FInstanceSizeBytes : NativeUInt;
    FBulkAllocSizeBytes : NativeUInt;
    FIndividualObjects : TStack<Pointer>;
    FBulkAllocations : TStack<Pointer>;
    {$ifdef COUNT_ALLOCATIONS}
      FNumNews,
      FNumReturns,
      FNumBulkAllocs : Int64;
    {$endif}
    const NUM_ALLOCS : NativeUInt = 4096 * 4;
    procedure BulkAllocate;
  public
    constructor Create;
    destructor Destroy; override;

    function New : Pointer;
    procedure Return(const P : Pointer);
  end;

implementation

uses
  Winapi.Windows;

{ TAllocator<T> }

constructor TAllocator<T>.Create;
begin
  inherited;

  FInstanceSizeBytes := TClass(T).InstanceSize;
  FBulkAllocSizeBytes := FInstanceSizeBytes * NUM_ALLOCS; // 4K blocks in Windows, so alloc several of them

  FIndividualObjects := TStack<Pointer>.Create;
  FIndividualObjects.Capacity := NUM_ALLOCS;
  FBulkAllocations := TStack<Pointer>.Create;
  FBulkAllocations.Capacity := 16;

  {$ifdef COUNT_ALLOCATIONS}
    FNumNews := 0;
    FNumReturns := 0;
    FNumBulkAllocs := 0;
  {$endif}

  BulkAllocate;
end;

destructor TAllocator<T>.Destroy;
var
  P : Pointer;
begin
  {$ifdef COUNT_ALLOCATIONS}
    assert(FNumNews = FNumReturns); // Otherwise objects leaked
  {$endif}

  FIndividualObjects.Clear;

  // Free all bulk allocs
  while FBulkAllocations.Count > 0 do
    VirtualFree(FBulkAllocations.Pop, 0, MEM_RELEASE);

  FBulkAllocations.Clear;

  FIndividualObjects.Free;
  FBulkAllocations.Free;

  inherited;
end;

procedure TAllocator<T>.BulkAllocate;
var
  P : Pointer;
  Item : NativeUInt;
  I : Integer;
begin
  P := VirtualAlloc(nil, FBulkAllocSizeBytes, MEM_COMMIT or MEM_RESERVE, PAGE_EXECUTE_READWRITE);
  FBulkAllocations.Push(P);

  // Now, split into a set of pointers which can become individual objects, ie
  // checks of memory FInstanceSize big each
  Item := NativeUInt(P);
  while Item < NativeUInt(P) + FBulkAllocSizeBytes do begin
    FIndividualObjects.Push(Pointer(Item));
    Inc(Item, FInstanceSizeBytes);
  end;
  assert(Item = NativeUInt(P) + FBulkAllocSizeBytes);

  {$ifdef COUNT_ALLOCATIONS}
    Inc(FNumBulkAllocs);
  {$endif}
end;

function TAllocator<T>.New: Pointer;
begin
  {$ifdef COUNT_ALLOCATIONS}
    try
  {$endif}

  if FIndividualObjects.Count = 0 then
    BulkAllocate;

  Result := FIndividualObjects.Pop;
  ZeroMemory(Result, FInstanceSizeBytes);

  {$ifdef COUNT_ALLOCATIONS}
    finally
      Inc(FNumNews);
    end;
  {$endif}
end;

procedure TAllocator<T>.Return(const P: Pointer);
begin
  FIndividualObjects.Push(P);

  {$ifdef COUNT_ALLOCATIONS}
    Inc(FNumReturns);
  {$endif}
end;

end.
