unit MiniStacks;

interface

const
  DefaultSize = 31;

{$UNDEF DEBUG}
{$Assertions off}

type
  /// <summary>
  /// The ministack stores 31 elements on the system's stack.
  /// It is coded for raw speed.
  /// The stack is safe for holding managed types
  /// It does not do range checking, other than through Assertions at debug time
  /// </summary>
  TMiniStack<T> = record
{$IFDEF DEBUG}
  strict private
    function Capacity: Integer;
    procedure Validate;
{$ENDIF}
  private
    SP: Integer;
{$IFDEF CPUX64}
    Filler: Integer; // Keep array aligned
{$ENDIF}
{$IFDEF DEBUG}
    HeapFlag: TGUID;
    HeapSize: Integer;
{$ENDIF}
    Items: array [0 .. DefaultSize - 1] of T;
    function GetItem(index: Integer): T; inline;
  public
    /// <summary>
    /// Free a heap-based stack.
    /// </summary>
    /// <remarks>
    /// Do not call on a stack-based stack.
    /// </remarks>
    procedure Free;
    /// <summary>
    /// Initializes the stack.
    /// Must be called before a stack-based stack can be used.
    /// </summary>
    procedure Init; inline;
    function Pop: T; inline;
    procedure Push(const Item: T); inline;
    /// <summary>
    /// Returns the top item on the stack, does not alter the stack pointer.
    /// </summary>
    /// <returns></returns>
    function Peek: T; inline;
    procedure Clear; inline;
    function IsEmpty: Boolean; inline;
    /// <summary>
    /// Allows the stack to be accessed as a read-only array.
    /// </summary>
    property Item[index: Integer]: T read GetItem;
    property Count: Integer read SP;
  end;

  MiniStack<T> = class
  public type
    PStack = ^Stack;
    Stack = TMiniStack<T>;
  public
    /// <summary>
    /// Creates new ministack on the **heap**.
    /// </summary>
    /// <param name="Size">The maximum number of elements the stack can hold</param>
    /// <returns>Pointer to the newly created stack.</returns>
    /// <remarks>
    /// Do not create and destroy a Ministack in a loop, use the stack based Ministack instead.
    /// Note that stack-based Ministacks have a fixed size. Edit the constant `DefaultSize`
    /// to increase the size.
    /// </remarks>
    class function Create(Size: Integer = DefaultSize): PStack;
  end;

{$IFDEF DEBUG}

const
  MagicStackFlag: TGUID = '{F0E0EEC2-F229-41F1-AFE2-08BECF2AA177}';
  MagicHeapFlag: TGUID = '{EF227045-27A9-4EF3-99E3-9D279D58F9A0}';
  FreedAlreadyFlag: TGUID = '{A76BBA2F-09C5-44B7-81BF-3C8869FB8D80}';
{$ENDIF}

implementation

uses
  System.SysUtils;

type
  EMiniStackException = Exception;

{ TMiniStack<T> }
procedure TMiniStack<T>.Init;
begin
  SP:= 0;
{$IFDEF DEBUG}
  HeapFlag:= MagicStackFlag;
{$ENDIF}
end;

class function MiniStack<T>.Create(Size: Integer = DefaultSize): PStack;
begin
  Result:= AllocMem(SizeOf(TMiniStack<T>) - (DefaultSize * SizeOf(T)) + (Size * SizeOf(T)));
  Result.SP:= 0;
{$IFDEF DEBUG}
  Result.HeapFlag:= MagicHeapFlag;
  Result.HeapSize:= Size;
{$ENDIF}
end;

{$IFDEF DEBUG}
procedure TMiniStack<T>.Validate;
begin
  if (HeapFlag <> MagicStackFlag) and (HeapFlag <> MagicHeapFlag) then
      raise EMiniStackException.Create('Call init on a stack based Ministack before using it.');
end;
{$ENDIF}

{$IFDEF DEBUG}
function TMiniStack<T>.Capacity: Integer;
begin
  if HeapFlag = MagicHeapFlag then begin
    Result:= HeapSize;
  end
  else Result:= DefaultSize;
end;
{$ENDIF}

procedure TMiniStack<T>.Free;
begin
{$IFDEF DEBUG}
  Assert((HeapFlag = MagicHeapFlag), 'Do not call free on stack based MiniStacks');
  Assert((HeapFlag <> FreedAlreadyFlag), 'This stack has already been freed');
{$ENDIF}
  Finalize(Items, Count); //Compiler intrinsic
  FreeMem(@Self);
end;

function TMiniStack<T>.GetItem(index: Integer): T;
begin
  {$IFDEF DEBUG} Validate; {$ENDIF}
  Assert((index >= 0) and (index < Count), Format('Trying to get item #%d, but there are only %d items on the stack',
    [index, Count]));
  Result:= Items[index];
end;

function TMiniStack<T>.IsEmpty: Boolean;
begin
  {$IFDEF DEBUG} Validate; {$ENDIF}
  //Result:= (SP = 0);
  Result:= Boolean(not(SP)); {eliminates the test}
end;

function TMiniStack<T>.Pop: T;
begin
  {$IFDEF DEBUG} Validate; {$ENDIF}
  //Assert(SP > 0, 'Stack underflow');
  Dec(SP);
  Result:= Items[SP];
end;

function TMiniStack<T>.Peek: T;
begin
  {$IFDEF DEBUG} Validate; {$ENDIF}
  //Assert(SP > 0, 'You cannot peek at an empty stack');
  Result:= Items[SP - 1];
end;

procedure TMiniStack<T>.Clear;
begin
  SP:= 0;
end;

procedure TMiniStack<T>.Push(const Item: T);
begin
  {$IFDEF DEBUG} Validate; {$ENDIF}
  Items[SP]:= Item;
  Inc(SP);
{$IFDEF DEBUG}
  Assert(SP <= Capacity, 'Stack overflow');
{$ENDIF}
end;

end.
