unit anonymousmethodcallingconvention;

interface

type
  TStdFunc = reference to function(const A: Integer): HRESULT stdcall;
  TCdeclProc = reference to procedure(P: Pointer) cdecl;

implementation

end.