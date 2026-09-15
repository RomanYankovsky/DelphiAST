unit proceduraltypes;

interface

type
  TPlainProc = procedure(const aX: Integer);
  TPlainFunc = function(const aX: Integer): Boolean;

  TMethodProc = procedure(const aX: Integer) of object;
  TMethodFunc = function(const aX: Integer): Boolean of object;

  TAnonProc = reference to procedure(const aX: Integer);
  TAnonFunc = reference to function(const aX: Integer): Boolean;

  TNoParamsProc = procedure;
  TNoParamsMethod = procedure of object;
  TNoParamsAnon = reference to procedure;

  TStdCallProc = procedure(const aX: Integer); stdcall;
  TStdCallMethod = procedure(const aX: Integer) of object; stdcall;

implementation

end.
