unit externalfunction;

interface

type
  TMyClass = class
    procedure ProcessMsg(var Msg: TMessage); message WM_USER;
  end;

implementation

end.