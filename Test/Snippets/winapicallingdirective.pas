unit winapicallingdirective;

interface

type
  TMyWinApiFunc = function(): Integer; winapi;
  
  TMyWinApiProc = procedure(); winapi;


function MyWinApiFunc(): Integer; winapi;

procedure MyWinApiProc(); winapi;

implementation

end.
