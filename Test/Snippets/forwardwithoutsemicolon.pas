unit forwardwithoutsemicolon;

interface

procedure proc1(); forward;
procedure proc2(); forward  // NO TRAILING SEMICOLON

implementation


procedure proc3(); forward  // NO TRAILING SEMICOLON

procedure proc1();
begin
  proc3;
end;

procedure proc2();
begin

end;

procedure proc3();
begin

end;

end.