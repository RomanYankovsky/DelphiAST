program DemoProject;

{$APPTYPE CONSOLE}

{$R *.res}

// Search path: sub2

uses
  System.SysUtils,
  Unit2, { in sub2\ }
  Unit1 in 'sub1\Unit1.pas';

begin
  try
    Writeln(Unit1Folder);
    Writeln(Unit1FolderIndirect);
    Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
