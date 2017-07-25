program ProjectIndexerResearch;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  ProjectIndexer in 'ProjectIndexer.pas';

var
  indexer: TProjectIndexer;

begin
  try
    if ParamCount <> 1 then
      Writeln(ParamStr(0) + ' <project.dpr>')
    else begin
      indexer := TProjectIndexer.Create;
      try
        indexer.Index(ParamStr(1));
      finally FreeAndNil(indexer); end;
    end;
    Write('>');
    Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
