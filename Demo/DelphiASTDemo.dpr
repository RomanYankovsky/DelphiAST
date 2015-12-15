program DelphiASTDemo;

uses
  Forms,
  uMainForm in 'uMainForm.pas' {MainForm};

{$R *.res}

begin
  System.ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
