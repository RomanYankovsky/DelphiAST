unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    memLog: TMemo;
    btnRun: TButton;
    procedure btnRunClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses
  FileCtrl, IOUtils, DelphiAST, DelphiAST.Classes;

{$R *.dfm}

procedure TForm2.btnRunClick(Sender: TObject);
var
  Path, FileName: string;
  SyntaxTree: TSyntaxNode;
begin
  memLog.Clear;

  Path := ExtractFilePath(Application.ExeName) + 'Snippets\';
  if not SelectDirectory('Select Folder', '', Path) then
    Exit;

  for FileName in TDirectory.GetFiles(Path, '*.pas', TSearchOption.soAllDirectories) do
  begin
    try
      SyntaxTree := TPasSyntaxTreeBuilder.Run(FileName);
      try
        memLog.Lines.Add('OK:     ' + FileName);
      finally
        SyntaxTree.Free;
      end;
    except
      on E: Exception do
      begin
        memLog.Lines.Add('FAILED: ' + FileName);
        memLog.Lines.Add('        ' + E.ClassName);
        memLog.Lines.Add('        ' + E.Message);
        memLog.Repaint;
      end;
    end;
  end;
end;

end.
