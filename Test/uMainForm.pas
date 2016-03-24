unit uMainForm;

{$IFDEF FPC}{$MODE Delphi}{$ENDIF}

interface

uses
  {$IFNDEF FPC}
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  {$ELSE}
    SysUtils, Variants, Classes, Controls, Forms, StdCtrls,
  {$ENDIF}
  SimpleParser.Lexer.Types;

type
  TForm2 = class(TForm)
    memLog: TMemo;
    btnRun: TButton;
    FileOpenDialog1: TFileOpenDialog;
    procedure btnRunClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TIncludeHandler = class(TInterfacedObject, IIncludeHandler)
  private
    FPath: string;
  public
    constructor Create(const Path: string);
    function GetIncludeFileContent(const FileName: string): string;
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
  LineNumber: integer;
  FilePath: string;
begin
  memLog.Clear;

  Path := ExtractFilePath(Application.ExeName) + 'Snippets\';
  FileOpenDialog1.DefaultFolder:= Path;
  if not(FileOpenDialog1.Execute) then Exit;
  Path:= FileOpenDialog1.FileName;

  //if not SelectDirectory('Select Folder', '', Path) then
  //  Exit;

  for FileName in TDirectory.GetFiles(Path, '*.pas', TSearchOption.soAllDirectories) do
  begin
    try
      LineNumber := memlog.Lines.Add('Testing:' + FileName);
      FilePath:= TPath.GetDirectoryName(Filename);
      SyntaxTree := TPasSyntaxTreeBuilder.Run(FileName, False, TIncludeHandler.Create(FilePath));
      try
        memLog.Lines[LineNumber]:= 'OK:     ' + FileName;
      finally
        SyntaxTree.Free;
      end;
    except
      on E: Exception do
      begin
          memLog.Lines[Linenumber]:= 'FAILED: ' + FileName;
          memLog.Lines.Add('        ' + E.ClassName);
          memLog.Lines.Add('        ' + E.Message);
          memLog.Repaint;
      end;
    end;
  end;
end;

{ TIncludeHandler }

constructor TIncludeHandler.Create(const Path: string);
begin
  inherited Create;
  FPath := Path;
end;

function TIncludeHandler.GetIncludeFileContent(const FileName: string): string;
var
  FileContent: TStringList;
begin
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(TPath.Combine(FPath, FileName));
    Result := FileContent.Text;
  finally
    FileContent.Free;
  end;
end;

end.
