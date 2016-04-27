unit uMainForm;

{$IFDEF FPC}{$MODE Delphi}{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, SimpleParser.Lexer.Types;

type
  TMainForm = class(TForm)
    OutputMemo: TMemo;
    MainMenu: TMainMenu;
    OpenDelphiUnit1: TMenuItem;
    OpenDialog: TOpenDialog;
    procedure OpenDelphiUnit1Click(Sender: TObject);
  private
    function Parse(const FileName: string): string;
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
  MainForm: TMainForm;

implementation

uses
  DelphiAST, DelphiAST.Writer, DelphiAST.Classes, IOUtils;

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

function TMainForm.Parse(const FileName: string): string;
var
  SyntaxTree: TSyntaxNode;
begin
  Result := '';
  try
    SyntaxTree := TPasSyntaxTreeBuilder.Run(FileName, False, TIncludeHandler.Create(ExtractFilePath(FileName)));
    try
      Result := TSyntaxTreeWriter.ToXML(SyntaxTree, True);
    finally
      SyntaxTree.Free;
    end;
  except
    on E: ESyntaxTreeException do
      Result := Format('[%d, %d] %s', [E.Line, E.Col, E.Message]) + sLineBreak + sLineBreak +
         TSyntaxTreeWriter.ToXML(E.SyntaxTree, True);
  end;
end;

procedure TMainForm.OpenDelphiUnit1Click(Sender: TObject);
begin
  if OpenDialog.Execute then
    OutputMemo.Lines.Text := Parse(OpenDialog.FileName);
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
    if not FileExists(TPath.Combine(FPath, FileName)) then
    begin
      Result := '';
      Exit;
    end;

    FileContent.LoadFromFile(TPath.Combine(FPath, FileName));
    Result := FileContent.Text;
  finally
    FileContent.Free;
  end;
end;

end.
