unit uMainForm;

{$IFDEF FPC}{$MODE Delphi}{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, SimpleParser.Lexer.Types;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    MainMenu1: TMainMenu;
    OpenDelphiUnit1: TMenuItem;
    OpenDialog1: TOpenDialog;
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
  Form1: TForm1;

implementation

uses
  DelphiAST, DelphiAST.Writer, DelphiAST.Classes, IOUtils;

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

function TForm1.Parse(const FileName: string): string;
var
  SyntaxTree: TSyntaxNode;
begin
  Result := '';
  SyntaxTree := TPasSyntaxTreeBuilder.Run(FileName, False,
    TIncludeHandler.Create(ExtractFilePath(FileName)));
  try
    Result := TSyntaxTreeWriter.ToXML(SyntaxTree, True);
  finally
    SyntaxTree.Free;
  end;
end;

procedure TForm1.OpenDelphiUnit1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    try
      Memo1.Lines.Text := Parse(OpenDialog1.FileName);
    except
      on E: EParserException do
        Memo1.Lines.Add(Format('[%d, %d] %s', [E.Line, E.Col, E.Message]));
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
