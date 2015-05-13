unit uMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    MainMenu1: TMainMenu;
    OpenDelphiUnit1: TMenuItem;
    OpenDialog1: TOpenDialog;
    procedure OpenDelphiUnit1Click(Sender: TObject);
  private
    { Private declarations }
  public
  end;

var
  Form1: TForm1;

implementation

uses
  DelphiAST, DelphiAST.Writer, DelphiAST.Classes;

{$R *.dfm}

function Parse(const FileName: string): string;
var
  SyntaxTree: TSyntaxNode;
begin
  Result := '';
  SyntaxTree := TPasSyntaxTreeBuilder.Run(FileName);
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

end.
