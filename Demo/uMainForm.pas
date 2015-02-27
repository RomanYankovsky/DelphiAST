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
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  DelphiAST, DelphiAST.Writer, DelphiAST.Classes;

{$R *.dfm}

function Parse(const FileName: string): string;
var
  ASTBuilder: TPasSyntaxTreeBuilder;
  StringStream: TStringStream;
  SyntaxTree: TSyntaxNode;
begin
  Result := '';

  StringStream := TStringStream.Create;
  try
    StringStream.LoadFromFile(FileName);

    ASTBuilder := TPasSyntaxTreeBuilder.Create;
    try
      ASTBuilder.InitDefinesDefinedByCompiler;

      SyntaxTree := ASTBuilder.Run(StringStream);
      try
        Result := TSyntaxTreeWriter.ToXML(SyntaxTree, True);
      finally
        SyntaxTree.Free;
      end;
    finally
      ASTBuilder.Free;
    end;
  finally
    StringStream.Free;
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
