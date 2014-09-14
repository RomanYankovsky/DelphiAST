unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.StdCtrls;

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
  DelphiAST, DelphiAST.Writer, DelphiAST.Classes, Xml.XmlDoc;

{$R *.dfm}

function Parse(const Content: string): string;
var
  Define: string;
  ASTBuilder: TPasSyntaxTreeBuilder;
  StringStream: TStringStream;
  SyntaxTree: TSyntaxNode;
begin
  Result := '';

  StringStream := TStringStream.Create(Content, TEncoding.Unicode);
  try
    StringStream.Position := 0;

    ASTBuilder := TPasSyntaxTreeBuilder.Create;
    try
      SyntaxTree := ASTBuilder.Run(StringStream);
      try
        Result := TSyntaxTreeWriter.ToXML(SyntaxTree);
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
var
  SL: TStringList;
  ParserStr: string;
  Col, Line: Integer;
begin
  if OpenDialog1.Execute then
  begin
    SL := TStringList.Create;
    try
      SL.LoadFromFile(OpenDialog1.FileName);

      Memo1.Lines.Text := FormatXMLData(Parse(SL.Text));
    finally
      SL.Free;
    end;
  end;
end;

end.
