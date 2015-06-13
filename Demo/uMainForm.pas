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
     procedure HandleMessage(Sender: TObject; const Typ: TMessageEventType; const Msg: string; X, Y: Integer);
  public
    { Public declarations }
  end;

  TIncludeHandler = class(TInterfacedObject, IIncludeHandler)
    function GetIncludeFileContent(const FileName: string): string;
  end;

var
  Form1: TForm1;

implementation

uses
  DelphiAST, DelphiAST.Writer, DelphiAST.Classes;

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
  SyntaxTree := TPasSyntaxTreeBuilder.Run(FileName, false, TIncludeHandler.Create);
  try
    Result := TSyntaxTreeWriter.ToXML(SyntaxTree, True);
  finally
    SyntaxTree.Free;
  end;
end;

procedure TForm1.HandleMessage(Sender: TObject; const Typ: TMessageEventType; const Msg: string; X, Y: Integer);
var
  sTyp: string;
begin
  if Typ = meError then sTyp := 'meError - ' else sTyp := 'meNotSupported - ';

  ShowMessage(sTyp + Msg)
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

function TIncludeHandler.GetIncludeFileContent(const FileName: string): string;
begin
  //ShowMessage(FileName);
  Result := '';
  if FileName <> 'CompilerSettings.inc' then
    Result := 'abc,' + #13#10 + 'def,' + #13#10 + 'ghi';
end;

end.
