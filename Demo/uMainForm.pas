unit uMainForm;

{$IFDEF FPC}{$MODE Delphi}{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, ComCtrls;

type
  TMainForm = class(TForm)
    OutputMemo: TMemo;
    MainMenu: TMainMenu;
    OpenDelphiUnit1: TMenuItem;
    OpenDialog: TOpenDialog;
    StatusBar: TStatusBar;
    CheckBox1: TCheckBox;
    procedure OpenDelphiUnit1Click(Sender: TObject);
  end;

var
  MainForm: TMainForm;

function Parse(const FileName: string; out StatusText: string;
  UseStringInterning: Boolean): string;

implementation

uses
  StringUsageLogging, FastMM4, StringPool,
  DelphiAST, DelphiAST.Writer, DelphiAST.Classes,
  SimpleParser.Lexer.Types, IOUtils, Diagnostics;

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

type
  TIncludeHandler = class(TInterfacedObject, IIncludeHandler)
  private
    FPath: string;
  public
    constructor Create(const Path: string);
    function GetIncludeFileContent(const FileName: string): string;
  end;

function MemoryUsed: Cardinal;
var
  st: TMemoryManagerState;
  sb: TSmallBlockTypeState;
begin
  GetMemoryManagerState(st);
  Result := st.TotalAllocatedMediumBlockSize + st.TotalAllocatedLargeBlockSize;
  for sb in st.SmallBlockTypeStates do
    Result := Result + sb.UseableBlockSize * sb.AllocatedBlockCount;
end;

function Parse(const FileName: string; out StatusText: string;
  UseStringInterning: Boolean): string;
var
  SyntaxTree: TSyntaxNode;
  memused: Cardinal;
  sw: TStopwatch;
  StringPool: TStringPool;
  OnHandleString: TStringEvent;
begin
  try
    if UseStringInterning then
    begin
      StringPool := TStringPool.Create;
      OnHandleString := StringPool.StringIntern;
    end
    else
    begin
      StringPool := nil;
      OnHandleString := nil;
    end;
    memused := MemoryUsed;
    sw := TStopwatch.StartNew;
    try
      SyntaxTree := TPasSyntaxTreeBuilder.Run(FileName, False,
        TIncludeHandler.Create(ExtractFilePath(FileName)), OnHandleString);
    finally
      if UseStringInterning then
        StringPool.Free;
    end;
    StatusText := Format('Parsed file in %d ms - used memory: %d K', [sw.ElapsedMilliseconds, (MemoryUsed - memused) div 1024]);

//    LogStringUsageToFile('strings.log');
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
var
  StatusText: string;
begin
  if OpenDialog.Execute then
  begin
    OutputMemo.Lines.Text := Parse(OpenDialog.FileName, StatusText, CheckBox1.Checked);
    StatusBar.Panels[0].Text := StatusText;
  end
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
