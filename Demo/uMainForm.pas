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
    procedure OpenDelphiUnit1Click(Sender: TObject);  
  private
    procedure UpdateStatusBarText(const StatusText: string);
  end;

var
  MainForm: TMainForm;

function Parse(const FileName: string; out StatusText: string): string;

implementation

uses
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

{$IFNDEF FPC}
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
{$ELSE}
function MemoryUsed: Cardinal;
begin
  Result := GetFPCHeapStatus.CurrHeapUsed;
end;
{$ENDIF}

function Parse(const FileName: string; out StatusText: string): string;
var
  SyntaxTree: TSyntaxNode;
  memused: Cardinal;
  sw: TStopwatch;
begin
  try
    memused := MemoryUsed;
    sw := TStopwatch.StartNew;
    SyntaxTree := TPasSyntaxTreeBuilder.Run(FileName, False, TIncludeHandler.Create(ExtractFilePath(FileName)));
    sw.Stop;
    StatusText := Format('Parsed file in %d ms - used memory: %d K', [sw.ElapsedMilliseconds, (MemoryUsed - memused) div 1024]);
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
    OutputMemo.Lines.Text := Parse(OpenDialog.FileName, StatusText);
    UpdateStatusBarText(StatusText);
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

procedure TMainForm.UpdateStatusBarText(const StatusText: string);
begin
  {$IFDEF FPC}
    StatusBar.SimpleText:= StatusText;
  {$ELSE}
    StatusBar.Panels[0].Text := StatusText;
  {$ENDIF}
end;

end.
