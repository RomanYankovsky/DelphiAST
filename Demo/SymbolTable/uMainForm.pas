unit uMainForm;

{$IFDEF FPC}{$MODE Delphi}{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, ComCtrls, Generics.Collections, DelphiAST.MSXML2,
  DelphiAST.SymbolTable, Vcl.ExtCtrls;

type
  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    OpenDialog: TOpenDialog;
    StatusBar: TStatusBar;
    OpenDelphiProject1: TMenuItem;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ScopeEdt: TEdit;
    SymbolNameEdt: TEdit;
    Lookup: TButton;
    SymbolsBox: TListBox;
    Splitter1: TSplitter;
    DeclarationMemo: TMemo;
    procedure OpenDelphiProject1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SymbolsBoxClick(Sender: TObject);
    procedure LookupClick(Sender: TObject);
  private
    FParsedXml: TDictionary<string, IXMLDOMDocument2>;
    FSymbolTable: TSymbolTable;
    procedure ClearUI;
    procedure ParseProject(const FileName: string);
    procedure ShowSymbol(const SymbolName: string);
  end;

var
  MainForm: TMainForm;

implementation

uses
  StringPool, XMLDoc,
  DelphiAST, DelphiAST.Writer, DelphiAST.Classes, DelphiAST.ProjectIndexer,
  SimpleParser.Lexer.Types, IOUtils, Diagnostics;

{$R *.dfm}

procedure TMainForm.ClearUI;
begin
  SymbolsBox.Clear;
  DeclarationMemo.Clear;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FParsedXml := TDictionary<string, IXMLDOMDocument2>.Create;
  FSymbolTable := TSymbolTable.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FSymbolTable.Free;
  FParsedXml.Free;
end;

procedure TMainForm.LookupClick(Sender: TObject);
var
  Symbol: TSymbol;
begin
  Symbol := FSymbolTable.LookupSymbol([ScopeEdt.Text], SymbolNameEdt.Text);

  if Assigned(Symbol) then
    ShowSymbol(Symbol.FullyQualifiedName)
  else
    DeclarationMemo.Clear;
end;

procedure TMainForm.ParseProject(const FileName: string);
var
  Indexer: TProjectIndexer;
  ParsedUnits: TProjectIndexer.TUnitInfo;
  Xml: IXMLDOMDocument2;
  SymbolName: string;
begin
  ClearUI;

  FParsedXml.Clear;
  FSymbolTable.Clear;

  Indexer := TProjectIndexer.Create;
  try
    Indexer.Index(FileName);

    for ParsedUnits in Indexer.ParsedUnits do
    begin
      if ParsedUnits.HasError then
        Continue;

      Xml := ComsDOMDocument.Create;
      Xml.SetProperty('SelectionLanguage', 'XPath');
      Xml.validateOnParse := False;
      Xml.preserveWhiteSpace := False;
      Xml.resolveExternals := False;
      Xml.loadXML(TSyntaxTreeWriter.ToXML(ParsedUnits.SyntaxTree));

      FParsedXml.AddOrSetValue(ParsedUnits.Path, Xml);
      FSymbolTable.ProcessUnit(ParsedUnits.Path, Xml);
    end;

    FSymbolTable.PostProcess;

    for SymbolName in FSymbolTable.Symbols.Keys do
      SymbolsBox.Items.Add(SymbolName);
  finally
    Indexer.Free;
  end;
end;

procedure TMainForm.OpenDelphiProject1Click(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    ParseProject(OpenDialog.FileName);
  end
end;

procedure TMainForm.ShowSymbol(const SymbolName: string);
var
  Symbol: TSymbol;
begin
  if FSymbolTable.Symbols.TryGetValue(SymbolName, Symbol) then
    DeclarationMemo.Text := FormatXMLData(Symbol.XmlNode.xml)
  else
    DeclarationMemo.Clear;
end;

procedure TMainForm.SymbolsBoxClick(Sender: TObject);
begin
  if SymbolsBox.ItemIndex = -1 then
    Exit;

  ShowSymbol(SymbolsBox.Items[SymbolsBox.ItemIndex]);
end;

end.
