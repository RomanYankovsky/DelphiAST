unit ProjectIndexer;

interface

type
  TProjectIndexer = class
  strict protected
    procedure ParseUnit(const fileName: string);
  public
    procedure Index(const fileName: string);
  end;

implementation

uses
  DelphiAST, DelphiAST.Classes;

{ TProjectIndexer }

procedure TProjectIndexer.Index(const fileName: string);
begin
  ParseUnit(fileName);
end;

procedure TProjectIndexer.ParseUnit(const fileName: string);
var
  SyntaxTree: TSyntaxNode;
begin
  SyntaxTree := TPasSyntaxTreeBuilder.Run(fileName, false);
end;

end.
