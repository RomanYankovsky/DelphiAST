{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit PascalAST;

{$warn 5023 off : no warning about unused units}
interface

uses
  DelphiAST.Writer, StringPool, DelphiAST.Consts, DelphiAST, 
  DelphiAST.ProjectIndexer, DelphiAST.Serialize.Binary, 
  DelphiAST.SimpleParserEx, Diagnostics, IOUtils, SimpleParser.Types, 
  SimpleParser.Lexer, SimpleParser.Lexer.Types, SimpleParser, 
  StringBuilderUnit, SimplerParser.Lexer.Config, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('PascalAST', @Register);
end.
