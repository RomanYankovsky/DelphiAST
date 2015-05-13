unit DelphiAST.Writer;

interface

uses
  DelphiAST.Classes, DelphiAST.Consts, SysUtils;

type
  TSyntaxNodeTypes = set of TSyntaxNodeType;

  TSyntaxTreeWriter = class
  private
    /// <summary>
    ///   Write the node in XML format
    /// </summary>
    /// <param name="Include">
    ///   Set of Nodetypes to include in the xml output.
    ///   An empty set means include all node types.
    /// </param>
    class procedure NodeToXML(const Builder: TStringBuilder;
  const Node: TSyntaxNode; Formatted: Boolean;
  const Exclude: TSyntaxNodeTypes = []; Include: TSyntaxNodeTypes = []);
  public
    class function ToXML(const Root: TSyntaxNode;
      Formatted: Boolean = False): string; static;
  end;

implementation

uses
  Generics.Collections;

{$I SimpleParser.inc}
{$IFDEF D18_NEWER}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

{ TSyntaxTreeWriter }

class procedure TSyntaxTreeWriter.NodeToXML(const Builder: TStringBuilder;
  const Node: TSyntaxNode; Formatted: Boolean; const Exclude: TSyntaxNodeTypes = []; Include: TSyntaxNodeTypes = []);
var
  InternalInclude: TSyntaxNodeTypes;

  function XMLEncode(const Data: string): string;
  var
    i, n: Integer;

    procedure Encode(const s: string);
    begin
      Move(s[1], Result[n], Length(s) * SizeOf(Char));
      Inc(n, Length(s));
    end;

  begin
    SetLength(Result, Length(Data) * 6);
    n := 1;
    for i := 1 to Length(Data) do
      case Data[i] of
        '<': Encode('&lt;');
        '>': Encode('&gt;');
        '&': Encode('&amp;');
        '"': Encode('&quot;');
        '''': Encode('&apos;');
      else
        Result[n] := Data[i];
        Inc(n);
      end;
    SetLength(Result, n - 1);
  end;

  procedure NodeToXMLInternal(const Node: TSyntaxNode; const Indent: string);
  var
    HasChildren: Boolean;
    NewIndent: string;
    Attr: TPair<TAttributeType, string>;
    ChildNode: TSyntaxNode;
    NodeOK: boolean;
  begin
    NodeOK:= (Node.Typ in InternalInclude);
    HasChildren := Node.HasChildren;
    if NodeOK then begin
      if Formatted then begin
        NewIndent:= Indent + '  ';
        Builder.Append(Indent);
      end;
      Builder.Append('<' + UpperCase(SyntaxNodeNames[Node.Typ]));

      if (Node is TCompoundSyntaxNode) then begin
        Builder.Append(' begin_line="' + IntToStr(TCompoundSyntaxNode(Node).Line) + '"');
        Builder.Append(' begin_col="' + IntToStr(TCompoundSyntaxNode(Node).Col) + '"');
        Builder.Append(' end_line="' + IntToStr(TCompoundSyntaxNode(Node).EndLine) + '"');
        Builder.Append(' end_col="' + IntToStr(TCompoundSyntaxNode(Node).EndCol) + '"');
      end else begin
        Builder.Append(' line="' + IntToStr(Node.Line) + '"');
        Builder.Append(' col="' + IntToStr(Node.Col) + '"');
      end;

      for Attr in Node.Attributes do Builder.Append(' ' + AttributeName[Attr.Key] + '="' + XMLEncode(Attr.Value) + '"');
      if HasChildren then Builder.Append('>')
      else Builder.Append('/>');
      if Formatted then Builder.AppendLine;
    end;
    for ChildNode in Node.ChildNodes do NodeToXMLInternal(ChildNode, NewIndent);
    if NodeOK then begin
      if HasChildren then begin
        if Formatted then Builder.Append(Indent);
        Builder.Append('</' + UpperCase(SyntaxNodeNames[Node.Typ]) + '>');
        if Formatted then Builder.AppendLine;
      end;
    end;
  end;

begin
  if Include = [] then InternalInclude:= [Low(TSyntaxNodeType)..High(TSyntaxNodeType)]
  else InternalInclude:= Include;
  InternalInclude:= InternalInclude - Exclude;
  NodeToXMLInternal(Node, '');
end;

class function TSyntaxTreeWriter.ToXML(const Root: TSyntaxNode;
  Formatted: Boolean): string;
var
  Builder: TStringBuilder;
begin
  Builder := TStringBuilder.Create;
  try
    NodeToXml(Builder, Root, Formatted);
    Result := '<?xml version="1.0"?>' + sLineBreak + Builder.ToString;
  finally
    Builder.Free;
  end;
end;

end.
