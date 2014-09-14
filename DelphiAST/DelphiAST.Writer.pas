unit DelphiAST.Writer;

interface

uses
  DelphiAST.Classes, Xml.XMLIntf, Generics.Collections;

type
  TSyntaxTreeWriter = class
  private
    class procedure NodeToXML(const XMLDoc: IXMLDocument; const ParentXMLNode: IXMLNode;
      Node: TSyntaxNode); static;
  public
    class function ToXML(Root: TSyntaxNode): string; static;
  end;

implementation

uses
  System.SysUtils, Xml.XMLDoc;

{ TSyntaxTreeWriter }

class procedure TSyntaxTreeWriter.NodeToXML(const XMLDoc: IXMLDocument;
  const ParentXMLNode: IXMLNode; Node: TSyntaxNode);
var
  NewXMLNode: IXMLNode;
  ChildNode: TSyntaxNode;
  Attr: TPair<string, string>;
begin
  if Assigned(ParentXMLNode) then
    NewXMLNode := ParentXMLNode.AddChild(UpperCase(Node.Name))
  else
    NewXMLNode := XMLDoc.AddChild(UpperCase(Node.Name));

  for Attr in Node.Attributes do
    NewXMLNode.Attributes[Attr.Key] := Attr.Value;

  for ChildNode in Node.ChildNodes do
    NodeToXML(XMLDoc, NewXMLNode, ChildNode);
end;

class function TSyntaxTreeWriter.ToXML(Root: TSyntaxNode): string;
var
  XMLDoc: IXMLDocument;
begin
  XMLDoc := NewXmlDocument;
  try
    NodeToXML(XMLDoc, nil, Root);
  finally
    XmlDoc.SaveToXML(Result);
  end;
end;

end.
