unit u_XmlLoaderByVSAGPS;

interface

uses
  Classes,
  xmldom;

function LoadXmlDomDocFromStream(
  out ADOMDocument: IDOMDocument;
  const AStream: TStream
): Boolean;

function GetXmlAttribute(
  const ANode: IDOMNode;
  const AName: WideString
): WideString;

function GetXmlNodeText(
  const ANode: IDOMNode
): WideString;

function GetXmlFirstSubNodeText(
  const ANode: IDOMNode;
  const ASubNodeName: WideString
): WideString;

implementation

uses
  vsagps_public_xml_dom,
  vsagps_public_xml_parser;

function LoadXmlDomDocFromStream(
  out ADOMDocument: IDOMDocument;
  const AStream: TStream
): Boolean;
begin
  Result := VSAGPS_Create_DOMDocument(
    ADOMDocument,
    False,
    1
  );

  if Result then begin
    Result := VSAGPS_Load_DOMDocument_FromStream(
      ADOMDocument,
      AStream,
      False
    );
  end;
end;

function GetXmlAttribute(
  const ANode: IDOMNode;
  const AName: WideString
): WideString;
var
  VNode: IDOMNode;
begin
  VNode := ANode.attributes.getNamedItem(AName);
  if (VNode <> nil) then begin
    // found
    Result := VSAGPS_XML_DOMNodeValue(VNode);
  end else begin
    // not found
    Result := '';
  end;
end;

function GetXmlNodeText(
  const ANode: IDOMNode
): WideString;
begin
  Result := VSAGPS_XML_DOMNodeValue(ANode);
end;

function GetXmlFirstSubNodeText(
  const ANode: IDOMNode;
  const ASubNodeName: WideString
): WideString;
var
  VNode: IDOMNode;
begin
  VNode := ANode.firstChild;
  while Assigned(VNode) do begin
    // check name
    if (VNode.nodeName = ASubNodeName) then begin
      Result := GetXmlNodeText(VNode);
      Exit;
    end;
    // next
    VNode := VNode.nextSibling;
  end;
  Result := '';
end;

end.