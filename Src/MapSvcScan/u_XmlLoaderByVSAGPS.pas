{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

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
  const AName: string
): string;

function GetXmlNodeText(
  const ANode: IDOMNode
): string;

function GetXmlFirstSubNodeText(
  const ANode: IDOMNode;
  const ASubNodeName: string
): string;

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
  const AName: string
): string;
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
): string;
begin
  Result := VSAGPS_XML_DOMNodeValue(ANode);
end;

function GetXmlFirstSubNodeText(
  const ANode: IDOMNode;
  const ASubNodeName: string
): string;
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