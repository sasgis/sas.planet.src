{******************************************************************************}
{* SAS.Planet (SAS.œÎ‡ÌÂÚ‡)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_AvailPicsBing;

interface

uses
  Windows,
  SysUtils,
  Classes,
  u_AvailPicsAbstract;

type
  TAvailPicsBing = class(TAvailPicsByKey)
  public
    procedure AfterConstruction; override;

    function ContentType: String; override;

    function ParseResponse(const AStream: TMemoryStream): Integer; override;

    function LinkToImages: String; override;
  end;

implementation

uses
  u_GeoToStr,
  xmldom,
  vsagps_public_xml_dom,
  vsagps_public_xml_parser;
  
(*
function _NodeVal(const ANode: IDOMNode): String;
var VChld: IDOMNode;
begin
  if Assigned(ANode) then begin
    VChld := ANode.firstChild;
    if Assigned(VChld) then
      Result := VChld.nodeValue
    else
      Result := ANode.nodeValue;
  end else
    Result := '';
end;
*)  

{ TAvailPicsBing }

procedure TAvailPicsBing.AfterConstruction;
begin
  inherited;
  FDefaultKey := 'AvuPoJ5DwFK0Htv75MetMjEN1QjQHiB8UIkTP0XZGHUQn-y2-r464Mjg27vyQ8Z1';
end;

function TAvailPicsBing.ContentType: String;
begin
  Result := 'application/xml';
end;

function TAvailPicsBing.LinkToImages: String;
var VZoom: Byte;
begin
  VZoom := FTileInfoPtr.Zoom;
  AdjustMinimalHiResZoom(VZoom);

  // use decremented zoom here!
    
  // http://dev.virtualearth.net/REST/V1/Imagery/Metadata/Aerial/63.011796,58.619231?zl=14&o=xml&key=AvuPoJ5DwFK0Htv75MetMjEN1QjQHiB8UIkTP0XZGHUQn-y2-r464Mjg27vyQ8Z1
  Result := 'http://dev.virtualearth.net/REST/V1/Imagery/Metadata/Aerial/'+
            RoundEx(FTileInfoPtr.LonLat.Y, 6)+','+RoundEx(FTileInfoPtr.LonLat.X, 6)+
            '?zl='+IntToStr(VZoom)+'&o=xml&key='+FDefaultKey;
end;

function TAvailPicsBing.ParseResponse(const AStream: TMemoryStream): Integer;
var
  VDOMDocument: IDOMDocument;
  VResponse: IDOMNode;
  VResourceSets: IDOMNode;
  VResourceSet: IDOMNode;
  VResources: IDOMNode;
  VImageryMetadata: IDOMNode;
  VParam: IDOMNode;
  VNodeName, VNodeValue: String;
  VVintageStart, VVintageEnd: String;
  VSLParams: TStrings;
begin
  Result:=0;

  if (not Assigned(FTileInfoPtr.AddImageProc)) then
    Exit;

  if (nil=AStream) or (0=AStream.Size) then
    Exit;

  VSLParams:=nil;
  VDOMDocument:=nil;
  VResponse:=nil;
  VResourceSets:=nil;
  VResourceSet:=nil;
  VResources:=nil;
  VImageryMetadata:=nil;
  VParam:=nil;
  try
    // create xml
    if VSAGPS_Create_DOMDocument(VDOMDocument, FALSE {no exception if false}, 1 {no validation}) then
    if VSAGPS_Load_DOMDocument_FromStream(VDOMDocument, AStream, FALSE {no exception if false}) then
    try
      VResponse := VDOMDocument.lastChild;
      // get StatusCode
      // get AuthenticationResultCode
      // get TraceId - no interesting info
      VResourceSets := VResponse.lastChild;
      VResourceSet := VResourceSets.lastChild;
      // get EstimatedTotal
      VResources := VResourceSet.lastChild;
      VImageryMetadata := VResources.lastChild;

      VVintageStart:='';
      VVintageEnd:='';
      FreeAndNil(VSLParams);
      VSLParams:=TStringList.Create;

      // get params
      VParam:=VImageryMetadata.firstChild;
      while Assigned(VParam) do begin
        // param node
        VNodeName:=VParam.nodeName;
        VNodeValue:=VSAGPS_XML_DOMNodeValue(VParam);
        VSLParams.Values[VNodeName]:=VNodeValue;

        // save some values
        if SameText(VNodeName,'VintageStart') then
          VVintageStart:=VNodeValue;
        if SameText(VNodeName,'VintageEnd') then
          VVintageEnd:=VNodeValue;

        // next
        VParam:=VParam.nextSibling;
      end;

      // check
      if (0<Length(VVintageStart)) and (0<Length(VVintageEnd)) then begin
        // set user date format
        VVintageStart[5]:=DateSeparator;
        VVintageStart[8]:=DateSeparator;
        VVintageEnd[5]:=DateSeparator;
        VVintageEnd[8]:=DateSeparator;

        // add
        if FTileInfoPtr.AddImageProc(Self, VVintageStart+' - '+VVintageEnd, 'Tile', VSLParams) then
          Inc(Result);
      end;

      FreeAndNil(VSLParams);
    except
      if (nil<>VSLParams) then begin
        try
          VSLParams.Free;
        except
        end;
        VSLParams:=nil;
      end;
    end;
  finally
    VParam:=nil;
    VImageryMetadata:=nil;
    VResources:=nil;
    VResourceSet:=nil;
    VResourceSets:=nil;
    VResponse:=nil;
    VDOMDocument:=nil;
  end;


(*
hi-res (3 images in single tile)
  <?xml version="1.0" encoding="utf-8" ?> 
- <Response xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns="http://schemas.microsoft.com/search/local/ws/rest/v1">
  <Copyright>Copyright © 2012 Microsoft and its suppliers. All rights reserved. This API cannot be accessed and the content and any results may not be used, reproduced or transmitted in any manner without express written permission from Microsoft Corporation.</Copyright> 
  <BrandLogoUri>http://dev.virtualearth.net/Branding/logo_powered_by.png</BrandLogoUri> 
  <StatusCode>200</StatusCode> 
  <StatusDescription>OK</StatusDescription> 
  <AuthenticationResultCode>ValidCredentials</AuthenticationResultCode> 
  <TraceId>3c1c320b8c8c4433a068283787096a50|AMSM001402|02.00.83.500|</TraceId> 
- <ResourceSets>
- <ResourceSet>
  <EstimatedTotal>1</EstimatedTotal> 
- <Resources>
- <ImageryMetadata>

  <ImageUrl>http://ecn.t1.tiles.virtualearth.net/tiles/a12101203321231.jpeg?g=863</ImageUrl>
  <ImageWidth>256</ImageWidth>
  <ImageHeight>256</ImageHeight>
  <ZoomMin>14</ZoomMin>
  <ZoomMax>14</ZoomMax>
  <VintageStart>2010-05-13</VintageStart>
  <VintageEnd>2011-05-25</VintageEnd>

  </ImageryMetadata>
  </Resources>
  </ResourceSet>
  </ResourceSets>
  </Response>

landsat
  <?xml version="1.0" encoding="utf-8" ?> 
- <Response xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns="http://schemas.microsoft.com/search/local/ws/rest/v1">
  <Copyright>Copyright © 2012 Microsoft and its suppliers. All rights reserved. This API cannot be accessed and the content and any results may not be used, reproduced or transmitted in any manner without express written permission from Microsoft Corporation.</Copyright> 
  <BrandLogoUri>http://dev.virtualearth.net/Branding/logo_powered_by.png</BrandLogoUri> 
  <StatusCode>200</StatusCode>
  <StatusDescription>OK</StatusDescription> 
  <AuthenticationResultCode>ValidCredentials</AuthenticationResultCode> 
  <TraceId>083ffbe19bf142aab3e2bed99eee5d15|AMSM001104|02.00.83.500|</TraceId>

- <ResourceSets>
- <ResourceSet>
  <EstimatedTotal>1</EstimatedTotal>
- <Resources>

- <ImageryMetadata>
  <ImageUrl>http://ecn.t0.tiles.virtualearth.net/tiles/a1210300200300.jpeg?g=863</ImageUrl>
  <ImageWidth>256</ImageWidth>
  <ImageHeight>256</ImageHeight>
  <ZoomMin>13</ZoomMin>
  <ZoomMax>13</ZoomMax>
  <VintageStart>2001-03-01</VintageStart>
  <VintageEnd>2001-03-01</VintageEnd>
  </ImageryMetadata>

  </Resources>
  </ResourceSet>
  </ResourceSets>

  </Response>


deeper
  <?xml version="1.0" encoding="utf-8" ?> 
- <Response xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns="http://schemas.microsoft.com/search/local/ws/rest/v1">
  <Copyright>Copyright © 2012 Microsoft and its suppliers. All rights reserved. This API cannot be accessed and the content and any results may not be used, reproduced or transmitted in any manner without express written permission from Microsoft Corporation.</Copyright> 
  <BrandLogoUri>http://dev.virtualearth.net/Branding/logo_powered_by.png</BrandLogoUri> 
  <StatusCode>200</StatusCode> 
  <StatusDescription>OK</StatusDescription> 
  <AuthenticationResultCode>ValidCredentials</AuthenticationResultCode> 
  <TraceId>d7743ea653894672853c26a464f6a3b5|AMSM001104|02.00.83.500|</TraceId> 
- <ResourceSets>
- <ResourceSet>
  <EstimatedTotal>1</EstimatedTotal> 
- <Resources>
- <ImageryMetadata>
  <ImageUrl>http://ecn.t2.tiles.virtualearth.net/tiles/a121030020030022.jpeg?g=863</ImageUrl> 
  <ImageWidth>256</ImageWidth> 
  <ImageHeight>256</ImageHeight> 
  <ZoomMin>15</ZoomMin> 
  <ZoomMax>15</ZoomMax> 
  <VintageStart>2011-06-05</VintageStart> 
  <VintageEnd>2011-06-05</VintageEnd> 
  </ImageryMetadata>
  </Resources>
  </ResourceSet>
  </ResourceSets>
  </Response>

*)
end;

end.
