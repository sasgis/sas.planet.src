{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
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

procedure AdjustMinimalBingHiResZoom(var VActualZoom: Byte);

implementation

uses
  u_GeoToStr,
  xmldom,
  vsagps_public_xml_dom,
  vsagps_public_xml_parser;

procedure AdjustMinimalBingHiResZoom(var VActualZoom: Byte);
begin
  //  do not check for small zooms
  // check decremented
  if (VActualZoom<14) then
    VActualZoom:=14;
end;
  
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
  AdjustMinimalBingHiResZoom(VZoom);

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
        if FTileInfoPtr.AddImageProc(Self, VVintageStart+' - '+VVintageEnd, 'Bing', VSLParams) then
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
end;

end.
