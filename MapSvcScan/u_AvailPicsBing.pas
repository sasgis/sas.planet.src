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
  SysUtils,
  Classes,
  i_InetConfig,
  i_DownloadResult,
  i_DownloadRequest,
  u_DownloadRequest,
  u_AvailPicsAbstract;

type
  TAvailPicsBing = class(TAvailPicsByKey)
  public
    procedure AfterConstruction; override;

    function ContentType: String; override;

    function ParseResponse(const AResultOk: IDownloadResultOk): Integer; override;

    function GetRequest(const AInetConfig: IInetConfig): IDownloadRequest; override;
  end;

procedure AdjustMinimalBingHiResZoom(var VActualZoom: Byte);

implementation

uses
  u_GeoToStr,
  xmldom,
  windows,
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

function TAvailPicsBing.ParseResponse(const AResultOk: IDownloadResultOk): Integer;
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
  VMemoryStream: TMemoryStream;
  VZoom: Byte;
begin
  VMemoryStream := TMemoryStream.Create;
  VMemoryStream.Position:=0;
  VMemoryStream.SetSize(AResultOk.Data.Size);
  CopyMemory(VMemoryStream.Memory, AResultOk.Data.Buffer, AResultOk.Data.Size);
  Result:=0;

  if (not Assigned(FTileInfoPtr.AddImageProc)) then
    Exit;

  if (nil=VMemoryStream) or (0=VMemoryStream.Size) then
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
    if VSAGPS_Load_DOMDocument_FromStream(VDOMDocument, VMemoryStream, FALSE {no exception if false}) then
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

        if VVintageStart <> VVintageEnd then
           VVintageStart := VVintageStart + ' - '+ VVintageEnd;
        VZoom := FTileInfoPtr.Zoom;
        AdjustMinimalBingHiResZoom(VZoom);

        if FTileInfoPtr.AddImageProc(
            Self,
            VVintageStart,
            'Bing (z'+inttostr(VZoom+1)+')',
            FALSE, // cannot check image identidier for BING
            0,
            VSLParams
        ) then
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

function TAvailPicsBing.GetRequest(const AInetConfig: IInetConfig): IDownloadRequest;
var VZoom: Byte;
    VLink: string;
begin
 VZoom := FTileInfoPtr.Zoom;
 AdjustMinimalBingHiResZoom(VZoom);
 VLink := 'http://dev.virtualearth.net/REST/V1/Imagery/Metadata/Aerial/'+
            RoundEx(FTileInfoPtr.LonLat.Y, 6)+','+RoundEx(FTileInfoPtr.LonLat.X, 6)+
            '?zl='+IntToStr(VZoom)+'&o=xml&key='+FDefaultKey;

 Result := TDownloadRequest.Create(
           VLink,
           '',
           AInetConfig.GetStatic
           );

end;

end.
