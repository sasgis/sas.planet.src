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
  private
    FForceZoom24: Byte;
  protected
    procedure AdjustCustomBingHiResZoom(var AActualZoom: Byte);
  public
    procedure AfterConstruction; override;

    function ContentType: String; override;

    function ParseResponse(const AResultOk: IDownloadResultOk): Integer; override;

    function GetRequest(const AInetConfig: IInetConfig): IDownloadRequest; override;
  end;

  TAvailPicsZ19Bing = class(TAvailPicsBing)
  public
    procedure AfterConstruction; override;
  end;

procedure AdjustMinimalBingHiResZoom(var AActualZoom: Byte);

implementation

uses
  ALString,
  u_GeoToStrFunc,
  xmldom,
  u_XmlLoaderByVSAGPS,
  u_StreamReadOnlyByBinaryData;

procedure AdjustMinimalBingHiResZoom(var AActualZoom: Byte);
begin
  //  do not check for small zooms
  // check decremented
  if (AActualZoom<14) then
    AActualZoom := 14;
end;

{ TAvailPicsBing }

procedure TAvailPicsBing.AdjustCustomBingHiResZoom(var AActualZoom: Byte);
begin
  if (FForceZoom24 <> 0) then begin
    // for special zoom
    AActualZoom := FForceZoom24 - 1;
  end else begin
    // default
    AdjustMinimalBingHiResZoom(AActualZoom);
  end;
end;

procedure TAvailPicsBing.AfterConstruction;
begin
  inherited;
  FForceZoom24 := 0;
  //FDefaultKey := 'AvuPoJ5DwFK0Htv75MetMjEN1QjQHiB8UIkTP0XZGHUQn-y2-r464Mjg27vyQ8Z1';
  // from Version=1145
  //FDefaultKey := 'Akw4XWHH0ngzzB_4DmHOv_XByRBtX5qwLAS9RgRYDamxvLeIxRfSzmuvWFB9RF7d';
  // from Version=1398
  FDefaultKey := 'Anqg-XzYo-sBPlzOWFHIcjC3F8s17P_O7L4RrevsHVg4fJk6g_eEmUBphtSn4ySg';
end;

function TAvailPicsBing.ContentType: String;
begin
  Result := 'application/xml';
end;

function TAvailPicsBing.ParseResponse(const AResultOk: IDownloadResultOk): Integer;
var
  VStream: TStreamReadOnlyByBinaryData;
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
  VZoom: Byte;
begin
  Result := 0;

  if (not Assigned(FTileInfoPtr.AddImageProc)) then
    Exit;

  VStream := TStreamReadOnlyByBinaryData.Create(AResultOk.Data);
  try
    if (0 = VStream.Size) then
      Exit;

    if not LoadXmlDomDocFromStream(VDOMDocument, VStream) then
      Exit;

    VResponse := VDOMDocument.lastChild;
    if (nil = VResponse) then
      Exit;
    // get StatusCode
    // get AuthenticationResultCode
    // get TraceId - no interesting info
    VResourceSets := VResponse.lastChild;
    if (nil = VResourceSets) then
      Exit;
    VResourceSet := VResourceSets.lastChild;
    if (nil = VResourceSet) then
      Exit;
    // get EstimatedTotal
    VResources := VResourceSet.lastChild;
    if (nil = VResources) then
      Exit;
    VImageryMetadata := VResources.lastChild;
    if (nil = VImageryMetadata) then
      Exit;

    VSLParams := TStringList.Create;
    try
      // init
      VVintageStart := '';
      VVintageEnd := '';

      // get params
      VParam := VImageryMetadata.firstChild;
      while Assigned(VParam) do begin
        // param node
        VNodeName := VParam.nodeName;
        if (0 < Length(VNodeName)) and (VNodeName[1] <> '#') then begin
          VNodeValue := GetXmlNodeText(VParam);
          VSLParams.Values[VNodeName] := VNodeValue;
          // save some values
          if SameText(VNodeName, 'VintageStart') then
            VVintageStart := VNodeValue;
          if SameText(VNodeName, 'VintageEnd') then
            VVintageEnd := VNodeValue;
          end;
        // next
        VParam := VParam.nextSibling;
      end;

      // check
      if (0 < Length(VVintageStart)) and (0 < Length(VVintageEnd)) then begin
        // set user date format
        VVintageStart[5] := DateSeparator;
        VVintageStart[8] := DateSeparator;
        VVintageEnd[5] := DateSeparator;
        VVintageEnd[8] := DateSeparator;

        if VVintageStart <> VVintageEnd then begin
           VVintageStart := VVintageStart + ' - '+ VVintageEnd;
        end;
        VZoom := FTileInfoPtr.Zoom;
        AdjustCustomBingHiResZoom(VZoom);

        if FTileInfoPtr.AddImageProc(
            Self,
            VVintageStart,
            'Bing (z' + inttostr(VZoom + 1) + ')',
            False, // cannot check image identifier for BING
            0,
            VSLParams
        ) then begin
          Inc(Result);
        end;
      end;
    finally
      VSLParams.Free;
    end;
  finally
    VStream.Free;
  end;
end;

function TAvailPicsBing.GetRequest(const AInetConfig: IInetConfig): IDownloadRequest;
var VZoom: Byte;
    VLink: AnsiString;
begin
 VZoom := FTileInfoPtr.Zoom;
 AdjustCustomBingHiResZoom(VZoom);
 VLink := 'http://dev.virtualearth.net/REST/V1/Imagery/Metadata/Aerial/'+
            RoundExAnsi(FTileInfoPtr.LonLat.Y, 6) + ',' + RoundExAnsi(FTileInfoPtr.LonLat.X, 6)+
            '?zl=' + ALIntToStr(VZoom) + '&o=xml&key=' + FDefaultKey;

 Result := TDownloadRequest.Create(
           VLink,
           '',
           AInetConfig.GetStatic
           );

end;

{ TAvailPicsZ19Bing }

procedure TAvailPicsZ19Bing.AfterConstruction;
begin
  inherited;
  FForceZoom24 := 19;
end;

end.
