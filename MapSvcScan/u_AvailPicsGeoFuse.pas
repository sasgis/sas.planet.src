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

unit u_AvailPicsGeoFuse;

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
  TAvailPicsGeoFuse = class(TAvailPicsAbstract)
  private
    function GetPlainJsonGeoFuseText(
      const AResultOk: IDownloadResultOk;
      const AList: TStrings
    ): Boolean;
    function GetSimpleJsonGeoFuseText(
      const AResultOk: IDownloadResultOk;
      const AList: TStrings
    ): Boolean;
    function GetUnzippedJsonGeoFuseText(
      const AResultOk: IDownloadResultOk;
      const AList: TStrings
    ): Boolean;
  public
    function ContentType: String; override;

    function ParseResponse(const AResultOk: IDownloadResultOk): Integer; override;

    function GetRequest(const AInetConfig: IInetConfig): IDownloadRequest; override;
  end;

implementation

uses
  ALZLibExGZ,
  u_StreamReadOnlyByBinaryData,
  u_InetFunc,
  u_GeoToStrFunc;

type
  TGeoFuseWorkingLineType = (gfwlt_StartOfAttributes, gfwlt_StartOfGeometry, gfwlt_Parameter, gfwlt_Value, gfwlt_MultiLine);

{ TAvailPicsGeoFuse }

function TAvailPicsGeoFuse.ContentType: String;
begin
  Result := 'text/plain'; // 'text/plain;charset=utf-8'   // 'text/html'
end;

function TAvailPicsGeoFuse.ParseResponse(const AResultOk: IDownloadResultOk): Integer;

  procedure _InitParams(var AObj: TStrings; const AClearExisting: Boolean);
  begin
    if (nil=AObj) then
      AObj := TStringList.Create
    else if AClearExisting then
      AObj.Clear;
  end;

const
  c_attributes_quoted = '"attributes"';
  c_rings_quoted = '"rings"';
  c_features = 'features';
  c_geometry = 'geometry';
  c_SRC_DATE = 'SRC_DATE';
  c_OBJECTID = 'OBJECTID';

  function PrepareWorkingLineAndGetType(var ALine: String): TGeoFuseWorkingLineType;
  var p: Integer;
  begin
    if (ALine[1]=':') then begin
      // as value
      System.Delete(ALine, 1, 1);
      Result := gfwlt_Value;
    end else begin
      // as parameter
      if SameText(ALine, c_geometry) then begin
        // is geometry
        Result := gfwlt_StartOfGeometry;
        Exit;
      end;
      Result := gfwlt_Parameter;
    end;

    // check start of attributes
    // [{"attributes":{"OBJECTID":6792959
    // {"attributes":{"OBJECTID":6348909
    p := System.Pos(c_attributes_quoted, ALine);
    if (p>0) then begin
      // check ':'
      if (System.Pos(':', ALine) > p) then begin
        Result := gfwlt_StartOfAttributes;
        System.Delete(ALine, 1, p + Length(c_attributes_quoted));
      end
    end;

    // check if quoted multilines for value
    if (gfwlt_Value=Result) then
    if (0<Length(ALine)) and (ALine[1]='"') then begin
      System.Delete(ALine, 1, 1);
      if (0=Length(ALine)) then begin
        // unclosed quote (empty line)
        Result := gfwlt_MultiLine;
      end else if (ALine[Length(ALine)]='"') then begin
        // simple value
        SetLength(ALine, Length(ALine) - 1);
      end else begin
        // unclosed quote (with text)
        Result := gfwlt_MultiLine;
      end
    end;
  end;

  function _ParseGeometryLineAndBreak(const AGeoLineSrc: String; var AFullGeoLine: String): Boolean;
  var
    VPos: Integer;
    VTrimmed, VPrefix: String;
  begin
    Result := FALSE;
    if (0=Length(AGeoLineSrc)) then
      Exit;
    VPrefix := ' ';
    VPos := System.Pos(c_rings_quoted, AGeoLineSrc);
    if (VPos>0) then begin
      // line as ':{"rings":[[[58.051913790000071'
      // get last number as first part of coordinates
      VTrimmed := System.Copy(AGeoLineSrc, VPos + Length(c_rings_quoted), Length(AGeoLineSrc));
      while (0<Length(VTrimmed)) and (VTrimmed[1] in ['[',':','{',' ']) do begin
        System.Delete(VTrimmed, 1, 1);
      end;
    end else if (AGeoLineSrc[1]='[') then begin
      // first part of coordinates
      // [57.860857047000081
      VTrimmed := AGeoLineSrc;
      while (0<Length(VTrimmed)) and (VTrimmed[1] in ['[',':','{',' ']) do begin
        System.Delete(VTrimmed, 1, 1);
      end;
    end else begin
      // second part of coordinates
      // 60.852726251000036]
      // 60.852726251000036]]]}}
      VPrefix := ',';
      VPos := System.Pos(']]]', AGeoLineSrc);
      if (VPos>0) then begin
        // very last line of geometry
        VTrimmed := System.Copy(AGeoLineSrc, 1, (VPos - 1));
        // cleanup start of geometry
        while (0<Length(AFullGeoLine)) and (AFullGeoLine[1] in ['[',':','{',' ']) do begin
          System.Delete(AFullGeoLine, 1, 1);
        end;
        Result := TRUE;
      end else begin
        VTrimmed := AGeoLineSrc;
      end;
      while (0<Length(VTrimmed)) and (VTrimmed[Length(VTrimmed)] in [']',':','}',' ']) do begin
        SetLength(VTrimmed, Length(VTrimmed) - 1);
      end;
    end;

    AFullGeoLine := AFullGeoLine + VPrefix + VTrimmed;
  end;

  procedure _AddAttributesLine(const AParams: TStrings; const ALine: String);
  begin
    // {"attributes":{"OBJECTID":6348909
    // TODO: extract OBJECTID=6348909 and add to list
  end;

  function _InternalAddItem(
    var AParams: TStrings;
    var AExternalResultCount: Integer): Boolean;
  var
    VDate, VID, VColl: String;
    VItemExisting: Boolean;
    VItemFetched: TDateTime;
  begin
    // add item
    VID := AParams.Values['IMAGE_ID'];
    VColl := AParams.Values['COLLECTION_VEHICLE_LONG'];
    VItemExisting := ItemExists(FBaseStorageName + '_' + VColl, VID, @VItemFetched);
    VDate := System.Copy(VID, 1, 4) + DateSeparator + System.Copy(VID, 5, 2) + DateSeparator + System.Copy(VID, 7, 2);
    // add date to params
    AParams.Values['Date'] := VDate;
    Result := FTileInfoPtr.AddImageProc(
      Self,
      VDate,
      VColl,
      VItemExisting,
      VItemFetched,
      AParams
    );
    FreeAndNil(AParams);
    // inc count
    if Result then begin
      Inc(AExternalResultCount);
    end;
  end;

var
  VIndex: Integer;
  VLineType: TGeoFuseWorkingLineType;
  VJSonParameter: String;
  VList: TStringList;
  VLine: String;
  VParams: TStrings;
  VHasFeatures: Boolean;
begin
  Result := 0;

  if (not Assigned(FTileInfoPtr.AddImageProc)) then
    Exit;

  VHasFeatures := FALSE;

  VParams := nil;
  VList := TStringList.Create;
  try
    // try to get plain text (unzip if gzipped)
    if not GetPlainJsonGeoFuseText(AResultOk, VList) then
      Exit;

    // full JSON parser for GeoFuse.GeoEye
    VIndex := 0;
    VJSonParameter := '';
    while VIndex<VList.Count do begin
      VLine :=Trim(VList[VIndex]);
      if (not VHasFeatures) and SameText(VLine, c_features) then begin
        // starting features
        VHasFeatures := TRUE;
      end else if VHasFeatures and (0<Length(VLine)) then begin
        // working line
        VLineType := PrepareWorkingLineAndGetType(VLine);
        case VLineType of
          gfwlt_StartOfAttributes: begin
            // first attributes or new attributes
            if Assigned(VParams) then begin
              // add item
              _InternalAddItem(VParams, Result);
            end;
            _InitParams(VParams, TRUE);
            _AddAttributesLine(VParams, VLine);
          end;
          gfwlt_StartOfGeometry: begin
            // geometry - parse several lines
            // VLine - current line
            // VJSonParameter - full geometry parsed text
            VJSonParameter := '';
            repeat
              Inc(VIndex);
              if (VIndex>=VList.Count) then begin
                VJSonParameter := '';
                break;
              end;
              // get geometry line
              VLine := Trim(Lowercase(VList[VIndex]));
              if _ParseGeometryLineAndBreak(VLine, VJSonParameter) then
                break;
            until FALSE;
            // check if geometry has value
            if (0<Length(VJSonParameter)) then begin
              // add pair
              _InitParams(VParams, FALSE);
              VParams.Values[c_geometry] := VJSonParameter;
            end;
          end;
          gfwlt_Parameter: begin
            // parameter name
            VJSonParameter := VLine;
          end;
          gfwlt_Value: begin
            // simple value
            while (0<Length(VLine)) and (VLine[Length(VLine)] in [']','}']) do begin
              SetLength(VLine, Length(VLine) - 1);
            end;
            if (0<Length(VLine)) and (0<Length(VJSonParameter)) then begin
              _InitParams(VParams, FALSE);
              VParams.Values[VJSonParameter] := VLine;
            end;
          end;
          gfwlt_MultiLine: begin
            // multiline value
            repeat
              // add next line until quote will be closed
              Inc(VIndex);
              if (VIndex>=VList.Count) then begin
                VLine := '';
                break;
              end;
              VLine := VLine + ' ' + Trim(VList[VIndex]);
              if VLine[Length(VLine)]='"' then begin
                SetLength(VLine, Length(VLine) - 1);
                break;
              end;
            until FALSE;
            // check if has value
            if (0<Length(VLine)) then begin
              // add pair
              _InitParams(VParams, FALSE);
              VParams.Values[VJSonParameter] := VLine;
            end;
          end
        end;
      end;
      // goto next line
      Inc(VIndex);
    end;

    if Assigned(VParams) then begin
      // add item
      _InternalAddItem(VParams, Result);
    end;
  finally
    FreeAndNil(VList);
    FreeAndNil(VParams);
  end;
end;

function TAvailPicsGeoFuse.GetPlainJsonGeoFuseText(
  const AResultOk: IDownloadResultOk;
  const AList: TStrings
): Boolean;
begin
  Result := FALSE;

  if (0=AResultOk.Data.Size) or (nil=AResultOk.Data.Buffer) then
    Exit;

  if IsGZipped(AResultOk.RawResponseHeader) then begin
    // gzipped
    try
      // try to unzip
      Result := GetUnzippedJsonGeoFuseText(AResultOk, AList);
    except
      // try as plain text
      Result := GetSimpleJsonGeoFuseText(AResultOk, AList);
    end;
  end else begin
    // plain
    Result := GetSimpleJsonGeoFuseText(AResultOk, AList);
  end;
end;

function TAvailPicsGeoFuse.GetRequest(const AInetConfig: IInetConfig): IDownloadRequest;
var
  VLink: String;
  VHeader: AnsiString;
begin
  VHeader :='User-Agent: Mozilla/5.0 (Windows NT 6.0; rv:14.0) Gecko/20100101 Firefox/14.0.1'+#$D#$A+
    'Host: geofuse.geoeye.com'+#$D#$A+
    'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8'+#$D#$A+
    'Accept-Language: ru-ru,ru;q=0.8,en-us;q=0.5,en;q=0.3'+#$D#$A+
    'Accept-Encoding: gzip, deflate'+#$D#$A+
    //'Accept-Encoding: deflate'+#$D#$A+
    'DNT: 1'+#$D#$A+
    'Connection: keep-alive'+#$D#$A+
    'Referer: http://geofuse.geoeye.com/maps/Map.aspx';

 VLink := 'http://geofuse.geoeye.com/ArcGIS/rest/services/GeoEyeCatalogFeatures/MapServer/exts/CatalogServer//query?'+
           'geometryType=esriGeometryEnvelope&geometry='+
           // 26,40,74,80 = lon_min,lat_min,lon_max,lat_max
           RoundEx(FTileInfoPtr.TileRect.Left, 6) + '%2C'+
           RoundEx(FTileInfoPtr.TileRect.Bottom, 6) + '%2C'+
           RoundEx(FTileInfoPtr.TileRect.Right, 6) + '%2C'+
           RoundEx(FTileInfoPtr.TileRect.Top, 6) +
           '&inSR=4326&outSR=4326' +
           '&spatialRel=esriSpatialRelEnvelopeIntersects&returnGeometry=true' +
           '&where=COLLECTION_ANGLE_ELEV%20BETWEEN%200%20AND%2090&outFields=*' +
           '&pageStart=1&pageSize=200&spatialRank=false&sort=true&f=json'; // json // kmz // html

 Result := TDownloadRequest.Create(
           AnsiString(VLink),
           VHeader,
           AInetConfig.GetStatic
           );

end;

function TAvailPicsGeoFuse.GetSimpleJsonGeoFuseText(
  const AResultOk: IDownloadResultOk;
  const AList: TStrings
): Boolean;
var
  VSimpleText: String;
begin
  SetString(VSimpleText, PChar(AResultOk.Data.Buffer), (AResultOk.Data.Size div SizeOf(Char)));
  AList.Clear;
  AList.QuoteChar := '"';
  AList.Delimiter := ',';
  AList.NameValueSeparator := ':';
  AList.DelimitedText := VSimpleText;
  Result := (AList.Count>1);
end;

function TAvailPicsGeoFuse.GetUnzippedJsonGeoFuseText(
  const AResultOk: IDownloadResultOk;
  const AList: TStrings
): Boolean;
var
  VZipped: TStreamReadOnlyByBinaryData;
  VUnzipped: TMemoryStream;
  VStrValue: String;
begin
  VUnzipped := nil;
  VZipped := TStreamReadOnlyByBinaryData.Create(AResultOk.Data);
  try
    VUnzipped := TMemoryStream.Create;

    // unzip
    GZDecompressStream(VZipped, VUnzipped);

    // failed to unzip - try to use as plain text
    if (VUnzipped.Memory=nil) or (VUnzipped.Size=0) then
      Abort;

    // unzipped
    SetString(VStrValue, PChar(VUnzipped.Memory), (VUnzipped.Size div SizeOf(Char)));

    // apply
    AList.Clear;
    AList.QuoteChar := '"';
    AList.Delimiter := ',';
    AList.NameValueSeparator := ':';
    AList.DelimitedText := VStrValue;
    Result := (AList.Count>1);
  finally
    VZipped.Free;
    VUnzipped.Free;
  end;
end;

end.
