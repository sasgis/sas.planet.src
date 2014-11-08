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

unit u_AvailPicsESRI;

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
  TAvailPicsESRI = class(TAvailPicsAbstract)
  public
    function ContentType: String; override;

    function ParseResponse(const AResultOk: IDownloadResultOk): Integer; override;

    function GetRequest(const AInetConfig: IInetConfig): IDownloadRequest; override;
  end;

implementation

uses
  u_StreamReadOnlyByBinaryData,
  u_GeoToStrFunc;

procedure _InitParams(var AObj: TStrings);
begin
  if (nil = AObj) then
    AObj := TStringList.Create
  else
    AObj.Clear;
end;

function _StartingWithKey(const AOriginalLine, AKeyToCheck: String): Boolean;
var VPos: Integer;
begin
  VPos := System.Pos(AKeyToCheck, AOriginalLine);
  Result := (2 = VPos) and ('"' = AOriginalLine[1]);
end;

procedure _ParseJSONKey(var AOutValue: String);
begin
  if (Length(AOutValue) > 0) and ('"' = AOutValue[Length(AOutValue)]) then
    SetLength(AOutValue, Length(AOutValue) - 1);
  if (Length(AOutValue) > 0) and ('"' = AOutValue[1]) then
    System.Delete(AOutValue, 1, 1);
end;

procedure _ParseJSONLine(const AOriginalLine: String; out AOutKey, AOutValue: String);
var VPos: Integer;
begin
  VPos := System.Pos(':', AOriginalLine);
  if (VPos > 0) then begin
    // "SRC_ACC" : 25.399999999999999,
    AOutKey := Trim(System.Copy(AOriginalLine, 1, VPos - 1));
    AOutValue := Trim(System.Copy(AOriginalLine, (VPos + 1), Length(AOriginalLine)));
    // remove last comma
    if (Length(AOutValue) > 0) and (',' = AOutValue[Length(AOutValue)]) then
      SetLength(AOutValue, Length(AOutValue) - 1);
    // remove both "
    _ParseJSONKey(AOutKey);
    _ParseJSONKey(AOutValue);
  end else begin
    // nothing
    AOutKey := '';
    AOutValue := '';
  end;
end;

function VDateToDate(const AOrigDate: String): String;
begin
  Result := AOrigDate;
  if (8 <= Length(Result)) then begin
    System.Insert(DateSeparator, Result, 5);
    System.Insert(DateSeparator, Result, 8);
  end;
end;

{ TAvailPicsESRI }

function TAvailPicsESRI.ContentType: String;
begin
  Result := 'text/plain'; // 'text/plain;charset=utf-8'   // 'text/html'
end;

function TAvailPicsESRI.ParseResponse(const AResultOk: IDownloadResultOk): Integer;
const
  c_geometry = 'geometry';
  c_features = 'features';
  c_attributes = 'attributes';
  c_SRC_DATE = 'SRC_DATE';
  c_OBJECTID = 'OBJECTID';

var
  VStream: TStreamReadOnlyByBinaryData;
  i: Integer;
  VList: TStringList;
  VLine: String;
  VDate, VId: String;
  VGeometry: String;
  VParams: TStrings;
  VHasFeatures, VInAttributes, VInGeometry: Boolean;
  VAddResult: Boolean;
  VKey, VValue: String;
  VItemExists: Boolean;
  VItemFetched: TDateTime;
begin
  Result := 0;

  if (not Assigned(FTileInfoPtr.AddImageProc)) then
    Exit;

  VStream := TStreamReadOnlyByBinaryData.Create(AResultOk.Data);
  try
    if (0 = VStream.Size) then
      Exit;

    VHasFeatures := False;
    VInAttributes := False;
    VInGeometry := False;
    VGeometry := '';
    VParams := nil;
    VList := TStringList.Create;
    try
      VList.LoadFromStream(VStream);

      // very simple JSON parser for ESRI
      // TODO: make JSON parser
      if (0 < VList.Count) then
      for i := 0 to VList.Count - 1 do
      try
        VLine := Trim(VList[i]);
        // iteration
        if VHasFeatures then begin
          // list of features
          if VInAttributes and ('}' = VLine) then begin
            // end of attributes
            VInAttributes := FALSE;
            // check
            VItemExists := ItemExists(FBaseStorageName, VId + '_' + VDate, @VItemFetched);
            // Добавляем строку geometry
            VParams.Values[c_geometry] := VGeometry;
            //Добавляем имя провайдера
            VParams.Values['ProviderName'] := 'ESRI';
            // add item
            VAddResult := FTileInfoPtr.AddImageProc(
              Self,
              VDateToDate(VDate),
              'ESRI'+' '+VId,
              VItemExists,
              VItemFetched,
              VParams
            );
            FreeAndNil(VParams);
            // inc count
            if VAddResult then begin
              Inc(Result);
            end;
            // обнуляем геометрию
            VInGeometry := False;
            VGeometry := '';
          end else if (not VInAttributes) and _StartingWithKey(VLine, c_attributes) then begin
            // begin of attributes
            VInAttributes := TRUE;
            _InitParams(VParams);
            VDate := '';
            VId := '';
          end else if VInAttributes then begin
            // make json geometry string
            if VInGeometry then begin
              VGeometry := VGeometry + VLine;
            end else if _StartingWithKey(VLine, c_geometry) then begin
              VInGeometry := True;
            end else begin
              // parse attribute key and value
              _ParseJSONLine(VLine, VKey, VValue);
              // fill special values
              if SameText(VKey, c_SRC_DATE) then
                VDate := VValue
              else if SameText(VKey, c_OBJECTID) then
                VId := VValue;
              // add to list
              VParams.Values[VKey] := VValue;
            end;
          end;
          // end of parser of features
        end else begin
          // starting lines - check for features
          if _StartingWithKey(VLine, c_features) then begin
            // features
            VHasFeatures := TRUE;
          end;
        end;
        // end of iteration
      except
        _InitParams(VParams);
      end;
    finally
      FreeAndNil(VList);
      FreeAndNil(VParams);
    end;
  finally
    VStream.Free;
  end;

(*

JSON

{
  "displayFieldName" : "NICE_NAME",
  "fieldAliases" : {
    "OBJECTID" : "OBJECTID",
    "SRC_DATE" : "DATE (YYYYMMDD)",
    "SRC_RES" : "RESOLUTION (M)",
    "SRC_ACC" : "ACCURACY (M)",
    "SRC_DESC" : "DESCRIPTION",
    "NICE_NAME" : "SOURCE_INFO",
    "NICE_DESC" : "SOURCE"
  },
  "fields" : [
    {
      "name" : "OBJECTID",
      "type" : "esriFieldTypeOID",
      "alias" : "OBJECTID"
    },
    {
      "name" : "SRC_DATE",
      "type" : "esriFieldTypeInteger",
      "alias" : "DATE (YYYYMMDD)"
    },
    {
      "name" : "SRC_RES",
      "type" : "esriFieldTypeDouble",
      "alias" : "RESOLUTION (M)"
    },
    {
      "name" : "SRC_ACC",
      "type" : "esriFieldTypeDouble",
      "alias" : "ACCURACY (M)"
    },
    {
      "name" : "SRC_DESC",
      "type" : "esriFieldTypeString",
      "alias" : "DESCRIPTION",
      "length" : 25
    },
    {
      "name" : "NICE_NAME",
      "type" : "esriFieldTypeString",
      "alias" : "SOURCE_INFO",
      "length" : 33
    },
    {
      "name" : "NICE_DESC",
      "type" : "esriFieldTypeString",
      "alias" : "SOURCE",
      "length" : 12
    }
  ],
  "features" : [
    {
      "attributes" : {
        "OBJECTID" : 29864,
        "SRC_DATE" : 20101025,
        "SRC_RES" : 1,
        "SRC_ACC" : 25.399999999999999,
        "SRC_DESC" : "Ikonos",
        "NICE_NAME" : "Northern Europe",
        "NICE_DESC" : "GeoEye"
      }
    },
    {
      "attributes" : {
        "OBJECTID" : 29871,
        "SRC_DATE" : 20101025,
        "SRC_RES" : 1,
        "SRC_ACC" : 25.399999999999999,
        "SRC_DESC" : "Ikonos",
        "NICE_NAME" : "Northern Europe",
        "NICE_DESC" : "GeoEye"
      }
    },
    {
      "attributes" : {
        "OBJECTID" : 29878,
        "SRC_DATE" : 20101025,
        "SRC_RES" : 1,
        "SRC_ACC" : 25.399999999999999,
        "SRC_DESC" : "Ikonos",
        "NICE_NAME" : "Northern Europe",
        "NICE_DESC" : "GeoEye"
      }
    },
    {
      "attributes" : {
        "OBJECTID" : 29879,
        "SRC_DATE" : 20101020,
        "SRC_RES" : 1,
        "SRC_ACC" : 25.399999999999999,
        "SRC_DESC" : "Ikonos",
        "NICE_NAME" : "Northern Europe",
        "NICE_DESC" : "GeoEye"
      }
    },
    {
      "attributes" : {
        "OBJECTID" : 29916,
        "SRC_DATE" : 20101020,
        "SRC_RES" : 1,
        "SRC_ACC" : 25.399999999999999,
        "SRC_DESC" : "Ikonos",
        "NICE_NAME" : "Northern Europe",
        "NICE_DESC" : "GeoEye"
      }
    }
  ]
}

*)
end;

function TAvailPicsESRI.GetRequest(const AInetConfig: IInetConfig): IDownloadRequest;
var
  VLink: string;
begin
 VLink := 'http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/'+
           '0'+
           '/query?text=&geometry='+
           // 26,40,74,80 = lon_min,lat_min,lon_max,lat_max
           RoundEx(FTileInfoPtr.TileRect.Left, 6) + '%2C'+
           RoundEx(FTileInfoPtr.TileRect.Bottom, 6) + '%2C'+
           RoundEx(FTileInfoPtr.TileRect.Right, 6) + '%2C'+
           RoundEx(FTileInfoPtr.TileRect.Top, 6) +
           '&geometryType=esriGeometryEnvelope&inSR=4326'+
           '&spatialRel=esriSpatialRelEnvelopeIntersects&relationParam=&objectIds=&where=&time='+
           '&returnCountOnly=false'+ // true
           '&returnIdsOnly=false'+
           '&returnGeometry=true'+
           '&maxAllowableOffset=&outSR=4326&outFields=*&f=pjson'; // pjson // kmz // html
 Result := TDownloadRequest.Create(
           AnsiString(VLink),
           '',
           AInetConfig.GetStatic
           );

end;

end.
