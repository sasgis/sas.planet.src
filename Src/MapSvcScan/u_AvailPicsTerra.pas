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

unit u_AvailPicsTerra;

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
  TAvailPicsTerraserver = class(TAvailPicsAbstract)
  public
    function ContentType: String; override;

    function ParseResponse(const AResultOk: IDownloadResultOk): Integer; override;

    function GetRequest(const AInetConfig: IInetConfig): IDownloadRequest; override;
  end;

implementation

uses
  ALString,
  ALStringList,
  {$IFNDef UNICODE}
  Compatibility,
  {$ENDIF}
  u_StrFunc,
  u_GeoToStrFunc,
  u_StreamReadOnlyByBinaryData;

function _RandInt5: AnsiString;
var i: Integer;
begin
  // returns integer value as string[5]
  i := 20000 + Random(60000);
  Result := ALIntToStr(i);
end;

function _ConvertToYYYYMMDD(const AText, ASep: AnsiString): AnsiString;
var
  VSepPos: Integer;
  Vyyyy, Vmm, Vdd: AnsiString;
begin
  Result := '';
  Vmm := '';
  Vdd := '';
  Vyyyy := AText;

  // get m[m]
  VSepPos := ALPos(ASep, Vyyyy);
  if VSepPos>0 then begin
    Vmm := System.Copy(Vyyyy, 1, VSepPos - 1);
    System.Delete(Vyyyy, 1, VSepPos);
    while Length(Vmm)<2 do
      Vmm := '0' + Vmm;
  end;

  // get d[d]
  VSepPos := ALPos(ASep, Vyyyy);
  if VSepPos>0 then begin
    Vdd := System.Copy(Vyyyy, 1, VSepPos - 1);
    System.Delete(Vyyyy, 1, VSepPos);
    while Length(Vdd)<2 do
      Vdd := '0' + Vdd;
  end;

  // check all parts
  if ALTryStrToInt(Vyyyy, VSepPos) then
  if ALTryStrToInt(Vmm, VSepPos) then
  if ALTryStrToInt(Vdd, VSepPos) then begin
    // ok
    Result := Vyyyy + '.' + Vmm + '.' + Vdd;
  end;
end;


{ TAvailPicsTerraserver }

function TAvailPicsTerraserver.ContentType: String;
begin
  Result := 'application/json; charset=utf-8';
end;

function TAvailPicsTerraserver.ParseResponse(const AResultOk: IDownloadResultOk): Integer;


  function _ProcessOption(const AOptionText: AnsiString;
                          var AParams: TStrings): Boolean;
  var
    //VText: String;
    VDate: AnsiString;
    VValue: AnsiString;
    VLayer: AnsiString;
    VSep: AnsiString; // actual separator
    VPos: Integer;
    VItemIdentifier: AnsiString;
    VItemExisting: Boolean;
    VItemFetched: TDateTime;
  begin
    // ':"47f6c122130d37e9e6b5cf17c9fde868","formatted_date":"09-17-2014","product_type":"pan sharpened natural color'
    VLayer := GetBetween(AOptionText, ':"', '","formatted_date"');

    VValue := GetBetween(AOptionText, 'formatted_date":"', '"');
    // try convert to date (m[m]/d[d]/yyyy)
    if (Length(VValue) in [8..10]) then begin
      // find separator
      VSep := '-';
      VPos := ALPos(VSep, VValue);
      if (VPos>0) then begin
        // sep defined
        VDate := _ConvertToYYYYMMDD(VValue, VSep);
      end else begin
        // lookup sep
        VPos := 1;
        VSep := '';
        while (VPos <= Length(VValue)) do begin
          if CharInSet(VValue[VPos], ['0','1'..'9']) then
            Inc(VPos)
          else begin
            // found
            VSep := VValue[VPos];
            break;
          end;
        end;

        // if found
        if (0<Length(VSep)) then begin
          VDate := _ConvertToYYYYMMDD(VValue, VSep);
        end else begin
          // no date
          VDate := VValue;
        end;
      end;
    end else begin
      // no date
      VDate := VValue;
    end;

    VValue := GetAfter('"product_type":"', AOptionText);

    if (VValue='-1') and (VDate='-1') and (VLayer='-1') then begin
      // no image
      Result := FALSE;
      Exit;
    end;

    // make params
    if (nil=AParams) then
      AParams := TStringList.Create
    else
      AParams.Clear;

    // check existing
    VItemIdentifier := VLayer;
    if (Length(VItemIdentifier)<=16) then begin
      // add provider
      VItemIdentifier := VItemIdentifier + '_' + VValue;
    end;
    VItemExisting := ItemExists(FBaseStorageName, VItemIdentifier, @VItemFetched);
    StoreImageDate(VItemIdentifier, VDate);
    // add
    AParams.Values['layer'] := VLayer;
    AParams.Values['date'] := VDate;
    AParams.Values['product_type'] := VValue;
//    AParams.Values['METADATA_URL'] := 'http://www.terraserver.com/view.asp?' +
//          'cx=' + RoundEx(FTileInfoPtr.LonLat.X, 4) +
//          '&cy=' + RoundEx(FTileInfoPtr.LonLat.Y, 4) +
//          '&mpp=5' +
//          '&proj=4326&pic=img&prov=' + VValue + '&stac=' + VLayer + '&ovrl=-1&drwl=' +
//          '&lgin=' + _RandInt5 +
//          '&styp=&vic=';

//  /viewers/47f6c122130d37e9e6b5cf17c9fde868/image?width=108&height=81&bbox=39.003825187683105,45.013951949748076,39.011549949645996,45.01804747927431

    // call
    Result := FTileInfoPtr.AddImageProc(
      Self,
      VDate,
      'Terraserver',
      VItemExisting,
      VItemFetched,
      AParams
    );
  end;

var
  VStream: TStreamReadOnlyByBinaryData;
  VResponse: TALStringList;
  VSLParams: TStrings;
  S: AnsiString;
begin
  Result := 0;

  if (not Assigned(FTileInfoPtr.AddImageProc)) then
    Exit;

  VResponse := nil;
  VSLParams := nil;
  VStream := TStreamReadOnlyByBinaryData.Create(AResultOk.Data);
  try
    if (0 = VStream.Size) then
      Exit;

    VResponse := TALStringList.Create;
    VResponse.LineBreak := '{';
    VResponse.LoadFromStream(VStream);
    // parse for date:
    // {"features_count":10,"finished_features":[{"feature_id":"47f6c122130d37e9e6b5cf17c9fde868","formatted_date":"09-17-2014","product_type":"pan sharpened natural color"},{"feature_id":"8d7ed6c02d21738460a4341f3cc49685","formatted_date":"09-29-2014","product_type":"pan sharpened natural color"},

    while (VResponse.Count>0) do begin
      S := ALTrim(VResponse[0]);
      S := GetBetween(S, '"feature_id"', '"}');
      if Length(S)>0 then begin
        if ALPos('<', S) = 0 then begin
          // ':"47f6c122130d37e9e6b5cf17c9fde868","formatted_date":"09-17-2014","product_type":"pan sharpened natural color'
          // layer = 47f6c122130d37e9e6b5cf17c9fde868
          // date = 09-17-2014
          // product_type = pan sharpened natural color
          if _ProcessOption(ALTrim(S), VSLParams) then
            Inc(Result);
        end;
      end;

      // skip this line
      VResponse.Delete(0);
    end;
  finally
    VStream.Free;
    VSLParams.Free;
    VResponse.Free;
  end;
end;

function TAvailPicsTerraserver.GetRequest(const AInetConfig: IInetConfig): IDownloadRequest;
var
  VLink: AnsiString;
  VHeader: AnsiString;
begin
 VLink := 'https://www.terraserver.com/viewers/features?bbox=' +
          RoundExAnsi(FTileInfoPtr.TileRect.Left, 14) + '%2C'+
          RoundExAnsi(FTileInfoPtr.TileRect.Bottom, 14) + '%2C'+
          RoundExAnsi(FTileInfoPtr.TileRect.Right, 14) + '%2C'+
          RoundExAnsi(FTileInfoPtr.TileRect.Top, 14) +
          '&image_option=';

 VHeader :='User-Agent: Opera/9.80 (Windows NT 6.1; MRA 5.10 (build 5231)) Presto/2.12.388 Version/12.18'+#$D#$A+
    'Host: www.terraserver.com'+#$D#$A+
    'Accept-Language: ru-RU,ru;q=0.9,en;q=0.8'+#$D#$A+
    'Accept-Encoding: gzip, deflate'+#$D#$A+
    'Referer: https://www.terraserver.com/view'+#$D#$A+
    'Accept: */*'+#$D#$A+
    'X-Requested-With: XMLHttpRequest';

 Result := TDownloadRequest.Create(
           VLink,
           VHeader,
           AInetConfig.GetStatic
           );
end;

end.
