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

unit u_GeoCoderByWikimapia;

interface

uses
  Classes,
  i_InterfaceListSimple,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_DownloadRequest,
  i_DownloadResult,
  u_GeoCoderBasic;

type
  TGeoCoderByWikiMapia = class(TGeoCoderBasic)
  protected
    function PrepareRequest(
      const ASearch: WideString;
      const ALocalConverter: ILocalCoordConverter
    ): IDownloadRequest; override;
    function ParseResultToPlacemarksList(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const AResult: IDownloadResultOk;
      const ASearch: WideString;
      const ALocalConverter: ILocalCoordConverter
    ): IInterfaceListSimple; override;
  public
  end;

implementation

uses
  SysUtils,
  ALString,
  RegExpr,
  t_GeoTypes,
  i_GeoCoder,
  i_VectorDataItemSimple,
  i_CoordConverter,
  i_BinaryData,
  u_BinaryData,
  u_DownloadRequest,
  u_InterfaceListSimple,
  u_ResStrings;


{ TGeoCoderByWikiMapia }

const
  cFloatRegEx = '[-+]?[0-9]*\.?[0-9]+';

  cSearchResultItemRegEx =
    '<li class="search-result-item".*?' +
    'data-latitude="(' + cFloatRegEx + ')".*?' +
    'data-longitude="(' + cFloatRegEx + ')".*?' +
    '<strong>(.*?)</strong>.*?' +
    '<small>(.*?)?(<.*?)*?</small>.*?' +
    '</li>';

(*

<li class="search-result-item"
    data-zoom="11"
    data-latitude="53.91"
    data-longitude="27.55">
    <div>
        <span style="color: #9BBDDE;">&bull;</span> <strong>Minsk (Минск, Мінск)</strong>
        <span class="label label-info">6508&nbsp;km</span>
    </div>
    <small>
        Minsk, Belarus <span class="small">(city)</span>
    </small>
</li>

*)

function TGeoCoderByWikiMapia.ParseResultToPlacemarksList(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const AResult: IDownloadResultOk;
  const ASearch: WideString;
  const ALocalConverter: ILocalCoordConverter
): IInterfaceListSimple;
var
  VStr: AnsiString;
  VName, VDesc: string;
  VPoint: TDoublePoint;
  VPlace: IVectorDataItemSimple;
  VList: IInterfaceListSimple;
  VFormatSettings: TFormatSettings;
  VRegExpr: TRegExpr;
begin
  if AResult.Data.Size <= 0 then begin
    raise EParserError.Create(SAS_ERR_EmptyServerResponse);
  end;

  SetLength(VStr, AResult.Data.Size);
  Move(AResult.Data.Buffer^, VStr[1], AResult.Data.Size);

  VFormatSettings.DecimalSeparator := '.';

  VList := TInterfaceListSimple.Create;

  VRegExpr := TRegExpr.Create;
  try
    VRegExpr.Expression := cSearchResultItemRegEx;

    VRegExpr.ModifierI := True;
    VRegExpr.ModifierM := True;

    if VRegExpr.Exec(Utf8ToAnsi(VStr)) then begin
      repeat
        try
          VPoint.Y := StrToFloat(VRegExpr.Match[1], VFormatSettings);
          VPoint.X := StrToFloat(VRegExpr.Match[2], VFormatSettings);
        except
          raise EParserError.CreateFmt(SAS_ERR_CoordParseError, [VRegExpr.Match[1], VRegExpr.Match[2]]);
        end;

        VName := StringReplace(VRegExpr.Match[3], '&quot;', '''', [rfReplaceAll]);
        VDesc := StringReplace(VRegExpr.Match[4], '&quot;', '''', [rfReplaceAll]);

        VPlace := PlacemarkFactory.Build(VPoint, Trim(VName), Trim(VDesc), '', 4);
        VList.Add(VPlace);

      until not VRegExpr.ExecNext;
    end;

    Result := VList;

  finally
    VRegExpr.Free;
  end;
end;

function TGeoCoderByWikiMapia.PrepareRequest(
  const ASearch: WideString;
  const ALocalConverter: ILocalCoordConverter
): IDownloadRequest;
var
  VSearch: AnsiString;
  VHeaders: AnsiString;
  VPostData: AnsiString;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VMapCenter: TDoublePoint;
begin
  VConverter := ALocalConverter.GetGeoConverter;
  VZoom := ALocalConverter.GetZoom;
  VMapCenter := VConverter.LonLat2TilePosFloat(ALocalConverter.GetCenterLonLat, VZoom);

  VSearch := URLEncode(AnsiToUtf8(ASearch));

  VPostData :=
    'y=' + ALIntToStr(Round(VMapCenter.Y)) + '&' +
    'x=' + ALIntToStr(Round(VMapCenter.X)) + '&' +
    'z=' + ALIntToStr(VZoom) + '&' +
    'qu=' + VSearch + '&' +
    'jtype=simple' + '&' +
    'start=0' + '&' +
    'try=0';

  VHeaders :=
    'Content-Type: application/x-www-form-urlencoded' + #13#10 +
    'Referer: http://wikimapia.org/' + #13#10 +
    'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8' + #13#10 +
    'Accept-Language: ru-RU,ru;q=0.8,en-US;q=0.5,en;q=0.3';

  Result :=
    TDownloadPostRequest.Create(
      'http://wikimapia.org/search/?q=' + VSearch,
      VHeaders,
      TBinaryData.CreateByAnsiString(VPostData) as IBinaryData,
      Self.InetSettings.GetStatic
    );
end;

end.
