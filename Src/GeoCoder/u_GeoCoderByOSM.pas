{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_GeoCoderByOSM;

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
  TGeoCoderByOSM = class(TGeoCoderBasic)
  protected
    function PrepareRequest(
      const ASearch: string;
      const ALocalConverter: ILocalCoordConverter
    ): IDownloadRequest; override;
    function ParseResultToPlacemarksList(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const AResult: IDownloadResultOk;
      const ASearch: string;
      const ALocalConverter: ILocalCoordConverter
    ): IInterfaceListSimple; override;
  public
  end;

implementation

uses
  SysUtils,
  StrUtils,
  t_GeoTypes,
  i_GeoCoder,
  i_VectorDataItemSimple,
  i_Projection,
  u_AnsiStr,
  u_InterfaceListSimple,
  u_ResStrings;


{ TGeoCoderByOSM }

function TGeoCoderByOSM.ParseResultToPlacemarksList(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const AResult: IDownloadResultOk;
  const ASearch: string;
  const ALocalConverter: ILocalCoordConverter
): IInterfaceListSimple;
var
  I, J, K: Integer;
  VLat, VLon: AnsiString;
  VName, VDesc, VFullDesc: string;
  VOsmType, VOsmId: AnsiString;
  VPoint: TDoublePoint;
  VPlace: IVectorDataItem;
  VList: IInterfaceListSimple;
  VFormatSettings: TFormatSettingsA;
  VStr: AnsiString;
begin
  VFullDesc := '';
  VDesc := '';
  if AResult.Data.Size <= 0 then begin
    raise EParserError.Create(SAS_ERR_EmptyServerResponse);
  end;

  VFormatSettings.DecimalSeparator := '.';
  VList := TInterfaceListSimple.Create;

  SetLength(VStr, AResult.Data.Size);
  Move(AResult.Data.Buffer^, VStr[1], AResult.Data.Size);

  I := PosA('<searchresults', VStr);

  while (PosA('<place', VStr, I) > I) and (I > 0) do begin
    J := I;

    I := PosA('osm_type="', VStr, J);
    J := PosA('"', VStr, I + 10);
    VOsmType := Copy(VStr, I + 10, J - (I + 10));

    I := PosA('osm_id="', VStr, J);
    J := PosA('"', VStr, I + 8);
    VOsmId := Copy(VStr, I + 8, J - (I + 8));

    I := PosA('lat="', VStr, J);
    J := PosA('"', VStr, I + 5);
    VLat := Copy(VStr, I + 5, J - (I + 5));

    I := PosA('lon="', VStr, J);
    J := PosA('"', VStr, I + 5);
    VLon := Copy(VStr, I + 5, J - (I + 5));

    I := PosA('display_name="', VStr, J);
    J := PosA('"', VStr, I + 14);
    VName := Utf8ToAnsi(Copy(VStr, I + 14, J - (I + 14)));

    I := PosA('class="', VStr, J);
    if I > J then begin
      J := PosA('"', VStr, I + 7);
      VDesc := Utf8ToAnsi(Copy(VStr, I + 7, J - (I + 7)));
    end;

    I := PosA('type="', VStr, J);
    if I > J then begin
      J := PosA('"', VStr, I + 6);
      VDesc := VDesc + '=' + Utf8ToAnsi(Copy(VStr, I + 6, J - (I + 6)));
    end;

    // финт ушам, дабы не занимать много места
    // будем разбивать "Кураж, 84, Вокзальная улица, Магнитогорск, Челябинская область, Уральский федеральный округ, 455000, Российская Федерация"
    // до первой запятой, остальное пихать в переменную sdesc
    K := PosEx(',', VName, 1);
    VDesc := VDesc + Copy(VName, K, Length(VName) - K + 1);
    VName := Copy(VName, 1, K - 1);
    // конец финта ушами


    VFullDesc := 'https://www.openstreetmap.org/browse/' + string(VOsmType) + '/' + string(VOsmId);

    //    Получение ссылки на иконку объекта, (на будущее), дабы обозначать найденные объекты...
    //    k := PosEx('icon=''', AStr, i);
    //    j := PosEx('><', AStr, i); // бывает что нету иконки тут проверяем на конец блока
    //    if k<j then begin
    //      j := PosEx('''', AStr, k + 6);
    //      sfulldesc:='<img src='''+Copy(AStr, k + 6, j - (k + 6))+'''>';
    //    end else sfulldesc:='';

    try
      VPoint.Y := StrToFloatA(VLat, VFormatSettings);
      VPoint.X := StrToFloatA(VLon, VFormatSettings);
    except
      raise EParserError.CreateFmt(SAS_ERR_CoordParseError, [VLat, VLon]);
    end;
    VPlace := PlacemarkFactory.Build(VPoint, VName, VDesc, VFullDesc, 4);
    VList.Add(VPlace);
  end;
  Result := VList;
end;

function TGeoCoderByOSM.PrepareRequest(
  const ASearch: string;
  const ALocalConverter: ILocalCoordConverter
): IDownloadRequest;
begin
  Result :=
    PrepareRequestByURL(
      'https://nominatim.openstreetmap.org/search?q=' + URLEncode(AnsiToUtf8(ASearch)) + '&format=xml'
    );
end;

end.
