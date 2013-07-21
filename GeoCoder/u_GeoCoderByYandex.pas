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

unit u_GeoCoderByYandex;

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
  TGeoCoderByYandex = class(TGeoCoderBasic)
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
  StrUtils,
  t_GeoTypes,
  i_GeoCoder,
  i_CoordConverter,
  u_InterfaceListSimple,
  u_GeoToStr,
  u_ResStrings,
  u_GeoCodePlacemark;

{ TGeoCoderByYandex }

function TGeoCoderByYandex.ParseResultToPlacemarksList(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const AResult: IDownloadResultOk;
  const ASearch: WideString;
  const ALocalConverter: ILocalCoordConverter
): IInterfaceListSimple;
var
  slat, slon, sname, sdesc, sfulldesc: string;
  i, j: integer;
  VPoint: TDoublePoint;
  VPlace: IGeoCodePlacemark;
  VList: IInterfaceListSimple;
  VFormatSettings: TFormatSettings;
  CurPos:integer;// позиция текущего символа
  BrLevel:integer;//глубина вложенности {=плюс один   }=минус один
  Buffer:string; // сюда складываем временно считанную строку
  CurChar:string; // текущий символ
  err:boolean;// признак окончания поиска когда уже всё нашли...
  ParseErr:boolean; // признак ошибки разбора строки
  Vstr2Find :string;
begin
  Buffer := '';
  BrLevel := 0;
  err := false;
  if AResult.Data.Size <= 0 then begin
    raise EParserError.Create(SAS_ERR_EmptyServerResponse);
  end;
  VFormatSettings.DecimalSeparator := '.';
  VList := TInterfaceListSimple.Create;
  SetLength(Vstr2Find, AResult.Data.Size);
  Move(AResult.Data.Buffer^, Vstr2Find[1], AResult.Data.Size);
  Vstr2Find := ReplaceStr(Vstr2Find,'\/','/'); // разделители
  Vstr2Find := ReplaceStr(Vstr2Find,'\"','"'); // разделители
  CurPos:=PosEx('"features":[', Vstr2Find, 1)-1;
  while (CurPos<length(Vstr2Find)) and (not err)do begin
   inc (CurPos);
   CurChar:=copy(Vstr2Find,CurPos,1);
   Buffer:=Buffer+CurChar;

   if CurChar='{' then inc(BrLevel);
   if CurChar='}' then begin
    dec(BrLevel);
    if BrLevel=0 then  begin
   // подстрока готова. теперь будем её парсит и дёргатьнужные нам значения
     sdesc:='';
     sname:='';
     sfulldesc:='';
     ParseErr:=False;
     j:=1;
     // если есть адресная информация - добавляем её
     // это же является признаком того что точку нужно обрабатывать
     i := PosEx('{"AddressLine":"', Buffer, 1);
     if i>0  then begin
      if i>j then begin
       j := PosEx('",', Buffer, i + 16);
       sdesc:=Utf8ToAnsi(Copy(Buffer, i + 16, j - (i + 16)));
      end;
     end
    else
    if PosEx('"Address":{"locality":"', Buffer, 1)>1 then begin
     i := PosEx('"Address":{"locality":"', Buffer, 1);
     j := PosEx('",', Buffer, i + 23);
     sdesc:=Utf8ToAnsi(Copy(Buffer, i + 23, j - (i + 23)));
    end else
    // у НЯК всё совсем по другому. проверяем няковские поля
    if PosEx('PSearchMetaData', Buffer, 1)>1 then begin
     i:=PosEx('"PSearchMetaData":{"id":"', Buffer, 1);
     j := PosEx('",', Buffer, i + 25);
     sfulldesc:='http://n.maps.yandex.ru/?l=wmap&oid='+Copy(Buffer, i + 25, j - (i + 25));
   end
   else ParseErr:=true; // дальше строку не разбираем ибо отсутствует наименование.

   if not ParseErr then begin // если нашли признак валидности данных
    // делаем ссылку на описание если оно есть.
    j:=1;
    i:= PosEx('"CompanyMetaData":{"id":"', Buffer, 1);
    if i>j then begin
     j := PosEx('",', Buffer, i + 25);
     sfulldesc:='http://maps.yandex.ru/sprav/'+Copy(Buffer, i + 25, j - (i + 25))+'/';
    end;

     // достаём наименование
     i := PosEx(',"name":"', Buffer, 1);
     j := PosEx('",', Buffer, i + 9);
     sname:= Utf8ToAnsi(Copy(Buffer, i + 9, j - (i + 9)));
     // прибавляем статусную часть (улица,посёлок..) если она идёт сразу за наименованием
     if Copy(Buffer,j,8)='","type"' then begin
      i := PosEx('"type":"', Buffer, j);
      j := PosEx('",', Buffer, i + 8);
      sname:= sname+' '+Utf8ToAnsi(Copy(Buffer, i + 8, j - (i + 8)));
     end;

     // Достаём координаты
     i := PosEx('"coordinates":[', Buffer, j);
     j := PosEx(',', Buffer, i + 15);
     slon := Copy(Buffer, i + 15, j - (i + 15));
     i := PosEx(']', Buffer, j);
     slat := Copy(Buffer, j + 1, i - (j + 1));
     if slat[1] = '\' then delete(slat, 1, 1);
     if slon[1] = '\' then delete(slon, 1, 1);


     i:=1;
     if PosEx('"Categories":', Buffer, i)>i then begin// значит есть категория
      sdesc:=sdesc+' (';
      while PosEx('{"name":"', Buffer, i)>i do begin // названий категорий может быть несколько поэтому будем их сцеплять (банки-банкоматы)
       i := PosEx('{"name":"', Buffer, i);
       j := PosEx('","', Buffer, i);
       sdesc:=sdesc+' '+Utf8ToAnsi(Copy(Buffer, i + 9, j - (i + 9)));
       i:=j;
       end;
      sdesc:=sdesc+')';
      end;

     // описание из НЯК
     j:=1;
     i:= PosEx('{"locality":"', Buffer, 1);
     if i>j then begin
      j := PosEx('"', Buffer, i + 13);
      sdesc:='(НЯК) '+Utf8ToAnsi(Copy(Buffer, i + 13, j - (i + 13)));
      end;

     // описание из НЯК
     j:=1;
     i:= PosEx('"category_name":"', Buffer, 1);
     if i>j then begin
      j := PosEx('"', Buffer, i + 17);
      if sdesc='' then sdesc:='(НЯК) ';
      sdesc:=sdesc+' ('+Utf8ToAnsi(Copy(Buffer, i + 17, j - (i + 17)))+')';
      end;

     try
      VPoint.Y := StrToFloat(slat, VFormatSettings);
      VPoint.X := StrToFloat(slon, VFormatSettings);
     except
      raise EParserError.CreateFmt(SAS_ERR_CoordParseError, [slat, slon]);
     end;

      // если нашли что-нибудь - тогда добавляем точку.
     if sdesc<>'' then begin
      VPlace := TGeoCodePlacemark.Create(VPoint, sname, sdesc, sfulldesc, 4);
      VList.Add(VPlace);
     end;
    end;
    Buffer:='';
   end;
   if (BrLevel=1) and (curpos>= length(Vstr2Find)) then err:=true;//  Хватит. Довольно. Выходим.
   end;
  end;
  Result := VList;
end;

function TGeoCoderByYandex.PrepareRequest(const ASearch: WideString;
  const ALocalConverter: ILocalCoordConverter): IDownloadRequest;
var
  VSearch: String;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VMapRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
begin
  VSearch := ASearch;
  VConverter:=ALocalConverter.GetGeoConverter;
  VZoom := ALocalConverter.GetZoom;
  VMapRect := ALocalConverter.GetRectInMapPixelFloat;
  VConverter.CheckPixelRectFloat(VMapRect, VZoom);
  VLonLatRect := VConverter.PixelRectFloat2LonLatRect(VMapRect, VZoom);
  Result :=
    PrepareRequestByURL(
            'http://maps.yandex.ru/?text='+URLEncode(AnsiToUtf8(VSearch))+
            '&sll='+R2StrPoint(ALocalConverter.GetCenterLonLat.x)+','+R2StrPoint(ALocalConverter.GetCenterLonLat.y)+
            '&sspn='+R2StrPoint(VLonLatRect.Right-VLonLatRect.Left)+','+R2StrPoint(VLonLatRect.Top-VLonLatRect.Bottom)+
            '&z='+inttostr(VZoom)+'&source=form&output=json'
    );
end;
end.
// examples
//  http://maps.yandex.ru/?text=%D0%A0%D0%BE%D1%81%D1%81%D0%B8%D1%8F%2C%20%D0%A2%D1%8E%D0%BC%D0%B5%D0%BD%D1%81%D0%BA%D0%B0%D1%8F%20%D0%BE%D0%B1%D0%BB%D0%B0%D1%81%D1%82%D1%8C%2C%20%D0%A2%D1%8E%D0%BC%D0%B5%D0%BD%D1%8C&sll=65.558412%2C57.182627&ll=38.975277%2C45.035407&spn=0.469666%2C0.261446&z=11&l=map
//  http://maps.yandex.ru/?text=%D0%B1%D0%B0%D0%BB%D0%BE%D1%87%D0%BA%D0%B0&sll=38.975276999999984%2C45.03540700001939&sspn=0.469666%2C0.261446&z=11&source=form&output=json

// http://maps.yandex.ru/?text=%D0%9C%D0%BE%D1%81%D0%BA%D0%B2%D0%B0&sll=37.4969343220393,55.7374581159436&sspn=0.211658477783203,0.0711842917507042&z=13&source=form&output=json
