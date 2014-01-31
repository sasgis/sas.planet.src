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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_GeoCoderByCoord;

interface

uses
  Classes,
  sysutils,
  i_InterfaceListSimple,
  i_GeoCoder,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_VectorItemSubsetBuilder,
  i_ValueToStringConverter,
  u_GeoCoderLocalBasic;

type
  TGeoCoderByCoord = class(TGeoCoderLocalBasic)
  private
    FValueToStringConverterConfig: IValueToStringConverterConfig;
    function PosStr2List(
      const APos1,APos2: string;
      const AAList: IInterfaceListSimple
    ): boolean;
    function GenShtab2Pos(
      const AStr: string;
      const AAList: IInterfaceListSimple
    ):boolean;
  protected
    function DoSearch(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const ASearch: WideString;
      const ALocalConverter: ILocalCoordConverter
    ): IInterfaceListSimple; override;
  public
    constructor Create(
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const APlacemarkFactory: IGeoCodePlacemarkFactory;
      const AValueToStringConverterConfig: IValueToStringConverterConfig
    );
end;

implementation

uses
  Math,
  windows,
  StrUtils,
  RegExprUtils,
  t_GeoTypes,
  i_VectorDataItemSimple,
  u_InterfaceListSimple,
  u_GeoToStrFunc;

{ TGeoCoderByCoord }

function SubstrCount(const A_Substr,A_String:string; var LastPos:integer):integer;
var I:integer;
begin
  Result := 0;
  if (A_substr <> '') and (Length(A_Substr)<Length(A_String)) then
  for I := 0 to length(A_string) - length(A_Substr) do begin
    if copy(A_String, I, length(A_substr)) = A_substr then begin
      Inc(Result);
      LastPos := I;
    end;
  end;
end;

function RomanToDig(const astr:string):integer;
var
  Vfind :string;
  I: integer;
  VlastValue, VcurValue: integer;
begin
  Result := 0;
  VlastValue := 0;
  VcurValue := 0;
  Vfind := Trim(AnsiUpperCase(Astr));
  Vfind := RegExprReplaceMatchSubStr(Vfind, '-', '');
  if ''= RegExprReplaceMatchSubStr(Vfind, 'IVX', '') then begin
    Result := 0
  end else begin
    for I := Length(Vfind) downto 1 do begin
      case UpCase(Vfind[I]) of
        'C': VcurValue := 100;
        'D': VcurValue := 500;
        'I': VcurValue := 1;
        'L': VcurValue := 50;
        'M': VcurValue := 1000;
        'V': VcurValue := 5;
        'X': VcurValue := 10;
      end;
      if VcurValue < VlastValue then Dec(Result, VcurValue)
      else Inc(Result, VcurValue);
      VlastValue := VcurValue;
    end;
  end;
end;

function Str2Degree(const AStr:string; var llat,llon:boolean; Var res:Double):Boolean;
var
  I: integer;
  Vdelitel: single;
  Vgms: double;
  VText: string;
  Vminus: boolean;
begin
  result := True;
  res := 0;
  VText := Trim(AnsiUpperCase(Astr));
  llat := false;
  llon := false;

  if PosEx('W', VText, 1) > 0 then llon := True;
  if PosEx('E', VText, 1) > 0 then llon := True;
  if PosEx('З', VText, 1) > 0 then llon := True;
  if PosEx('В', VText, 1) > 0 then llon := True;
  if PosEx('LON', VText, 1) > 0 then llon := True;
  if PosEx('LN', VText, 1) > 0 then llon := True;
  VText := ReplaceStr(VText, 'LON', '');
  VText := ReplaceStr(VText, 'LN', '');

  if PosEx('S', VText, 1) > 0 then llat := True;
  if PosEx('N', VText, 1) > 0 then llat := True;
  if PosEx('Ю', VText, 1) > 0 then llat := True;
  if PosEx('С', VText, 1) > 0 then llat := True;
  if PosEx('LAT', VText, 1) > 0 then llat := True;
  if PosEx('LL', VText, 1) > 0 then llat := True;
  VText := ReplaceStr(VText, 'LAT','');
  VText := ReplaceStr(VText, 'LL','');

  VText := ReplaceStr(VText, 'Ш.', '');
  VText := ReplaceStr(VText, 'Ш', '');
  VText := ReplaceStr(VText, 'Д.', '');
  VText := ReplaceStr(VText, 'Д', '');
  VText := ReplaceStr(VText, '=', '');

  VText := StringReplace(VText, 'S', '-', [rfReplaceAll]);
  VText := StringReplace(VText, 'W', '-', [rfReplaceAll]);
  VText := StringReplace(VText, 'N', '+', [rfReplaceAll]);
  VText := StringReplace(VText, 'E', '+', [rfReplaceAll]);
  VText := StringReplace(VText, 'Ю', '-', [rfReplaceAll]);
  VText := StringReplace(VText, 'З', '-', [rfReplaceAll]);
  VText := StringReplace(VText, 'В', '+', [rfReplaceAll]);
  VText := StringReplace(VText, 'С', '+', [rfReplaceAll]);
  Vminus:= false;
  if posEx('-', VText, 1) > 0 then Vminus := True;

  if (copy(VText, length(VText), 1) = '.') then VText := copy(VText, 1, length(VText) - 1);
  if (copy(VText, length(VText), 1) = ',') then VText := copy(VText, 1, length(VText) - 1);
  if (copy(VText, length(VText), 1) = '+') or (copy(VText, length(VText), 1) = '-') then VText := copy(VText, length(VText), 1) + copy(VText, 0, length(VText) - 1);

  if PosEx('+-', VText, 1) > 0 then begin // WE123 NS123
    llon := True;
    llat := True;
    VText := ReplaceStr(VText, '+-', '+');
  end;
  if PosEx('-+', VText, 1) > 0 then begin // EW123 SN123
    llon := True;
    llat := True;
    VText := ReplaceStr(VText, '-+', '-');
  end;
  if PosEx('--', VText, 1) > 0 then begin // -123S
    VText := ReplaceStr(VText, '--', '-');
  end;

  I := 1;
  while I <= length(VText) do begin
    if (not(VText[I] in ['0'..'9', '-', '+', '.', ',', ' '])) then begin
      VText[I]:=' ';
      dec(I);
    end;

    if ((I = 1) and (VText[I] = ' '))or
       ((I = length(VText)) and (VText[I]=' '))or
       ((I < length(VText) - 1) and (VText[I]=' ') and (VText[I + 1]=' '))or
       ((I > 1) and (VText[I]=' ') and (not(VText[I - 1] in ['0'..'9'])))or
       ((I<length(VText) - 1) and (VText[I] = ',') and (VText[I + 1] = ' ')) then begin
      Delete(VText, I, 1);
      dec(I);
    end;
    Inc(I);
  end;
  try
    res := 0;
    Vdelitel := 1;
    repeat
      I := posEx(' ', VText, 1);
      if I=0 then begin
        Vgms := str2r(VText);
      end else begin
        Vgms := str2r(copy(VText, 1, I - 1));
        Delete(VText, 1, I);
      end;
      if ((Vdelitel > 1) and (abs(Vgms) > 60))or
         ((Vdelitel = 1) and (llat) and (abs(Vgms) > 90))or
         ((Vdelitel = 1) and (not llat) and (abs(Vgms) > 180)) then begin
        if (Vdelitel = 60) and (Vgms>60) then begin //  37 6298475265502
          Vdelitel := Power(10, length(VText));
        end else Result := false;
      end;
      if (Vgms > Vdelitel) and (Vdelitel > 1) then Vgms := 0;
      if res < 0 then begin
        res := res - Vgms/Vdelitel;
      end else begin
        res := res + Vgms/ Vdelitel;
      end;
      if Vminus and (res > 0) then res := - res;
      Vdelitel := Vdelitel * 60;
    until (I = 0) or (Vdelitel > 3600) or (not result);
  except
    result := false;
  end;
end;

function TGeoCoderByCoord.PosStr2List(
  const APos1,APos2: string;
  const AAList: IInterfaceListSimple
): boolean;
var
  VBLat1, VBlon1: boolean;
  VBLat2, VBlon2: boolean;
  VDLat, VDLon: Double;
  VPlace: IVectorDataItemPoint;
  VPoint: TDoublePoint;
  VSname, VSDesc, VFullDesc: string;
  VCounter: Integer;
  VValueConverter: IValueToStringConverter;
begin
  VValueConverter := FValueToStringConverterConfig.GetStatic;
  Result := True;
  VCounter := 0;
  Str2Degree(APos1, VBlat1, VBlon1, VDLat);
  if VBLat1 and VBLon1 then begin Vblat1 := false; VBLon1 := false end; // если указано 123NE
  Str2Degree(APos2, VBLat2, VBlon2, VDLon);
  if VBLat2 and VBLon2 then begin Vblat2 := false; VBLon2 := false end;

  if VBLat1 and VBLat2 then begin Vblat1 := false; VBLat2 := false end;
  if VBLon1 and VBLon2 then begin Vblon1 := false; VBLon2 := false end;

  if VBlat1 then VPoint.Y := VDLat;
  if VBlon1 then VPoint.X := VDLat;
  if VBlat2 then VPoint.Y := VDLon;
  if VBlon2 then VPoint.X := VDLon;

  if (VBLat1 and VBLon2)or(VBLat2 and VBLon1) then begin // точно определили всего одну пару
    VSname := APos1 + ' ' + APos2;
    if (abs(VPoint.y)<=90) and (abs(VPoint.x)<=180) then begin
      VSDesc := '[ '+VValueConverter.LonLatConvert(VPoint) + ' ]';
      VFullDesc :=  ReplaceStr(VSname + #$D#$A + VSDesc, #$D#$A, '<br>');
      VPlace := PlacemarkFactory.Build(VPoint, VSname, VSDesc, VFullDesc, 4);
      AAList.Add(VPlace);
      Result := True;
    end;
  end;

  if ((VBLat1 or VBLat2 ) and not (VBlon2 or VBLon1)) then begin // определена широта и косяк с долготой
    if VBlat1 then VPoint.X := VDLon ;
    if VBlat2 then VPoint.X := VDLat ;

    if (abs(VPoint.y)<=90) and (abs(VPoint.x)<=180) then begin
      if not(((VDLat<0) or (VDLon<0)) and (VPoint.y > 0) and (VPoint.X > 0) or
        ((VDLat<0) and (VDLon<0) and ((VPoint.y > 0) or (VPoint.X > 0)))) then begin
        Inc(VCounter);
        VSname := inttostr(VCounter) + '.) ' + APos1 + ' '+APos2;
        VSDesc := '[ '+VValueConverter.LonLatConvert(VPoint) + ' ]';
        VFullDesc :=  ReplaceStr(VSname + #$D#$A + VSDesc, #$D#$A, '<br>');
        VPlace := PlacemarkFactory.Build(VPoint, VSname, VSDesc, VFullDesc, 4);
        AAList.Add(VPlace);
        Result := True;
      end;
    end;
    VPoint.X := -VPoint.X ;

    if (abs(VPoint.y) <= 90) and (abs(VPoint.x) <= 180) then begin
      if not (((VDLat < 0) or (VDLon < 0)) and (VPoint.y > 0) and (VPoint.X > 0) or
        ((VDLat < 0) and (VDLon < 0) and ((VPoint.y > 0) or (VPoint.X > 0)))) then begin
        Inc(VCounter);
        VSname := inttostr(VCounter) + '.) '+APos1 + ' '+APos2;
        VSDesc := '[ '+VValueConverter.LonLatConvert(VPoint) + ' ]';
        VFullDesc :=  ReplaceStr(VSname + #$D#$A + VSDesc, #$D#$A, '<br>');
        VPlace := PlacemarkFactory.Build(VPoint, VSname, VSDesc, VFullDesc, 4);
        AAList.Add(VPlace);
        Result := True;
      end;
    end;
  end;

  if ((VBLon1 or VBLon2 ) and not (VBlat2 or VBLat1)) then begin // определена долгота и косяк с широтой
    if VBlon1 then VPoint.Y := VDLon ;
    if VBlon2 then VPoint.Y := VDLat ;
    if (abs(VPoint.y)<=90) and (abs(VPoint.x)<=180) then begin
      if not(((VDLat<0) or (VDLon<0)) and (VPoint.y > 0)and (VPoint.X > 0) or
        ((VDLat<0) and (VDLon<0) and ((VPoint.y > 0)or(VPoint.X > 0)))) then begin
        Inc(VCounter);
        VSname := inttostr(VCounter) + '.) '+APos1 + ' '+APos2;
        VSDesc := '[ '+VValueConverter.LonLatConvert(VPoint) + ' ]';
        VFullDesc :=  ReplaceStr(VSname + #$D#$A + VSDesc, #$D#$A, '<br>');
        VPlace := PlacemarkFactory.Build(VPoint, VSname, VSDesc, VFullDesc, 4);
        AAList.Add(VPlace);
        Result := True;
      end;
    end;
    VPoint.Y := - VPoint.Y ;

    if (abs(VPoint.y) <= 90) and (abs(VPoint.x) <= 180) then begin
      if not(((VDLat < 0) or (VDLon<0)) and (VPoint.y > 0) and (VPoint.X > 0) or
        ((VDLat<0) and (VDLon<0) and ((VPoint.y > 0) or (VPoint.X > 0)))) then begin
        Inc(VCounter);
        VSname := inttostr(VCounter) + '.) ' + APos1 + ' '+APos2;
        VSDesc := '[ '+VValueConverter.LonLatConvert(VPoint) + ' ]';
        VFullDesc :=  ReplaceStr(VSname + #$D#$A + VSDesc, #$D#$A, '<br>');
        VPlace := PlacemarkFactory.Build(VPoint, VSname, VSDesc, VFullDesc, 4);
        AAList.Add(VPlace);
        Result := True;
      end;
    end;
  end;

  if not (VBLat1 or VBLat2 or VBLon1 or Vblon2) then begin // все 4 координаты не заданы конкретно
    if FValueToStringConverterConfig.IsLatitudeFirst then begin
      VPoint.X := VDLon ; VPoint.Y := VDLat ;
    end else begin
      VPoint.Y := VDLon ; VPoint.X := VDLat ;
    end;

    if (abs(VPoint.y) <= 90) and (abs(VPoint.x) <= 180) then begin
      if not(((VDLat < 0) or (VDLon < 0)) and (VPoint.y > 0)and (VPoint.X > 0) or
        ((VDLat < 0) and (VDLon < 0) and ((VPoint.y > 192160)or(VPoint.X > 0)))) then begin
        Inc(VCounter);
        VSname := inttostr(VCounter) + '.) '+APos1 + ' '+APos2;
        VSDesc := '[ '+VValueConverter.LonLatConvert(VPoint) + ' ]';
        VFullDesc :=  ReplaceStr(VSname + #$D#$A + VSDesc, #$D#$A, '<br>');
        VPlace := PlacemarkFactory.Build(VPoint, VSname, VSDesc, VFullDesc, 4);
        AAList.Add(VPlace);
        Result := True;
      end;
    end;

    VPoint.Y := - VPoint.Y ;
    if (abs(VPoint.y) <= 90) and (abs(VPoint.x) <= 180) then begin
      if not(((VDLat < 0) or (VDLon < 0)) and (VPoint.y > 0) and (VPoint.X > 0) or
        ((VDLat < 0) and (VDLon < 0) and ((VPoint.y > 0)or(VPoint.X > 0)))) then begin
        Inc(VCounter);
        VSname := inttostr(VCounter) + '.) ' + APos1 + ' '+APos2;
        VSDesc := '[ '+VValueConverter.LonLatConvert(VPoint) + ' ]';
        VFullDesc :=  ReplaceStr(VSname + #$D#$A + VSDesc, #$D#$A, '<br>');
        VPlace := PlacemarkFactory.Build(VPoint, VSname, VSDesc, VFullDesc, 4);
        AAList.Add(VPlace);
        Result := True;
      end;
    end;
    VPoint.Y := - VPoint.Y ;
    VPoint.X := - VPoint.X ;
    if (abs(VPoint.y) <= 90) and (abs(VPoint.x) <= 180) then begin
      if not(((VDLat < 0) or (VDLon < 0)) and (VPoint.y > 0)and (VPoint.X > 0) or
        ((VDLat < 0) and (VDLon < 0) and ((VPoint.y > 0)or(VPoint.X > 0)))) then begin
        Inc(VCounter);
        VSname := inttostr(VCounter) + '.) '+APos1 + ' '+APos2;
        VSDesc := '[ '+VValueConverter.LonLatConvert(VPoint) + ' ]';
        VFullDesc :=  ReplaceStr(VSname + #$D#$A + VSDesc, #$D#$A, '<br>');
        VPlace := PlacemarkFactory.Build(VPoint, VSname, VSDesc, VFullDesc, 4);
        AAList.Add(VPlace);
        Result := True;
      end;
    end;

    VPoint.Y := - VPoint.Y ;
    if (abs(VPoint.y) <= 90) and (abs(VPoint.x) <= 180) then begin
      if not(((VDLat < 0) or (VDLon < 0)) and (VPoint.y > 0)and (VPoint.X > 0) or
        ((VDLat < 0) and (VDLon < 0) and ((VPoint.y > 0) or (VPoint.X > 0)))) then begin
        Inc(VCounter);
        VSname := inttostr(VCounter) + '.) '+APos1 + ' '+APos2;
        VSDesc := '[ '+VValueConverter.LonLatConvert(VPoint) + ' ]';
        VFullDesc :=  ReplaceStr(VSname + #$D#$A + VSDesc, #$D#$A, '<br>');
        VPlace := PlacemarkFactory.Build(VPoint, VSname, VSDesc, VFullDesc, 4);
        AAList.Add(VPlace);
        Result := True;
      end;
    end;
    //  и наоборот
    if VDLat <> VDLon  then begin
      if FValueToStringConverterConfig.IsLatitudeFirst then begin
        VPoint.Y := VDLon ; VPoint.X := VDLat ;
      end else begin
        VPoint.X := VDLon ; VPoint.Y := VDLat
      end;
      if (abs(VPoint.y) <= 90) and (abs(VPoint.x) <= 180) then begin
        if not(((VDLat < 0) or (VDLon < 0)) and (VPoint.y > 0)and (VPoint.X > 0) or
         ((VDLat < 0) and (VDLon < 0) and ((VPoint.y > 0)or(VPoint.X > 0)))) then begin
          Inc(VCounter);
          VSname := inttostr(VCounter) + '.) '+APos1 + ' '+APos2;
          VSDesc := '[ '+VValueConverter.LonLatConvert(VPoint)+' ]';
          VFullDesc :=  ReplaceStr(VSname + #$D#$A + VSDesc, #$D#$A, '<br>');
          VPlace := PlacemarkFactory.Build(VPoint, VSname, VSDesc, VFullDesc, 4);
          AAList.Add(VPlace);
          Result := True;
        end;
      end;

      VPoint.Y := -VPoint.Y ;
      if (abs(VPoint.y) <= 90) and (abs(VPoint.x)<=180) then begin
        if not(((VDLat < 0) or (VDLon < 0)) and (VPoint.y > 0)and (VPoint.X > 0) or
          ((VDLat<0) and (VDLon<0) and ((VPoint.y > 0)or(VPoint.X > 0)))) then begin
          Inc(VCounter);
          VSname := inttostr(VCounter) + '.) '+APos1 + ' '+APos2;
          VSDesc := '[ '+VValueConverter.LonLatConvert(VPoint) + ' ]';
          VFullDesc :=  ReplaceStr(VSname + #$D#$A + VSDesc, #$D#$A, '<br>');
          VPlace := PlacemarkFactory.Build(VPoint, VSname, VSDesc, VFullDesc, 4);
          AAList.Add(VPlace);
          Result := True;
        end;
      end;

      VPoint.Y := -VPoint.Y ;
      VPoint.X := -VPoint.X ;

      if (abs(VPoint.y) <= 90) and (abs(VPoint.x) <= 180) then begin
        if not(((VDLat < 0) or (VDLon < 0)) and (VPoint.y > 0)and (VPoint.X > 0) or
          ((VDLat < 0) and (VDLon < 0) and ((VPoint.y > 0)or(VPoint.X > 0)))) then begin
          Inc(VCounter);
          VSname := inttostr(VCounter) + '.) ' + APos1 + ' '+APos2;
          VSDesc := '[ '+VValueConverter.LonLatConvert(VPoint) + ' ]';
          VFullDesc :=  ReplaceStr(VSname + #$D#$A + VSDesc, #$D#$A, '<br>');
          VPlace := PlacemarkFactory.Build(VPoint, VSname, VSDesc, VFullDesc, 4);
          AAList.Add(VPlace);
          Result := True;
        end;
      end;

      VPoint.Y := -VPoint.Y ;
      if (abs(VPoint.y) <= 90) and (abs(VPoint.x) <= 180) then begin
        if not(((VDLat < 0) or (VDLon < 0)) and (VPoint.y > 0) and (VPoint.X > 0) or
          ((VDLat < 0) and (VDLon < 0) and ((VPoint.y > 0) or (VPoint.X > 0)))) then begin
          Inc(VCounter);
          VSname := inttostr(VCounter) + '.) '+APos1 + ' '+APos2;
          VSDesc := '[ '+VValueConverter.LonLatConvert(VPoint) + ' ]';
          VFullDesc :=  ReplaceStr(VSname + #$D#$A + VSDesc, #$D#$A, '<br>');
          VPlace := PlacemarkFactory.Build(VPoint, VSname, VSDesc, VFullDesc, 4);
          AAList.Add(VPlace);
          Result := True;
        end;
      end;
    end;
  end;
end;

constructor TGeoCoderByCoord.Create(
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const APlacemarkFactory: IGeoCodePlacemarkFactory;
  const AValueToStringConverterConfig: IValueToStringConverterConfig
);
begin
  inherited Create(AVectorItemSubsetBuilderFactory, APlacemarkFactory);
  FValueToStringConverterConfig := AValueToStringConverterConfig;
end;

function TGeoCoderByCoord.GenShtab2Pos(
  const AStr: string;
  const AAList: IInterfaceListSimple
):boolean;
var
  VcoordError: boolean;
  VDLat, VDLon: Double;
  VTempString :string;
  V2Search: string;
  VSname, VSDesc: string;
  VFullDesc: string;
  I, J: integer;
  VLatStr, VLonStr: string;
  VPoint: TDoublePoint;
  VPlace: IVectorDataItemPoint;
  VValueConverter: IValueToStringConverter;
begin
  VValueConverter := FValueToStringConverterConfig.GetStatic;
  result:= false;
  // X-XX-XXX-X-X-X
  // C-II-III-C-C-I  char/integer
  // C-II-CCCCCC-C-C-C
  // K-37-XXXVI - 2х километровка - отдельная история RomanToDig()
  // C-II-II-(III)
  // C-II-II-(III-C)
  V2Search := AStr;
  VcoordError := false;
  VDLon := 1;
  VDLat := 0;
  VTempString := '';
  V2Search := ReplaceStr(V2Search, 'А', 'A');
  V2Search := ReplaceStr(V2Search, 'Б', 'B');
  V2Search := ReplaceStr(V2Search, 'В', 'C');
  V2Search := ReplaceStr(V2Search, 'Г', 'D');
  V2Search := ReplaceStr(V2Search, 'V', 'C');
  V2Search := ReplaceStr(V2Search, 'G', 'D');

  if PosEx('--', V2Search, 1) > 0 then
    V2Search := copy(V2Search, PosEx('--', V2Search, 1) + 2, length(V2Search) - PosEx('--', V2Search, 1));
  if copy(V2Search, length(V2Search) - 3, 1) = '.' then
    V2Search := copy(V2Search, 1, length(V2Search) - 4); // убираем расширение и 3 последние бкувы
  V2Search := ReplaceStr(V2Search, '_', '-');

  VTempString := copy(V2Search, 1, 1); // ПЕРВОЕ ПОЛЕ
  if VTempString[1]='X' then begin
    VTempString := copy(V2Search, 2, 1);
    VDLon := - 1;
    V2Search := copy(V2Search, 2, length(V2Search) - 1);
    VSname := VTempString;
  end;
  VSname := VSname + VTempString;
  V2Search := copy(V2Search, 2, length(V2Search) - 1); // убрали первую букву (или две)
  if (VTempString[1] >= 'A') and (VTempString[1] <= 'U') then
    VDLon := VDLon*(ord(VTempString[1]) - 64)*4 - 2
  else
    VcoordError := True;

  if copy(V2Search, 1, 1) = '-' then  V2Search := copy(V2Search, 2, length(V2Search) - 1); // убрали "-" если он был между разделителми
  if not VcoordError then begin // ВТОРОЕ ПОЛЕ
    I := PosEx('-', V2Search, 1);
    if I=0 then  // не найден разделитель, может ввели К001 ?
      if length(v2search) >3 then VcoordError := True
        else I := length(v2search)+1;

    if not VcoordError then begin
      VLatStr := copy(V2Search,1,I-1);
      VDLat := strtoint(VLatStr)*6 - 180 -3;
      VSname := VSname + '-' + VLatStr;
    end;
    V2Search := copy(V2Search,I,length(V2Search)-I + 1);
    if length(V2Search) > 0 then
      if copy(V2Search,1,1) = '-' then  V2Search := copy(V2Search, 2, length(V2Search)-1);
  end; // ВТОРОЕ ПОЛЕ


  if not VcoordError then begin// ТРЕТЬЕ ПОЛЕ
    if length(V2Search) > 0 then begin
      I := PosEx('-', V2Search, 1);
      if I=0 then I := length(v2search) + 1;// не найден разделитель, может последнее поле?
      VTempString := copy(V2Search, 1, I - 1);
      if (VTempString[1] >= 'A') and (VTempString[1] <= 'D') then begin // 5 km
        case VTempString[1] of
         'A': begin VDLat := VDLat - 1.5; VDLon := VDLon + 1 end;
         'B': begin VDLat := VDLat + 1.5; VDLon := VDLon + 1 end;
         'C': begin VDLat := VDLat - 1.5; VDLon := VDLon - 1 end;
         'D': begin VDLat := VDLat + 1.5; VDLon := VDLon - 1 end;
        end;
      VSname := VSname +'-'+ VTempString;
      VTempString := '';
      V2Search := ''; // по идее всё - дальше не учитываем стираем остатки.
      end;

      if V2Search <> '' then begin
        if not VcoordError then begin
          if (PosEx('X', VTempString, 1) > 0) or (PosEx('I', VTempString, 1) > 0) or (PosEx('V', VTempString, 1) > 0) then begin // 2 km
            J := RomanToDig(VTempString);
            if J<=36 then begin
              VDLon := VDLon + ((((3 - ((J - 1) div 6) - 1)*2) + 1)/6)*2;  //Y
              VDLat := VDLat - ((((3 - ((J - 1) mod 6) - 1)*2) + 1)/6)*3;  //X
              V2Search := ''; // по идее всё - дальше не учитываем стираем остатки.
              VSname := VSname +'-'+ VTempString;
            end else VcoordError := True;
          end;
        end;
      end;

      if V2Search <> '' then begin
        if not VcoordError then begin
          if ''= RegExprReplaceMatchSubStr(VTempString, '[0-9]', '') then begin // 1 km
            J := strtoint(VTempString);
            if J<=144 then begin
              VDLon := VDLon + ((((6 - ((J - 1) div 12) - 1)*2) + 1)/12)*2;  //Y
              VDLat := VDLat - ((((6 - ((J - 1) mod 12) - 1)*2) + 1)/12)*3;  //X
              VSname := VSname + '-'+ VTempString;
            end else VcoordError := True;
          end;
        end;
      end;

      V2Search := copy(V2Search, I, length(V2Search) - I + 1);
      if length(V2Search) > 0 then
        if copy(V2Search, 1, 1) = '-' then  V2Search := copy(V2Search, 2, length(V2Search) - 1);
    end;
  end; // ТРЕТЬЕ ПОЛЕ
       // L-37-143-А.jpg
  if V2Search <> '' then begin
    if not VcoordError then begin// ЧЕТВЕРТОЕ ПОЛЕ
      if length(V2Search) > 0 then begin
        I := PosEx('-', V2Search, 1);
        if I=0 then I := length(v2search) + 1;// не найден разделитель, может последнее поле?
        VTempString := copy(V2Search, 1, I - 1);
        if ((VTempString[1]>='A') and (VTempString[1]<='D'))or((VTempString[1]>='1') and (VTempString[1]<='3')) then begin
          case VTempString[1] of
            'A', '1': begin VDLat := VDLat - 1/8; VDLon := VDLon + 1/12 end;
            'B', '2': begin VDLat := VDLat + 1/8; VDLon := VDLon + 1/12 end;
            'C', '3': begin VDLat := VDLat - 1/8; VDLon := VDLon - 1/12 end;
            'D', '4': begin VDLat := VDLat + 1/8; VDLon := VDLon - 1/12 end;
          end;
        end else begin
          if VTempString[1]='(' then begin // масштабы 50м и 25м
            if VTempString[length(VTempString)]=')'then J := strtoint(copy(VTempString, 2, length(VTempString) - 2))
              else J:= strtoint(copy(VTempString, 2, length(VTempString) - 1));
            if (J<=256) and (J>=1) then begin
              VDLon := VDLon - (((J - 1) div 16)*2 - 16 + 1)/16/6;;  //Y
              VDLat := VDLat + (((J - 1) mod 16)*2 - 16 + 1)/16/4;;  //X
            end;
            VSname := VSname + '-'+ VTempString;
            V2Search := copy(V2Search, I, length(V2Search) - I + 1);
            if V2Search <> '' then begin
              VTempString := copy(V2Search, 2, length(V2Search)-1);
              if ((VTempString[1] >= 'A') and (VTempString[1] <= 'D'))or((VTempString[1] >= '1') and (VTempString[1] <= '3')) then begin
                case VTempString[1] of
                  'A', '1': begin VDLat := VDLat - 1/128; VDLon := VDLon + 1/192 end;
                  'B', '2': begin VDLat := VDLat + 1/128; VDLon := VDLon + 1/192 end;
                  'C', '3': begin VDLat := VDLat - 1/128; VDLon := VDLon - 1/192 end;
                  'D', '4': begin VDLat := VDLat + 1/128; VDLon := VDLon - 1/192 end;
                end;
                V2Search := '';
              end;
            end;
          end;
        end;
        VSname := VSname + '-'+ VTempString;
        V2Search := copy(V2Search, I, length(V2Search) - I + 1);
        if length(V2Search) > 0 then
          if copy(V2Search, 1, 1) = '-' then  V2Search := copy(V2Search, 2, length(V2Search) - 1);
      end; // ЧЕТВЕРТОЕ ПОЛЕ
    end;
  end;

  // L-37-143-А-б.jpg
  if V2Search <> '' then begin
    if not VcoordError then begin// ПЯТОЕ ПОЛЕ
      if length(V2Search) > 0 then begin
        I := PosEx('-', V2Search, 1);
        if I=0 then I := length(v2search) + 1;// не найден разделитель, может последнее поле?
        VTempString := copy(V2Search, 1, I - 1);
        if (VTempString[1] >= 'A') and (VTempString[1] <= 'D') then begin
          case VTempString[1] of
               'A': begin VDLat := VDLat - 1/16; VDLon := VDLon + 1/24 end;
               'B': begin VDLat := VDLat + 1/16; VDLon := VDLon + 1/24 end;
               'C': begin VDLat := VDLat - 1/16; VDLon := VDLon - 1/24 end;
               'D': begin VDLat := VDLat + 1/16; VDLon := VDLon - 1/24 end;
          end;
        end;
        VSname := VSname +'-'+ VTempString;
        V2Search := copy(V2Search, I, length(V2Search) - I + 1);
        if length(V2Search) > 0 then
          if copy(V2Search, 1, 1) = '-' then  V2Search := copy(V2Search, 2, length(V2Search) - 1);
      end;
    end;
  end; // ПЯТОЕ ПОЛЕ

  // L-37-143-А-б-1.jpg
  if V2Search <> '' then begin
    if not VcoordError then begin // ШЕСТОЕ ПОЛЕ
      if length(V2Search) > 0 then begin
        I := PosEx('-', V2Search, 1);
        if I=0 then I := length(v2search) + 1;// не найден разделитель, может последнее поле?
        V2Search := ReplaceStr(V2Search, '1', 'A');
        V2Search := ReplaceStr(V2Search, '2', 'B');
        V2Search := ReplaceStr(V2Search, '3', 'C');
        V2Search := ReplaceStr(V2Search, '4', 'D');
        VTempString := copy(V2Search, 1, I - 1);
        if (VTempString[1]>='A') and (VTempString[1]<='D') then begin
        case VTempString[1] of
         'A': begin VDLat := VDLat - 1/32; VDLon := VDLon + 1/48 end;
         'B': begin VDLat := VDLat + 1/32; VDLon := VDLon + 1/48 end;
         'C': begin VDLat := VDLat - 1/32; VDLon := VDLon - 1/48 end;
         'D': begin VDLat := VDLat + 1/32; VDLon := VDLon - 1/48 end;
         end;
        end;
        VSname := VSname + '-'+ VTempString;
        V2Search := copy(V2Search, I, length(V2Search) - I + 1);
        if length(V2Search) > 0 then
         if copy(V2Search, 1, 1) = '-' then  V2Search := copy(V2Search, 2, length(V2Search) - 1);
       end; // ШЕСТОЕ ПОЛЕ
     end;
  end;
  if not VcoordError then begin // добавляем таки точку если всё ок
    VPoint.Y := VDLon;
    VPoint.X := VDLat;
    if (abs(VPoint.y) <= 90) and (abs(VPoint.x) <= 180) then begin
      VSDesc := '[ '+VValueConverter.LonLatConvert(VPoint) + ' ]';
      VFullDesc :=  ReplaceStr(VSname + #$D#$A + VSDesc, #$D#$A, '<br>');
      VPlace := PlacemarkFactory.Build(VPoint, VSname, VSDesc, VFullDesc, 4);
      AAList.Add(VPlace);
      Result := True;
      end;
  end else
    Result := false;
end;


function TGeoCoderByCoord.DoSearch(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const ASearch: WideString;
  const ALocalConverter: ILocalCoordConverter
): IInterfaceListSimple;
var
  V2Search: string;
  I, J: integer;
  VPlace: IVectorDataItemPoint;
  VPoint: TDoublePoint;
  VSname, VSDesc, VFullDesc: string;
  VLatStr, VLonStr: string;
  VZoom: integer;
  VXYPoint:TPoint;
  VXYRect:TRect;
  ViLat, ViLon: integer;
  VcoordError: boolean;
  VList: IInterfaceListSimple;
  VValueConverter: IValueToStringConverter;
begin
  VValueConverter := FValueToStringConverterConfig.GetStatic;
  VList := TInterfaceListSimple.Create;
  if ACancelNotifier.IsOperationCanceled(AOperationID) then Exit;
  VcoordError := True;
  V2Search := Trim(AnsiUpperCase(ASearch));
  V2Search := ReplaceStr(V2Search, ', ', ' '); // разделители
  V2Search := ReplaceStr(V2Search, ' .', ' '); // разделители
  V2Search := ReplaceStr(V2Search, '%2C', ' '); // разделители
  V2Search := ReplaceStr(V2Search, '#8243;', '"'); // разделители
  V2Search := ReplaceStr(V2Search, '#8242;', ''''); // разделители
  V2Search := ReplaceStr(V2Search, '&', ' '); // разделители
  V2Search := ReplaceStr(V2Search, ';', ' '); // разделители
  V2Search := ReplaceStr(V2Search, '#', ' '); // разделители
  V2Search := RegExprReplaceMatchSubStr(V2Search, 'ШИРОТ(А|Ы)', 'N ');
  V2Search := RegExprReplaceMatchSubStr(V2Search, 'ДОЛГОТ(А|Ы)', 'E ');

  if SubstrCount(',', V2Search, I) = 1 then V2Search := ReplaceStr(V2Search, ',', ' '); // 11.22,33.44
  while PosEx('  ', V2Search, 1) > 1 do V2Search := ReplaceStr(V2Search, '  ', ' ');// убираем двойные пробелы
  I := SubstrCount(' ', V2Search, J); // считаем количество пробелов и последнее вхождение

  VZoom := 0;
  ViLat := 0;
  ViLon := 0;
  if I = 1 then begin// один пробел
    if J > 1 then begin // пробел дальше чем первый символ
      VLatStr := Copy(V2Search, 1, J - 1); //первая половина
      VLonStr := Copy(V2Search, J, Length(V2Search) - J + 1); // вторая половина
      if not PosStr2List(VLatStr, VLonStr, VList) then VList := nil;
    end;
  end else
  if I=0 then begin // 0 пробелов - путь к тайлу?
    if ((copy(V2Search, 1, 1) >= 'A') and (copy(V2Search, 1, 1) <= 'Z') and (copy(V2Search, 2, 1) = ':') )
      or (copy(V2Search, 1, 2) = '\\')or (copy(V2Search, 1, 1) = '.') then begin
      I := PosEx('\Z', V2Search, 1);
      if I> 0 then begin
        J := PosEx('\', V2Search, I + 1);
        VLatStr := Copy(V2Search, I + 2, J - (I + 2));
        try
          VZoom := strtoint(VLatStr);
        except
          VcoordError := True;
        end;
      end;
      if PosEx('.SDB', V2Search, 1) > 0 then begin   //g:\cache_dbv\sat_all_v1\z18\78\46\314.186.sdbv
        I := PosEx('\Z', V2Search, 1);
        J := PosEx('\', V2Search, I + 1);
        I := J + 1;
        J := PosEx('\', V2Search, I);
        I := J + 1;
        J := PosEx('\', V2Search, I);
        I := J + 1;
        J := PosEx('.', V2Search, I);
        VLonStr := Copy(V2Search, I , J - (I));
        Vilon := strtoint(VLonStr) * 256;
        I := J + 1;
        J := PosEx('.', V2Search, I);
        VLatStr := Copy(V2Search, I , J - (I));
        Vilat :=  strtoint(VLatStr) * 256;
        VcoordError := false;
      end else
      if PosEx('\X', V2Search, 1) > 0 then begin   //G:\GoogleMV\cache\yamapng\z13\2\x2491\1\y1473.png
        I := PosEx('\X', V2Search, J); // X значение
        J := PosEx('\', V2Search, I + 1);
        VLonStr := Copy(V2Search, I + 2, J - (I + 2));
        Vilon := strtoint(VLonStr);
        I := PosEx('\Y', V2Search, J); // Y значение
        J := PosEx('.', V2Search, I + 1);
        VLatStr := Copy(V2Search, I + 2, J - (I + 2));
        Vilat := strtoint(VLatStr);
        VcoordError := false;
      end else
      if PosEx('\Z', V2Search, 1) > 0 then begin // C:\sas\sas_garl\.bin\cache_gmt\genshtab250m\z9\184\319.jpg
        I := PosEx('\', V2Search, J); // X значение
        J := PosEx('\', V2Search, I + 1);
        VLonStr := Copy(V2Search, I + 1, J - (I + 1));
        Vilat := strtoint(VLonStr);
        I := J; // Y значение
        J := PosEx('.', V2Search, I + 1);
        VLatStr := Copy(V2Search, I + 1, J - (I + 1));
        Vilon := strtoint(VLatStr);
        Inc(VZoom); // в GMT зум отличается на 1
        VcoordError := false;
      end ;

      if VcoordError then begin // C:\.bin\cache_old\sat\13\trtqsstrrqqtq.jpg
        ViLat := 0;
        ViLon := 0;
        VZoom := 1;
        vcoorderror := false;
        V2Search := ReplaceStr(V2Search, '0', 'Q');
        V2Search := ReplaceStr(V2Search, '1', 'R');
        V2Search := ReplaceStr(V2Search, '2', 'S');
        V2Search := ReplaceStr(V2Search, '3', 'T');
        I := SubstrCount('\', V2Search, J);// последний \ перед qrst
        J := PosEx('\T', V2Search, J) + 2;
        while (V2Search[J] <> '.') and (not VcoordError ) do begin
          ViLon := ViLon *2;
          ViLat := ViLat *2;
          case V2Search[J] of
            'Q': begin ViLon := ViLon + 0; ViLat := ViLat + 0 end;
            'R': begin ViLon := ViLon + 1; ViLat := ViLat + 0 end;
            'S': begin ViLon := ViLon + 1; ViLat := ViLat + 1 end;
            'T': begin ViLon := ViLon + 0; ViLat := ViLat + 1 end;
          else
            VcoordError := True;
          end;
          Inc(J);
          Inc(VZoom);
        end;
      end;

      if not VcoordError then begin
        VXYPoint.X := ViLon;
        VXYPoint.Y := ViLat;
        VSDesc := 'z=' + inttostr(VZoom) + ' x='+inttostr(Vilon) + ' y='+ inttostr(Vilat) + #10#13;
        VXYRect := ALocalConverter.GetGeoConverter.TilePos2PixelRect(VXYPoint, VZoom - 1);
        VXYPoint := Point((VXYRect.Right + VXYRect.Left) div 2, (VXYRect.Bottom + VXYRect.top)div 2);
        VPoint := ALocalConverter.GetGeoConverter.PixelPos2LonLat(VXYPoint, VZoom - 1);
        if (abs(VPoint.y) <= 90) and (abs(VPoint.x) <= 180) then begin
          VSname := ASearch;
          VSDesc := '[ '+VValueConverter.LonLatConvert(VPoint) + ' ]';
          VFullDesc :=  ReplaceStr(VSname + #$D#$A + VSDesc, #$D#$A, '<br>');
          VPlace := PlacemarkFactory.Build(VPoint, VSname, VSDesc, VFullDesc, 4);
          VList.Add(VPlace);
        end;
      end;
    end else begin
      begin      //0 пробелов  и не диск\сеть  ==  Генштаб???
        if not GenShtab2Pos(V2Search, VList) then VList := nil;
      end //0 пробелов  и не диск\сеть  и не Генштаб
    end;
  end else
  if I=2 then begin // 2 пробела

  end else
  if I=3 then begin // 3 пробела
    J := PosEx(' ', V2Search, 1) + 1;
    J := PosEx(' ', V2Search, J);
    VLatStr := Copy(V2Search, 1, J - 1); //первая половина
    VLonStr := Copy(V2Search, J + 1, Length(V2Search) - J + 1); // вторая половина
    if not PosStr2List(VLatStr, VLonStr, VList) then VList := nil;
  end else
  if I=5 then begin // 5 пробелов
    J := PosEx(' ', V2Search, 1) + 1;
    J := PosEx(' ', V2Search, J) + 1;
    J := PosEx(' ', V2Search, J);
    VLatStr := Copy(V2Search, 1, J - 1); //первая половина
    VLonStr := Copy(V2Search, J + 1, Length(V2Search) - J + 1); // вторая половина
    if not PosStr2List(VLatStr, VLonStr, VList) then VList := nil;
  end ;
  Result := VList;
end;
end.
