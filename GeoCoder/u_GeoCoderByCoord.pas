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

unit u_GeoCoderByCoord;

interface

uses
  Classes,
  sysutils,
  i_InterfaceListSimple,
  i_GeoCoder,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_ValueToStringConverter,
  u_GeoCoderLocalBasic;

type
  TGeoCoderByCoord = class(TGeoCoderLocalBasic)
  private
    FValueToStringConverterConfig: IValueToStringConverterConfig;
    function PosStr2List(
      const APos1,APos2: string;
      const AAList: IInterfaceListSimple
    ) : boolean;
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
  u_GeoToStr;

{ TGeoCoderByCoord }


function SubstrCount(const A_Substr,A_String:string; var LastPos:integer):integer;
var i:integer;
begin
Result := 0;
if (A_substr<>'') and (Length(A_Substr)<Length(A_String)) then
 for i := 0 to length(A_string)-length(A_Substr) do
   if copy(A_String,i,length(A_substr))=A_substr then begin
   inc(Result);
   LastPos := i;
   end;
end;

function RomanToDig(const astr:string):integer;
var Vfind :string;
i, lastValue, curValue: integer;
begin
 Result := 0;
 lastValue := 0;
 curValue := 0;
 Vfind := Trim(AnsiUpperCase(Astr));
 Vfind := RegExprReplaceMatchSubStr(Vfind,'-','');
 if ''= RegExprReplaceMatchSubStr(Vfind,'IVX','') then
  Result := 0
  else
 begin
  for i := Length(Vfind) downto 1 do
  begin
   case UpCase(Vfind[i]) of
   'C': curValue := 100;
   'D': curValue := 500;
   'I': curValue := 1;
   'L': curValue := 50;
   'M': curValue := 1000;
   'V': curValue := 5;
   'X': curValue := 10;
   end;
   if curValue < lastValue then Dec(Result, curValue)
   else Inc(Result, curValue);
   lastValue := curValue;
  end;
 end;
end;

function Str2Degree(const AStr:string; var llat,llon:boolean; Var res:Double):Boolean;
var
  i : integer;
 delitel : single;
  gms : double;
  VText : string;
  minus:boolean;
begin
    result:=true;
    res:=0;
    VText := Trim(AnsiUpperCase(Astr));
    llat := false;
    llon := false;

    if PosEx('W', VText, 1) > 0 then llon := true;
    if PosEx('E', VText, 1) > 0 then llon := true;
    if PosEx('З', VText, 1) > 0 then llon := true;
    if PosEx('В', VText, 1) > 0 then llon := true;
    if PosEx('LON', VText, 1) > 0 then llon := true;
    if PosEx('LN', VText, 1) > 0 then llon := true;
    VText := ReplaceStr(VText,'LON','');
    VText := ReplaceStr(VText,'LN','');

    if PosEx('S', VText, 1) > 0 then llat := true;
    if PosEx('N', VText, 1) > 0 then llat := true;
    if PosEx('Ю', VText, 1) > 0 then llat := true;
    if PosEx('С', VText, 1) > 0 then llat := true;
    if PosEx('LAT', VText, 1) > 0 then llat := true;
    if PosEx('LL', VText, 1) > 0 then llat := true;
    VText := ReplaceStr(VText,'LAT','');
    VText := ReplaceStr(VText,'LL','');

    VText := ReplaceStr(VText,'Ш.','');
    VText := ReplaceStr(VText,'Ш','');
    VText := ReplaceStr(VText,'Д.','');
    VText := ReplaceStr(VText,'Д','');
    VText := ReplaceStr(VText,'=','');

    VText := StringReplace(VText,'S','-',[rfReplaceAll]);
    VText := StringReplace(VText,'W','-',[rfReplaceAll]);
    VText := StringReplace(VText,'N','+',[rfReplaceAll]);
    VText := StringReplace(VText,'E','+',[rfReplaceAll]);
    VText := StringReplace(VText,'Ю','-',[rfReplaceAll]);
    VText := StringReplace(VText,'З','-',[rfReplaceAll]);
    VText := StringReplace(VText,'В','+',[rfReplaceAll]);
    VText := StringReplace(VText,'С','+',[rfReplaceAll]);
    minus:= false;
    if posEx('-',VText,1)>0 then minus := true;

    if (copy(VText,length(VText),1)='.') then VText := copy(VText,1,length(VText)-1);
    if (copy(VText,length(VText),1)=',') then VText := copy(VText,1,length(VText)-1);
    if (copy(VText,length(VText),1)='+') or (copy(VText,length(VText),1)='-') then VText:=copy(VText,length(VText),1)+copy(VText,0,length(VText)-1);

    if PosEx('+-', VText, 1) > 0 then begin // WE123 NS123
     llon := true;
     llat := true;
     VText := ReplaceStr(VText,'+-','+');
    end;
    if PosEx('-+', VText, 1) > 0 then begin // EW123 SN123
     llon := true;
     llat := true;
     VText := ReplaceStr(VText,'-+','-');
    end;
    if PosEx('--', VText, 1) > 0 then begin // -123S
     VText := ReplaceStr(VText,'--','-');
    end;

  i:=1;
  while i<=length(VText) do begin
    if (not(VText[i] in ['0'..'9','-','+','.',',',' '])) then begin
      VText[i]:=' ';
      dec(i);
    end;

    if ((i=1)and(VText[i]=' '))or
       ((i=length(VText))and(VText[i]=' '))or
       ((i<length(VText)-1)and(VText[i]=' ')and(VText[i+1]=' '))or
       ((i>1) and (VText[i]=' ') and (not(VText[i-1] in ['0'..'9'])))or
       ((i<length(VText)-1)and(VText[i]=',')and(VText[i+1]=' ')) then begin
      Delete(VText,i,1);
      dec(i);
    end;
    inc(i);
  end;
  try
    res:=0;
    delitel:=1;
    repeat
     i:=posEx(' ',VText,1);
     if i=0 then begin
       gms:=str2r(VText);
     end else begin
       gms:=str2r(copy(VText,1,i-1));
       Delete(VText,1,i);
     end;
     if ((delitel>1)and(abs(gms)>60))or
        ((delitel=1)and(llat)and(abs(gms)>90))or
        ((delitel=1)and(not llat)and(abs(gms)>180)) then begin
      if (delitel=60) and (GMS>60) then begin //  37 6298475265502
         delitel := Power(10,length(VText));
      end else Result:=false;
     end;
     if (gms>delitel) and (delitel>1) then gms := 0;
     if res<0 then begin
       res:=res-gms/delitel;
     end else begin
       res:=res+gms/delitel;
     end;
     if minus and (res>0) then res:=-res;
     delitel:=delitel*60;
    until (i=0)or(delitel>3600)or(not result);
  except
    result:=false;
  end;
end;

function TGeoCoderByCoord.PosStr2List(const APos1,APos2: string; const AAList: IInterfaceListSimple) : boolean;
var
 VBLat1, VBlon1: boolean;
 VBLat2, VBlon2: boolean;
 VDLat, VDLon : Double;
 VPlace : IVectorDataItemPoint;
 VPoint : TDoublePoint;
 sname, sdesc, sfulldesc : string;
 VCounter : Integer;
 VValueConverter: IValueToStringConverter;
begin
 VValueConverter := FValueToStringConverterConfig.GetStatic;
 result := true;
 VCounter:=0;
 Str2Degree(APos1, VBlat1, VBlon1, VDLat);
 if VBLat1 and VBLon1 then begin Vblat1 := false ; VBLon1 := false end; // если указано 123NE
      Str2Degree(APos2, VBLat2, VBlon2, VDLon);
      if VBLat2 and VBLon2 then begin Vblat2 := false ; VBLon2 := false end;

      if VBLat1 and VBLat2 then begin Vblat1 := false ; VBLat2 := false end;
      if VBLon1 and VBLon2 then begin Vblon1 := false ; VBLon2 := false end;

      if VBlat1 then VPoint.Y := VDLat ;
      if VBlon1 then VPoint.X := VDLat ;
      if VBlat2 then VPoint.Y := VDLon ;
      if VBlon2 then VPoint.X := VDLon ;

      if (VBLat1 and VBLon2)or(VBLat2 and VBLon1) then begin // точно определили всего одну пару
        sname := APos1+' '+APos2;
        if (abs(VPoint.y)<=90)and(abs(VPoint.x)<=180) then begin
         sdesc := '[ '+VValueConverter.LonLatConvert(VPoint)+' ]';
         sfulldesc :=  ReplaceStr( sname + #$D#$A+ sdesc,#$D#$A,'<br>');
         VPlace := PlacemarkFactory.Build(VPoint, sname, sdesc, sfulldesc, 4);
         AAList.Add(VPlace);
         result:=true;
        end;
      end;

      if ((VBLat1 or VBLat2 )and not (VBlon2 or VBLon1))then begin // определена широта и косяк с долготой
        if VBlat1 then VPoint.X := VDLon ;
        if VBlat2 then VPoint.X := VDLat ;

        if (abs(VPoint.y)<=90)and(abs(VPoint.x)<=180) then
         if not( ((VDLat<0) or (VDLon<0)) and (VPoint.y >0)and (VPoint.X >0) or
         ((VDLat<0)and(VDLon<0)and((VPoint.y >0)or(VPoint.X >0)))) then begin
          inc(VCounter);sname := inttostr(VCounter)+'.) '+APos1+' '+APos2;
          sdesc := '[ '+VValueConverter.LonLatConvert(VPoint)+' ]';
          sfulldesc :=  ReplaceStr( sname + #$D#$A+ sdesc,#$D#$A,'<br>');
          VPlace := PlacemarkFactory.Build(VPoint, sname, sdesc, sfulldesc, 4);
          AAList.Add(VPlace);
          result:=true;
        end;

        VPoint.X := -VPoint.X ;

        if (abs(VPoint.y)<=90)and(abs(VPoint.x)<=180) then
         if not( ((VDLat<0) or (VDLon<0)) and (VPoint.y >0)and (VPoint.X >0) or
         ((VDLat<0)and(VDLon<0)and((VPoint.y >0)or(VPoint.X >0)))) then begin
          inc(VCounter);sname := inttostr(VCounter)+'.) '+APos1+' '+APos2;
          sdesc := '[ '+VValueConverter.LonLatConvert(VPoint)+' ]';
          sfulldesc :=  ReplaceStr( sname + #$D#$A+ sdesc,#$D#$A,'<br>');
          VPlace := PlacemarkFactory.Build(VPoint, sname, sdesc, sfulldesc, 4);
          AAList.Add(VPlace);
          result:=true;
        end;
      end;

      if ((VBLon1 or VBLon2 )and not (VBlat2 or VBLat1))then begin // определена долгота и косяк с широтой
        if VBlon1 then VPoint.Y := VDLon ;
        if VBlon2 then VPoint.Y := VDLat ;

        if (abs(VPoint.y)<=90)and(abs(VPoint.x)<=180) then
         if not( ((VDLat<0) or (VDLon<0)) and (VPoint.y >0)and (VPoint.X >0) or
         ((VDLat<0)and(VDLon<0)and((VPoint.y >0)or(VPoint.X >0)))) then begin
          inc(VCounter);sname := inttostr(VCounter)+'.) '+APos1+' '+APos2;
          sdesc := '[ '+VValueConverter.LonLatConvert(VPoint)+' ]';
          sfulldesc :=  ReplaceStr( sname + #$D#$A+ sdesc,#$D#$A,'<br>');
          VPlace := PlacemarkFactory.Build(VPoint, sname, sdesc, sfulldesc, 4);
          AAList.Add(VPlace);
          result:=true;
        end;

        VPoint.Y := -VPoint.Y ;

        if (abs(VPoint.y)<=90)and(abs(VPoint.x)<=180) then
         if not( ((VDLat<0) or (VDLon<0)) and (VPoint.y >0)and (VPoint.X >0) or
         ((VDLat<0)and(VDLon<0)and((VPoint.y >0)or(VPoint.X >0)))) then begin
          inc(VCounter);sname := inttostr(VCounter)+'.) '+APos1+' '+APos2;
          sdesc := '[ '+VValueConverter.LonLatConvert(VPoint)+' ]';
          sfulldesc :=  ReplaceStr( sname + #$D#$A+ sdesc,#$D#$A,'<br>');
          VPlace := PlacemarkFactory.Build(VPoint, sname, sdesc, sfulldesc, 4);
          AAList.Add(VPlace);
          result:=true;
        end;
      end;

     if not(VBLat1 or VBLat2 or VBLon1 or Vblon2) then begin // все 4 координаты не заданы конкретно

        if FValueToStringConverterConfig.IsLatitudeFirst then
        begin
         VPoint.X := VDLon ; VPoint.Y := VDLat ;
        end else begin
         VPoint.Y := VDLon ; VPoint.X := VDLat ;
        end;

        if (abs(VPoint.y)<=90)and(abs(VPoint.x)<=180) then
         if not( ((VDLat<0) or (VDLon<0)) and (VPoint.y >0)and (VPoint.X >0) or
         ((VDLat<0)and(VDLon<0)and((VPoint.y >0)or(VPoint.X >0)))) then begin
          inc(VCounter);sname := inttostr(VCounter)+'.) '+APos1+' '+APos2;
          sdesc := '[ '+VValueConverter.LonLatConvert(VPoint)+' ]';
          sfulldesc :=  ReplaceStr( sname + #$D#$A+ sdesc,#$D#$A,'<br>');
          VPlace := PlacemarkFactory.Build(VPoint, sname, sdesc, sfulldesc, 4);
          AAList.Add(VPlace);
          result:=true;
        end;

        VPoint.Y := -VPoint.Y ;

        if (abs(VPoint.y)<=90)and(abs(VPoint.x)<=180) then
         if not( ((VDLat<0) or (VDLon<0)) and (VPoint.y >0)and (VPoint.X >0) or
         ((VDLat<0)and(VDLon<0)and((VPoint.y >0)or(VPoint.X >0)))) then begin
          inc(VCounter);sname := inttostr(VCounter)+'.) '+APos1+' '+APos2;
          sdesc := '[ '+VValueConverter.LonLatConvert(VPoint)+' ]';
          sfulldesc :=  ReplaceStr( sname + #$D#$A+ sdesc,#$D#$A,'<br>');
          VPlace := PlacemarkFactory.Build(VPoint, sname, sdesc, sfulldesc, 4);
          AAList.Add(VPlace);
          result:=true;
        end;

        VPoint.Y := -VPoint.Y ;
        VPoint.X := -VPoint.X ;

        if (abs(VPoint.y)<=90)and(abs(VPoint.x)<=180) then
         if not( ((VDLat<0) or (VDLon<0)) and (VPoint.y >0)and (VPoint.X >0) or
         ((VDLat<0)and(VDLon<0)and((VPoint.y >0)or(VPoint.X >0)))) then begin
          inc(VCounter);sname := inttostr(VCounter)+'.) '+APos1+' '+APos2;
          sdesc := '[ '+VValueConverter.LonLatConvert(VPoint)+' ]';
          sfulldesc :=  ReplaceStr( sname + #$D#$A+ sdesc,#$D#$A,'<br>');
          VPlace := PlacemarkFactory.Build(VPoint, sname, sdesc, sfulldesc, 4);
          AAList.Add(VPlace);
          result:=true;
        end;

        VPoint.Y := -VPoint.Y ;

        if (abs(VPoint.y)<=90)and(abs(VPoint.x)<=180) then
         if not( ((VDLat<0) or (VDLon<0)) and (VPoint.y >0)and (VPoint.X >0) or
         ((VDLat<0)and(VDLon<0)and((VPoint.y >0)or(VPoint.X >0)))) then begin
          inc(VCounter);sname := inttostr(VCounter)+'.) '+APos1+' '+APos2;
          sdesc := '[ '+VValueConverter.LonLatConvert(VPoint)+' ]';
          sfulldesc :=  ReplaceStr( sname + #$D#$A+ sdesc,#$D#$A,'<br>');
          VPlace := PlacemarkFactory.Build(VPoint, sname, sdesc, sfulldesc, 4);
          AAList.Add(VPlace);
          result:=true;
        end;
//  и наоборот
      if VDLat <> VDLon  then begin

        if FValueToStringConverterConfig.IsLatitudeFirst then
        begin
         VPoint.Y := VDLon ; VPoint.X := VDLat ;
        end else begin
         VPoint.X := VDLon ; VPoint.Y := VDLat
        end;

        if (abs(VPoint.y)<=90)and(abs(VPoint.x)<=180) then
         if not( ((VDLat<0) or (VDLon<0)) and (VPoint.y >0)and (VPoint.X >0) or
         ((VDLat<0)and(VDLon<0)and((VPoint.y >0)or(VPoint.X >0)))) then begin
          inc(VCounter);sname := inttostr(VCounter)+'.) '+APos1+' '+APos2;
          sdesc := '[ '+VValueConverter.LonLatConvert(VPoint)+' ]';
          sfulldesc :=  ReplaceStr( sname + #$D#$A+ sdesc,#$D#$A,'<br>');
          VPlace := PlacemarkFactory.Build(VPoint, sname, sdesc, sfulldesc, 4);
          AAList.Add(VPlace);
          result:=true;
        end;

        VPoint.Y := -VPoint.Y ;

        if (abs(VPoint.y)<=90)and(abs(VPoint.x)<=180) then
         if not( ((VDLat<0) or (VDLon<0)) and (VPoint.y >0)and (VPoint.X >0) or
         ((VDLat<0)and(VDLon<0)and((VPoint.y >0)or(VPoint.X >0)))) then begin
          inc(VCounter);sname := inttostr(VCounter)+'.) '+APos1+' '+APos2;
          sdesc := '[ '+VValueConverter.LonLatConvert(VPoint)+' ]';
          sfulldesc :=  ReplaceStr( sname + #$D#$A+ sdesc,#$D#$A,'<br>');
          VPlace := PlacemarkFactory.Build(VPoint, sname, sdesc, sfulldesc, 4);
          AAList.Add(VPlace);
          result:=true;
        end;

        VPoint.Y := -VPoint.Y ;
        VPoint.X := -VPoint.X ;

        if (abs(VPoint.y)<=90)and(abs(VPoint.x)<=180) then
         if not( ((VDLat<0) or (VDLon<0)) and (VPoint.y >0)and (VPoint.X >0) or
         ((VDLat<0)and(VDLon<0)and((VPoint.y >0)or(VPoint.X >0)))) then begin
          inc(VCounter);sname := inttostr(VCounter)+'.) '+APos1+' '+APos2;
          sdesc := '[ '+VValueConverter.LonLatConvert(VPoint)+' ]';
          sfulldesc :=  ReplaceStr( sname + #$D#$A+ sdesc,#$D#$A,'<br>');
          VPlace := PlacemarkFactory.Build(VPoint, sname, sdesc, sfulldesc, 4);
          AAList.Add(VPlace);
          result:=true;
        end;

        VPoint.Y := -VPoint.Y ;

        if (abs(VPoint.y)<=90)and(abs(VPoint.x)<=180) then
         if not( ((VDLat<0) or (VDLon<0)) and (VPoint.y >0)and (VPoint.X >0) or
         ((VDLat<0)and(VDLon<0)and((VPoint.y >0)or(VPoint.X >0)))) then begin
          inc(VCounter);sname := inttostr(VCounter)+'.) '+APos1+' '+APos2;
          sdesc := '[ '+VValueConverter.LonLatConvert(VPoint)+' ]';
          sfulldesc :=  ReplaceStr( sname + #$D#$A+ sdesc,#$D#$A,'<br>');
          VPlace := PlacemarkFactory.Build(VPoint, sname, sdesc, sfulldesc, 4);
          AAList.Add(VPlace);
          result:=true;
        end;

      end;
 end;
end;

constructor TGeoCoderByCoord.Create(
  const APlacemarkFactory: IGeoCodePlacemarkFactory;
  const AValueToStringConverterConfig: IValueToStringConverterConfig
);
begin
  inherited Create(APlacemarkFactory);
  FValueToStringConverterConfig := AValueToStringConverterConfig;
end;

function TGeoCoderByCoord.GenShtab2Pos(
  const AStr: string;
  const AAList: IInterfaceListSimple
):boolean;
var
 VcoordError : boolean;
 VDLat, VDLon : Double;
 temp_string :string;
 V2Search : string;
 sname, sdesc : string;
 sfulldesc : string;
 i, j : integer;
 slat, slon : string;
 VPoint : TDoublePoint;
 VPlace : IVectorDataItemPoint;
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
       temp_string := '';
       V2Search := ReplaceStr(V2Search,'А','A');
       V2Search := ReplaceStr(V2Search,'Б','B');
       V2Search := ReplaceStr(V2Search,'В','C');
       V2Search := ReplaceStr(V2Search,'Г','D');
       V2Search := ReplaceStr(V2Search,'V','C');
       V2Search := ReplaceStr(V2Search,'G','D');

       if PosEx('--', V2Search, 1)>0 then  V2Search := copy(V2Search,PosEx('--', V2Search, 1)+2,length(V2Search)-PosEx('--', V2Search, 1));
       if copy(V2Search,length(V2Search)-3,1)='.' then V2Search := copy(V2Search,1,length(V2Search)-4); // убираем расширение и 3 последние бкувы
       V2Search := ReplaceStr(V2Search,'_','-');

       temp_string := copy(V2Search,1,1); // ПЕРВОЕ ПОЛЕ
       if temp_string[1]='X' then begin
          temp_string := copy(V2Search,2,1);
          VDLon := -1;
          V2Search := copy(V2Search,2,length(V2Search)-1);
          sname := temp_string;
          end;
       sname := sname + temp_string;
       V2Search := copy(V2Search,2,length(V2Search)-1); // убрали первую букву (или две)
       if (temp_string[1]>='A') and (temp_string[1]<='U') then
            VDLon := VDLon*(ord(temp_string[1])-64)*4 -2 else VcoordError := true;
       if copy(V2Search,1,1)='-' then  V2Search := copy(V2Search,2,length(V2Search)-1); // убрали "-" если он был между разделителми

       if not VcoordError then begin // ВТОРОЕ ПОЛЕ
         i := PosEx('-', V2Search, 1);
         if i=0 then  // не найден разделитель, может ввели К001 ?
         if length(v2search)>3 then VcoordError := true else begin
            i:=length(v2search)+1;
            end;

         if not VcoordError then begin
           slat := copy(V2Search,1,i-1);
           VDLat := strtoint(slat)*6 - 180 -3;
           sname := sname + '-' + slat;
         end;
       V2Search := copy(V2Search,i,length(V2Search)-i+1);
       if length(V2Search)>0 then
         if copy(V2Search,1,1)='-' then  V2Search := copy(V2Search,2,length(V2Search)-1);
       end; // ВТОРОЕ ПОЛЕ


       if not VcoordError then // ТРЕТЬЕ ПОЛЕ
       if length(V2Search)>0 then begin
         i := PosEx('-', V2Search, 1);
         if i=0 then i:=length(v2search)+1;// не найден разделитель, может последнее поле?
         temp_string := copy(V2Search,1,i-1);

        if (temp_string[1]>='A') and (temp_string[1]<='D') then begin // 5 km
        case temp_string[1] of
         'A' : begin VDLat := VDLat -1.5; VDLon := VDLon +1 end;
         'B' : begin VDLat := VDLat +1.5; VDLon := VDLon +1 end;
         'C' : begin VDLat := VDLat -1.5; VDLon := VDLon -1 end;
         'D' : begin VDLat := VDLat +1.5; VDLon := VDLon -1 end;
        end;
        sname := sname +'-'+ temp_string;
        temp_string := '';
        V2Search := ''; // по идее всё - дальше не учитываем стираем остатки.
        end;

        if V2Search<>'' then
        if not VcoordError then
         if (PosEx('X', temp_string, 1)>0)or(PosEx('I', temp_string, 1)>0)or(PosEx('V', temp_string, 1)>0) then begin // 2 km
          j := RomanToDig(temp_string);
          if j<=36 then begin
           VDLon := VDLon + ((((3-((j-1) div 6)-1)*2)+1)/6)*2;  //Y
           VDLat := VDLat - ((((3-((j-1) mod 6)-1)*2)+1)/6)*3;  //X
           V2Search := ''; // по идее всё - дальше не учитываем стираем остатки.
           sname := sname +'-'+ temp_string;
          end else VcoordError := true;
         end;

        if V2Search<>'' then
        if not VcoordError then
         if ''= RegExprReplaceMatchSubStr(temp_string,'[0-9]','') then// 1 km
         begin
         j := strtoint(temp_string);
          if j<=144 then begin
           VDLon := VDLon + ((((6-((j-1) div 12)-1)*2)+1)/12)*2;  //Y
           VDLat := VDLat - ((((6-((j-1) mod 12)-1)*2)+1)/12)*3;  //X
           sname := sname +'-'+ temp_string;
          end else VcoordError := true;
         end;

       V2Search := copy(V2Search,i,length(V2Search)-i+1);
       if length(V2Search)>0 then
         if copy(V2Search,1,1)='-' then  V2Search := copy(V2Search,2,length(V2Search)-1);
       end;// ТРЕТЬЕ ПОЛЕ

       // L-37-143-А.jpg
       if V2Search<>'' then
       if not VcoordError then // ЧЕТВЕРТОЕ ПОЛЕ
        if length(V2Search)>0 then begin
         i := PosEx('-', V2Search, 1);
         if i=0 then i:=length(v2search)+1;// не найден разделитель, может последнее поле?
         temp_string := copy(V2Search,1,i-1);

        if ((temp_string[1]>='A') and (temp_string[1]<='D'))or((temp_string[1]>='1') and (temp_string[1]<='3')) then begin
        case temp_string[1] of
         'A','1' : begin VDLat := VDLat -1/8; VDLon := VDLon +1/12 end;
         'B','2' : begin VDLat := VDLat +1/8; VDLon := VDLon +1/12 end;
         'C','3' : begin VDLat := VDLat -1/8; VDLon := VDLon -1/12 end;
         'D','4' : begin VDLat := VDLat +1/8; VDLon := VDLon -1/12 end;
         end;
        end else if temp_string[1]='(' then begin // масштабы 50м и 25м
          if temp_string[length(temp_string)]=')'then j := strtoint(copy(temp_string,2,length(temp_string)-2))
            else j:= strtoint(copy(temp_string,2,length(temp_string)-1));
          if (j<=256) and (j>=1) then begin
              VDLon := VDLon - (((j-1) div 16)*2-16+1)/16/6;;  //Y
              VDLat := VDLat + (((j-1) mod 16)*2-16+1)/16/4;;  //X
              end;
          sname := sname +'-'+ temp_string;
          V2Search := copy(V2Search,i,length(V2Search)-i+1);
          if V2Search <> '' then begin
            temp_string := copy(V2Search,2,length(V2Search)-1);
            if ((temp_string[1]>='A') and (temp_string[1]<='D'))or((temp_string[1]>='1') and (temp_string[1]<='3')) then
              case temp_string[1] of
                'A','1' : begin VDLat := VDLat -1/128; VDLon := VDLon + 1/192 end;
                'B','2' : begin VDLat := VDLat +1/128; VDLon := VDLon + 1/192 end;
                'C','3' : begin VDLat := VDLat -1/128; VDLon := VDLon - 1/192 end;
                'D','4' : begin VDLat := VDLat +1/128; VDLon := VDLon - 1/192 end;
              end;
              V2Search := '';
          end;
        end;
        sname := sname +'-'+ temp_string;
        V2Search := copy(V2Search,i,length(V2Search)-i+1);
        if length(V2Search)>0 then
         if copy(V2Search,1,1)='-' then  V2Search := copy(V2Search,2,length(V2Search)-1);
       end; // ЧЕТВЕРТОЕ ПОЛЕ

       // L-37-143-А-б.jpg
       if V2Search<>'' then
       if not VcoordError then // ПЯТОЕ ПОЛЕ
        if length(V2Search)>0 then begin
         i := PosEx('-', V2Search, 1);
         if i=0 then i:=length(v2search)+1;// не найден разделитель, может последнее поле?
         temp_string := copy(V2Search,1,i-1);

        if (temp_string[1]>='A') and (temp_string[1]<='D') then begin
        case temp_string[1] of
         'A' : begin VDLat := VDLat -1/16; VDLon := VDLon +1/24 end;
         'B' : begin VDLat := VDLat +1/16; VDLon := VDLon +1/24 end;
         'C' : begin VDLat := VDLat -1/16; VDLon := VDLon -1/24 end;
         'D' : begin VDLat := VDLat +1/16; VDLon := VDLon -1/24 end;
         end;
        end;

        sname := sname +'-'+ temp_string;
        V2Search := copy(V2Search,i,length(V2Search)-i+1);
        if length(V2Search)>0 then
         if copy(V2Search,1,1)='-' then  V2Search := copy(V2Search,2,length(V2Search)-1);
       end; // ПЯТОЕ ПОЛЕ


       // L-37-143-А-б-1.jpg
       if V2Search<>'' then
       if not VcoordError then // ШЕСТОЕ ПОЛЕ
        if length(V2Search)>0 then begin
         i := PosEx('-', V2Search, 1);
         if i=0 then i:=length(v2search)+1;// не найден разделитель, может последнее поле?
         V2Search := ReplaceStr(V2Search,'1','A');
         V2Search := ReplaceStr(V2Search,'2','B');
         V2Search := ReplaceStr(V2Search,'3','C');
         V2Search := ReplaceStr(V2Search,'4','D');
         temp_string := copy(V2Search,1,i-1);

        if (temp_string[1]>='A') and (temp_string[1]<='D') then begin
        case temp_string[1] of
         'A' : begin VDLat := VDLat -1/32; VDLon := VDLon +1/48 end;
         'B' : begin VDLat := VDLat +1/32; VDLon := VDLon +1/48 end;
         'C' : begin VDLat := VDLat -1/32; VDLon := VDLon -1/48 end;
         'D' : begin VDLat := VDLat +1/32; VDLon := VDLon -1/48 end;
         end;
        end;

        sname := sname +'-'+ temp_string;
        V2Search := copy(V2Search,i,length(V2Search)-i+1);
        if length(V2Search)>0 then
         if copy(V2Search,1,1)='-' then  V2Search := copy(V2Search,2,length(V2Search)-1);
       end; // ШЕСТОЕ ПОЛЕ

       if not VcoordError then begin // добавляем таки точку если всё ок
         VPoint.Y:=VDLon;
         VPoint.X:=VDLat;
         if (abs(VPoint.y)<=90)and(abs(VPoint.x)<=180) then begin
          sdesc := '[ '+VValueConverter.LonLatConvert(VPoint)+' ]';
          sfulldesc :=  ReplaceStr( sname + #$D#$A+ sdesc,#$D#$A,'<br>');
          VPlace := PlacemarkFactory.Build(VPoint, sname, sdesc, sfulldesc, 4);
          AAList.Add(VPlace);
          Result := true;
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
 V2Search : string;
 i,j : integer;
 VPlace : IVectorDataItemPoint;
 VPoint : TDoublePoint;
 sname, sdesc, sfulldesc : string;
 slat , slon : string;
 VZoom : integer;
 XYPoint:TPoint;
 XYRect:TRect;
 ViLat, ViLon : integer;
 VcoordError: boolean;
 VList: IInterfaceListSimple;
 VValueConverter: IValueToStringConverter;
begin
 VValueConverter := FValueToStringConverterConfig.GetStatic;
 VList := TInterfaceListSimple.Create;
 if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
   Exit;
 end;
 VcoordError := true;
 V2Search := Trim(AnsiUpperCase(ASearch));
 V2Search := ReplaceStr(V2Search,', ',' '); // разделители
 V2Search := ReplaceStr(V2Search,' .',' '); // разделители
 V2Search := ReplaceStr(V2Search,'%2C',' '); // разделители
 V2Search := ReplaceStr(V2Search,'#8243;','"'); // разделители
 V2Search := ReplaceStr(V2Search,'#8242;',''''); // разделители
 V2Search := ReplaceStr(V2Search,'&',' '); // разделители
 V2Search := ReplaceStr(V2Search,';',' '); // разделители
 V2Search := ReplaceStr(V2Search,'#',' '); // разделители
 V2Search := RegExprReplaceMatchSubStr(V2Search,'ШИРОТ(А|Ы)','N ');
 V2Search := RegExprReplaceMatchSubStr(V2Search,'ДОЛГОТ(А|Ы)','E ');

 if SubstrCount(',',V2Search,i)=1 then V2Search := ReplaceStr(V2Search,',',' '); // 11.22,33.44
 while PosEx('  ',V2Search, 1)>1 do V2Search := ReplaceStr(V2Search,'  ',' ');// убираем двойные пробелы
 i:=SubstrCount(' ',V2Search,j); // считаем количество пробелов и последнее вхождение

 VZoom := 0;
 ViLat := 0;
 ViLon := 0;
 if i = 1 then begin// один пробел
    if j > 1 then begin // пробел дальше чем первый символ
      slat := Copy(V2Search,1,j-1); //первая половина
      slon := Copy(V2Search,j,Length(V2Search)-j+1); // вторая половина
      if not PosStr2List(slat,slon,VList) then VList := nil;
     end;
  end
  else
  if i=0 then begin // 0 пробелов - путь к тайлу?
   if ((copy(V2Search,1,1)>='A')and(copy(V2Search,1,1)<='Z')and(copy(V2Search,2,1)=':') )
   or (copy(V2Search,1,2)='\\')or (copy(V2Search,1,1)='.') then
   begin
       i := PosEx('\Z', V2Search, 1);
       if i>0 then begin
        j := PosEx('\', V2Search, i+1);
        slat := Copy(V2Search, i + 2, j - (i + 2));
        try
          VZoom := strtoint(slat);
        except
          VcoordError := true;
        end;
       end;
       if  PosEx('.SDB', V2Search, 1)>0 then begin   //g:\cache_dbv\sat_all_v1\z18\78\46\314.186.sdbv
         i := PosEx('\Z', V2Search, 1);
         j := PosEx('\', V2Search, i+1);
         i := j+1;
         j := PosEx('\', V2Search, i);
         i := j+1;
         j := PosEx('\', V2Search, i);
         i := j+1;
         j := PosEx('.', V2Search, i);
         slon := Copy(V2Search, i , j - (i));
         Vilon := strtoint(slon) * 256;
         i := j+1;
         j := PosEx('.', V2Search, i);
         slat := Copy(V2Search, i , j - (i));
         Vilat :=  strtoint(slat) * 256;
         VcoordError := false;
         end else
       if PosEx('\X', V2Search, 1)>0 then begin   //G:\GoogleMV\cache\yamapng\z13\2\x2491\1\y1473.png
         i := PosEx('\X', V2Search, j); // X значение
         j := PosEx('\', V2Search, i+1);
         slon := Copy(V2Search, i + 2, j - (i + 2));
         Vilon := strtoint(slon);
         i := PosEx('\Y', V2Search, j); // Y значение
         j := PosEx('.', V2Search, i+1);
         slat := Copy(V2Search, i + 2, j - (i + 2));
         Vilat := strtoint(slat);
         VcoordError := false;
         end else
       if PosEx('\Z', V2Search, 1)>0 then begin // C:\sas\sas_garl\.bin\cache_gmt\genshtab250m\z9\184\319.jpg
         i := PosEx('\', V2Search, j); // X значение
         j := PosEx('\', V2Search, i+1);
         slon := Copy(V2Search, i + 1, j - (i + 1));
         Vilat := strtoint(slon);
         i := j; // Y значение
         j := PosEx('.', V2Search, i+1);
         slat := Copy(V2Search, i + 1, j - (i + 1));
         Vilon := strtoint(slat);
         inc(VZoom); // в GMT зум отличается на 1
         VcoordError := false;
       end ;

       if VcoordError then begin // C:\.bin\cache_old\sat\13\trtqsstrrqqtq.jpg
         ViLat := 0;
         ViLon := 0;
         VZoom := 1;
         vcoorderror := false;
         V2Search := ReplaceStr(V2Search,'0','Q');
         V2Search := ReplaceStr(V2Search,'1','R');
         V2Search := ReplaceStr(V2Search,'2','S');
         V2Search := ReplaceStr(V2Search,'3','T');
         i:=SubstrCount('\',V2Search,j);// последний \ перед qrst
         j:=PosEx('\T', V2Search, j)+2;
         while (V2Search[j]<>'.') and (not VcoordError )do begin
            ViLon := ViLon *2;
            ViLat := ViLat *2;
            case V2Search[j] of
               'Q' : begin ViLon := ViLon + 0 ; ViLat := ViLat + 0 end;
               'R' : begin ViLon := ViLon + 1 ; ViLat := ViLat + 0 end;
               'S' : begin ViLon := ViLon + 1 ; ViLat := ViLat + 1 end;
               'T' : begin ViLon := ViLon + 0 ; ViLat := ViLat + 1 end;
               else
               VcoordError := true;
            end;
            inc(j);
            inc(VZoom);
         end;
       end;

       if not VcoordError then begin
        XYPoint.X:=ViLon;
        XYPoint.Y:=ViLat;
        sdesc := 'z='+inttostr(VZoom)+' x='+inttostr(Vilon)+' y='+inttostr(Vilat)+#10#13;
        XYRect := ALocalConverter.GetGeoConverter.TilePos2PixelRect(XYPoint,VZoom-1);
        XYPoint := Point((XYRect.Right+XYRect.Left)div 2,(XYRect.Bottom+XYRect.top)div 2);
        VPoint := ALocalConverter.GetGeoConverter.PixelPos2LonLat(XYPoint,VZoom-1);
        if (abs(VPoint.y)<=90)and(abs(VPoint.x)<=180) then begin
         sname := ASearch;
         sdesc := '[ '+VValueConverter.LonLatConvert(VPoint)+' ]';
         sfulldesc :=  ReplaceStr( sname + #$D#$A+ sdesc,#$D#$A,'<br>');
         VPlace := PlacemarkFactory.Build(VPoint, sname, sdesc, sfulldesc, 4);
         VList.Add(VPlace);
         end;
    end;
   end else
      begin      //0 пробелов  и не диск\сеть  ==  Генштаб???
        if not GenShtab2Pos(V2Search, VList) then VList := nil;
      end //0 пробелов  и не диск\сеть  и не Генштаб
  end else
  if i=2 then begin // 2 пробела

  end else
  if i=3 then begin // 3 пробела
      j := PosEx(' ',V2Search, 1)+1;
      j := PosEx(' ',V2Search, j);
      slat := Copy(V2Search,1,j-1); //первая половина
      slon := Copy(V2Search,j+1,Length(V2Search)-j+1); // вторая половина
      if not PosStr2List(slat,slon,VList) then VList := nil;
  end else
  if i=5 then begin // 5 пробелов
      j := PosEx(' ',V2Search, 1)+1;
      j := PosEx(' ',V2Search, j)+1;
      j := PosEx(' ',V2Search, j);
      slat := Copy(V2Search,1,j-1); //первая половина
      slon := Copy(V2Search,j+1,Length(V2Search)-j+1); // вторая половина
      if not PosStr2List(slat,slon,VList) then VList := nil;
  end ;

  Result := VList;
end;

end.
