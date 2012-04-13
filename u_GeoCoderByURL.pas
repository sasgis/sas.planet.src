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

unit u_GeoCoderByURL;

interface

uses
  Classes,
  u_GeoCoderBasic,
  i_GeoCoder;


type
  TGeoCoderByURL = class(TGeoCoderBasic)
  protected
    function PrepareURL(const ASearch: WideString): string; override;
    function ParseStringToPlacemarksList(const AStr: string; const ASearch: WideString): IInterfaceList; override;
  public
  end;

implementation

uses
  SysUtils,
  StrUtils,
  RegExprUtils,
  t_GeoTypes,
  u_ResStrings,
  u_GeoCodePlacemark,
  ALHTTPCommon,
  ALHttpClient,
  ALWinInetHttpClient,
  i_InetConfig,
  i_ProxySettings,
  u_GlobalState,
  u_GeoToStr,
  windows,
  Math,
  i_LocalCoordConverter;

{ TGeoCoderByExtLink }

function DoHttpRequest(const ARequestUrl, ARequestHeader, APostData: string; out AResponseHeader, AResponseData: string): Cardinal;
var
  VHttpClient: TALWinInetHTTPClient;
  VHttpResponseHeader: TALHTTPResponseHeader;
  VHttpResponseBody: TMemoryStream;
  VHttpPostData: TMemoryStream;
  VInetConfig: IInetConfigStatic;
  VProxyConfig: IProxyConfigStatic;
  VTmp:TStringList;
begin
  try
    VHttpClient := TALWinInetHTTPClient.Create(nil);
    try
      VHttpResponseHeader := TALHTTPResponseHeader.Create;
      try
        // config
        VInetConfig := GState.InetConfig.GetStatic;
        VHttpClient.RequestHeader.RawHeaderText := ARequestHeader;
        VHttpClient.RequestHeader.Accept := '*/*';
        VHttpClient.ConnectTimeout := VInetConfig.TimeOut;
        VHttpClient.SendTimeout := VInetConfig.TimeOut;
        VHttpClient.ReceiveTimeout := VInetConfig.TimeOut;
        VHttpClient.InternetOptions := [  wHttpIo_No_cache_write,
                                          wHttpIo_Pragma_nocache,
                                          wHttpIo_No_cookies,
                                          wHttpIo_Ignore_cert_cn_invalid,
                                          wHttpIo_Ignore_cert_date_invalid,
                                          wHttpIo_No_auto_redirect
                                       ];
        VProxyConfig := VInetConfig.ProxyConfigStatic;
        if Assigned(VProxyConfig) then begin
          if VProxyConfig.UseIESettings then begin
            VHttpClient.AccessType := wHttpAt_Preconfig
          end else if VProxyConfig.UseProxy then begin
            VHttpClient.AccessType := wHttpAt_Proxy;
            VHttpClient.ProxyParams.ProxyServer :=
              Copy(VProxyConfig.Host, 0, Pos(':', VProxyConfig.Host) - 1);
            VHttpClient.ProxyParams.ProxyPort :=
              StrToInt(Copy(VProxyConfig.Host, Pos(':', VProxyConfig.Host) + 1));
            if VProxyConfig.UseLogin then begin
              VHttpClient.ProxyParams.ProxyUserName := VProxyConfig.Login;
              VHttpClient.ProxyParams.ProxyPassword := VProxyConfig.Password;
            end;
          end else begin
            VHttpClient.AccessType := wHttpAt_Direct;
          end;
        end;
        // request
        VHttpResponseBody := TMemoryStream.Create;
        try
          VTmp := TStringList.Create;
          try
            if APostData <> '' then begin
              VHttpPostData := TMemoryStream.Create;
              try
                VHttpPostData.Position := 0;
                VTmp.Text := APostData;
                VTmp.SaveToStream(VHttpPostData);
                VHttpClient.Post(ARequestUrl, VHttpPostData, VHttpResponseBody, VHttpResponseHeader);
              finally
                VHttpPostData.Free;
              end;
            end else begin
              VHttpClient.Get(ARequestUrl, VHttpResponseBody, VHttpResponseHeader);
            end;
            Result := StrToIntDef(VHttpResponseHeader.StatusCode, 0);
            AResponseHeader := VHttpResponseHeader.RawHeaderText;
            if VHttpResponseBody.Size > 0 then begin
              VHttpResponseBody.Position := 0;
              VTmp.Clear;
              VTmp.LoadFromStream(VHttpResponseBody);
              AResponseData := VTmp.Text;
            end;
          finally
            AResponseHeader := VHttpResponseHeader.RawHeaderText; // save redirect header
            VTmp.Free;
          end;
        finally
          VHttpResponseBody.Free;
        end;
      finally
        VHttpResponseHeader.Free;
      end;
    finally
      VHttpClient.Free;
    end;
  except
    on E: EALHTTPClientException do begin
      Result := E.StatusCode;
      AResponseData := E.Message;
    end;
    on E: EOSError do begin
      Result := E.ErrorCode;
      AResponseHeader := '';
      AResponseData := E.Message;
    end;
  end;
end;

function RomanToDig(astr:string):integer;
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

procedure meters_to_lonlat( in_x,in_y : Double; var out_x, out_y : string);
const
 pi = 3.1415926535897932384626433832795;
begin
  out_X := floattostr(in_X/6378137*180/pi);
  out_Y := floattostr(((arctan(exp(in_Y/6378137))-pi/4)*360)/pi);
end;

function SubstrCount(const A_Substr,A_String:string; var LastPos:integer):integer;
var i:integer;
begin
Result := 0;
if (A_substr<>'') and (Length(A_Substr)<Length(A_String)) then
 for I := 0 to length(A_string)-length(A_Substr) do
   if copy(A_String,i,length(A_substr))=A_substr then begin
   inc(Result);
   LastPos := i;
   end;
end;


function Str2Degree(const AStr:string; var llat,llon:boolean; Var res:Double):Boolean;
var
  i : integer;
 delitel : single;
  gms : double;
  text : string;
  minus:boolean;
begin
    result:=true;
    res:=0;
    text := Trim(AnsiUpperCase(Astr));
    llat := false;
    llon := false;

    if PosEx('W', Text, 1) > 0 then llon := true;
    if PosEx('E', Text, 1) > 0 then llon := true;
    if PosEx('З', Text, 1) > 0 then llon := true;
    if PosEx('В', Text, 1) > 0 then llon := true;
    if PosEx('LON', Text, 1) > 0 then llon := true;
    if PosEx('LN', Text, 1) > 0 then llon := true;
    Text := ReplaceStr(Text,'LON','');
    Text := ReplaceStr(Text,'LN','');

    if PosEx('S', Text, 1) > 0 then llat := true;
    if PosEx('N', Text, 1) > 0 then llat := true;
    if PosEx('Ю', Text, 1) > 0 then llat := true;
    if PosEx('С', Text, 1) > 0 then llat := true;
    if PosEx('LAT', Text, 1) > 0 then llat := true;
    if PosEx('LL', Text, 1) > 0 then llat := true;
    Text := ReplaceStr(Text,'LAT','');
    Text := ReplaceStr(Text,'LL','');

    Text := ReplaceStr(Text,'Ш.','');
    Text := ReplaceStr(Text,'Ш','');
    Text := ReplaceStr(Text,'Д.','');
    Text := ReplaceStr(Text,'Д','');
    Text := ReplaceStr(Text,'=','');

    text := StringReplace(text,'S','-',[rfReplaceAll]);
    text := StringReplace(text,'W','-',[rfReplaceAll]);
    text := StringReplace(text,'N','+',[rfReplaceAll]);
    text := StringReplace(text,'E','+',[rfReplaceAll]);
    text := StringReplace(text,'Ю','-',[rfReplaceAll]);
    text := StringReplace(text,'З','-',[rfReplaceAll]);
    text := StringReplace(text,'В','+',[rfReplaceAll]);
    text := StringReplace(text,'С','+',[rfReplaceAll]);
    minus:= false;
    if posEx('-',text,1)>0 then minus := true;

    if (copy(text,length(text),1)='.') then text := copy(text,1,length(text)-1);
    if (copy(text,length(text),1)=',') then text := copy(text,1,length(text)-1);
    if (copy(text,length(text),1)='+') or (copy(text,length(text),1)='-') then text:=copy(text,length(text),1)+copy(text,0,length(text)-1);

    if PosEx('+-', Text, 1) > 0 then begin // WE123 NS123
     llon := true;
     llat := true;
     Text := ReplaceStr(Text,'+-','+');
    end;
    if PosEx('-+', Text, 1) > 0 then begin // EW123 SN123
     llon := true;
     llat := true;
     Text := ReplaceStr(Text,'-+','-');
    end;
    if PosEx('--', Text, 1) > 0 then begin // -123S
     Text := ReplaceStr(Text,'--','-');
    end;

  i:=1;
  while i<=length(text) do begin
    if (not(text[i] in ['0'..'9','-','+','.',',',' '])) then begin
      text[i]:=' ';
      dec(i);
    end;

    if ((i=1)and(text[i]=' '))or
       ((i=length(text))and(text[i]=' '))or
       ((i<length(text)-1)and(text[i]=' ')and(text[i+1]=' '))or
       ((i>1) and (text[i]=' ') and (not(text[i-1] in ['0'..'9'])))or
       ((i<length(text)-1)and(text[i]=',')and(text[i+1]=' ')) then begin
      Delete(text,i,1);
      dec(i);
    end;
    inc(i);
  end;
  try
    res:=0;
    delitel:=1;
    repeat
     i:=posEx(' ',text,1);
     if i=0 then begin
       gms:=str2r(text);
     end else begin
       gms:=str2r(copy(text,1,i-1));
       Delete(text,1,i);
     end;
     if ((delitel>1)and(abs(gms)>60))or
        ((delitel=1)and(llat)and(abs(gms)>90))or
        ((delitel=1)and(not llat)and(abs(gms)>180)) then begin
      if (delitel=60) and (GMS>60) then begin //  37 6298475265502
         delitel := Power(10,length(text));
      end else Result:=false;
     end;
     if res<0 then begin
       res:=res-gms/delitel;
     end else begin
       res:=res+gms/delitel;
     end;
     if minus and (res>0) then res:=-res;
     delitel:=delitel*60;
    until (i=0)or(delitel>3600)or(result=false);
  except
    result:=false;
  end;
end;

function deg2strvalue( aDeg:Double; Alat,NeedChar:boolean):string;
var
  Vmin :integer;
  VDegScale : Double;
begin
   VDegScale := abs(aDeg);
   result := IntToStr(Trunc(VDegScale)) + '°';
   VDegScale := Frac(VDegScale) * 60;
   Vmin := Trunc(VDegScale);
   if Vmin < 10 then begin
     result := result + '0' + IntToStr(Vmin) + '''';
   end else begin
     result := result + IntToStr(Vmin) + '''';
   end;
   VDegScale := Frac(VDegScale) * 60;
   result := result + FormatFloat('00.00', VDegScale) + '"';

   if NeedChar then
    if Alat then begin
    if aDeg>0 then Result := 'N'+ Result else Result := 'S'+ Result ;
    end else
    if aDeg>0 then Result := 'E'+ Result else Result := 'W'+ Result ;
end;


function PosStr2List(const Apos1,Apos2:string;var AAList:IInterfaceList) : boolean;
var
 VBLat1, VBlon1: boolean;
 VBLat2, VBlon2: boolean;
 VDLat, VDLon : Double;
 VPlace : IGeoCodePlacemark;
 VPoint : TDoublePoint;
 sname, sdesc, sfulldesc : string;
 vCounter : Integer;
begin
 result :=true;
 vCounter:=0;
 Str2Degree(apos1, VBlat1, VBlon1, VDlat);
 if VBLat1 and VBLon1 then begin Vblat1 := false ; VBLon1 := false end; // если указано 123NE
      Str2Degree(apos2, VBLat2, VBlon2, VDlon);
      if VBLat2 and VBLon2 then begin Vblat2 := false ; VBLon2 := false end;

      if VBLat1 and VBLat2 then begin Vblat1 := false ; VBLat2 := false end;
      if VBLon1 and VBLon2 then begin Vblon1 := false ; VBLon2 := false end;

      if VBlat1 then VPoint.Y := VDLat ;
      if VBlon1 then VPoint.X := VDLat ;
      if VBlat2 then VPoint.Y := VDLon ;
      if VBlon2 then VPoint.X := VDLon ;

      if (VBLat1 and VBLon2)or(VBLat2 and VBLon1) then begin // точно определили всего одну пару
        sname := apos1+' '+apos2;
        if (abs(VPoint.y)<=90)and(abs(VPoint.x)<=180) then begin
         if GState.ValueToStringConverterConfig.IsLatitudeFirst = true then
         sdesc := '[ '+deg2strvalue(VPoint.Y,true,true)+' '+deg2strvalue(VPoint.X,false,true)+' ]' else
          sdesc := '[ '+deg2strvalue(VPoint.X,false,true)+' '+deg2strvalue(VPoint.Y,true,true)+' ]';
         VPlace := TGeoCodePlacemark.Create(VPoint, sname, sdesc, sfulldesc, 4);
         AAList.Add(VPlace);
         result:=true;
        end;
      end;

      if ((VBLat1 or VBLat2 )and not (VBlon2 or VBLon1))then begin // определена широта и косяк с долготой
        if VBlat1 then VPoint.X := VDLon ;
        if VBlat2 then VPoint.X := VDLat ;

        if (abs(VPoint.y)<=90)and(abs(VPoint.x)<=180) then
         if not( ((VDLAT<0) or (VDLON<0)) and (VPoint.y >0)and (VPoint.X >0) or
         ((VDLAT<0)and(VDLON<0)and((VPoint.y >0)or(VPoint.X >0)))) then begin
          inc(Vcounter);sname := inttostr(vcounter)+'.) '+apos1+' '+apos2;
          if GState.ValueToStringConverterConfig.IsLatitudeFirst = true then
          sdesc := '[ '+deg2strvalue(VPoint.Y,true,true)+' '+deg2strvalue(VPoint.X,false,true)+' ]' else
           sdesc := '[ '+deg2strvalue(VPoint.X,false,true)+' '+deg2strvalue(VPoint.Y,true,true)+' ]';
          VPlace := TGeoCodePlacemark.Create(VPoint, sname, sdesc, sfulldesc, 4);
          AAList.Add(VPlace);
          result:=true;
        end;

        VPoint.X := -VPoint.X ;

        if (abs(VPoint.y)<=90)and(abs(VPoint.x)<=180) then
         if not( ((VDLAT<0) or (VDLON<0)) and (VPoint.y >0)and (VPoint.X >0) or
         ((VDLAT<0)and(VDLON<0)and((VPoint.y >0)or(VPoint.X >0)))) then begin
          inc(Vcounter);sname := inttostr(vcounter)+'.) '+apos1+' '+apos2;
          if GState.ValueToStringConverterConfig.IsLatitudeFirst = true then
          sdesc := '[ '+deg2strvalue(VPoint.Y,true,true)+' '+deg2strvalue(VPoint.X,false,true)+' ]' else
           sdesc := '[ '+deg2strvalue(VPoint.X,false,true)+' '+deg2strvalue(VPoint.Y,true,true)+' ]';
          VPlace := TGeoCodePlacemark.Create(VPoint, sname, sdesc, sfulldesc, 4);
          AAList.Add(VPlace);
          result:=true;
        end;
      end;

      if ((VBLon1 or VBLon2 )and not (VBlat2 or VBLat1))then begin // определена долгота и косяк с широтой
        if VBlon1 then VPoint.Y := VDLon ;
        if VBlon2 then VPoint.Y := VDLat ;

        if (abs(VPoint.y)<=90)and(abs(VPoint.x)<=180) then
         if not( ((VDLAT<0) or (VDLON<0)) and (VPoint.y >0)and (VPoint.X >0) or
         ((VDLAT<0)and(VDLON<0)and((VPoint.y >0)or(VPoint.X >0)))) then begin
          inc(Vcounter);sname := inttostr(vcounter)+'.) '+apos1+' '+apos2;
          if GState.ValueToStringConverterConfig.IsLatitudeFirst = true then
          sdesc := '[ '+deg2strvalue(VPoint.Y,true,true)+' '+deg2strvalue(VPoint.X,false,true)+' ]' else
           sdesc := '[ '+deg2strvalue(VPoint.X,false,true)+' '+deg2strvalue(VPoint.Y,true,true)+' ]';
          VPlace := TGeoCodePlacemark.Create(VPoint, sname, sdesc, sfulldesc, 4);
          AAList.Add(VPlace);
          result:=true;
        end;

        VPoint.Y := -VPoint.Y ;

        if (abs(VPoint.y)<=90)and(abs(VPoint.x)<=180) then
         if not( ((VDLAT<0) or (VDLON<0)) and (VPoint.y >0)and (VPoint.X >0) or
         ((VDLAT<0)and(VDLON<0)and((VPoint.y >0)or(VPoint.X >0)))) then begin
          inc(Vcounter);sname := inttostr(vcounter)+'.) '+apos1+' '+apos2;
          if GState.ValueToStringConverterConfig.IsLatitudeFirst = true then
          sdesc := '[ '+deg2strvalue(VPoint.Y,true,true)+' '+deg2strvalue(VPoint.X,false,true)+' ]' else
           sdesc := '[ '+deg2strvalue(VPoint.X,false,true)+' '+deg2strvalue(VPoint.Y,true,true)+' ]';
          VPlace := TGeoCodePlacemark.Create(VPoint, sname, sdesc, sfulldesc, 4);
          AAList.Add(VPlace);
          result:=true;
        end;
      end;

     if VBLat1 or VBLat2 or VBLon1 or Vblon2 = False then begin // все 4 координаты не заданы конкретно

        if GState.ValueToStringConverterConfig.IsLatitudeFirst = true then
        begin
         VPoint.X := VDLon ; VPoint.Y := VDLat ;
        end else begin
         VPoint.Y := VDLon ; VPoint.X := VDLat ;
        end;

        if (abs(VPoint.y)<=90)and(abs(VPoint.x)<=180) then
         if not( ((VDLAT<0) or (VDLON<0)) and (VPoint.y >0)and (VPoint.X >0) or
         ((VDLAT<0)and(VDLON<0)and((VPoint.y >0)or(VPoint.X >0)))) then begin
          inc(Vcounter);sname := inttostr(vcounter)+'.) '+apos1+' '+apos2;
          if GState.ValueToStringConverterConfig.IsLatitudeFirst = true then
          sdesc := '[ '+deg2strvalue(VPoint.Y,true,true)+' '+deg2strvalue(VPoint.X,false,true)+' ]' else
           sdesc := '[ '+deg2strvalue(VPoint.X,false,true)+' '+deg2strvalue(VPoint.Y,true,true)+' ]';
          VPlace := TGeoCodePlacemark.Create(VPoint, sname, sdesc, sfulldesc, 4);
          AAList.Add(VPlace);
          result:=true;
        end;

        VPoint.Y := -VPoint.Y ;

        if (abs(VPoint.y)<=90)and(abs(VPoint.x)<=180) then
         if not( ((VDLAT<0) or (VDLON<0)) and (VPoint.y >0)and (VPoint.X >0) or
         ((VDLAT<0)and(VDLON<0)and((VPoint.y >0)or(VPoint.X >0)))) then begin
          inc(Vcounter);sname := inttostr(vcounter)+'.) '+apos1+' '+apos2;
          if GState.ValueToStringConverterConfig.IsLatitudeFirst = true then
          sdesc := '[ '+deg2strvalue(VPoint.Y,true,true)+' '+deg2strvalue(VPoint.X,false,true)+' ]' else
           sdesc := '[ '+deg2strvalue(VPoint.X,false,true)+' '+deg2strvalue(VPoint.Y,true,true)+' ]';
          VPlace := TGeoCodePlacemark.Create(VPoint, sname, sdesc, sfulldesc, 4);
          AAList.Add(VPlace);
          result:=true;
        end;

        VPoint.Y := -VPoint.Y ;
        VPoint.X := -VPoint.X ;

        if (abs(VPoint.y)<=90)and(abs(VPoint.x)<=180) then
         if not( ((VDLAT<0) or (VDLON<0)) and (VPoint.y >0)and (VPoint.X >0) or
         ((VDLAT<0)and(VDLON<0)and((VPoint.y >0)or(VPoint.X >0)))) then begin
          inc(Vcounter);sname := inttostr(vcounter)+'.) '+apos1+' '+apos2;
          if GState.ValueToStringConverterConfig.IsLatitudeFirst = true then
          sdesc := '[ '+deg2strvalue(VPoint.Y,true,true)+' '+deg2strvalue(VPoint.X,false,true)+' ]' else
           sdesc := '[ '+deg2strvalue(VPoint.X,false,true)+' '+deg2strvalue(VPoint.Y,true,true)+' ]';
          VPlace := TGeoCodePlacemark.Create(VPoint, sname, sdesc, sfulldesc, 4);
          AAList.Add(VPlace);
          result:=true;
        end;

        VPoint.Y := -VPoint.Y ;

        if (abs(VPoint.y)<=90)and(abs(VPoint.x)<=180) then
         if not( ((VDLAT<0) or (VDLON<0)) and (VPoint.y >0)and (VPoint.X >0) or
         ((VDLAT<0)and(VDLON<0)and((VPoint.y >0)or(VPoint.X >0)))) then begin
          inc(Vcounter);sname := inttostr(vcounter)+'.) '+apos1+' '+apos2;
          if GState.ValueToStringConverterConfig.IsLatitudeFirst = true then
          sdesc := '[ '+deg2strvalue(VPoint.Y,true,true)+' '+deg2strvalue(VPoint.X,false,true)+' ]' else
           sdesc := '[ '+deg2strvalue(VPoint.X,false,true)+' '+deg2strvalue(VPoint.Y,true,true)+' ]';
          VPlace := TGeoCodePlacemark.Create(VPoint, sname, sdesc, sfulldesc, 4);
          AAList.Add(VPlace);
          result:=true;
        end;
//  и наоборот
      if vdlat <> vdlon  then begin

        if GState.ValueToStringConverterConfig.IsLatitudeFirst = true then
        begin
         VPoint.Y := VDLon ; VPoint.X := VDLat ;
        end else begin
         VPoint.X := VDLon ; VPoint.Y := VDLat
        end;

        if (abs(VPoint.y)<=90)and(abs(VPoint.x)<=180) then
         if not( ((VDLAT<0) or (VDLON<0)) and (VPoint.y >0)and (VPoint.X >0) or
         ((VDLAT<0)and(VDLON<0)and((VPoint.y >0)or(VPoint.X >0)))) then begin
          inc(Vcounter);sname := inttostr(vcounter)+'.) '+apos1+' '+apos2;
          if GState.ValueToStringConverterConfig.IsLatitudeFirst = true then
          sdesc := '[ '+deg2strvalue(VPoint.Y,true,true)+' '+deg2strvalue(VPoint.X,false,true)+' ]' else
           sdesc := '[ '+deg2strvalue(VPoint.X,false,true)+' '+deg2strvalue(VPoint.Y,true,true)+' ]';
          VPlace := TGeoCodePlacemark.Create(VPoint, sname, sdesc, sfulldesc, 4);
          AAList.Add(VPlace);
          result:=true;
        end;

        VPoint.Y := -VPoint.Y ;

        if (abs(VPoint.y)<=90)and(abs(VPoint.x)<=180) then
         if not( ((VDLAT<0) or (VDLON<0)) and (VPoint.y >0)and (VPoint.X >0) or
         ((VDLAT<0)and(VDLON<0)and((VPoint.y >0)or(VPoint.X >0)))) then begin
          inc(Vcounter);sname := inttostr(vcounter)+'.) '+apos1+' '+apos2;
          if GState.ValueToStringConverterConfig.IsLatitudeFirst = true then
          sdesc := '[ '+deg2strvalue(VPoint.Y,true,true)+' '+deg2strvalue(VPoint.X,false,true)+' ]' else
           sdesc := '[ '+deg2strvalue(VPoint.X,false,true)+' '+deg2strvalue(VPoint.Y,true,true)+' ]';
          VPlace := TGeoCodePlacemark.Create(VPoint, sname, sdesc, sfulldesc, 4);
          AAList.Add(VPlace);
          result:=true;
        end;

        VPoint.Y := -VPoint.Y ;
        VPoint.X := -VPoint.X ;

        if (abs(VPoint.y)<=90)and(abs(VPoint.x)<=180) then
         if not( ((VDLAT<0) or (VDLON<0)) and (VPoint.y >0)and (VPoint.X >0) or
         ((VDLAT<0)and(VDLON<0)and((VPoint.y >0)or(VPoint.X >0)))) then begin
          inc(Vcounter);sname := inttostr(vcounter)+'.) '+apos1+' '+apos2;
          if GState.ValueToStringConverterConfig.IsLatitudeFirst = true then
          sdesc := '[ '+deg2strvalue(VPoint.Y,true,true)+' '+deg2strvalue(VPoint.X,false,true)+' ]' else
           sdesc := '[ '+deg2strvalue(VPoint.X,false,true)+' '+deg2strvalue(VPoint.Y,true,true)+' ]';
          VPlace := TGeoCodePlacemark.Create(VPoint, sname, sdesc, sfulldesc, 4);
          AAList.Add(VPlace);
          result:=true;
        end;

        VPoint.Y := -VPoint.Y ;

        if (abs(VPoint.y)<=90)and(abs(VPoint.x)<=180) then
         if not( ((VDLAT<0) or (VDLON<0)) and (VPoint.y >0)and (VPoint.X >0) or
         ((VDLAT<0)and(VDLON<0)and((VPoint.y >0)or(VPoint.X >0)))) then begin
          inc(Vcounter);sname := inttostr(vcounter)+'.) '+apos1+' '+apos2;
          if GState.ValueToStringConverterConfig.IsLatitudeFirst = true then
          sdesc := '[ '+deg2strvalue(VPoint.Y,true,true)+' '+deg2strvalue(VPoint.X,false,true)+' ]' else
           sdesc := '[ '+deg2strvalue(VPoint.X,false,true)+' '+deg2strvalue(VPoint.Y,true,true)+' ]';
          VPlace := TGeoCodePlacemark.Create(VPoint, sname, sdesc, sfulldesc, 4);
          AAList.Add(VPlace);
          result:=true;
        end;

      end;
 end;
end;

function GetListByText(const astr:string ; Var AList:IInterfaceList): boolean;
var
 Vtext, V2Search : string;
 VBLat1, VBlon1: boolean;
 VBLat2, VBlon2: boolean;
 VDLat, VDLon : Double;
 i,j : integer;
 VPlace : IGeoCodePlacemark;
 VPoint : TDoublePoint;
 sname, sdesc, sfulldesc : string;
 slat , slon : string;
 vCounter : Integer;
 vZoom : integer;
 VLocalConverter: ILocalCoordConverter;
 XYPoint:TPoint;
 XYRect:TRect;
 ViLat, ViLon : integer;
 VcoordError: boolean;
 temp_string :string;
 Vtext2: string;
begin
result :=true;
//vCounter:=0;
Vilon := 0;
ViLat := 0;
VZoom := 0;
VcoordError := false;
V2Search := Trim(AnsiUpperCase(astr));
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

  if i = 1 then begin// один пробел
    if j > 1 then begin // пробел дальше чем первый символ
      slat := Copy(V2Search,1,j-1); //первая половина
      slon := Copy(V2Search,j,Length(V2Search)-j+1); // вторая половина
      if PosStr2List(slat,slon,AList)=false then alist := nil;
     end;
  end else
  if i=0 then begin // 0 пробелов - путь к тайлу?
   if ((copy(V2Search,1,1)>='A')and(copy(V2Search,1,1)<='Z')and(copy(V2Search,2,1)=':') )
     or (copy(V2Search,1,2)='\\')or (copy(V2Search,1,1)='.') or (copy(V2Search,1,4)='HTTP') then
      begin
       i := PosEx('\Z', V2Search, 1);
       j := PosEx('\', V2Search, i+1);
       slat := Copy(V2Search, i + 2, j - (i + 2));
       try
         vZoom := strtoint(slat);
       except
         VcoordError := true;
       end;
       if  PosEx('.SDB', V2Search, 1)>0 then begin   // G:\GoogleMV\cache_db\Nokia.Map.Creator.sat\z15\9\5\38.23.sdb\x9961y5888.jpg
         i := PosEx('\X', V2Search, j); // X значение
         j := PosEx('\', V2Search, i+1);
         slon := Copy(V2Search, i + 2, j - (i + 2));
         Vilon := strtoint(slon);
         i := j+1;
         j := PosEx('.', V2Search, i+1);
         slat := Copy(V2Search, i + 1, j - (i + 1));
         Vilat :=  strtoint(slat);
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
       end else // http://a.tile.openstreetmap.org/15/19928/11707.png | http://otile4.mqcdn.com/tiles/1.0.0/osm/14/12358/5285.png
        if RegExprGetMatchSubStr(V2Search,'HTTP://.+(\.(OPENSTREETMAP|OPENCYCLEMAP)\.|OSM).+(/[0-9]+)+\.',0)<>''  then begin
         i := PosEx(RegExprGetMatchSubStr(V2Search,'/[0-9][0-9]/',0), V2Search, 1); // Z значение
         j := PosEx('/', V2Search, i+1);
         VZoom := (strtoint(Copy(V2Search, i + 1, j - (i + 1))));
         inc(VZoom); 
         i:= j;
         j := PosEx('/', V2Search, i+1);
         slon := Copy(V2Search, i + 1, j - (i + 1));
         Vilon := strtoint(slon);
         i:= j;
         j := PosEx('.', V2Search, i+1);
         slat := Copy(V2Search, i + 1, j - (i + 1));
         Vilat := strtoint(slat);
         VcoordError := false;
        end;


       if VcoordError then begin // C:\.bin\cache_old\sat\13\trtqsstrrqqtq.jpg
         ViLat := 0;
         ViLon := 0;
         vzoom := 1;
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
            inc(Vzoom);
         end;
       end;

       if VcoordError = false then begin
        VLocalConverter := GState.MainFormConfig.ViewPortState.GetVisualCoordConverter;
        XYPoint.X:=ViLon;
        XYPoint.Y:=ViLat;
        sdesc := 'z='+inttostr(vzoom)+' x='+inttostr(Vilon)+' y='+inttostr(Vilat)+#10#13;
        XYRect := VLocalConverter.GetGeoConverter.TilePos2PixelRect(XYPoint,VZoom-1);
        XYPoint := Point((XYRect.Right+XYRect.Left)div 2,(XYRect.Bottom+XYRect.top)div 2);
        VPoint := VLocalConverter.GetGeoConverter.PixelPos2LonLat(XYPoint,VZoom-1);
        if (abs(VPoint.y)<=90)and(abs(VPoint.x)<=180) then begin
         sname := astr;
         if GState.ValueToStringConverterConfig.IsLatitudeFirst = true then
           sdesc := sdesc + '[ '+deg2strvalue(VPoint.Y,true,true)+' '+deg2strvalue(VPoint.X,false,true)+' ]' else
           sdesc := sdesc + '[ '+deg2strvalue(VPoint.X,false,true)+' '+deg2strvalue(VPoint.Y,true,true)+' ]';
         VPlace := TGeoCodePlacemark.Create(VPoint, sname, sdesc, sfulldesc, 4);
         AList.Add(VPlace);
         end;
        end;
      end else
      begin      //0 пробелов  и не диск\сеть  ==  Генштаб???
       // X-XX-XXX-X-X-X
       // C-II-III-C-C-I  char/integer
       // C-II-CCCCCC-C-C-C
       // K-37-XXXVI - 2х километровка - отдельная история RomanToDig()

       VcoordError := false;
       VDLon := 1;
       VDlat := 0;
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
            VDlon := VDlon*(ord(temp_string[1])-64)*4 -2 else VcoordError := true;
       if copy(V2Search,1,1)='-' then  V2Search := copy(V2Search,2,length(V2Search)-1); // убрали "-" если он был между разделителми

       if VcoordError=false then begin // ВТОРОЕ ПОЛЕ
         i := PosEx('-', V2Search, 1);
         if i=0 then  // не найден разделитель, может ввели К001 ?
         if length(v2search)>3 then VcoordError := true else begin
            i:=length(v2search)+1;
            end;

         if VcoordError=false then begin
           slat := copy(V2Search,1,i-1);
           VDLat := strtoint(slat)*6 - 180 -3;
           sname := sname + '-' + slat;
         end;
       V2Search := copy(V2Search,i,length(V2Search)-i+1);
       if length(V2Search)>0 then
         if copy(V2Search,1,1)='-' then  V2Search := copy(V2Search,2,length(V2Search)-1);
       end; // ВТОРОЕ ПОЛЕ


       if VcoordError=false then // ТРЕТЬЕ ПОЛЕ
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
        if VcoordError = false then
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
        if VcoordError = false then
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
       if VcoordError=false then // ЧЕТВЕРТОЕ ПОЛЕ
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
        end;

        sname := sname +'-'+ temp_string;
        V2Search := copy(V2Search,i,length(V2Search)-i+1);
        if length(V2Search)>0 then
         if copy(V2Search,1,1)='-' then  V2Search := copy(V2Search,2,length(V2Search)-1);
       end; // ЧЕТВЕРТОЕ ПОЛЕ

       // L-37-143-А-б.jpg
       if V2Search<>'' then
       if VcoordError=false then // ПЯТОЕ ПОЛЕ
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
       if VcoordError=false then // ШЕСТОЕ ПОЛЕ
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

       if VcoordError=false then begin // добавляем таки точку если всё ок
         VPoint.Y:=VDLon;
         VPoint.X:=VDLat;
         if (abs(VPoint.y)<=90)and(abs(VPoint.x)<=180) then begin
          if GState.ValueToStringConverterConfig.IsLatitudeFirst = true then
            sdesc := sdesc + '[ '+deg2strvalue(VPoint.Y,true,true)+' '+deg2strvalue(VPoint.X,false,true)+' ]' else
            sdesc := sdesc + '[ '+deg2strvalue(VPoint.X,false,true)+' '+deg2strvalue(VPoint.Y,true,true)+' ]';
          VPlace := TGeoCodePlacemark.Create(VPoint, sname, sdesc, sfulldesc, 4);
          AList.Add(VPlace);
          end;
         end;

      end //0 пробелов  и не диск\сеть  и не Генштаб
  end else
  if i=2 then begin // 2 пробела

  end else
  if i=3 then begin // 3 пробела
      j := PosEx(' ',V2Search, 1)+1;
      j := PosEx(' ',V2Search, j);
      slat := Copy(V2Search,1,j-1); //первая половина
      slon := Copy(V2Search,j+1,Length(V2Search)-j+1); // вторая половина
      if PosStr2List(slat,slon,AList)=false then alist := nil;
  end else
  if i=5 then begin // 5 пробелов
      j := PosEx(' ',V2Search, 1)+1;
      j := PosEx(' ',V2Search, j)+1;
      j := PosEx(' ',V2Search, j);
      slat := Copy(V2Search,1,j-1); //первая половина
      slon := Copy(V2Search,j+1,Length(V2Search)-j+1); // вторая половина
      if PosStr2List(slat,slon,AList)=false then alist := nil;
  end ;
end;

function TGeoCoderByURL.ParseStringToPlacemarksList(
  const AStr: string; const ASearch: WideString): IInterfaceList;
var
 VFormatSettings: TFormatSettings;
 VPlace : IGeoCodePlacemark;
 VPoint : TDoublePoint;
 slat, slon, sname, sdesc, sfulldesc : string;
 Vlink : string;
 VList: IInterfaceList;
 VLinkErr : boolean;
 vErrCode: cardinal;
 VHeader: string;
 i, j : integer;
begin
 VLinkErr := false;
 VList := TInterfaceList.Create;
 VFormatSettings.DecimalSeparator := '.';
 Vlink := ReplaceStr(ASearch,'%2C',',');
 vErrCode := 200;
 // http://maps.google.com/?ll=48.718079,44.504639&spn=0.722115,1.234589&t=h&z=10
 // http://maps.google.ru/maps?hl=ru&ll=43.460987,39.948606&spn=0.023144,0.038581&t=m&z=15&vpsrc=6
 if PosEx('maps.google.', Vlink, 1) > 0 then begin
  sname := 'Google';
  i := PosEx('ll', Vlink, 1);
  j := PosEx(',', Vlink, i);
  slat := Copy(Vlink, i + 3, j - (i + 3));
  i := j;
  j := PosEx('&', Vlink, i);
  slon := Copy(Vlink, i + 1, j - (i + 1));
  sdesc := '[ '+slon+' , '+slat+' ]';
  sfulldesc := Vlink;
 end else
  // http://maps.yandex.ru/?ll=44.514541%2C48.708958&spn=0.322723%2C0.181775&z=12&l=map
  // http://harita.yandex.com.tr/?ll=29.086777%2C41.000749&spn=0.005043%2C0.003328&z=18&l=sat%2Ctrf&trfm=cur
  if RegExprGetMatchSubStr(vlink,'\.yandex\..+/\?ll=',0)<>'' then begin
  sname := 'Yandex';
  i := PosEx('ll', Vlink, 1);
  j := PosEx(',', Vlink, i);
  slon := Copy(Vlink, i + 3, j - (i + 3));
  i := j;
  j := PosEx('&', Vlink, i);
  slat := Copy(Vlink, i + 1, j - (i + 1));
  sdesc := '[ '+slon+' , '+slat+' ]';
  sfulldesc := Vlink;
 end else
// http://maps.navitel.su/?zoom=16&lat=45.03446&lon=38.96867&fl=J&rId=hN21H5ByVER8e4A%3D&rp=5
 if copy(Vlink,1,22) = 'http://maps.navitel.su' then begin
  sname := 'Navitel';
  i := PosEx('lat=', Vlink, 1);
  j := PosEx('&', Vlink, i);
  slat := Copy(Vlink, i + 4, j - (i + 4));
  i := PosEx('lon=', Vlink, j);
  j := PosEx('&', Vlink, i);
  if j = 0 then j := length(Vlink) +1;

  slon := Copy(Vlink, i + 4, j - (i + 4));
  sdesc := '[ '+slon+' , '+slat+' ]';
  sfulldesc := Vlink;
 end else
// http://kosmosnimki.ru/?x=44.1053254382903&y=45.6876903573303&z=6&fullscreen=false&mode=satellite
 if copy(Vlink,1,23) = 'http://kosmosnimki.ru/?' then begin
  sname := 'Kosmosnimki';
  i := PosEx('x=', Vlink, 1);
  j := PosEx('&', Vlink, i);
  slon := Copy(Vlink, i + 2, j - (i + 2));
  i := PosEx('y=', Vlink, j);
  j := PosEx('&', Vlink, i);
  slat := Copy(Vlink, i + 2, j - (i + 2));
  sdesc := '[ '+slon+' , '+slat+' ]';
  sfulldesc := Vlink;
 end else
// http://www.bing.com/maps/default.aspx?v=2&cp=45.5493750107145~41.6883332507903&style=h&lvl=6
 if PosEx('bing.com', Vlink, 1) > 0 then begin
  sname := 'Bing';
  i := PosEx('cp=', Vlink, 1);
  j := PosEx('~', Vlink, i);
  slat := Copy(Vlink, i + 3, j - (i + 3));
  i := j;
  j :=PosEx('&', Vlink, i);
  slon := Copy(Vlink, i + 1, j - (i + 1));
  sdesc := '[ '+slon+' , '+slat+' ]';
  sfulldesc := Vlink;
 end  else
// http://www.openstreetmap.org/?lat=45.227&lon=39.001&zoom=10&layers=M
// http://osm.org.ru/#layer=M&zoom=3&lat=61.98&lon=88
 if  (RegExprGetMatchSubStr(Vlink,'openstreetmap\..+lat',0)<>'')
 or (RegExprGetMatchSubStr(Vlink,'osm\..+lat',0)<>'')then begin
  sname := 'OpenStreetMap';
  i := PosEx('lat=', Vlink, 1);
  j := PosEx('&', Vlink, i);
  slat := Copy(Vlink, i + 4, j - (i + 4));
  i := PosEx('lon=', Vlink, j);
  j := PosEx('&', Vlink, i);
  if j = 0 then j := length(Vlink) +1;
  slon := Copy(Vlink, i + 4, j - (i + 4));
  sdesc := '[ '+slon+' , '+slat+' ]';
  sfulldesc := Vlink;
 end  else
// http://wikimapia.org#lat=45.0328&lon=38.9769&z=10&l=1&m=b
 if PosEx('wikimapia.org', Vlink, 1) > 0 then begin
  sname := 'WikiMapia';
  i := PosEx('lat=', Vlink, 1);
  j := PosEx('&', Vlink, i);
  slat := Copy(Vlink, i + 4, j - (i + 4));
  i := PosEx('lon=', Vlink, j);
  j := PosEx('&', Vlink, i);
  slon := Copy(Vlink, i + 4, j - (i + 4));
  sdesc := '[ '+slon+' , '+slat+' ]';
  sfulldesc := Vlink;
 end  else
// http://maps.rosreestr.ru/Portal/?l=11&x=4595254.155000001&y=5398402.163800001&mls=map|anno&cls=cadastre
 if PosEx('maps.rosreestr.ru', Vlink, 1) > 0 then begin
  sname := 'Rosreestr';
  i := PosEx('x=', Vlink, 1);
  j := PosEx('&', Vlink, i);
  slon := Copy(Vlink, i + 2, j - (i + 2));
  i := PosEx('y=', Vlink, j);
  j := PosEx('&', Vlink, i);
  slat := Copy(Vlink, i + 2, j - (i + 2));
  meters_to_lonlat(StrToFloat(slon, VFormatSettings),StrToFloat(slat, VFormatSettings),slon,slat);
  slon := ReplaceStr(slon,',','.');
  slat := ReplaceStr(slat,',','.');
  sdesc := '[ '+slon+' , '+slat+' ]';
  sfulldesc := Vlink;
 end  else
// http://maps.mail.ru/?z=10&ll=37.619948,55.750023&j=1
 if PosEx('maps.mail.ru', Vlink, 1) > 0 then begin
  sname := 'Mail.ru';
  i := PosEx('ll=', Vlink, 1);
  j := PosEx(',', Vlink, i);
  slon := Copy(Vlink, i + 3, j - (i + 3));
  i := j;
  j := PosEx('&', Vlink, i);
  if j = 0 then j := length(Vlink) +1;
  slat := Copy(Vlink, i + 1, j - (i + 1));
  sdesc := '[ '+slon+' , '+slat+' ]';
  sfulldesc := Vlink;
 end  else
// http://maps.nokia.com/#|43.5669132|41.2836342|14|0|0|hybrid.day
// http://maps.nokia.com/mapcreator/?ns=true#|55.32530472503459|37.811186150077816|18|0|0|
 if PosEx('maps.nokia.com', Vlink, 1) > 0 then begin
  sname := 'Nokia';
  i := PosEx('#|', Vlink, 1);
  j := PosEx('|', Vlink, i+2);
  slat := Copy(Vlink, i + 2, j - (i + 2));
  i := j;
  j := PosEx('|', Vlink, i+1);
  if j = 0 then j := length(Vlink) +1;
  slon := Copy(Vlink, i + 1, j - (i + 1));
  sdesc := '[ '+slon+' , '+slat+' ]';
  sfulldesc := Vlink;
 end  else
// http://mobile.maps.yandex.net/ylocation/?lat=55.870155&lon=37.665367&desc=dima%40dzhus.org
 if PosEx('yandex.net', Vlink, 1) > 0 then begin
  sname := 'Yandex';
  i := PosEx('lat=', Vlink, 1);
  j := PosEx('&', Vlink, i+3);
  slat := Copy(Vlink, i + 4, j - (i + 4));
  i := PosEx('lon=', Vlink, j);
  j := PosEx('&', Vlink, i);
  if j = 0 then j := length(Vlink) +1;
  slon := Copy(Vlink, i + 4, j - (i + 4));
  sdesc := '[ '+slon+' , '+slat+' ]';
  sfulldesc := Vlink;
 end  else
//http://maps.2gis.ru/#/?history=project/krasnodar/center/38.993668%2C45.197055/zoom/17/state/index/sort/relevance
 if PosEx('maps.2gis.ru', Vlink, 1) > 0 then begin
  sname := '2Gis';
  i := PosEx('center/', Vlink, 1);
  j := PosEx(',', Vlink, i);
  slon := Copy(Vlink, i + 7, j - (i + 7));
  i:=j;
  j := PosEx('/', Vlink, i);
  if j = 0 then j := length(Vlink) +1;
  slat := Copy(Vlink, i + 1, j - (i + 1));
  sdesc := '[ '+slon+' , '+slat+' ]';
  sfulldesc := Vlink;
 end
 else  // short link
 if PosEx('http://g.co/', Vlink, 1) > 0then begin
  Vlink := Astr;
  sname := 'google';
  i := PosEx('ll', Vlink, 1);
  j := PosEx(',', Vlink, i);
  slat := Copy(Vlink, i + 3, j - (i + 3));
  i := j;
  j := PosEx('&', Vlink, i);
  slon := Copy(Vlink, i + 1, j - (i + 1));
  sdesc := '[ '+slon+' , '+slat+' ]';
  sfulldesc := ASearch;
 end else
 if (RegExprGetMatchSubStr(vlink,'\.yandex\..+/-/',0)<>'' ) or
    (PosEx('maps.yandex.ru/?oid=', Vlink, 1) > 0 ) then begin
  Vlink := ReplaceStr(astr,'''','');
  sname := 'yandex';
  i := PosEx('{ll:', Vlink, 1);
  if i=0 then i := PosEx(',ll:', Vlink, 1);
  j := PosEx(',', Vlink, i+1);
  slon := Copy(Vlink, i + 4, j - (i + 4));
  i := j;
  j := PosEx(',', Vlink, i+1);
  slat := Copy(Vlink, i + 1, j - (i + 1));
  sdesc := '[ '+slon+' , '+slat+' ]';
  sfulldesc := ASearch;
 end else
 if (PosEx('maps.yandex.ru/?um=', Vlink, 1) > 0 ) then begin // need 2 more test
  sname := 'yandex';
  Vlink := astr;
  i := PosEx('{''bounds'':[[', Vlink, 1);
  if i=0 then i := PosEx(',ll:', Vlink, 1);
  j := PosEx(',', Vlink, i+1);
  slon := Copy(Vlink, i + 12, j - (i + 12));
  i := j;
  j := PosEx(']', Vlink, i+1);
  slat := Copy(Vlink, i + 1, j - (i + 1));
  sdesc := '[ '+slon+' , '+slat+' ]';
  sfulldesc := ASearch;
 end else
 if PosEx('binged.it', Vlink, 1) > 0then begin
  Vlink := Astr;
  sname := 'bing';
  i := PosEx('cp=', Vlink, 1);
  j := PosEx('~', Vlink, i);
  slat := Copy(Vlink, i + 3, j - (i + 3));
  i := j;
  j := PosEx('&', Vlink, i);
  slon := Copy(Vlink, i + 1, j - (i + 1));
  sdesc := '[ '+slon+' , '+slat+' ]';
  sfulldesc := ASearch;
 end else
 if PosEx('osm.org', Vlink, 1) > 0then begin
  Vlink := Astr;
  sname := 'osm';
  i := PosEx('LonLat(', Vlink, 1);
  j := PosEx(',', Vlink, i);
  slon := Copy(Vlink, i + 7, j - (i + 7));
  i := j+1;
  j := PosEx(')', Vlink, i);
  slat := Copy(Vlink, i + 1, j - (i + 1));
  sdesc := '[ '+slon+' , '+slat+' ]';
  sfulldesc := ASearch;
 end else
 if PosEx('permalink.html', Vlink, 1) > 0then begin
  sdesc := 'http://kosmosnimki.ru/TinyReference.ashx?id='+Copy(vlink,38,9);
  VHeader := 'Referer: '+vlink+' Cookie: TinyReference='+Copy(vlink,38,9);
  Vlink := '';
  vErrCode := DoHttpRequest(sdesc, VHeader ,'',sname,Vlink);
  i := PosEx('"x":', Vlink, 1);
  j := PosEx(',', Vlink, i + 4 );
  slon := Copy(Vlink, i + 4, j - (i + 4));
  i := PosEx('"y":', Vlink, j);
  j := PosEx(',', Vlink, i + 4 );
  slat := Copy(Vlink, i + 4, j - (i + 4));
  sfulldesc := Vlink;
  sname := 'kosmosnimki';
  meters_to_lonlat(StrToFloat(slon, VFormatSettings),StrToFloat(slat, VFormatSettings),slon,slat);
  slon := ReplaceStr(slon,',','.');
  slat := ReplaceStr(slat,',','.');
  sdesc := '[ '+slon+' , '+slat+' ]';
  sfulldesc := ASearch;
 end else
 if PosEx('api/index.html?permalink=', Vlink, 1) > 0then begin
  slat := Copy(vlink,53,5);
  slon := Copy(vlink,59,5);
  sdesc := 'http://maps.kosmosnimki.ru/TinyReference/Get.ashx?id='+slat;
  VHeader := 'Referer: http://maps.kosmosnimki.ru/api/index.html?'+slon;
  Vlink := '';
  vErrCode := DoHttpRequest(sdesc, VHeader ,'',sname,Vlink);
  Vlink := ReplaceStr(Vlink,'\','');
  i := PosEx('"x":', Vlink, 1);
  j := PosEx(',', Vlink, i + 4 );
  slon := Copy(Vlink, i + 4, j - (i + 4));
  i := PosEx('"y":', Vlink, j);
  j := PosEx(',', Vlink, i + 4 );
  slat := Copy(Vlink, i + 4, j - (i + 4));
  sfulldesc := Vlink;
  sname := 'maps.kosmosnimki';
  meters_to_lonlat(StrToFloat(slon, VFormatSettings),StrToFloat(slat, VFormatSettings),slon,slat);
  slon := ReplaceStr(slon,',','.');
  slat := ReplaceStr(slat,',','.');
  sdesc := '[ '+slon+' , '+slat+' ]';
  sfulldesc := ASearch;
 end else
 if PosEx('go.2gis.ru', Vlink, 1) > 0then begin
  sdesc := vlink;
  VHeader := 'Cookie: 2gisAPI=c2de06c2dd3109de8ca09a59ee197a4210495664eeae8d4075848.943590';
  Vlink := '';
  vErrCode := DoHttpRequest(sdesc, VHeader ,'',sname,Vlink);
  i := PosEx('center/', sname, 1);
  j := PosEx(',', sname, i );
  slon := Copy(sname, i + 7, j - (i + 7));
  i := j;
  j := PosEx('/', sname, i );
  slat := Copy(sname, i + 1, j - (i + 1));
  sname := '2gis';
  sdesc := '[ '+slon+' , '+slat+' ]';
  sfulldesc := ASearch;
 end else
 if PosEx('rambler.ru', Vlink, 1) > 0then begin
  Vlink := ReplaceStr(astr,'\"','');
  sname := 'rambler';
  i := PosEx('lon:', Vlink, 1);
  j := PosEx(',', Vlink, i+1);
  slon := Copy(Vlink, i + 4, j - (i + 4));
  i := PosEx('lat:', Vlink, j);
  j := PosEx('}', Vlink, i+1);
  slat := Copy(Vlink, i + 4, j - (i + 4));
  sdesc := '[ '+slon+' , '+slat+' ]';
  sfulldesc := ASearch;
 end else begin
 VLinkErr := GetListByText(ASearch,VList);
 end;



 if (vErrCode <> 200)and(vErrCode <> 302) then VLinkErr := true;
 if (slat='') or (slon='') then VLinkErr := true;

 if VLinkErr <> true then begin
  try
    VPoint.Y := StrToFloat(slat, VFormatSettings);
    VPoint.X := StrToFloat(slon, VFormatSettings);
  except
    raise EParserError.CreateFmt(SAS_ERR_CoordParseError, [slat, slon]);
  end;
  VPlace := TGeoCodePlacemark.Create(VPoint, sname, sdesc, sfulldesc, 4);
  VList.Add(VPlace);
  Result := VList;
 end
 else
  Result := VList;
end;


function TGeoCoderByURL.PrepareURL(const ASearch: WideString): string;
 var
  VlocalLink :boolean;
begin
  VlocalLink := true;
  if (PosEx('http://g.co/', ASearch, 1) > 0 )or
     (PosEx('yandex.ru/?oid=', ASearch, 1) > 0 )or
     (PosEx('binged.it', ASearch, 1) > 0 )or
     (PosEx('osm.org', ASearch, 1) > 0 )or
     (PosEx('permalink.html', ASearch, 1) > 0 )or
     (PosEx('api/index.html?permalink=', ASearch, 1) > 0 ) or
     (PosEx('rambler.ru/?', ASearch, 1) > 0 ) or
     (PosEx('yandex.ru/?um=', ASearch, 1) > 0 ) or
     (RegExprGetMatchSubStr(ASearch,'\.yandex\..+/-/',0)<>'' )
   then begin
   VlocalLink := false;
   Result := ASearch;
  end;
  if VlocalLink = true then Result := '';
end;
begin

end.


// Полные ссылки
// http://maps.google.com/?ll=48.718079,44.504639&spn=0.722115,1.234589&t=h&z=10
// http://maps.yandex.ru/?ll=44.514541%2C48.708958&spn=0.322723%2C0.181775&z=12&l=map
// http://maps.navitel.su/?zoom=6&lat=55.8&lon=37.6
// http://kosmosnimki.ru/?x=44.1053254382903&y=45.6876903573303&z=6&fullscreen=false&mode=satellite
// http://www.bing.com/maps/default.aspx?v=2&cp=45.5493750107145~41.6883332507903&style=h&lvl=6
// http://www.openstreetmap.org/?lat=45.227&lon=39.001&zoom=10&layers=M
// http://wikimapia.org#lat=45.0328&lon=38.9769&z=10&l=1&m=b
// http://maps.rosreestr.ru/Portal/?l=11&x=4595254.155000001&y=5398402.163800001&mls=map|anno&cls=cadastre
// http://maps.mail.ru/?z=10&ll=37.619948,55.750023
// http://maps.nokia.com/#|43.5669132|41.2836342|14|0|0|hybrid.day
// http://maps.nokia.com/mapcreator/?ns=true#|55.32530472503459|37.811186150077816|18|0|0|
// http://mobile.maps.yandex.net/ylocation/?lat=55.870155&lon=37.665367&desc=dima%40dzhus.org
// http://maps.2gis.ru/#/?history=project/krasnodar/center/38.993668%2C45.197055/zoom/17/state/index/sort/relevance
// http://harita.yandex.com.tr/?ll=29.086777%2C41.000749&spn=0.005043%2C0.003328&z=18&l=sat%2Ctrf&trfm=cur
// http://osm.org.ru/#layer=M&zoom=3&lat=61.98&lon=88

// тайловые ссылки
// http://a.tile.openstreetmap.org/15/19928/11707.png

// Короткие
// http://g.co/maps/7anbg
// http://maps.yandex.ru/-/CBa6ZCOt
// http://maps.yandex.ru/-/CFVIfLi-#
// http://osm.org/go/0oqbju
// http://binged.it/vqaOQQ
// http://kosmosnimki.ru/permalink.html?Na1d0e33d
// http://maps.kosmosnimki.ru/api/index.html?permalink=ZWUJK&SA5JU
// http://go.2gis.ru/1hox
// http://maps.rambler.ru/?6rJJy58
// http://maps.yandex.ru/?um=m4VoZPqVSEwQ3YdT5Lmley6KrBsHb2oh&l=sat
// http://harita.yandex.com.tr/-/CFXxAO3m

