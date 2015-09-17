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

unit u_PosFromGSM;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Dialogs,
  StrUtils,
  SwinHttp,
  CPDrv,
  t_GeoTypes,
  i_MapViewGoto,
  i_Projection,
  i_GSMGeoCodeConfig;

type
  TPosFromGSM = class
  private
    FConfig: IGSMGeoCodeConfig;
    FMapGoto: IMapViewGoto;
    FProjection: IProjection;
    CommPortDriver: TCommPortDriver;
    LAC: string;
    CellID: string;
    CC: string;
    NC: string;
    procedure CommPortDriver1ReceiveData(
      Sender: TObject;
      DataPtr: Pointer;
      DataSize: Cardinal
    );
    function GetCoordFromGoogle(var LL: TDoublePoint): boolean;
  public
    constructor Create(
      const AConfig: IGSMGeoCodeConfig;
      const AMapGoto: IMapViewGoto
    );
    function GetPos(const AProjection: IProjection): boolean;
  end;

implementation

uses
  u_ResStrings;

{ TPosFromGSM }

constructor TPosFromGSM.Create(
  const AConfig: IGSMGeoCodeConfig;
  const AMapGoto: IMapViewGoto
);
begin
  inherited Create;
  FConfig := AConfig;
  FMapGoto := AMapGoto;
end;

function TPosFromGSM.GetCoordFromGoogle(var LL: TDoublePoint): boolean;
var
  strA, strB, strC, strAll: string;
  sResult: AnsiString;
  ms: TMemoryStream;
  iLat, iLon: Integer;
  i: Integer;
  b: byte;
  sTmp, sTmp2: string;
  iCntr: Integer;
  SwinHttp: TSwinHttp;
  post: AnsiString;
begin
  Result := true;
  NC := '0' + NC;
  CC := '0' + CC;
  strA := '000E00000000000000000000000000001B0000000000000000000000030000';
  strB := '0000' + CellID + '0000' + LAC;
  strC := '000000' + IntToHex(strtoint(NC), 2) + '000000' + IntToHex(strtoint(CC), 2);
  strAll := strA + strB + strC + 'FFFFFFFF00000000';
  SwinHttp := TSwinHttp.Create(nil);
  SwinHttp.InThread := false;
  SwinHttp.Request.Headers.Clear;
  SwinHttp.Request.Headers.Add('Content-Type: application/x-www-form-urlencoded');
  SwinHttp.Request.Headers.Add('Content-Length: ' + inttostr(Length(strAll) div 2));
  SwinHttp.Request.Headers.Add('Accept: text/html, */*');

  ms := TMemoryStream.Create;
  try
    iCntr := 1;
    for i := 1 to (Length(strAll) div 2) do begin
      b := StrToInt('0x' + Copy(strAll, iCntr, 2));
      iCntr := iCntr + 2;
      ms.Write(b, 1);
    end;
    ms.Seek(0, soFromBeginning);
    try
      ms.Position := 0;
      setLength(post, ms.Size);
      ms.ReadBuffer(post[1], ms.Size);
      SwinHttp.Post('http://www.google.com/glm/mmap', post);
      SetLength(sResult, SwinHttp.Response.Content.Size);
      SwinHttp.Response.Content.ReadBuffer(sResult[1], SwinHttp.Response.Content.Size);
      if (SwinHttp.Error = 0) then begin
        if (Length(sResult) > 14) then begin
          sTmp := '0x';
          for i := 1 to 5 do begin
            sTmp2 := Copy(sResult, i + 6, 1);
            sTmp := sTmp + IntToHex(Ord(sTmp2[1]), 2);
          end;
          iLat := StrToInt(sTmp);
          sTmp := '0x';
          for i := 1 to 4 do begin
            sTmp2 := Copy(sResult, i + 11, 1);
            sTmp := sTmp + IntToHex(Ord(sTmp2[1]), 2);
          end;
          iLon := StrToInt(sTmp);
          LL.y := iLat / 1000000;
          LL.x := iLon / 1000000;
        end else begin
          result := false;
          ShowMessage(SAS_ERR_UnablePposition);
        end;
      end else begin
        result := false;
        ShowMessage(SAS_ERR_Communication);
      end;
    except
      result := false;
      ShowMessage(SAS_ERR_UnablePposition);
    end;
  finally
    SwinHttp.Free;
    ms.Free;
  end;
end;

procedure TPosFromGSM.CommPortDriver1ReceiveData(
  Sender: TObject;
  DataPtr: Pointer;
  DataSize: Cardinal
);
  function DelKov(s: string): string;
  var
    i: integer;
  begin
    i := Pos('"', s);
    while i > 0 do begin
      Delete(s, i, 1);
      i := Pos('"', s);
    end;
    result := s;
  end;

  function addT4(const s: string): string;
  begin
    Result := s;
    while length(Result) < 4 do begin
      Result := '0' + Result;
    end;
  end;

var
  s: string;
  VPos, pose: integer;
  LL: TDoublePoint;
begin
  setlength(s, DataSize);
  if DataSize < 10 then begin
    exit;
  end;
  CopyMemory(@s[1], DataPtr, DataSize);
  VPos := posEx('+CREG:', s);
  if VPos > 0 then begin
    VPos := posEx(',', s, VPos + 1);
    VPos := posEx(',', s, VPos + 1);
    pose := posEx(',', s, VPos + 1);
    if VPos > 0 then begin
      LAC := addT4(DelKov(copy(s, VPos + 1, pose - (VPos + 1))));
    end;
    VPos := posEx(#$D, s, pose + 1);
    if VPos > 0 then begin
      CellID := addT4(DelKov(copy(s, pose + 1, VPos - (pose + 1))));
    end;
  end else begin
    exit;
  end;
  VPos := posEx('+COPS:', s);
  if VPos > 0 then begin
    VPos := posEx(',"', s, VPos);
    if VPos > 0 then begin
      NC := DelKov(copy(s, VPos + 2, 3));
      CC := DelKov(copy(s, VPos + 5, 2));
    end;
  end else begin
    exit;
  end;
  CommPortDriver.SendString('AT+CREG=1' + #13);
  CommPortDriver.Disconnect;
  if GetCoordFromGoogle(LL) then begin
    FMapGoto.GotoPos(LL, FProjection, True);
  end;
  free;
end;

function GetWord(
  Str: string;
  const Smb: string;
  WordNmbr: Byte
): string;
var
  SWord: string;
  StrLen, N: Byte;
begin
  StrLen := Length(Str);
  N := 1;
  while ((WordNmbr >= N) and (StrLen <> 0)) do begin
    StrLen := System.Pos(Smb, str);
    if StrLen <> 0 then begin
      SWord := Copy(Str, 1, StrLen - 1);
      Delete(Str, 1, StrLen);
      Inc(N);
    end else begin
      SWord := Str;
    end;
  end;
  if WordNmbr <= N then begin
    Result := SWord;
  end else begin
    Result := '';
  end;
end;


function TPosFromGSM.GetPos(const AProjection: IProjection): boolean;
var
  paramss: string;
  LL: TDoublePoint;
  VUseGSM: Boolean;
  VPort: string;
  VRate: Cardinal;
  VWait: Cardinal;
begin
  Result := False;
  FProjection := AProjection;
  FConfig.LockRead;
  try
    VUseGSM := FConfig.GetUseGSMByCOM;
    VPort := FConfig.GetPortName;
    VRate := FConfig.GetBaudRate;
    VWait := FConfig.GetWaitTime;
  finally
    FConfig.UnlockRead;
  end;
  if VUseGSM then begin
    CommPortDriver := TCommPortDriver.Create(nil);
    CommPortDriver.PortName := '\\.\' + VPort;
    CommPortDriver.BaudRateValue := VRate;
    CommPortDriver.OnReceiveData := CommPortDriver1ReceiveData;
    CommPortDriver.Connect;
    if CommPortDriver.Connected then begin
      if (CommPortDriver.SendString('AT+CREG=2' + #13)) and
        (CommPortDriver.SendString('AT+COPS=0,2' + #13)) then begin
        sleep(VWait);
        CommPortDriver.SendString('AT+CREG?' + #13);
        CommPortDriver.SendString('AT+COPS?' + #13);
        Result := true;
      end;
    end else begin
      ShowMessage(SAS_ERR_PortOpen);
      Result := false;
    end;
  end else begin
    if InputQuery(SAS_STR_InputLacitpCaption, SAS_STR_InputLacitp, paramss) then begin
      try
        CC := GetWord(paramss, ',', 1);
        NC := GetWord(paramss, ',', 2);
        LAC := IntToHex(strtoint(GetWord(paramss, ',', 3)), 4);
        CellID := IntToHex(strtoint(GetWord(paramss, ',', 4)), 4);
        if GetCoordFromGoogle(LL) then begin
          FMapGoto.GotoPos(LL, FProjection, True);
          Result := true;
        end else begin
          Result := false;
        end;
      except
        ShowMessage(SAS_ERR_ParamsInput);
        Result := false;
      end;
    end;
  end;
end;

end.
