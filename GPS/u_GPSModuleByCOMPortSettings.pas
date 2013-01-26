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

unit u_GPSModuleByCOMPortSettings;

interface

uses
  Windows,
  i_GPSModuleByCOMPortSettings,
  u_BaseInterfacedObject;

type
  TGPSModuleByCOMPortSettings = class(TBaseInterfacedObject, IGPSModuleByCOMPortSettings)
  private
    FPort: DWORD;
    FBaudRate: DWORD;
    FConnectionTimeout: DWORD;
    FDelay: DWORD;
    FNMEALog: Boolean;
    FLogPath: WideString;
    FUSBGarmin: Boolean;
    FAutodetectCOMOnConnect: Boolean;
    FAutodetectCOMFlags: DWORD;
  private
    function GetPort: DWORD; safecall;
    function GetBaudRate: DWORD; safecall;
    function GetConnectionTimeout: DWORD; safecall;
    function GetDelay: DWORD; safecall;
    function GetNMEALog: Boolean; safecall;
    function GetLogPath: WideString; safecall;
    function GetUSBGarmin: Boolean; safecall;
    function GetAutodetectCOMOnConnect: Boolean; safecall;
    function GetAutodetectCOMFlags: DWORD; safecall;
  public
    constructor Create(
      const APort: DWORD;
      const ABaudRate: DWORD;
      const AConnectionTimeout: DWORD;
      const ADelay: DWORD;
      const ANMEALog: Boolean;
      const ALogPath: WideString;
      const AUSBGarmin: Boolean;
      const AAutodetectCOMOnConnect: Boolean;
      const AAutodetectCOMFlags: DWORD
    );
  end;

implementation

{ TGPSModuleByCOMPortSettings }

constructor TGPSModuleByCOMPortSettings.Create(
  const APort: DWORD;
  const ABaudRate: DWORD;
  const AConnectionTimeout: DWORD;
  const ADelay: DWORD;
  const ANMEALog: Boolean;
  const ALogPath: WideString;
  const AUSBGarmin: Boolean;
  const AAutodetectCOMOnConnect: Boolean;
  const AAutodetectCOMFlags: DWORD
);
begin
  inherited Create;
  FPort := APort;
  FBaudRate := ABaudRate;
  FConnectionTimeout := AConnectionTimeout;
  FDelay := ADelay;
  FNMEALog := ANMEALog;
  FLogPath := ALogPath;
  FUSBGarmin := AUSBGarmin;
  FAutodetectCOMOnConnect := AAutodetectCOMOnConnect;
  FAutodetectCOMFlags := AAutodetectCOMFlags;
end;

function TGPSModuleByCOMPortSettings.GetAutodetectCOMFlags: DWORD;
begin
  Result := FAutodetectCOMFlags;
end;

function TGPSModuleByCOMPortSettings.GetAutodetectCOMOnConnect: Boolean;
begin
  Result := FAutodetectCOMOnConnect;
end;

function TGPSModuleByCOMPortSettings.GetBaudRate: DWORD;
begin
  Result := FBaudRate;
end;

function TGPSModuleByCOMPortSettings.GetConnectionTimeout: DWORD;
begin
  Result := FConnectionTimeout;
end;

function TGPSModuleByCOMPortSettings.GetDelay: DWORD;
begin
  Result := FDelay;
end;

function TGPSModuleByCOMPortSettings.GetLogPath: WideString;
begin
  Result := FLogPath;
end;

function TGPSModuleByCOMPortSettings.GetNMEALog: Boolean;
begin
  Result := FNMEALog;
end;

function TGPSModuleByCOMPortSettings.GetPort: DWORD;
begin
  Result := FPort;
end;

function TGPSModuleByCOMPortSettings.GetUSBGarmin: Boolean;
begin
  Result := FUSBGarmin;
end;

end.
