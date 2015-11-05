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
    FLowLevelLog: Boolean;
    FLogPath: string;
    FGPSOrigin: TGPSOrigin;
    FAutodetectCOMOnConnect: Boolean;
    FAutodetectCOMFlags: DWORD;
  private
    function GetPort: DWORD; safecall;
    function GetBaudRate: DWORD; safecall;
    function GetConnectionTimeout: DWORD; safecall;
    function GetDelay: DWORD; safecall;
    function GetLowLevelLog: Boolean; safecall;
    function GetLogPath: string; safecall;
    function GetGPSOrigin: TGPSOrigin; safecall;
    function GetAutodetectCOMOnConnect: Boolean; safecall;
    function GetAutodetectCOMFlags: DWORD; safecall;
  public
    constructor Create(
      const APort: DWORD;
      const ABaudRate: DWORD;
      const AConnectionTimeout: DWORD;
      const ADelay: DWORD;
      const ALowLevelLog: Boolean;
      const ALogPath: string;
      const AGPSOrigin: TGPSOrigin;
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
  const ALowLevelLog: Boolean;
  const ALogPath: string;
  const AGPSOrigin: TGPSOrigin;
  const AAutodetectCOMOnConnect: Boolean;
  const AAutodetectCOMFlags: DWORD
);
begin
  inherited Create;
  FPort := APort;
  FBaudRate := ABaudRate;
  FConnectionTimeout := AConnectionTimeout;
  FDelay := ADelay;
  FLowLevelLog := ALowLevelLog;
  FLogPath := ALogPath;
  FGPSOrigin := AGPSOrigin;
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

function TGPSModuleByCOMPortSettings.GetLogPath: string;
begin
  Result := FLogPath;
end;

function TGPSModuleByCOMPortSettings.GetLowLevelLog: Boolean;
begin
  Result := FLowLevelLog;
end;

function TGPSModuleByCOMPortSettings.GetPort: DWORD;
begin
  Result := FPort;
end;

function TGPSModuleByCOMPortSettings.GetGPSOrigin: TGPSOrigin;
begin
  Result := FGPSOrigin;
end;

end.
