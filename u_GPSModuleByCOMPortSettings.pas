{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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
  i_GPSModuleByCOMPortSettings;

type
  TGPSModuleByCOMPortSettings = class(TInterfacedObject, IGPSModuleByCOMPortSettings)
  private
    FPort: Integer;
    FBaudRate: Integer;
    FConnectionTimeout: Integer;
    FDelay: Integer;
    FNMEALog: Boolean;
    FLogPath: WideString;
  protected
    function GetPort: Integer; safecall;
    function GetBaudRate: Integer; safecall;
    function GetConnectionTimeout: Integer; safecall;
    function GetDelay: Integer; safecall;
    function GetNMEALog: Boolean; safecall;
    function GetLogPath: WideString; safecall;
  public
    constructor Create(
      APort: Integer;
      ABaudRate: Integer;
      AConnectionTimeout: Integer;
      ADelay: Integer;
      ANMEALog: Boolean;
      ALogPath: WideString
    );
  end;

implementation

{ TGPSModuleByCOMPortSettings }

constructor TGPSModuleByCOMPortSettings.Create(
  APort,
  ABaudRate,
  AConnectionTimeout,
  ADelay: Integer;
  ANMEALog: Boolean;
  ALogPath: WideString
);
begin
  inherited Create;
  FPort := APort;
  FBaudRate := ABaudRate;
  FConnectionTimeout := AConnectionTimeout;
  FDelay := ADelay;
  FNMEALog := ANMEALog;
  FLogPath := ALogPath;
end;

function TGPSModuleByCOMPortSettings.GetBaudRate: Integer;
begin
  Result := FBaudRate;
end;

function TGPSModuleByCOMPortSettings.GetConnectionTimeout: Integer;
begin
  Result := FConnectionTimeout;
end;

function TGPSModuleByCOMPortSettings.GetDelay: Integer;
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

function TGPSModuleByCOMPortSettings.GetPort: Integer;
begin
  Result := FPort;
end;

end.
