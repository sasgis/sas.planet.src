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

unit u_SensorBatteryStatus;

interface

uses
  i_BatteryStatus,
  i_Sensor,
  u_SensorBase;

type
  TSensorBatteryStatus = class(TSensorByteValue, ISensorBatteryLifePercent)
  private
    FBatteryStatus: IBatteryStatus;
  protected
    function GetSensorTypeIID: TGUID; override;
    function GetCurrentValue: Byte; override;
  public
    constructor Create(
      const ABatteryStatus: IBatteryStatus
    );
  end;

implementation

{ TSensorBatteryStatus }

constructor TSensorBatteryStatus.Create(const ABatteryStatus: IBatteryStatus);
begin
  inherited Create(ABatteryStatus.ChangeNotifier);
  FBatteryStatus := ABatteryStatus;
end;

function TSensorBatteryStatus.GetCurrentValue: Byte;
var
  VState: IBatteryStatusStatic;
begin
  VState := FBatteryStatus.GetStatic;
  Result := 255;
  if VState <> nil then begin
    if VState.ACLineStatus = 0 then begin
      case VState.BatteryFlag of
        128: begin
          Result := 200;
        end;
        8: begin
          Result := 101;
        end;
      else begin
        if VState.BatteryLifePercent <= 100 then begin
          Result := VState.BatteryLifePercent;
        end;
      end;
      end;
    end else begin
      Result := 200;
    end;
  end;
end;

function TSensorBatteryStatus.GetSensorTypeIID: TGUID;
begin
  Result := ISensorBatteryLifePercent;
end;

end.
