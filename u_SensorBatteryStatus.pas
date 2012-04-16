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

unit u_SensorBatteryStatus;

interface

uses
  Windows,
  ExtCtrls,
  i_LanguageManager,
  i_Sensor,
  u_SensorBase;

type
  TSensorBatteryStatus = class(TSensorBase, ISensorText)
  private
    FStatusText: string;
    FTimer: TTimer;
    procedure OnTimer(Sender: TObject);
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;
  protected
    function GetText: string;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  c_SensorsGUIDSimple,
  u_ResStrings;

{ TSensorBatteryStatus }

constructor TSensorBatteryStatus.Create(const ALanguageManager: ILanguageManager);
begin
  inherited Create(CSensorBatteryGUID, False, ISensorText, ALanguageManager);
  FStatusText := '-';
  FTimer := TTimer.Create(nil);
  FTimer.Interval := 1000;
  FTimer.OnTimer := Self.OnTimer;
  FTimer.Enabled := True;
end;

destructor TSensorBatteryStatus.Destroy;
begin
  FreeAndNil(FTimer);
  inherited;
end;

function TSensorBatteryStatus.GetCaptionTranslated: string;
begin
  Result := SAS_STR_SensorBatteryStatusCaption;
end;

function TSensorBatteryStatus.GetDescriptionTranslated: string;
begin
  Result := SAS_STR_SensorBatteryStatusDescription;
end;

function TSensorBatteryStatus.GetMenuItemNameTranslated: string;
begin
  Result := SAS_STR_SensorBatteryStatusMenuItemName;
end;

function TSensorBatteryStatus.GetText: string;
begin
  LockRead;
  try
    Result := FStatusText
  finally
    UnlockRead;
  end;
end;

procedure TSensorBatteryStatus.OnTimer(Sender: TObject);
var
  sps: _SYSTEM_POWER_STATUS;
  VResult: string;
  VDataUpdate: Boolean;
begin
  GetSystemPowerStatus(sps);
  if sps.ACLineStatus=0 then begin
    case sps.BatteryFlag of
    128: begin
      VResult := SAS_STR_BattaryStateOnLine;
    end;
    8: begin
      VResult := SAS_STR_BattaryStateCharge;
    end;
    else
      if sps.BatteryLifePercent=255 then begin
        VResult := SAS_STR_BattaryStateUnknown
      end else begin
        VResult := inttostr(sps.BatteryLifePercent)+'%';
      end;
    end
  end else begin
    VResult := SAS_STR_BattaryStateOnLine;
  end;
  VDataUpdate := False;
  LockWrite;
  try
    if FStatusText <> VResult then begin
      FStatusText := VResult;
      VDataUpdate := True;
    end;
  finally
    UnlockWrite;
  end;
  if VDataUpdate then begin
    NotifyDataUpdate;
  end;
end;

end.
