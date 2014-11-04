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

unit u_BatteryStatus;

interface

uses
  Windows,
  ExtCtrls,
  SysUtils,
  i_BatteryStatus,
  u_ChangeableBase;

type
  TBatteryStatus = class(TChangeableBase, IBatteryStatus)
  private
    FLock: IReadWriteSync;
    FTimer: TTimer;
    FStatic: IBatteryStatusStatic;

    procedure OnTimer(Sender: TObject);
  private
    function GetStatic: IBatteryStatusStatic;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  u_Synchronizer,
  u_BaseInterfacedObject;

type
  TBatteryStatusStatic = class(TBaseInterfacedObject, IBatteryStatusStatic)
  private
    FACLineStatus: Byte;
    FBatteryFlag: Byte;
    FBatteryLifePercent: Byte;
    FBatteryLifeTime: LongWord;
  private
    function GetACLineStatus: Byte;
    function GetBatteryFlag: Byte;
    function GetBatteryLifePercent: Byte;
    function GetBatteryLifeTime: LongWord;
  public
    constructor Create(
      AACLineStatus: Byte;
      ABatteryFlag: Byte;
      ABatteryLifePercent: Byte;
      ABatteryLifeTime: LongWord
    );
  end;

{ TBatteryStatusStatic }

constructor TBatteryStatusStatic.Create(
  AACLineStatus, ABatteryFlag, ABatteryLifePercent: Byte;
  ABatteryLifeTime: LongWord
);
begin
  inherited Create;
  FACLineStatus := AACLineStatus;
  FBatteryFlag := ABatteryFlag;
  FBatteryLifePercent := ABatteryLifePercent;
  FBatteryLifeTime := ABatteryLifeTime;
end;

function TBatteryStatusStatic.GetACLineStatus: Byte;
begin
  Result := FACLineStatus;
end;

function TBatteryStatusStatic.GetBatteryFlag: Byte;
begin
  Result := FBatteryFlag;
end;

function TBatteryStatusStatic.GetBatteryLifePercent: Byte;
begin
  Result := FBatteryLifePercent;
end;

function TBatteryStatusStatic.GetBatteryLifeTime: LongWord;
begin
  Result := FBatteryLifeTime;
end;

{ TBatteryStatus }

constructor TBatteryStatus.Create;
begin
  inherited Create(GSync.SyncVariable.Make(Self.ClassName + 'Notifiers'));
  FLock := GSync.SyncVariable.Make(Self.ClassName);
  FTimer := TTimer.Create(nil);
  FTimer.Interval := 1000;
  FTimer.OnTimer := Self.OnTimer;
  OnTimer(nil);
  FTimer.Enabled := True;
end;

destructor TBatteryStatus.Destroy;
begin
  FreeAndNil(FTimer);
  inherited;
end;

function TBatteryStatus.GetStatic: IBatteryStatusStatic;
begin
  FLock.BeginRead;
  try
    Result := FStatic;
  finally
    FLock.EndRead;
  end;
end;

procedure TBatteryStatus.OnTimer(Sender: TObject);
var
  sps: _SYSTEM_POWER_STATUS;
  VChanged: Boolean;
  VStatic: IBatteryStatusStatic;
begin
  GetSystemPowerStatus(sps);
  VStatic := GetStatic;
  VChanged :=
    (VStatic = nil) or
    (sps.ACLineStatus <> VStatic.ACLineStatus) or
    (sps.BatteryFlag <> VStatic.BatteryFlag) or
    (sps.BatteryLifePercent <> VStatic.BatteryLifePercent) or
    (sps.BatteryFullLifeTime <> VStatic.BatteryLifeTime);
  if VChanged then begin
    FLock.BeginWrite;
    try
      FStatic :=
        TBatteryStatusStatic.Create(
          sps.ACLineStatus,
          sps.BatteryFlag,
          sps.BatteryLifePercent,
          sps.BatteryFullLifeTime
        );
    finally
      FLock.EndWrite;
    end;
    DoChangeNotify;
  end;
end;

end.
