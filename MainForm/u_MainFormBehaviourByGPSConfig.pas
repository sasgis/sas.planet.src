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

unit u_MainFormBehaviourByGPSConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MainFormBehaviourByGPSConfig,
  u_ConfigDataElementBase;

type
  TMainFormBehaviourByGPSConfig = class(TConfigDataElementBase, IMainFormBehaviourByGPSConfig)
  private
    FMapMove: Boolean;
    FMapMoveCentered: Boolean;
    FMinMoveDelta: Double;
    FSensorsAutoShow: Boolean;
    FProcessGPSIfActive: Boolean;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetMapMove: Boolean;
    procedure SetMapMove(AValue: Boolean);
    function GetMapMoveCentered: Boolean;
    procedure SetMapMoveCentered(AValue: Boolean);
    function GetMinMoveDelta: Double;
    procedure SetMinMoveDelta(AValue: Double);
    function GetSensorsAutoShow: Boolean;
    procedure SetSensorsAutoShow(AValue: Boolean);
    function GetProcessGPSIfActive: Boolean;
    procedure SetProcessGPSIfActive(AValue: Boolean);
  public
    constructor Create;
  end;

implementation

{ TMainFormBehaviourByGPSConfig }

constructor TMainFormBehaviourByGPSConfig.Create;
begin
  inherited Create;
  FMapMove := True;
  FMapMoveCentered := False;
  FMinMoveDelta := 10;
  FSensorsAutoShow := True;
  FProcessGPSIfActive := True;
end;

procedure TMainFormBehaviourByGPSConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider
);
begin
  inherited;
  if AConfigData <> nil then begin
    FMapMove := AConfigData.ReadBool('MoveMapByGPS', FMapMove);
    FMapMoveCentered := AConfigData.ReadBool('GPSPosInCenter', FMapMoveCentered);
    FMinMoveDelta := AConfigData.ReadFloat('MinGPSMoveDelta', FMinMoveDelta);
    FSensorsAutoShow := AConfigData.ReadBool('SensorsAutoShow', FSensorsAutoShow);
    FProcessGPSIfActive := AConfigData.ReadBool('ProcessGpsIfFormActive', FProcessGPSIfActive);
    SetChanged;
  end;
end;

procedure TMainFormBehaviourByGPSConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteBool('MoveMapByGPS', FMapMove);
  AConfigData.WriteBool('GPSPosInCenter', FMapMoveCentered);
  AConfigData.WriteFloat('MinGPSMoveDelta', FMinMoveDelta);
  AConfigData.WriteBool('SensorsAutoShow', FSensorsAutoShow);
  AConfigData.WriteBool('ProcessGpsIfFormActive', FProcessGPSIfActive);
end;

function TMainFormBehaviourByGPSConfig.GetMapMove: Boolean;
begin
  LockRead;
  try
    Result := FMapMove;
  finally
    UnlockRead;
  end;
end;

function TMainFormBehaviourByGPSConfig.GetMapMoveCentered: Boolean;
begin
  LockRead;
  try
    Result := FMapMoveCentered;
  finally
    UnlockRead;
  end;
end;

function TMainFormBehaviourByGPSConfig.GetMinMoveDelta: Double;
begin
  LockRead;
  try
    Result := FMinMoveDelta;
  finally
    UnlockRead;
  end;
end;

function TMainFormBehaviourByGPSConfig.GetProcessGPSIfActive: Boolean;
begin
  LockRead;
  try
    Result := FProcessGPSIfActive;
  finally
    UnlockRead;
  end;
end;

function TMainFormBehaviourByGPSConfig.GetSensorsAutoShow: Boolean;
begin
  LockRead;
  try
    Result := FSensorsAutoShow;
  finally
    UnlockRead;
  end;
end;

procedure TMainFormBehaviourByGPSConfig.SetMapMove(AValue: Boolean);
begin
  LockWrite;
  try
    if FMapMove <> AValue then begin
      FMapMove := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMainFormBehaviourByGPSConfig.SetMapMoveCentered(AValue: Boolean);
begin
  LockWrite;
  try
    if FMapMoveCentered <> AValue then begin
      FMapMoveCentered := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMainFormBehaviourByGPSConfig.SetMinMoveDelta(AValue: Double);
begin
  LockWrite;
  try
    if FMinMoveDelta <> AValue then begin
      FMinMoveDelta := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMainFormBehaviourByGPSConfig.SetProcessGPSIfActive(AValue: Boolean);
begin
  LockWrite;
  try
    if FProcessGPSIfActive <> AValue then begin
      FProcessGPSIfActive := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMainFormBehaviourByGPSConfig.SetSensorsAutoShow(AValue: Boolean);
begin
  LockWrite;
  try
    if FSensorsAutoShow <> AValue then begin
      FSensorsAutoShow := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
