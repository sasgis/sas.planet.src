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

unit u_KeyMovingConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_KeyMovingConfig,
  u_ConfigDataElementBase;

type
  TKeyMovingConfig = class(TConfigDataElementWithStaticBase, IKeyMovingConfig)
  private
    FFirstKeyPressDelta: Double;
    FMinPixelPerSecond: Double;
    FMaxPixelPerSecond: Double;
    FSpeedChangeTime: Double;
    FStopTime: Double;
  protected
    function CreateStatic: IInterface; override;
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetFirstKeyPressDelta: Double;
    procedure SetFirstKeyPressDelta(const AValue: Double);

    function GetMinPixelPerSecond: Double;
    procedure SetMinPixelPerSecond(const AValue: Double);

    function GetMaxPixelPerSecond: Double;
    procedure SetMaxPixelPerSecond(const AValue: Double);

    function GetSpeedChangeTime: Double;
    procedure SetSpeedChangeTime(const AValue: Double);

    function GetStopTime: Double;
    procedure SetStopTime(const AValue: Double);

    function GetStatic: IKeyMovingConfigStatic;
  public
    constructor Create;
  end;

implementation

uses
  u_BaseInterfacedObject;

{ TKeyMovingConfigStatic }

type
  TKeyMovingConfigStatic = class(TBaseInterfacedObject, IKeyMovingConfigStatic)
  private
    FFirstKeyPressDelta: Double;
    FMinPixelPerSecond: Double;
    FMaxPixelPerSecond: Double;
    FSpeedChangeTime: Double;
    FStopTime: Double;
  private
    function GetFirstKeyPressDelta: Double;
    function GetMinPixelPerSecond: Double;
    function GetMaxPixelPerSecond: Double;
    function GetSpeedChangeTime: Double;
    function GetStopTime: Double;
  public
    constructor Create(
      const AFirstKeyPressDelta: Double;
      const AMinPixelPerSecond: Double;
      const AMaxPixelPerSecond: Double;
      const ASpeedChangeTime: Double;
      const AStopTime: Double
    );
  end;

constructor TKeyMovingConfigStatic.Create(
  const AFirstKeyPressDelta, AMinPixelPerSecond, AMaxPixelPerSecond, ASpeedChangeTime, AStopTime: Double
);
begin
  inherited Create;
  FFirstKeyPressDelta := AFirstKeyPressDelta;
  FMinPixelPerSecond := AMinPixelPerSecond;
  FMaxPixelPerSecond := AMaxPixelPerSecond;
  FSpeedChangeTime := ASpeedChangeTime;
  FStopTime := AStopTime;
end;

function TKeyMovingConfigStatic.GetFirstKeyPressDelta: Double;
begin
  Result := FFirstKeyPressDelta;
end;

function TKeyMovingConfigStatic.GetMaxPixelPerSecond: Double;
begin
  Result := FMaxPixelPerSecond;
end;

function TKeyMovingConfigStatic.GetMinPixelPerSecond: Double;
begin
  Result := FMinPixelPerSecond;
end;

function TKeyMovingConfigStatic.GetSpeedChangeTime: Double;
begin
  Result := FSpeedChangeTime;
end;

function TKeyMovingConfigStatic.GetStopTime: Double;
begin
  Result := FStopTime;
end;

{ TKeyMovingConfig }

constructor TKeyMovingConfig.Create;
begin
  inherited Create;
  FFirstKeyPressDelta := 30;
  FMinPixelPerSecond := 20;
  FMaxPixelPerSecond := 1024;
  FSpeedChangeTime := 3;
  FStopTime := 1;
end;

function TKeyMovingConfig.CreateStatic: IInterface;
var
  VStatic: IKeyMovingConfigStatic;
begin
  VStatic := TKeyMovingConfigStatic.Create(FFirstKeyPressDelta, FMinPixelPerSecond, FMaxPixelPerSecond, FSpeedChangeTime, FStopTime);
  Result := VStatic;
end;

procedure TKeyMovingConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    SetFirstKeyPressDelta(AConfigData.ReadFloat('FirstKeyPressDelta', FFirstKeyPressDelta));
    SetMinPixelPerSecond(AConfigData.ReadFloat('MinPixelPerSecond', FMinPixelPerSecond));
    SetMaxPixelPerSecond(AConfigData.ReadFloat('MaxPixelPerSecond', FMaxPixelPerSecond));
    SetSpeedChangeTime(AConfigData.ReadFloat('SpeedChangeTime', FSpeedChangeTime));
    SetStopTime(AConfigData.ReadFloat('StopTime', FStopTime));
    SetChanged;
  end;
end;

procedure TKeyMovingConfig.DoWriteConfig(const AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteFloat('FirstKeyPressDelta', FFirstKeyPressDelta);
  AConfigData.WriteFloat('MinPixelPerSecond', FMinPixelPerSecond);
  AConfigData.WriteFloat('MaxPixelPerSecond', FMaxPixelPerSecond);
  AConfigData.WriteFloat('SpeedChangeTime', FSpeedChangeTime);
  AConfigData.WriteFloat('StopTime', FStopTime);
end;

function TKeyMovingConfig.GetFirstKeyPressDelta: Double;
begin
  LockRead;
  try
    Result := FFirstKeyPressDelta;
  finally
    UnlockRead;
  end;
end;

function TKeyMovingConfig.GetMaxPixelPerSecond: Double;
begin
  LockRead;
  try
    Result := FMaxPixelPerSecond;
  finally
    UnlockRead;
  end;
end;

function TKeyMovingConfig.GetMinPixelPerSecond: Double;
begin
  LockRead;
  try
    Result := FMinPixelPerSecond;
  finally
    UnlockRead;
  end;
end;

function TKeyMovingConfig.GetSpeedChangeTime: Double;
begin
  LockRead;
  try
    Result := FSpeedChangeTime;
  finally
    UnlockRead;
  end;
end;

function TKeyMovingConfig.GetStatic: IKeyMovingConfigStatic;
begin
  Result := IKeyMovingConfigStatic(GetStaticInternal);
end;

function TKeyMovingConfig.GetStopTime: Double;
begin
  LockRead;
  try
    Result := FStopTime;
  finally
    UnlockRead;
  end;
end;

procedure TKeyMovingConfig.SetFirstKeyPressDelta(const AValue: Double);
var
  VValue: Double;
begin
  VValue := AValue;
  if VValue < 0 then begin
    VValue := 0;
  end else if VValue > 2000 then begin
    VValue := 2000;
  end;

  LockWrite;
  try
    if FFirstKeyPressDelta <> VValue then begin
      FFirstKeyPressDelta := VValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TKeyMovingConfig.SetMaxPixelPerSecond(const AValue: Double);
var
  VValue: Double;
begin
  VValue := AValue;
  if VValue < 0 then begin
    VValue := 0;
  end else if VValue > 2000 then begin
    VValue := 2000;
  end;

  LockWrite;
  try
    if FMaxPixelPerSecond <> VValue then begin
      FMaxPixelPerSecond := VValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TKeyMovingConfig.SetMinPixelPerSecond(const AValue: Double);
var
  VValue: Double;
begin
  VValue := AValue;
  if VValue < 0 then begin
    VValue := 0;
  end else if VValue > 2000 then begin
    VValue := 2000;
  end;

  LockWrite;
  try
    if FMinPixelPerSecond <> VValue then begin
      FMinPixelPerSecond := VValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TKeyMovingConfig.SetSpeedChangeTime(const AValue: Double);
var
  VValue: Double;
begin
  VValue := AValue;
  if VValue < 0 then begin
    VValue := 0;
  end;

  LockWrite;
  try
    if FSpeedChangeTime <> VValue then begin
      FSpeedChangeTime := VValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TKeyMovingConfig.SetStopTime(const AValue: Double);
var
  VValue: Double;
begin
  VValue := AValue;
  if VValue < 0 then begin
    VValue := 0;
  end;

  LockWrite;
  try
    if FStopTime <> VValue then begin
      FStopTime := VValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
