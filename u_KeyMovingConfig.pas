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

unit u_KeyMovingConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_KeyMovingConfig,
  u_ConfigDataElementBase;

type
  TKeyMovingConfig = class(TConfigDataElementBase, IKeyMovingConfig)
  private
    FFirstKeyPressDelta: Double;
    FMinPixelPerSecond: Double;
    FMaxPixelPerSecond: Double;
    FSpeedChangeTime: Double;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetFirstKeyPressDelta: Double;
    procedure SetFirstKeyPressDelta(AValue: Double);

    function GetMinPixelPerSecond: Double;
    procedure SetMinPixelPerSecond(AValue: Double);

    function GetMaxPixelPerSecond: Double;
    procedure SetMaxPixelPerSecond(AValue: Double);

    function GetSpeedChangeTime: Double;
    procedure SetSpeedChangeTime(AValue: Double);
  public
    constructor Create;
  end;

implementation

{ TKeyMovingConfig }

constructor TKeyMovingConfig.Create;
begin
  inherited;
  FFirstKeyPressDelta := 30;
  FMinPixelPerSecond := 20;
  FMaxPixelPerSecond := 1024;
  FSpeedChangeTime := 3;
end;

procedure TKeyMovingConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FFirstKeyPressDelta := AConfigData.ReadFloat('FirstKeyPressDelta', FFirstKeyPressDelta);
    FMinPixelPerSecond := AConfigData.ReadFloat('MinPixelPerSecond', FMinPixelPerSecond);
    FMaxPixelPerSecond := AConfigData.ReadFloat('MaxPixelPerSecond', FMaxPixelPerSecond);
    FSpeedChangeTime := AConfigData.ReadFloat('SpeedChangeTime', FSpeedChangeTime);
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

procedure TKeyMovingConfig.SetFirstKeyPressDelta(AValue: Double);
begin
  LockWrite;
  try
    if FFirstKeyPressDelta <> AValue then begin
      FFirstKeyPressDelta := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TKeyMovingConfig.SetMaxPixelPerSecond(AValue: Double);
begin
  LockWrite;
  try
    if FMaxPixelPerSecond <> AValue then begin
      FMaxPixelPerSecond := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TKeyMovingConfig.SetMinPixelPerSecond(AValue: Double);
begin
  LockWrite;
  try
    if FMinPixelPerSecond <> AValue then begin
      FMinPixelPerSecond := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TKeyMovingConfig.SetSpeedChangeTime(AValue: Double);
begin
  LockWrite;
  try
    if FSpeedChangeTime <> AValue then begin
      FSpeedChangeTime := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
