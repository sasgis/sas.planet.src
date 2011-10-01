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

unit u_GPSModuleByCOMPortConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_GPSModuleByCOMPortSettings,
  i_GPSModuleByCOMPortConfig,
  u_ConfigDataElementBase;

type
  TGPSModuleByCOMPortConfig = class(TConfigDataElementBase, IGPSModuleByCOMPortConfig)
  private
    FPort: Integer;
    FBaudRate: Integer;
    FConnectionTimeout: Integer;
    FDelay: Integer;
    FNMEALog: Boolean;
    FLogPath: WideString;
    FStatic: IGPSModuleByCOMPortSettings;
    function CreateStatic: IGPSModuleByCOMPortSettings;
  protected
    procedure DoBeforeChangeNotify; override;
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetPort: Integer;
    procedure SetPort(const AValue: Integer);

    function GetBaudRate: Integer;
    procedure SetBaudRate(const AValue: Integer);

    function GetConnectionTimeout: Integer;
    procedure SetConnectionTimeout(const AValue: Integer);

    function GetDelay: Integer;
    procedure SetDelay(const AValue: Integer);

    function GetNMEALog: Boolean;
    procedure SetNMEALog(const AValue: Boolean);

    function GetLogPath: WideString;

    function GetStatic: IGPSModuleByCOMPortSettings;
  public
    constructor Create(ALogPath: string);
  end;

implementation

uses
  u_GPSModuleByCOMPortSettings;

{ TGPSModuleByCOMPortConfig }

constructor TGPSModuleByCOMPortConfig.Create(ALogPath: string);
begin
  inherited Create;
  FLogPath := ALogPath;
  FPort := 1;
  FBaudRate := 4800;
  FConnectionTimeout := 300;
  FDelay := 1000;
  FNMEALog := False;
  FStatic := CreateStatic;
end;

function TGPSModuleByCOMPortConfig.CreateStatic: IGPSModuleByCOMPortSettings;
begin
  Result :=
    TGPSModuleByCOMPortSettings.Create(
      FPort,
      FBaudRate,
      FConnectionTimeout,
      FDelay,
      FNMEALog,
      FLogPath
    );
end;

procedure TGPSModuleByCOMPortConfig.DoBeforeChangeNotify;
begin
  inherited;
  LockWrite;
  try
    FStatic := CreateStatic;
  finally
    UnlockWrite;
  end;
end;

procedure TGPSModuleByCOMPortConfig.DoReadConfig(
  AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    SetPort(AConfigData.ReadInteger('COM', FPort));
    SetBaudRate(AConfigData.ReadInteger('BaudRate', FBaudRate));
    SetConnectionTimeout(AConfigData.ReadInteger('timeout', FConnectionTimeout));
    SetDelay(AConfigData.ReadInteger('update', FDelay));
    SetNMEALog(AConfigData.ReadBool('NMEAlog', FNMEALog));
  end;
end;

procedure TGPSModuleByCOMPortConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteInteger('COM', FPort);
  AConfigData.WriteInteger('BaudRate', FBaudRate);
  AConfigData.WriteInteger('timeout', FConnectionTimeout);
  AConfigData.WriteInteger('update', FDelay);
  AConfigData.WriteBool('NMEAlog', FNMEALog);
end;

function TGPSModuleByCOMPortConfig.GetBaudRate: Integer;
begin
  LockRead;
  try
    Result := FBaudRate;
  finally
    UnlockRead;
  end;
end;

function TGPSModuleByCOMPortConfig.GetConnectionTimeout: Integer;
begin
  LockRead;
  try
    Result := FConnectionTimeout;
  finally
    UnlockRead;
  end;
end;

function TGPSModuleByCOMPortConfig.GetDelay: Integer;
begin
  LockRead;
  try
    Result := FDelay;
  finally
    UnlockRead;
  end;
end;

function TGPSModuleByCOMPortConfig.GetLogPath: WideString;
begin
  LockRead;
  try
    Result := FLogPath;
  finally
    UnlockRead;
  end;
end;

function TGPSModuleByCOMPortConfig.GetNMEALog: Boolean;
begin
  LockRead;
  try
    Result := FNMEALog;
  finally
    UnlockRead;
  end;
end;

function TGPSModuleByCOMPortConfig.GetPort: Integer;
begin
  LockRead;
  try
    Result := FPort;
  finally
    UnlockRead;
  end;
end;

function TGPSModuleByCOMPortConfig.GetStatic: IGPSModuleByCOMPortSettings;
begin
  LockRead;
  try
    Result := FStatic;
  finally
    UnlockRead;
  end;
end;

procedure TGPSModuleByCOMPortConfig.SetBaudRate(const AValue: Integer);
begin
  if (AValue > 0) and (AValue <= 1000000) then begin
    LockWrite;
    try
      if FBaudRate <> AValue then begin
        FBaudRate := AValue;
        SetChanged;
      end;
    finally
      UnlockWrite;
    end;
  end;
end;

procedure TGPSModuleByCOMPortConfig.SetConnectionTimeout(const AValue: Integer);
begin
  if (AValue >= 0) and (AValue <= 600) then begin
    LockWrite;
    try
      if FConnectionTimeout <> AValue then begin
        FConnectionTimeout := AValue;
        SetChanged;
      end;
    finally
      UnlockWrite;
    end;
  end;
end;

procedure TGPSModuleByCOMPortConfig.SetDelay(const AValue: Integer);
begin
  if (AValue >= 0) and (AValue <= 300000) then begin
    LockWrite;
    try
      if FDelay <> AValue then begin
        FDelay := AValue;
        SetChanged;
      end;
    finally
      UnlockWrite;
    end;
  end;
end;

procedure TGPSModuleByCOMPortConfig.SetNMEALog(const AValue: Boolean);
begin
  LockWrite;
  try
    if FNMEALog <> AValue then begin
      FNMEALog := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TGPSModuleByCOMPortConfig.SetPort(const AValue: Integer);
begin
  if (AValue >= 1) and (AValue <= 255) then begin
    LockWrite;
    try
      if FPort <> AValue then begin
        FPort := AValue;
        SetChanged;
      end;
    finally
      UnlockWrite;
    end;
  end;
end;

end.
