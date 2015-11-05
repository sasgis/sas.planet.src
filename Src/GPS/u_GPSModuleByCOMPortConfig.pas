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

unit u_GPSModuleByCOMPortConfig;

interface

uses
  Windows,
  i_PathConfig,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_GPSModuleByCOMPortSettings,
  i_GPSModuleByCOMPortConfig,
  u_ConfigDataElementBase;

type
  TGPSModuleByCOMPortConfig = class(TConfigDataElementWithStaticBase, IGPSModuleByCOMPortConfig)
  private
    FPort: DWORD;
    FBaudRate: DWORD;
    FConnectionTimeout: DWORD;
    FDelay: DWORD;
    FLowLevelLog: Boolean;
    FLogPath: IPathConfig;
    FGPSOrigin: TGPSOrigin;
    FAutodetectCOMOnConnect: Boolean;
    FAutodetectCOMFlags: DWORD;
  protected
    function CreateStatic: IInterface; override;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetPort: DWORD;
    procedure SetPort(const AValue: DWORD);

    function GetBaudRate: DWORD;
    procedure SetBaudRate(const AValue: DWORD);

    function GetConnectionTimeout: DWORD;
    procedure SetConnectionTimeout(const AValue: DWORD);

    function GetDelay: DWORD;
    procedure SetDelay(const AValue: DWORD);

    function GetLowLevelLog: Boolean;
    procedure SetLowLevelLog(const AValue: Boolean);

    function GetLogPath: string;

    function GetStatic: IGPSModuleByCOMPortSettings;

    function GetGPSOrigin: TGPSOrigin;
    procedure SetGPSOrigin(const AValue: TGPSOrigin);

    function GetAutodetectCOMOnConnect: Boolean;
    procedure SetAutodetectCOMOnConnect(const AValue: Boolean);

    function GetAutodetectCOMFlags: DWORD;
    procedure SetAutodetectCOMFlags(const AValue: DWORD);
  public
    constructor Create(const ALogPath: IPathConfig);
  end;

implementation

uses
  u_GPSModuleByCOMPortSettings;

{ TGPSModuleByCOMPortConfig }

constructor TGPSModuleByCOMPortConfig.Create(const ALogPath: IPathConfig);
begin
  inherited Create;
  FLogPath := ALogPath;
  FPort := 1;
  FBaudRate := 4800;
  FConnectionTimeout := 300;
  FDelay := 1000;
  FLowLevelLog := False;
  FGPSOrigin := gpsoNMEA;
  FAutodetectCOMOnConnect := FALSE;
  FAutodetectCOMFlags := 0;
end;

function TGPSModuleByCOMPortConfig.CreateStatic: IInterface;
var
  VStatic: IGPSModuleByCOMPortSettings;
begin
  VStatic :=
    TGPSModuleByCOMPortSettings.Create(
      FPort,
      FBaudRate,
      FConnectionTimeout,
      FDelay,
      FLowLevelLog,
      FLogPath.FullPath,
      FGPSOrigin,
      FAutodetectCOMOnConnect,
      FAutodetectCOMFlags
    );
  Result := VStatic;
end;

procedure TGPSModuleByCOMPortConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider
);
begin
  inherited;
  if AConfigData <> nil then begin
    SetPort(AConfigData.ReadInteger('COM', FPort));
    SetBaudRate(AConfigData.ReadInteger('BaudRate', FBaudRate));
    SetConnectionTimeout(AConfigData.ReadInteger('timeout', FConnectionTimeout));
    SetDelay(AConfigData.ReadInteger('update', FDelay));
    SetLowLevelLog(AConfigData.ReadBool('NMEAlog', FLowLevelLog));
    SetGPSOrigin(TGPSOrigin(AConfigData.ReadInteger('USBGarmin', Ord(FGPSOrigin))));
    SetAutodetectCOMOnConnect(AConfigData.ReadBool('AutodetectCOMOnConnect', FAutodetectCOMOnConnect));
    SetAutodetectCOMFlags(AConfigData.ReadInteger('AutodetectCOMFlags', FAutodetectCOMFlags));
  end;
end;

procedure TGPSModuleByCOMPortConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteInteger('COM', FPort);
  AConfigData.WriteInteger('BaudRate', FBaudRate);
  AConfigData.WriteInteger('timeout', FConnectionTimeout);
  AConfigData.WriteInteger('update', FDelay);
  AConfigData.WriteBool('NMEAlog', FLowLevelLog);
  AConfigData.WriteInteger('USBGarmin', Ord(FGPSOrigin));
  AConfigData.WriteBool('AutodetectCOMOnConnect', FAutodetectCOMOnConnect);
  AConfigData.WriteInteger('AutodetectCOMFlags', FAutodetectCOMFlags);
end;

function TGPSModuleByCOMPortConfig.GetAutodetectCOMFlags: DWORD;
begin
  LockRead;
  try
    Result := FAutodetectCOMFlags;
  finally
    UnlockRead;
  end;
end;

function TGPSModuleByCOMPortConfig.GetAutodetectCOMOnConnect: Boolean;
begin
  LockRead;
  try
    Result := FAutodetectCOMOnConnect;
  finally
    UnlockRead;
  end;
end;

function TGPSModuleByCOMPortConfig.GetBaudRate: DWORD;
begin
  LockRead;
  try
    Result := FBaudRate;
  finally
    UnlockRead;
  end;
end;

function TGPSModuleByCOMPortConfig.GetConnectionTimeout: DWORD;
begin
  LockRead;
  try
    Result := FConnectionTimeout;
  finally
    UnlockRead;
  end;
end;

function TGPSModuleByCOMPortConfig.GetDelay: DWORD;
begin
  LockRead;
  try
    Result := FDelay;
  finally
    UnlockRead;
  end;
end;

function TGPSModuleByCOMPortConfig.GetLogPath: string;
begin
  Result := FLogPath.FullPath;
end;

function TGPSModuleByCOMPortConfig.GetLowLevelLog: Boolean;
begin
  LockRead;
  try
    Result := FLowLevelLog;
  finally
    UnlockRead;
  end;
end;

function TGPSModuleByCOMPortConfig.GetPort: DWORD;
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
  Result := IGPSModuleByCOMPortSettings(GetStaticInternal);
end;

function TGPSModuleByCOMPortConfig.GetGPSOrigin: TGPSOrigin;
begin
  LockRead;
  try
    Result := FGPSOrigin;
  finally
    UnlockRead;
  end;
end;

procedure TGPSModuleByCOMPortConfig.SetAutodetectCOMFlags(const AValue: DWORD);
begin
  LockWrite;
  try
    if FAutodetectCOMFlags <> AValue then begin
      FAutodetectCOMFlags := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TGPSModuleByCOMPortConfig.SetAutodetectCOMOnConnect(const AValue: Boolean);
begin
  LockWrite;
  try
    if FAutodetectCOMOnConnect <> AValue then begin
      FAutodetectCOMOnConnect := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TGPSModuleByCOMPortConfig.SetBaudRate(const AValue: DWORD);
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

procedure TGPSModuleByCOMPortConfig.SetConnectionTimeout(const AValue: DWORD);
begin
  if (AValue > 0) and (AValue <= 6000) then begin
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

procedure TGPSModuleByCOMPortConfig.SetDelay(const AValue: DWORD);
begin
  if (AValue > 0) and (AValue <= 300000) then begin
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

procedure TGPSModuleByCOMPortConfig.SetLowLevelLog(const AValue: Boolean);
begin
  LockWrite;
  try
    if FLowLevelLog <> AValue then begin
      FLowLevelLog := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TGPSModuleByCOMPortConfig.SetPort(const AValue: DWORD);
begin
  if (AValue > 0) and (AValue <= 255) then begin
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

procedure TGPSModuleByCOMPortConfig.SetGPSOrigin(const AValue: TGPSOrigin);
begin
  LockWrite;
  try
    if FGPSOrigin <> AValue then begin
      FGPSOrigin := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
