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

unit u_GPSConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_GPSModuleByCOMPortConfig,
  i_GPSConfig,
  u_ConfigDataElementComplexBase;

type
  TGPSConfig = class(TConfigDataElementComplexBase, IGPSConfig)
  private
    FGPSEnabled: Boolean;
    FNoDataTimeOut: Integer;
    FWriteLog: Boolean;
    FLogPath: WideString;
    FModuleConfig: IGPSModuleByCOMPortConfig;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetGPSEnabled: Boolean;
    procedure SetGPSEnabled(const AValue: Boolean);

    function GetNoDataTimeOut: Integer;
    procedure SetNoDataTimeOut(const AValue: Integer);

    function GetWriteLog: Boolean;
    procedure SetWriteLog(const AValue: Boolean);

    function GetLogPath: WideString;
    function GetModuleConfig: IGPSModuleByCOMPortConfig;
  public
    constructor Create(ALogPath: string);
  end;

implementation

uses
  u_ConfigSaveLoadStrategyBasicProviderSubItem,
  u_GPSModuleByCOMPortConfig;

{ TGPSConfig }

constructor TGPSConfig.Create(ALogPath: string);
begin
  inherited Create;
  FLogPath := ALogPath;
  FGPSEnabled := False;
  FWriteLog := True;
  FNoDataTimeOut := 5000;  
  FModuleConfig := TGPSModuleByCOMPortConfig.Create(FLogPath);
  Add(FModuleConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('Module'));
end;

procedure TGPSConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FGPSEnabled := AConfigData.ReadBool('Enabled', FGPSEnabled);
    FNoDataTimeOut := AConfigData.ReadInteger('NoDataTimeOut', FNoDataTimeOut);
    FWriteLog := AConfigData.ReadBool('LogWrite', FWriteLog);
    SetChanged;
  end;
end;

procedure TGPSConfig.DoWriteConfig(AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('Enabled', FGPSEnabled);
  AConfigData.WriteBool('LogWrite', FWriteLog);
  AConfigData.WriteInteger('NoDataTimeOut', FNoDataTimeOut);
end;

function TGPSConfig.GetGPSEnabled: Boolean;
begin
  LockRead;
  try
    Result := FGPSEnabled;
  finally
    UnlockRead;
  end;
end;

function TGPSConfig.GetLogPath: WideString;
begin
  Result := FLogPath;
end;

function TGPSConfig.GetModuleConfig: IGPSModuleByCOMPortConfig;
begin
  Result := FModuleConfig;
end;

function TGPSConfig.GetNoDataTimeOut: Integer;
begin
  LockRead;
  try
    Result := FNoDataTimeOut;
  finally
    UnlockRead;
  end;
end;

function TGPSConfig.GetWriteLog: Boolean;
begin
  LockRead;
  try
    Result := FWriteLog;
  finally
    UnlockRead;
  end;
end;

procedure TGPSConfig.SetGPSEnabled(const AValue: Boolean);
begin
  LockWrite;
  try
    if FGPSEnabled <> AValue then begin
      FGPSEnabled := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TGPSConfig.SetNoDataTimeOut(const AValue: Integer);
begin
  LockWrite;
  try
    if FNoDataTimeOut <> AValue then begin
      FNoDataTimeOut := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TGPSConfig.SetWriteLog(const AValue: Boolean);
begin
  LockWrite;
  try
    if FWriteLog <> AValue then begin
      FWriteLog := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
