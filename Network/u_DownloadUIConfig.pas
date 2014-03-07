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

unit u_DownloadUIConfig;

interface

uses
  t_CommonTypes,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ThreadConfig,
  i_DownloadUIConfig,
  u_ConfigDataElementComplexBase;

type
  TDownloadUIConfig = class(TConfigDataElementComplexBase, IDownloadUIConfig)
  private
    FUseDownload: TTileSource;
    FTilesOut: Integer;
    FTileMaxAgeInInternet: TDateTime;
    FRequestCount: Integer;
    FThreadConfig: IThreadConfig;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetUseDownload: TTileSource;
    procedure SetUseDownload(const AValue: TTileSource);

    function GetTileMaxAgeInInternet: TDateTime;
    procedure SetTileMaxAgeInInternet(const AValue: TDateTime);

    function GetTilesOut: Integer;
    procedure SetTilesOut(const AValue: Integer);

    function GetMapUiRequestCount: Integer;
    procedure SetMapUiRequestCount(const AValue: Integer);

    function GetThreadConfig: IThreadConfig;
  public
    constructor Create;
  end;

implementation

uses
  Classes,
  u_ConfigSaveLoadStrategyBasicUseProvider,
  u_ThreadConfig;

{ TDownloadUIConfig }

constructor TDownloadUIConfig.Create;
begin
  inherited Create;
  FUseDownload := tsCacheInternet;
  FTilesOut := 0;
  FTileMaxAgeInInternet := 1 / 24 / 60;
  FRequestCount := 32;

  FThreadConfig := TThreadConfig.Create(tpLowest);
  Add(FThreadConfig, TConfigSaveLoadStrategyBasicUseProvider.Create);
end;

procedure TDownloadUIConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    case AConfigData.ReadInteger('TileSource', Integer(FUseDownload)) of
      0: begin
        FUseDownload := tsInternet;
      end;
      1: begin
        FUseDownload := tsCache;
      end;
      2: begin
        FUseDownload := tsCacheInternet;
      end;
    else begin
      FUseDownload := tsCache;
    end;
    end;
    FTileMaxAgeInInternet := AConfigData.ReadTime('TileMaxAgeInInternet', FTileMaxAgeInInternet);
    FTilesOut := AConfigData.ReadInteger('TilesOut', FTilesOut);
    FRequestCount := AConfigData.ReadInteger('QueueRequestCount', FRequestCount);
    SetChanged;
  end;
end;

procedure TDownloadUIConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  case FUseDownload of
    tsInternet: begin
      AConfigData.WriteInteger('TileSource', 0);
    end;
    tsCache: begin
      AConfigData.WriteInteger('TileSource', 1);
    end;
    tsCacheInternet: begin
      AConfigData.WriteInteger('TileSource', 2);
    end;
  end;
  AConfigData.WriteTime('TileMaxAgeInInternet', FTileMaxAgeInInternet);
  AConfigData.WriteInteger('TilesOut', FTilesOut);
  AConfigData.WriteInteger('QueueRequestCount', FRequestCount);
end;

function TDownloadUIConfig.GetThreadConfig: IThreadConfig;
begin
  Result := FThreadConfig;
end;

function TDownloadUIConfig.GetTileMaxAgeInInternet: TDateTime;
begin
  LockRead;
  try
    Result := FTileMaxAgeInInternet;
  finally
    UnlockRead;
  end;
end;

function TDownloadUIConfig.GetTilesOut: Integer;
begin
  LockRead;
  try
    Result := FTilesOut;
  finally
    UnlockRead;
  end;
end;

function TDownloadUIConfig.GetUseDownload: TTileSource;
begin
  LockRead;
  try
    Result := FUseDownload;
  finally
    UnlockRead;
  end;
end;

function TDownloadUIConfig.GetMapUiRequestCount: Integer;
begin
  LockRead;
  try
    Result := FRequestCount;
  finally
    UnlockRead;
  end;
end;

procedure TDownloadUIConfig.SetTileMaxAgeInInternet(const AValue: TDateTime);
begin
  LockWrite;
  try
    if FTileMaxAgeInInternet <> AValue then begin
      FTileMaxAgeInInternet := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TDownloadUIConfig.SetTilesOut(const AValue: Integer);
begin
  LockWrite;
  try
    if FTilesOut <> AValue then begin
      FTilesOut := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TDownloadUIConfig.SetUseDownload(const AValue: TTileSource);
begin
  LockWrite;
  try
    if FUseDownload <> AValue then begin
      FUseDownload := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TDownloadUIConfig.SetMapUiRequestCount(const AValue: Integer);
begin
  LockWrite;
  try
    if FRequestCount <> AValue then begin
      FRequestCount := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
