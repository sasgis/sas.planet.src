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

unit u_GPSConfig;

interface

uses
  i_PathConfig,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_GPSModuleByCOMPortConfig,
  i_GPSConfig,
  vsagps_public_tracks,
  u_ConfigDataElementComplexBase;

type
  TGPSConfig = class(TConfigDataElementComplexBase, IGPSConfig)
  private
    FGPSEnabled: Boolean;
    FNoDataTimeOut: Integer;
    FWriteLogs: array [TVSAGPS_TrackType] of Boolean;
    FLogPath: IPathConfig;
    FModuleConfig: IGPSModuleByCOMPortConfig;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    procedure InternalApplyLogWrite(const AValues: Byte);
    function InternalGetLogWriteAsInt: Byte;
    procedure InternalSetWriteLogValue(
      const ATrackTypes: TVSAGPS_TrackTypes;
      const AValue: Boolean
    );
    function InternalGetWriteLogValue(out ATrackTypes: TVSAGPS_TrackTypes): Boolean;
  private
    function GetGPSEnabled: Boolean;
    procedure SetGPSEnabled(const AValue: Boolean);

    function GetNoDataTimeOut: Integer;
    procedure SetNoDataTimeOut(const AValue: Integer);

    function GetWriteLog(const ATrackType: TVSAGPS_TrackType): Boolean;
    procedure SetWriteLog(
      const ATrackType: TVSAGPS_TrackType;
      const AValue: Boolean
    );

    function GetLogPath: WideString;
    function GetModuleConfig: IGPSModuleByCOMPortConfig;

    function AllowWriteLog(out ATrackTypes: TVSAGPS_TrackTypes): Boolean;
    procedure AbortWriteLog(const ATrackTypes: TVSAGPS_TrackTypes);
  public
    constructor Create(const ALogPath: IPathConfig);
  end;

implementation

uses
  u_ConfigSaveLoadStrategyBasicProviderSubItem,
  u_GPSModuleByCOMPortConfig;

{ TGPSConfig }

procedure TGPSConfig.AbortWriteLog(const ATrackTypes: TVSAGPS_TrackTypes);
begin
  LockWrite;
  try
    InternalSetWriteLogValue(ATrackTypes, FALSE);
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

function TGPSConfig.AllowWriteLog(out ATrackTypes: TVSAGPS_TrackTypes): Boolean;
begin
  LockRead;
  try
    Result := InternalGetWriteLogValue(ATrackTypes);
  finally
    UnlockRead;
  end;
end;

constructor TGPSConfig.Create(const ALogPath: IPathConfig);
begin
  inherited Create;
  FLogPath := ALogPath;
  FGPSEnabled := False;
  InternalApplyLogWrite(0);
  FNoDataTimeOut := 5000;
  FModuleConfig := TGPSModuleByCOMPortConfig.Create(FLogPath);
  Add(FModuleConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('Module'));
end;

procedure TGPSConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FGPSEnabled := AConfigData.ReadBool('Enabled', FGPSEnabled);
    FNoDataTimeOut := AConfigData.ReadInteger('NoDataTimeOut', FNoDataTimeOut);
    InternalApplyLogWrite(AConfigData.ReadInteger('LogWrite', 0));
    SetChanged;
  end;
end;

procedure TGPSConfig.DoWriteConfig(const AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('Enabled', FGPSEnabled);
  AConfigData.WriteInteger('LogWrite', InternalGetLogWriteAsInt);
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
  Result := FLogPath.FullPath;
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

function TGPSConfig.GetWriteLog(const ATrackType: TVSAGPS_TrackType): Boolean;
begin
  LockRead;
  try
    Result := FWriteLogs[ATrackType];
  finally
    UnlockRead;
  end;
end;

procedure TGPSConfig.InternalApplyLogWrite(const AValues: Byte);
var
  VTrackTypes: TVSAGPS_TrackTypes;
begin
  VTrackTypes := TVSAGPS_TrackTypes(AValues);
  InternalSetWriteLogValue(VTrackTypes, TRUE);
end;

function TGPSConfig.InternalGetLogWriteAsInt: Byte;
var
  VTrackTypes: TVSAGPS_TrackTypes;
begin
  InternalGetWriteLogValue(VTrackTypes);
  Result := Byte(VTrackTypes);
end;

function TGPSConfig.InternalGetWriteLogValue(out ATrackTypes: TVSAGPS_TrackTypes): Boolean;
var
  i: TVSAGPS_TrackType;
begin
  Result := FALSE;
  ATrackTypes := [];
  // add enabled items
  for i := Low(TVSAGPS_TrackType) to High(TVSAGPS_TrackType) do begin
    if FWriteLogs[i] then begin
      Result := TRUE;
      System.Include(ATrackTypes, i);
    end;
  end;
end;

procedure TGPSConfig.InternalSetWriteLogValue(
  const ATrackTypes: TVSAGPS_TrackTypes;
  const AValue: Boolean
);
var
  i: TVSAGPS_TrackType;
begin
  for i := Low(TVSAGPS_TrackType) to High(TVSAGPS_TrackType) do begin
    if (i in ATrackTypes) then begin
      FWriteLogs[i] := AValue;
    end else if AValue then begin
      FWriteLogs[i] := FALSE;
    end;
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

procedure TGPSConfig.SetWriteLog(
  const ATrackType: TVSAGPS_TrackType;
  const AValue: Boolean
);
begin
  LockWrite;
  try
    if FWriteLogs[ATrackType] <> AValue then begin
      FWriteLogs[ATrackType] := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
