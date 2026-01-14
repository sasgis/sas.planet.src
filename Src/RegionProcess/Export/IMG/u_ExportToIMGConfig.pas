{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_ExportToIMGConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ExportToIMGConfig,
  u_ConfigDataElementBase;

type
  TExportToIMGConfig = class(TConfigDataElementBase, IExportToIMGConfig)
  private
    FMapCompilerPath: string;
    FMapCompilerLicensePath: string;
    FGMTPath: string;
    FTempFilesPath: string;
    FZoomOptionsVisible: Boolean;
    FSASZoomList: string;

    FIsInitialized: Boolean;
    procedure DoLazyInitialize;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    { IExportToIMGConfig }
    function GetMapCompilerPath: string;
    procedure SetMapCompilerPath(const AValue: string);

    function GetMapCompilerLicensePath: string;
    procedure SetMapCompilerLicensePath(const AValue: string);

    function GetGMTPath: string;
    procedure SetGMTPath(const AValue: string);

    function GetTempFilesPath: string;
    procedure SetTempFilesPath(const AValue: string);

    function GetZoomOptionsVisible: Boolean;
    procedure SetZoomOptionsVisible(AValue: Boolean);

    function GetSASZoomList: string;
    procedure SetSASZoomList(const AValue: string);
  end;


implementation

uses
  Windows,
  Registry,
  SysUtils;

{ TExportToIMGConfig }

procedure TExportToIMGConfig.DoLazyInitialize;
var
  VRegistry: TRegistry;
  VMpcPath: string;
  VPath: string;
  VSearchRec: TSearchRec;
begin
  if FIsInitialized then begin
    Exit;
  end;

  LockWrite;
  try
    if FIsInitialized then begin
      Exit;
    end;

    VRegistry := TRegistry.Create;
    try
      VMpcPath := '';

      if FMapCompilerPath = '' then begin
        VRegistry.RootKey := HKEY_LOCAL_MACHINE;
        if VRegistry.OpenKeyReadOnly('SOFTWARE\Garmin\MPC') and VRegistry.ValueExists('InstallPath') then begin
          VMpcPath := IncludeTrailingPathDelimiter(VRegistry.ReadString('InstallPath'));
          if DirectoryExists(VMpcPath) then begin
            VPath := VMpcPath + 'Tools\bld_gmap32\bld_gmap32.exe';
            if FileExists(VPath) then begin
              FMapCompilerPath := VPath;
            end;
          end;
        end;
      end else begin
        VMpcPath := ExtractFilePath(FMapCompilerPath);
      end;

      if FGMTPath = '' then begin
        if VRegistry.OpenKeyReadOnly('SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\{1873789F-59D5-4002-8A2F-60A827B78F98}_is1') and VRegistry.ValueExists('InstallLocation') then begin
          VPath := IncludeTrailingPathDelimiter(VRegistry.ReadString('InstallLocation'));
          if DirectoryExists(VPath) then begin
            VPath := VPath + 'gmt\gmt.exe';
            if FileExists(VPath) then begin
              FGMTPath := VPath;
            end;
          end;
        end;
      end;

      if FMapCompilerLicensePath = '' then begin
        VRegistry.RootKey := HKEY_CURRENT_USER;
        if VRegistry.OpenKeyReadOnly('Software\GARMIN\ProductCreator\LastLicenseLocation') and VRegistry.ValueExists('Location') then begin
          VPath := VRegistry.ReadString('Location');
          if FileExists(VPath) then begin
            FMapCompilerLicensePath := VPath
          end else begin
            if FindFirst(VMpcPath + '*.mpl', faAnyFile, VSearchRec) = 0 then begin
              FMapCompilerLicensePath := VMpcPath + VSearchRec.Name;
              FindClose(VSearchRec);
            end;
          end;
        end;
      end;

      if FTempFilesPath = '' then begin
        SetLength(FTempFilesPath, MAX_PATH);
        SetLength(FTempFilesPath, Windows.GetTempPath(Length(FTempFilesPath), PChar(FTempFilesPath)));
      end;
    finally
      VRegistry.Free;
    end;

    FIsInitialized := True;
  finally
    UnlockWrite;
  end;
end;

procedure TExportToIMGConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;

  if AConfigData <> nil then begin
    FMapCompilerPath := AConfigData.ReadString('MapCompilerPath', FMapCompilerPath);
    FMapCompilerLicensePath := AConfigData.ReadString('MapCompilerLicensePath', FMapCompilerLicensePath);
    FGMTPath := AConfigData.ReadString('GMTPath', FGMTPath);
    FTempFilesPath := AConfigData.ReadString('TempFilesPath', FTempFilesPath);
    FZoomOptionsVisible := AConfigData.ReadBool('ZoomOptionsVisible', FZoomOptionsVisible);
    FSASZoomList := AConfigData.ReadString('SASZoomList', FSASZoomList);
    SetChanged;
  end;
end;

procedure TExportToIMGConfig.DoWriteConfig(const AConfigData: IConfigDataWriteProvider);
begin
  inherited;

  AConfigData.WriteString('MapCompilerPath', FMapCompilerPath);
  AConfigData.WriteString('MapCompilerLicensePath', FMapCompilerLicensePath);
  AConfigData.WriteString('GMTPath', FGMTPath);
  AConfigData.WriteString('TempFilesPath', FTempFilesPath);
  AConfigData.WriteBool('ZoomOptionsVisible', FZoomOptionsVisible);
  AConfigData.WriteString('SASZoomList', FSASZoomList);
end;

function TExportToIMGConfig.GetMapCompilerPath: string;
begin
  DoLazyInitialize;

  LockRead;
  try
    Result := FMapCompilerPath;
  finally
    UnlockRead;
  end;
end;

function TExportToIMGConfig.GetSASZoomList: string;
begin
  DoLazyInitialize;

  LockRead;
  try
    Result := FSASZoomList;
  finally
    UnlockRead;
  end;
end;

function TExportToIMGConfig.GetTempFilesPath: string;
begin
  DoLazyInitialize;

  LockRead;
  try
    Result := FTempFilesPath;
  finally
    UnlockRead;
  end;
end;

function TExportToIMGConfig.GetZoomOptionsVisible: Boolean;
begin
  DoLazyInitialize;

  LockRead;
  try
    Result := FZoomOptionsVisible;
  finally
    UnlockRead;
  end;
end;

function TExportToIMGConfig.GetMapCompilerLicensePath: string;
begin
  DoLazyInitialize;

  LockRead;
  try
    Result := FMapCompilerLicensePath;
  finally
    UnlockRead;
  end;
end;

function TExportToIMGConfig.GetGMTPath: string;
begin
  DoLazyInitialize;

  LockRead;
  try
    Result := FGMTPath;
  finally
    UnlockRead;
  end;
end;

procedure TExportToIMGConfig.SetMapCompilerPath(const AValue: string);
begin
  LockWrite;
  try
    if FMapCompilerPath <> AValue then begin
      FMapCompilerPath := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TExportToIMGConfig.SetSASZoomList(const AValue: string);
begin
  LockWrite;
  try
    if FSASZoomList <> AValue then begin
      FSASZoomList := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TExportToIMGConfig.SetTempFilesPath(const AValue: string);
begin
  LockWrite;
  try
    if FTempFilesPath <> AValue then begin
      FTempFilesPath := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TExportToIMGConfig.SetZoomOptionsVisible(AValue: Boolean);
begin
  LockWrite;
  try
    if FZoomOptionsVisible <> AValue then begin
      FZoomOptionsVisible := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TExportToIMGConfig.SetMapCompilerLicensePath(const AValue: string);
begin
  LockWrite;
  try
    if FMapCompilerLicensePath <> AValue then begin
      FMapCompilerLicensePath := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TExportToIMGConfig.SetGMTPath(const AValue: string);
begin
  LockWrite;
  try
    if FGMTPath <> AValue then begin
      FGMTPath := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
