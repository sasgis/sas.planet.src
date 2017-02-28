{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2017, SAS.Planet development team.                      *}
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

unit u_ExportToIMGConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ExportToIMGConfig,
  u_BaseInterfacedObject,
  u_ConfigDataElementBase;

type
  TExportToIMGConfig = class(TConfigDataElementBase, IExportToIMGConfig)
  private
    FMapCompilerPath: String;
    FMapCompilerLicensePath: String;
    FGMTPath: String;
    FZoomOptionsVisible: Boolean;
    FSASZoomList: String;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetMapCompilerPath: String;
    procedure SetMapCompilerPath(const AValue: String);

    function GetMapCompilerLicensePath: String;
    procedure SetMapCompilerLicensePath(const AValue: String);

    function GetGMTPath: String;
    procedure SetGMTPath(const AValue: String);

    function GetZoomOptionsVisible: Boolean;
    procedure SetZoomOptionsVisible(AValue: Boolean);

    function GetSASZoomList: String;
    procedure SetSASZoomList(const AValue: String);
  public
    constructor Create;
  end;


implementation

uses
  Windows,
  Registry,
  SysUtils;

{ TExportToIMGConfig }

constructor TExportToIMGConfig.Create;
var
  Registry: TRegistry;
  MPCPath: String;
  Path: String;
  SearchRec: TSearchRec;
begin
  inherited Create;

  Registry := TRegistry.Create;
  try
    MPCPath := '';

    Registry.RootKey := HKEY_LOCAL_MACHINE;
    if Registry.OpenKeyReadOnly('SOFTWARE\Garmin\MPC') and Registry.ValueExists('InstallPath') then begin
      MPCPath := IncludeTrailingPathDelimiter(Registry.ReadString('InstallPath'));
      if DirectoryExists(MPCPath) then begin
        Path := MPCPath + 'Tools\bld_gmap32\bld_gmap32.exe';
        if FileExists(Path) then begin
          FMapCompilerPath := Path;
        end;
      end;
    end;

    if Registry.OpenKeyReadOnly('SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\{1873789F-59D5-4002-8A2F-60A827B78F98}_is1') and Registry.ValueExists('InstallLocation') then begin
      Path := IncludeTrailingPathDelimiter(Registry.ReadString('InstallLocation'));
      if DirectoryExists(Path) then begin
        Path := Path + 'gmt\gmt.exe';
        if FileExists(Path) then begin
          FGMTPath := Path;
        end;
      end;
    end;

    Registry.RootKey := HKEY_CURRENT_USER;
    if Registry.OpenKeyReadOnly('Software\GARMIN\ProductCreator\LastLicenseLocation') and Registry.ValueExists('Location') then begin
      Path := Registry.ReadString('Location');
      if FileExists(Path) then begin
        FMapCompilerLicensePath := Path
      end else begin
        if FindFirst(MPCPath + '*.mpl', faAnyFile, SearchRec) = 0 then begin
          FMapCompilerLicensePath := MPCPath + SearchRec.Name;
          FindClose(SearchRec);
        end;
      end;
    end;
  finally
    Registry.Free;
  end;
end;

procedure TExportToIMGConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider
);
begin
  inherited;

  if AConfigData <> nil then begin
    FMapCompilerPath := AConfigData.ReadString('MapCompilerPath', FMapCompilerPath);
    FMapCompilerLicensePath := AConfigData.ReadString('MapCompilerLicensePath', FMapCompilerLicensePath);
    FGMTPath := AConfigData.ReadString('GMTPath', FGMTPath);
    FZoomOptionsVisible := AConfigData.ReadBool('ZoomOptionsVisible', FZoomOptionsVisible);
    FSASZoomList := AConfigData.ReadString('SASZoomList', FSASZoomList);
    SetChanged;
  end;
end;

procedure TExportToIMGConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;

  AConfigData.WriteString('MapCompilerPath', FMapCompilerPath);
  AConfigData.WriteString('MapCompilerLicensePath', FMapCompilerLicensePath);
  AConfigData.WriteString('GMTPath', FGMTPath);
  AConfigData.WriteBool('ZoomOptionsVisible', FZoomOptionsVisible);
  AConfigData.WriteString('SASZoomList', FSASZoomList);
end;

function TExportToIMGConfig.GetMapCompilerPath: String;
begin
  LockRead;
  try
    Result := FMapCompilerPath;
  finally
    UnlockRead;
  end;
end;

function TExportToIMGConfig.GetSASZoomList: String;
begin
  LockRead;
  try
    Result := FSASZoomList;
  finally
    UnlockRead;
  end;
end;

function TExportToIMGConfig.GetZoomOptionsVisible: Boolean;
begin
  LockRead;
  try
    Result := FZoomOptionsVisible;
  finally
    UnlockRead;
  end;
end;

function TExportToIMGConfig.GetMapCompilerLicensePath: String;
begin
  LockRead;
  try
    Result := FMapCompilerLicensePath;
  finally
    UnlockRead;
  end;
end;

function TExportToIMGConfig.GetGMTPath: String;
begin
  LockRead;
  try
    Result := FGMTPath;
  finally
    UnlockRead;
  end;
end;

procedure TExportToIMGConfig.SetMapCompilerPath(
  const AValue: String);
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

procedure TExportToIMGConfig.SetSASZoomList(const AValue: String);
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

procedure TExportToIMGConfig.SetMapCompilerLicensePath(
  const AValue: String);
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

procedure TExportToIMGConfig.SetGMTPath(
  const AValue: String);
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
