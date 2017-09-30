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

unit u_MarkPictureConfig;

interface

uses
  Classes,
  t_GeoTypes,
  c_MarkPictureAnchor,
  i_PathConfig,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MarkPictureConfig,
  u_ConfigDataElementBase;

type
  TMarkPictureConfig = class(TConfigDataElementBase, IMarkPictureConfig)
  private
    FRootPath: string;
    FMarksIconsPath: IPathConfig;
    FFolderConfigList: TStringList;
    procedure Clear;
    procedure BuildFolderConfigs;
    function MakeConfigName(const AConfigPath: string): string;
    function GetConfigByPicName(const APicName: string): IMarkPictureConfig;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    { IMarkPictureConfig }
    function GetDefaultAnchor(const APicName: string): TDoublePoint;
    function GetAnchor(const APicName: string): TDoublePoint;
    procedure SetAnchor(const APicName: string; const AAnchor: TDoublePoint);
  public
    constructor Create(const AMarksIconsPath: IPathConfig);
    destructor Destroy; override;
  end;

implementation

uses
  Windows,
  SysUtils,
  StrUtils,
  u_MarkPictureConfigByFolder;

procedure GetFoldersList(const APath: string; var AList: TStringList);
var
  VRec: TSearchRec;
  VPath: string;
begin
  VPath := IncludeTrailingPathDelimiter(APath);

  AList.Add(AnsiLowerCase(VPath));

  if FindFirst(VPath + '*.*', faDirectory, VRec) = 0 then begin
    try
      repeat
        if ((VRec.Attr and faDirectory) <> 0) and (VRec.Name <> '.') and (VRec.Name <> '..') then begin
          GetFoldersList(VPath + VRec.Name, AList); // recursion
        end;
      until FindNext(VRec) <> 0;
    finally
      FindClose(VRec);
    end;
  end;
end;

{ TMarkPictureConfig }

constructor TMarkPictureConfig.Create(
  const AMarksIconsPath: IPathConfig
);
begin
  Assert(AMarksIconsPath <> nil);
  inherited Create;
  FMarksIconsPath := AMarksIconsPath;
  FRootPath := AnsiLowerCase(IncludeTrailingPathDelimiter(FMarksIconsPath.FullPath));

  FFolderConfigList := TStringList.Create;
  FFolderConfigList.Sorted := False;
  FFolderConfigList.CaseSensitive := False;
  FFolderConfigList.Duplicates := dupIgnore;

  BuildFolderConfigs;
end;

destructor TMarkPictureConfig.Destroy;
begin
  Clear;
  FreeAndNil(FFolderConfigList);
  inherited Destroy;
end;

procedure TMarkPictureConfig.BuildFolderConfigs;
var
  I: Integer;
  VFoldersList: TStringList;
  VFolderConfig: IMarkPictureConfig;
  VConfigName: string;
begin
  VFoldersList := TStringList.Create;
  try
    GetFoldersList(FRootPath, VFoldersList);
    for I := 0 to VFoldersList.Count - 1 do begin
      VFolderConfig := TMarkPictureConfigByFolder.Create(VFoldersList.Strings[I]);
      VConfigName := MakeConfigName(VFoldersList.Strings[I]);
      VFolderConfig._AddRef;
      FFolderConfigList.AddObject(VConfigName, TObject(Pointer(VFolderConfig)));
    end;
  finally
    VFoldersList.Free;
  end;
end;

procedure TMarkPictureConfig.Clear;
var
  I: Integer;
  VFolderConfig: IMarkPictureConfig;
begin
  if Assigned(FFolderConfigList) then begin
    for I := 0 to FFolderConfigList.Count - 1 do begin
      VFolderConfig := IMarkPictureConfig(Pointer(FFolderConfigList.Objects[I]));
      if Assigned(VFolderConfig) then begin
        VFolderConfig._Release;
      end;
    end;
    FFolderConfigList.Clear;
  end;
end;

procedure TMarkPictureConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider
);
var
  I: Integer;
  VFolderConfig: IMarkPictureConfig;
begin
  inherited;
  for I := 0 to FFolderConfigList.Count - 1 do begin
    VFolderConfig := IMarkPictureConfig(Pointer(FFolderConfigList.Objects[I]));
    if Assigned(VFolderConfig) then begin
      VFolderConfig.ReadConfig(nil);
    end;
  end;
end;

procedure TMarkPictureConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
var
  I: Integer;
  VFolderConfig: IMarkPictureConfig;
begin
  inherited;
  for I := 0 to FFolderConfigList.Count - 1 do begin
    VFolderConfig := IMarkPictureConfig(Pointer(FFolderConfigList.Objects[I]));
    if Assigned(VFolderConfig) then begin
      VFolderConfig.WriteConfig(nil);
    end;
  end;
end;

function TMarkPictureConfig.GetDefaultAnchor(
  const APicName: string
): TDoublePoint;
var
  VFolderConfig: IMarkPictureConfig;
begin
  VFolderConfig := GetConfigByPicName(APicName);
  Result := VFolderConfig.GetDefaultAnchor(APicName);
end;

function TMarkPictureConfig.GetAnchor(
  const APicName: string
): TDoublePoint;
var
  VFolderConfig: IMarkPictureConfig;
begin
  VFolderConfig := GetConfigByPicName(APicName);
  Result := VFolderConfig.GetAnchor(APicName);
end;

procedure TMarkPictureConfig.SetAnchor(
  const APicName: string;
  const AAnchor: TDoublePoint
);
var
  VFolderConfig: IMarkPictureConfig;
begin
  VFolderConfig := GetConfigByPicName(APicName);
  VFolderConfig.SetAnchor(APicName, AAnchor);
end;

function TMarkPictureConfig.GetConfigByPicName(
  const APicName: string
): IMarkPictureConfig;
var
  I: Integer;
  VConfigName: string;
begin
  Assert(APicName <> '');
  VConfigName := MakeConfigName(AnsiLowerCase(ExtractFilePath(APicName)));
  I := FFolderConfigList.IndexOf(VConfigName);
  if I >= 0 then begin
    Result := IMarkPictureConfig(Pointer(FFolderConfigList.Objects[I]));
    Assert(Result <> nil);
  end else begin
    raise Exception.CreateFmt(
      'Can''t find config "%s" for mark picture "%s"',
      [VConfigName, APicName]
    );
  end;

end;

function TMarkPictureConfig.MakeConfigName(
  const AConfigPath: string
): string;
begin
  Assert(AConfigPath <> '');
  if AnsiSameStr(FRootPath, AConfigPath) then begin
    Result := '.\';
  end else begin
    if AnsiStartsStr(FRootPath, AConfigPath) then begin
      Result := StringReplace(AConfigPath, FRootPath, '', [rfIgnoreCase]);
    end else begin
      raise Exception.CreateFmt(
        'ConfigPath "%s" not in RootPath "%s"',
        [AConfigPath, FRootPath]
      );
    end;
  end;
end;

end.
