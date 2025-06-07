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

unit u_ZmpInfoProxy;

interface

uses
  Windows,
  SysUtils,
  Classes,
  i_Bitmap32Static,
  i_ProjectionSet,
  i_ConfigDataProvider,
  i_LanguageListStatic,
  i_BinaryDataListStatic,
  i_Bitmap32BufferFactory,
  i_ContentTypeSubst,
  i_AppearanceOfMarkFactory,
  i_MarkPicture,
  i_TileDownloadRequestBuilderConfig,
  i_TileDownloaderConfig,
  i_TilePostDownloadCropConfig,
  i_LanguageManager,
  i_StringByLanguage,
  i_ProjectionSetFactory,
  i_ContentTypeManager,
  i_MapAbilitiesConfig,
  i_SimpleTileStorageConfig,
  i_ImportConfig,
  i_ZmpConfig,
  i_ZmpInfo,
  u_BaseInterfacedObject;

type
  TZmpInfoGuiProxy = class(TBaseInterfacedObject, IZmpInfoGui, IZmpInfoGuiProxy)
  protected
    class var FLock: IReadWriteSync;
  private
    FGUID: TGUID;
    FLanguageManager: ILanguageManager;
    FContentTypeManager: IContentTypeManager;
    FBitmapFactory: IBitmap32StaticFactory;
    FPnum: Integer;

    FName, FInfoUrl, FParentSubMenu: IStringByLanguage;
    FIcon: IBitmap32Static;

    FZmpInfoGui: IZmpInfoGui;
    function GetZmpInfoGui: IZmpInfoGui;
    procedure SetZmpInfoGui(const AValue: IZmpInfoGui);
  private
    { IZmpInfoGui }
    function GetName: IStringByLanguage;
    function GetSortIndex: Integer;
    function GetInfoUrl: IStringByLanguage;
    function GetBmp18: IBitmap32Static;
    function GetBmp24: IBitmap32Static;
    function GetHotKey: TShortCut;
    function GetSeparator: Boolean;
    function GetParentSubMenu: IStringByLanguage;
    function GetEnabled: Boolean;
    { IZmpInfoGuiProxy }
    procedure Initialize(const AZmpMapConfig: IConfigDataProvider);
    procedure Reset;
    function GetIsInitialized: Boolean;
  public
    constructor Create(
      const AGUID: TGUID;
      const ALanguageManager: ILanguageManager;
      const AContentTypeManager: IContentTypeManager;
      const ABitmapFactory: IBitmap32StaticFactory;
      const APnum: Integer
    );
  end;

  TZmpInfoProxy = class(TBaseInterfacedObject, IZmpInfo, IZmpInfoProxy)
  protected
    class var FLock: IReadWriteSync;
  private
    FGUID: TGUID;
    FZmpConfig: IZmpConfig;
    FLanguageManager: ILanguageManager;
    FProjectionSetFactory: IProjectionSetFactory;
    FContentTypeManager: IContentTypeManager;
    FAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
    FMarkPictureList: IMarkPictureList;
    FBitmapFactory: IBitmap32StaticFactory;
    FPnum: Integer;

    FGuiProxy: IZmpInfoGUIProxy;
    FAbilities: IMapAbilitiesConfigStatic;
    FTileDownloaderConfig: ITileDownloaderConfigStatic;
    FIsBitmapTiles: Boolean;
    FIsLayer: Boolean;

    FLicense: IStringByLanguage;

    FZmpInfo: IZmpInfo;
    function GetZmpInfo: IZmpInfo;
    procedure SetZmpInfo(const AValue: IZmpInfo);
  private
    { IZmpInfo }
    function GetGUID: TGUID;
    function GetIsLayer: Boolean;
    function GetGUI: IZmpInfoGUI;
    function GetLayerZOrder: Integer;
    function GetLicense: IStringByLanguage;
    function GetFileName: string;
    function GetVersion: string;
    function GetTileDownloadRequestBuilderConfig: ITileDownloadRequestBuilderConfigStatic;
    function GetTileDownloaderConfig: ITileDownloaderConfigStatic;
    function GetTilePostDownloadCropConfig: ITilePostDownloadCropConfigStatic;
    function GetContentTypeSubst: IContentTypeSubst;
    function GetProjectionSet: IProjectionSet;
    function GetViewProjectionSet: IProjectionSet;
    function GetAbilities: IMapAbilitiesConfigStatic;
    function GetPointParams: IImportPointParams;
    function GetLineParams: IImportLineParams;
    function GetPolyParams: IImportPolyParams;
    function GetEmptyTileSamples: IBinaryDataListStatic;
    function GetBanTileSamples: IBinaryDataListStatic;
    function GetStorageConfig: ISimpleTileStorageConfigStatic;
    function GetDataProvider: IConfigDataProvider;
    { IZmpInfoProxy }
    procedure Initialize(const AZmpMapConfig: IConfigDataProvider);
    procedure Reset;
    function GetIsInitialized: Boolean;
    function GetIsBitmapTiles: Boolean;
    function GetIsKmlTiles: Boolean;
  public
    constructor Create(
      const AZmpConfig: IZmpConfig;
      const ALanguageManager: ILanguageManager;
      const AProjectionSetFactory: IProjectionSetFactory;
      const AContentTypeManager: IContentTypeManager;
      const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
      const AMarkPictureList: IMarkPictureList;
      const ABitmapFactory: IBitmap32StaticFactory;
      const APnum: Integer;
      const AIsBitmapTiles, AIsLayer: Boolean
    );
  end;

implementation

uses
  Types,
  Graphics,
  GR32,
  gnugettext,
  c_ZeroGUID,
  i_BinaryData,
  i_StringListStatic,
  i_BitmapTileSaveLoad,
  i_Appearance,
  i_TileStorageAbilities,
  u_AnsiStr,
  u_BinaryDataListStatic,
  u_StringByLanguageWithStaticList,
  u_TileDownloadRequestBuilderConfig,
  u_TileDownloaderConfigStatic,
  u_TilePostDownloadCropConfigStatic,
  u_ContentTypeSubstByList,
  u_MapAbilitiesConfigStatic,
  u_TileStorageAbilities,
  u_ImportConfig,
  u_ConfigProviderHelpers,
  u_SimpleTileStorageConfigStatic,
  u_ResStrings,
  u_Synchronizer,
  u_ZmpInfo;

function InternalMakeStringByLanguage: IStringByLanguage;
var
  VValueList: TStringList;
begin
  VValueList := TStringList.Create;
  try
    VValueList.Add('');
    Result := TStringByLanguageWithStaticList.Create(VValueList);
  finally
    VValueList.Free;
  end;
end;

function CreateDefaultIcon(
  const ABitmapFactory: IBitmap32StaticFactory
): IBitmap32Static;
var
  VBitmap: TBitmap32;
begin
  VBitmap := TBitmap32.Create;
  try
    VBitmap.SetSize(32, 32);
    VBitmap.Clear(Color32(clMenu));
    Result :=
      ABitmapFactory.Build(
        Types.Point(VBitmap.Width, VBitmap.Height),
        VBitmap.Bits
      )
  finally
    VBitmap.Free;
  end;
end;

{ TZmpInfoGuiProxy }

constructor TZmpInfoGuiProxy.Create(
  const AGUID: TGUID;
  const ALanguageManager: ILanguageManager;
  const AContentTypeManager: IContentTypeManager;
  const ABitmapFactory: IBitmap32StaticFactory;
  const APnum: Integer
);
begin
  inherited Create;

  FGUID := AGUID;
  FLanguageManager := ALanguageManager;
  FContentTypeManager := AContentTypeManager;
  FBitmapFactory := ABitmapFactory;
  FPnum := APnum;

  FIcon := CreateDefaultIcon(ABitmapFactory);

  FName := InternalMakeStringByLanguage;
  FParentSubMenu := FName;
  FInfoUrl := FName;
end;

procedure TZmpInfoGuiProxy.Initialize(const AZmpMapConfig: IConfigDataProvider);
var
  VZmpInfoGui: IZmpInfoGui;
  VConfigIni: IConfigDataProvider;
  VConfigIniParams: IConfigDataProvider;
begin
  VConfigIni := AZmpMapConfig.GetSubItem('params.txt');
  VConfigIniParams := VConfigIni.GetSubItem('PARAMS');

  VZmpInfoGui :=
    TZmpInfoGUI.Create(
      FGUID,
      FLanguageManager,
      FContentTypeManager,
      FBitmapFactory,
      AZmpMapConfig,
      VConfigIni,
      VConfigIniParams,
      FPnum
    );

  SetZmpInfoGui(VZmpInfoGui);
end;

procedure TZmpInfoGuiProxy.Reset;
begin
  SetZmpInfoGui(nil);
end;

function TZmpInfoGuiProxy.GetIsInitialized: Boolean;
begin
  Result := GetZmpInfoGui <> nil;
end;

function TZmpInfoGuiProxy.GetBmp18: IBitmap32Static;
var
  VZmpInfoGui: IZmpInfoGui;
begin
  VZmpInfoGui := GetZmpInfoGui;
  if VZmpInfoGui <> nil then begin
    Result := VZmpInfoGui.Bmp18;
    Exit;
  end;

  Result := FIcon;
end;

function TZmpInfoGuiProxy.GetBmp24: IBitmap32Static;
var
  VZmpInfoGui: IZmpInfoGui;
begin
  VZmpInfoGui := GetZmpInfoGui;
  if VZmpInfoGui <> nil then begin
    Result := VZmpInfoGui.Bmp24;
    Exit;
  end;

  Result := FIcon;
end;

function TZmpInfoGuiProxy.GetEnabled: Boolean;
var
  VZmpInfoGui: IZmpInfoGui;
begin
  VZmpInfoGui := GetZmpInfoGui;
  if VZmpInfoGui <> nil then begin
    Result := VZmpInfoGui.Enabled;
    Exit;
  end;

  Result := False;
end;

function TZmpInfoGuiProxy.GetHotKey: TShortCut;
var
  VZmpInfoGui: IZmpInfoGui;
begin
  VZmpInfoGui := GetZmpInfoGui;
  if VZmpInfoGui <> nil then begin
    Result := VZmpInfoGui.HotKey;
    Exit;
  end;

  Result := 0;
end;

function TZmpInfoGuiProxy.GetInfoUrl: IStringByLanguage;
var
  VZmpInfoGui: IZmpInfoGui;
begin
  VZmpInfoGui := GetZmpInfoGui;
  if VZmpInfoGui <> nil then begin
    Result := VZmpInfoGui.InfoUrl;
    Exit;
  end;

  Result := FInfoUrl;
end;

function TZmpInfoGuiProxy.GetName: IStringByLanguage;
var
  VZmpInfoGui: IZmpInfoGui;
begin
  VZmpInfoGui := GetZmpInfoGui;
  if VZmpInfoGui <> nil then begin
    Result := VZmpInfoGui.Name;
    Exit;
  end;

  Result := FName;
end;

function TZmpInfoGuiProxy.GetParentSubMenu: IStringByLanguage;
var
  VZmpInfoGui: IZmpInfoGui;
begin
  VZmpInfoGui := GetZmpInfoGui;
  if VZmpInfoGui <> nil then begin
    Result := VZmpInfoGui.ParentSubMenu;
    Exit;
  end;

  Result := FParentSubMenu;
end;

function TZmpInfoGuiProxy.GetSeparator: Boolean;
var
  VZmpInfoGui: IZmpInfoGui;
begin
  VZmpInfoGui := GetZmpInfoGui;
  if VZmpInfoGui <> nil then begin
    Result := VZmpInfoGui.Separator;
    Exit;
  end;

  Result := False;
end;

function TZmpInfoGuiProxy.GetSortIndex: Integer;
var
  VZmpInfoGui: IZmpInfoGui;
begin
  VZmpInfoGui := GetZmpInfoGui;
  if VZmpInfoGui <> nil then begin
    Result := VZmpInfoGui.SortIndex;
    Exit;
  end;

  Result := -1;
end;

function TZmpInfoGuiProxy.GetZmpInfoGui: IZmpInfoGui;
begin
  FLock.BeginRead;
  try
    Result := FZmpInfoGui;
  finally
    FLock.EndRead;
  end;
end;

procedure TZmpInfoGuiProxy.SetZmpInfoGui(const AValue: IZmpInfoGui);
begin
  FLock.BeginWrite;
  try
    FZmpInfoGui := AValue;
  finally
    FLock.EndWrite;
  end;
end;

{ TZmpInfoProxy }

constructor TZmpInfoProxy.Create(
  const AZmpConfig: IZmpConfig;
  const ALanguageManager: ILanguageManager;
  const AProjectionSetFactory: IProjectionSetFactory;
  const AContentTypeManager: IContentTypeManager;
  const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
  const AMarkPictureList: IMarkPictureList;
  const ABitmapFactory: IBitmap32StaticFactory;
  const APnum: Integer;
  const AIsBitmapTiles: Boolean;
  const AIsLayer: Boolean
);
begin
  inherited Create;

  FGuid := TGUID.NewGuid;

  FZmpConfig := AZmpConfig;
  FLanguageManager := ALanguageManager;
  FProjectionSetFactory := AProjectionSetFactory;
  FContentTypeManager := AContentTypeManager;
  FAppearanceOfMarkFactory := AAppearanceOfMarkFactory;
  FMarkPictureList := AMarkPictureList;
  FBitmapFactory := ABitmapFactory;
  FPnum := APnum;

  FIsBitmapTiles := AIsBitmapTiles;
  FIsLayer := AIsLayer;

  FGuiProxy :=
    TZmpInfoGuiProxy.Create(
      FGUID,
      ALanguageManager,
      AContentTypeManager,
      ABitmapFactory,
      APnum
    );

  FAbilities := TMapAbilitiesConfigStatic.Create(True, False);

  FTileDownloaderConfig := TTileDownloaderConfigStatic.Create(
    nil, False, False, 0, 0, False, False, '', '', Types.Point(0, 0), False
  );

  FLicense := InternalMakeStringByLanguage;
end;

procedure TZmpInfoProxy.Initialize(const AZmpMapConfig: IConfigDataProvider);
var
  VZmpInfo: IZmpInfo;
begin
  FGuiProxy.Initialize(AZmpMapConfig);

  VZmpInfo :=
    TZmpInfo.Create(
      FZmpConfig,
      FLanguageManager,
      FProjectionSetFactory,
      FContentTypeManager,
      FAppearanceOfMarkFactory,
      FMarkPictureList,
      FBitmapFactory,
      '',
      AZmpMapConfig,
      FPnum
    );

  SetZmpInfo(VZmpInfo);
end;

procedure TZmpInfoProxy.Reset;
begin
  FGuiProxy.Reset;
  SetZmpInfo(nil);
end;

function TZmpInfoProxy.GetIsInitialized: Boolean;
begin
  Result := GetZmpInfo <> nil;
end;

function TZmpInfoProxy.GetIsBitmapTiles: Boolean;
begin
  Result := FIsBitmapTiles;
end;

function TZmpInfoProxy.GetIsKmlTiles: Boolean;
begin
  Result := not FIsBitmapTiles;
end;

function TZmpInfoProxy.GetAbilities: IMapAbilitiesConfigStatic;
var
  VZmpInfo: IZmpInfo;
begin
  VZmpInfo := GetZmpInfo;
  if VZmpInfo <> nil then begin
    Result := VZmpInfo.Abilities;
    Exit;
  end;

  Result := FAbilities;
end;

function TZmpInfoProxy.GetBanTileSamples: IBinaryDataListStatic;
var
  VZmpInfo: IZmpInfo;
begin
  VZmpInfo := GetZmpInfo;
  if VZmpInfo <> nil then begin
    Result := VZmpInfo.BanTileSamples;
    Exit;
  end;

  Result := nil;
end;

function TZmpInfoProxy.GetContentTypeSubst: IContentTypeSubst;
var
  VZmpInfo: IZmpInfo;
begin
  VZmpInfo := GetZmpInfo;
  if VZmpInfo <> nil then begin
    Result := VZmpInfo.ContentTypeSubst;
    Exit;
  end;

  Result := nil;
end;

function TZmpInfoProxy.GetDataProvider: IConfigDataProvider;
var
  VZmpInfo: IZmpInfo;
begin
  VZmpInfo := GetZmpInfo;
  if VZmpInfo <> nil then begin
    Result := VZmpInfo.DataProvider;
    Exit;
  end;

  Result := nil;
end;

function TZmpInfoProxy.GetEmptyTileSamples: IBinaryDataListStatic;
var
  VZmpInfo: IZmpInfo;
begin
  VZmpInfo := GetZmpInfo;
  if VZmpInfo <> nil then begin
    Result := VZmpInfo.EmptyTileSamples;
    Exit;
  end;

  Result := nil;
end;

function TZmpInfoProxy.GetFileName: string;
var
  VZmpInfo: IZmpInfo;
begin
  VZmpInfo := GetZmpInfo;
  if VZmpInfo <> nil then begin
    Result := VZmpInfo.FileName;
    Exit;
  end;

  Result := '';
end;

function TZmpInfoProxy.GetPointParams: IImportPointParams;
var
  VZmpInfo: IZmpInfo;
begin
  VZmpInfo := GetZmpInfo;
  if VZmpInfo <> nil then begin
    Result := VZmpInfo.PointParams;
    Exit;
  end;

  Result := nil;
end;

function TZmpInfoProxy.GetLineParams: IImportLineParams;
var
  VZmpInfo: IZmpInfo;
begin
  VZmpInfo := GetZmpInfo;
  if VZmpInfo <> nil then begin
    Result := VZmpInfo.LineParams;
    Exit;
  end;

  Result := nil;
end;

function TZmpInfoProxy.GetPolyParams: IImportPolyParams;
var
  VZmpInfo: IZmpInfo;
begin
  VZmpInfo := GetZmpInfo;
  if VZmpInfo <> nil then begin
    Result := VZmpInfo.PolyParams;
    Exit;
  end;

  Result := nil;
end;

function TZmpInfoProxy.GetProjectionSet: IProjectionSet;
var
  VZmpInfo: IZmpInfo;
begin
  VZmpInfo := GetZmpInfo;
  if VZmpInfo <> nil then begin
    Result := VZmpInfo.ProjectionSet;
    Exit;
  end;

  Result := nil;
end;

function TZmpInfoProxy.GetGUI: IZmpInfoGUI;
begin
  Result := FGuiProxy;
end;

function TZmpInfoProxy.GetGUID: TGUID;
var
  VZmpInfo: IZmpInfo;
begin
  VZmpInfo := GetZmpInfo;
  if VZmpInfo <> nil then begin
    Result := VZmpInfo.GUID;
    Assert(IsEqualGUID(FGUID, Result));
    Exit;
  end;

  Result := FGUID;
end;

function TZmpInfoProxy.GetIsLayer: Boolean;
var
  VZmpInfo: IZmpInfo;
begin
  VZmpInfo := GetZmpInfo;
  if VZmpInfo <> nil then begin
    Result := VZmpInfo.IsLayer;
    Assert(Result = FIsLayer);
    Exit;
  end;

  Result := FIsLayer;
end;

function TZmpInfoProxy.GetLayerZOrder: Integer;
var
  VZmpInfo: IZmpInfo;
begin
  VZmpInfo := GetZmpInfo;
  if VZmpInfo <> nil then begin
    Result := VZmpInfo.LayerZOrder;
    Exit;
  end;

  Result := -1;
end;

function TZmpInfoProxy.GetLicense: IStringByLanguage;
var
  VZmpInfo: IZmpInfo;
begin
  VZmpInfo := GetZmpInfo;
  if VZmpInfo <> nil then begin
    Result := VZmpInfo.License;
    Exit;
  end;

  Result := FLicense;
end;

function TZmpInfoProxy.GetStorageConfig: ISimpleTileStorageConfigStatic;
var
  VZmpInfo: IZmpInfo;
begin
  VZmpInfo := GetZmpInfo;
  if VZmpInfo <> nil then begin
    Result := VZmpInfo.StorageConfig;
    Exit;
  end;

  Result := nil;
end;

function TZmpInfoProxy.GetViewProjectionSet: IProjectionSet;
var
  VZmpInfo: IZmpInfo;
begin
  VZmpInfo := GetZmpInfo;
  if VZmpInfo <> nil then begin
    Result := VZmpInfo.ViewProjectionSet;
    Exit;
  end;

  Result := nil;
end;

function TZmpInfoProxy.GetTileDownloaderConfig: ITileDownloaderConfigStatic;
var
  VZmpInfo: IZmpInfo;
begin
  VZmpInfo := GetZmpInfo;
  if VZmpInfo <> nil then begin
    Result := VZmpInfo.TileDownloaderConfig;
    Exit;
  end;

  Result := FTileDownloaderConfig;
end;

function TZmpInfoProxy.GetTilePostDownloadCropConfig: ITilePostDownloadCropConfigStatic;
var
  VZmpInfo: IZmpInfo;
begin
  VZmpInfo := GetZmpInfo;
  if VZmpInfo <> nil then begin
    Result := VZmpInfo.TilePostDownloadCropConfig;
    Exit;
  end;

  Result := nil;
end;

function TZmpInfoProxy.GetTileDownloadRequestBuilderConfig: ITileDownloadRequestBuilderConfigStatic;
var
  VZmpInfo: IZmpInfo;
begin
  VZmpInfo := GetZmpInfo;
  if VZmpInfo <> nil then begin
    Result := VZmpInfo.TileDownloadRequestBuilderConfig;
    Exit;
  end;

  Result := nil;
end;

function TZmpInfoProxy.GetVersion: string;
var
  VZmpInfo: IZmpInfo;
begin
  VZmpInfo := GetZmpInfo;
  if VZmpInfo <> nil then begin
    Result := VZmpInfo.Version;
    Exit;
  end;

  Result := '';
end;

function TZmpInfoProxy.GetZmpInfo: IZmpInfo;
begin
  FLock.BeginRead;
  try
    Result := FZmpInfo;
  finally
    FLock.EndRead;
  end;
end;

procedure TZmpInfoProxy.SetZmpInfo(const AValue: IZmpInfo);
begin
  FLock.BeginWrite;
  try
    FZmpInfo := AValue;
  finally
    FLock.EndWrite;
  end;
end;

initialization
  TZmpInfoProxy.FLock := GSync.SyncVariable.Make('TZmpInfoProxy');
  TZmpInfoGuiProxy.FLock := GSync.SyncVariable.Make('TZmpInfoGuiProxy');

finalization
  TZmpInfoProxy.FLock := nil;
  TZmpInfoGuiProxy.FLock := nil;

end.
