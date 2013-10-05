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

unit u_ZmpInfo;

interface

uses
  SysUtils,
  Classes,
  i_Bitmap32Static,
  i_CoordConverter,
  i_ConfigDataProvider,
  i_LanguageListStatic,
  i_MapVersionInfo,
  i_BinaryDataListStatic,
  i_Bitmap32StaticFactory,
  i_ContentTypeSubst,
  i_TileDownloadRequestBuilderConfig,
  i_TileDownloaderConfig,
  i_TilePostDownloadCropConfig,
  i_LanguageManager,
  i_StringByLanguage,
  i_CoordConverterFactory,
  i_ContentTypeManager,
  i_MapAbilitiesConfig,
  i_MapAttachmentsInfo,
  i_SimpleTileStorageConfig,
  i_ZmpConfig,
  i_ZmpInfo,
  u_BaseInterfacedObject;

type
  TZmpInfoGUI = class(TBaseInterfacedObject, IZmpInfoGUI)
  private
    FGUID: TGUID;
    FName: IStringByLanguage;
    FParentSubMenu: IStringByLanguage;

    FInfoUrl: IStringByLanguage;

    FSortIndex: Integer;
    FBmp18: IBitmap32Static;
    FBmp24: IBitmap32Static;
    FHotKey: TShortCut;
    FSeparator: Boolean;
    FEnabled: Boolean;
  private
    procedure LoadConfig(
      const ALangList: ILanguageListStatic;
      const AContentTypeManager: IContentTypeManager;
      const AConfig: IConfigDataProvider;
      const AConfigIni: IConfigDataProvider;
      const AConfigIniParams: IConfigDataProvider;
      const ABitmapFactory: IBitmap32StaticFactory;
      Apnum: Integer
    );
    function CreateDefaultIcon(
      const ABitmapFactory: IBitmap32StaticFactory;
      Apnum: Integer
    ): IBitmap32Static;
    procedure LoadIcons(
      const AContentTypeManager: IContentTypeManager;
      const AConfig: IConfigDataProvider;
      const AConfigIniParams: IConfigDataProvider;
      const ABitmapFactory: IBitmap32StaticFactory;
      Apnum: Integer
    );
    procedure LoadUIParams(
      const ALangList: ILanguageListStatic;
      const AConfig: IConfigDataProvider;
      Apnum: Integer
    );
    procedure LoadInfo(
      const ALangList: ILanguageListStatic;
      const AConfig: IConfigDataProvider
    );
  private
    function GetName: IStringByLanguage;
    function GetSortIndex: Integer;
    function GetInfoUrl: IStringByLanguage;
    function GetBmp18: IBitmap32Static;
    function GetBmp24: IBitmap32Static;
    function GetHotKey: TShortCut;
    function GetSeparator: Boolean;
    function GetParentSubMenu: IStringByLanguage;
    function GetEnabled: Boolean;
  public
    constructor Create(
      const AGUID: TGUID;
      const ALanguageManager: ILanguageManager;
      const AContentTypeManager: IContentTypeManager;
      const ABitmapFactory: IBitmap32StaticFactory;
      const AConfig: IConfigDataProvider;
      const AConfigIni: IConfigDataProvider;
      const AConfigIniParams: IConfigDataProvider;
      Apnum: Integer
    );
  end;

  TZmpInfo = class(TBaseInterfacedObject, IZmpInfo)
  private
    FGUID: TGUID;
    FIsLayer: Boolean;
    FLayerZOrder: Integer;
    FLicense: IStringByLanguage;
    FFileName: string;
    FVersionConfig: IMapVersionInfo;
    FTileDownloadRequestBuilderConfig: ITileDownloadRequestBuilderConfigStatic;
    FTileDownloaderConfig: ITileDownloaderConfigStatic;
    FTilePostDownloadCropConfig: ITilePostDownloadCropConfigStatic;
    FContentTypeSubst: IContentTypeSubst;
    FGeoConvert: ICoordConverter;
    FViewGeoConvert: ICoordConverter;
    FGUI: IZmpInfoGUI;
    FAbilities: IMapAbilitiesConfigStatic;
    FEmptyTileSamples: IBinaryDataListStatic;
    FBanTileSamples: IBinaryDataListStatic;
    FMapAttachmentsInfo: IMapAttachmentsInfo;
    FStorageConfig: ISimpleTileStorageConfigStatic;

    FZmpConfig: IZmpConfig;
    FConfig: IConfigDataProvider;
    FConfigIni: IConfigDataProvider;
    FConfigIniParams: IConfigDataProvider;
  private
    procedure LoadConfig(
      const ACoordConverterFactory: ICoordConverterFactory;
      const ALanguageManager: ILanguageManager
    );
    procedure LoadCropConfig(const AConfig: IConfigDataProvider);
    procedure LoadAbilities(const AConfig: IConfigDataProvider);
    procedure LoadStorageConfig(const AConfig: IConfigDataProvider);
    function LoadGUID(const AConfig: IConfigDataProvider): TGUID;
    procedure LoadVersion(const AConfig: IConfigDataProvider);
    function GetBinaryListByConfig(const AConfig: IConfigDataProvider): IBinaryDataListStatic;
    procedure LoadSamples(const AConfig: IConfigDataProvider);
    procedure LoadAttachmentsInfo(
      const AConfig: IConfigDataProvider;
      const ALanguageManager: ILanguageManager
    );
    procedure LoadProjectionInfo(
      const AConfig: IConfigDataProvider;
      const ACoordConverterFactory: ICoordConverterFactory
    );
    procedure LoadTileRequestBuilderConfig(
      const ACoordConverterFactory: ICoordConverterFactory;
      const AConfig: IConfigDataProvider
    );
    procedure LoadTileDownloaderConfig(const AConfig: IConfigDataProvider);
  private
    { IZmpInfo }
    function GetGUID: TGUID;
    function GetIsLayer: Boolean;
    function GetGUI: IZmpInfoGUI;
    function GetLayerZOrder: Integer;
    function GetLicense: IStringByLanguage;
    function GetFileName: string;
    function GetVersionConfig: IMapVersionInfo;
    function GetTileDownloadRequestBuilderConfig: ITileDownloadRequestBuilderConfigStatic;
    function GetTileDownloaderConfig: ITileDownloaderConfigStatic;
    function GetTilePostDownloadCropConfig: ITilePostDownloadCropConfigStatic;
    function GetContentTypeSubst: IContentTypeSubst;
    function GetGeoConvert: ICoordConverter;
    function GetViewGeoConvert: ICoordConverter;
    function GetAbilities: IMapAbilitiesConfigStatic;
    function GetEmptyTileSamples: IBinaryDataListStatic;
    function GetBanTileSamples: IBinaryDataListStatic;
    function GetStorageConfig: ISimpleTileStorageConfigStatic;
    function GetDataProvider: IConfigDataProvider;
    function GetMapAttachmentsInfo: IMapAttachmentsInfo;
  public
    constructor Create(
      const AZmpConfig: IZmpConfig;
      const ALanguageManager: ILanguageManager;
      const ACoordConverterFactory: ICoordConverterFactory;
      const AContentTypeManager: IContentTypeManager;
      const ABitmapFactory: IBitmap32StaticFactory;
      const AFileName: string;
      const AConfig: IConfigDataProvider;
      Apnum: Integer
    );
  end;

  EZmpError = class(Exception);
  EZmpIniNotFound = class(EZmpError);
  EZmpParamsNotFound = class(EZmpError);
  EZmpGUIDError = class(EZmpError);

implementation

uses
  Types,
  ALfcnString,
  GR32,
  gnugettext,
  c_ZeroGUID,
  i_BinaryData,
  i_StringListStatic,
  i_BitmapTileSaveLoad,
  i_ContentTypeInfo,
  i_TileStorageAbilities,
  u_BinaryDataListStatic,
  u_StringByLanguageWithStaticList,
  u_TileDownloadRequestBuilderConfig,
  u_TileDownloaderConfigStatic,
  u_TilePostDownloadCropConfigStatic,
  u_ContentTypeSubstByList,
  u_MapAbilitiesConfigStatic,
  u_TileStorageAbilities,
  u_SimpleTileStorageConfigStatic,
  u_MapVersionInfo,
  u_MapAttachmentsInfo,
  u_ResStrings;

// common subroutine
function InternalMakeStringListByLanguage(
  const ALangList: ILanguageListStatic;
  const AConfig: IConfigDataProvider;
  const AParamName: String;
  const ADefValue: String
): TStringList;
var
  VDefValue: string;
  i: Integer;
  VLanguageCode: string;
  VValue: string;
begin
  VDefValue := AConfig.ReadString(AParamName, ADefValue);
  Result := TStringList.Create;
  try
    for i := 0 to ALangList.Count - 1 do begin
      VValue := VDefValue;
      VLanguageCode := ALangList.Code[i];
      VValue := AConfig.ReadString(AParamName + '_' + VLanguageCode, VDefValue);
      Result.Add(VValue);
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function InternalMakeStringByLanguage(
  const ALangList: ILanguageListStatic;
  const AConfig: IConfigDataProvider;
  const AParamName: String;
  const ADefValue: String
): TStringByLanguageWithStaticList;
var
  VValueList: TStringList;
begin
  VValueList := InternalMakeStringListByLanguage(ALangList, AConfig, AParamName, ADefValue);
  try
    Result := TStringByLanguageWithStaticList.Create(VValueList);
  finally
    VValueList.Free;
  end;
end;

{ TZmpInfoGUI }

constructor TZmpInfoGUI.Create(
  const AGUID: TGUID;
  const ALanguageManager: ILanguageManager;
  const AContentTypeManager: IContentTypeManager;
  const ABitmapFactory: IBitmap32StaticFactory;
  const AConfig: IConfigDataProvider;
  const AConfigIni: IConfigDataProvider;
  const AConfigIniParams: IConfigDataProvider;
  Apnum: Integer
);
var
  VLangList: ILanguageListStatic;
begin
  inherited Create;
  FGUID := AGUID;
  VLangList := ALanguageManager.LanguageList;
  LoadConfig(
    VLangList,
    AContentTypeManager,
    AConfig,
    AConfigIni,
    AConfigIniParams,
    ABitmapFactory,
    Apnum
  );
end;

function TZmpInfoGUI.CreateDefaultIcon(
  const ABitmapFactory: IBitmap32StaticFactory;
  Apnum: Integer
): IBitmap32Static;
var
  VBitmap: TBitmap32;
  VNameDef: string;
  VTextSize: TSize;
  VPos: TPoint;
begin
  VBitmap := TBitmap32.Create;
  try
    VNameDef := copy(IntToStr(Apnum), 1, 2);
    VBitmap.SetSize(32, 32);
    VBitmap.Clear(clLightGray32);
    VTextSize := VBitmap.TextExtent(VNameDef);
    VPos.X := (VBitmap.Width - VTextSize.cx) div 2;
    VPos.Y := (VBitmap.Height - VTextSize.cy) div 2;
    VBitmap.RenderText(VPos.X, VPos.Y, VNameDef, 2, clBlack32);
    Result :=
      ABitmapFactory.Build(
        Types.Point(VBitmap.Width, VBitmap.Height),
        VBitmap.Bits
      )
  finally
    VBitmap.Free;
  end;
end;

function TZmpInfoGUI.GetBmp18: IBitmap32Static;
begin
  Result := FBmp18;
end;

function TZmpInfoGUI.GetBmp24: IBitmap32Static;
begin
  Result := FBmp24;
end;

function TZmpInfoGUI.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TZmpInfoGUI.GetHotKey: TShortCut;
begin
  Result := FHotKey;
end;

function TZmpInfoGUI.GetInfoUrl: IStringByLanguage;
begin
  Result := FInfoUrl;
end;

function TZmpInfoGUI.GetName: IStringByLanguage;
begin
  Result := FName;
end;

function TZmpInfoGUI.GetParentSubMenu: IStringByLanguage;
begin
  Result := FParentSubMenu;
end;

function TZmpInfoGUI.GetSeparator: Boolean;
begin
  Result := FSeparator;
end;

function TZmpInfoGUI.GetSortIndex: Integer;
begin
  Result := FSortIndex;
end;

procedure TZmpInfoGUI.LoadConfig(
  const ALangList: ILanguageListStatic;
  const AContentTypeManager: IContentTypeManager;
  const AConfig: IConfigDataProvider;
  const AConfigIni: IConfigDataProvider;
  const AConfigIniParams: IConfigDataProvider;
  const ABitmapFactory: IBitmap32StaticFactory;
  Apnum: Integer
);
begin
  LoadUIParams(ALangList, AConfigIniParams, Apnum);
  LoadIcons(AContentTypeManager, AConfig, AConfigIniParams, ABitmapFactory, Apnum);
  LoadInfo(ALangList, AConfig);
end;

function UpdateBMPTransp(
  const ABitmapFactory: IBitmap32StaticFactory;
  AMaskColor: TColor32;
  const ABitmap: IBitmap32Static
): IBitmap32Static;
var
  VSourceLine: PColor32Array;
  VTargetLine: PColor32Array;
  i: Integer;
  VSize: TPoint;
begin
  Result := nil;
  if ABitmap <> nil then begin
    VSize := ABitmap.Size;
    Result := ABitmapFactory.BuildEmpty(VSize);
    if Result <> nil then begin
      VSourceLine := ABitmap.Data;
      VTargetLine := Result.Data;
      for i := 0 to VSize.X * VSize.Y - 1 do begin
        if VSourceLine[i] = AMaskColor then begin
          VTargetLine[i] := 0;
        end else begin
          VTargetLine[i] := VSourceLine[i];
        end;
      end;
    end;
  end;
end;

function GetBitmap(
  const AContentTypeManager: IContentTypeManager;
  const AConfig: IConfigDataProvider;
  const AConfigIniParams: IConfigDataProvider;
  const ABitmapFactory: IBitmap32StaticFactory;
  const ADefName: string;
  const AIdent: string
): IBitmap32Static;
var
  VImageName: string;
  VData: IBinaryData;
  VExt: string;
  VTypeInfo: IContentTypeInfoBasic;
  VBitmapTypeInfo: IContentTypeInfoBitmap;
  VLoader: IBitmapTileLoader;
begin
  Result := nil;
  VImageName := ADefName;
  VImageName := AConfigIniParams.ReadString(AIdent, VImageName);
  VData := AConfig.ReadBinary(VImageName);
  if (VData <> nil) and (VData.Size > 0) then begin
    VExt := LowerCase(ExtractFileExt(VImageName));
    VTypeInfo := AContentTypeManager.GetInfoByExt(VExt);
    if Supports(VTypeInfo, IContentTypeInfoBitmap, VBitmapTypeInfo) then begin
      VLoader := VBitmapTypeInfo.GetLoader;
      if VLoader <> nil then begin
        Result := VLoader.Load(VData);
        if Result <> nil then begin
          if VExt = '.bmp' then begin
            Result := UpdateBMPTransp(ABitmapFactory, Color32(255, 0, 255, 255), Result);
          end;
        end;
      end;
    end;
  end;
end;

procedure TZmpInfoGUI.LoadIcons(
  const AContentTypeManager: IContentTypeManager;
  const AConfig: IConfigDataProvider;
  const AConfigIniParams: IConfigDataProvider;
  const ABitmapFactory: IBitmap32StaticFactory;
  Apnum: Integer
);
begin
  try
    FBmp24 :=
      GetBitmap(
        AContentTypeManager,
        AConfig,
        AConfigIniParams,
        ABitmapFactory,
        '24.bmp',
        'BigIconName'
      );
  except
    FBmp24 := nil;
  end;
  if FBmp24 = nil then begin
    FBmp24 := CreateDefaultIcon(ABitmapFactory, Apnum);
  end;

  try
    FBmp18 :=
      GetBitmap(
        AContentTypeManager,
        AConfig,
        AConfigIniParams,
        ABitmapFactory,
        '18.bmp',
        'SmallIconName'
      );
  except
    FBmp18 := nil;
  end;
  if FBmp18 = nil then begin
    FBmp18 := FBmp24;
  end;
end;

procedure TZmpInfoGUI.LoadInfo(
  const ALangList: ILanguageListStatic;
  const AConfig: IConfigDataProvider
);
var
  VDefValue: string;
  VFileName: string;
  i: Integer;
  VLanguageCode: string;
  VValueList: TStringList;
  VValue: string;
begin
  // 'sas://ZmpInfo/' + GUIDToString(FGUID)
  if AConfig.ReadString('index.html', '') <> '' then begin
    VDefValue := '/index.html';
  end else if AConfig.ReadString('info.txt', '') <> '' then begin
    VDefValue := '/info.txt';
  end else begin
    VDefValue := '';
  end;
  VValueList := TStringList.Create;
  try
    for i := 0 to ALangList.Count - 1 do begin
      VValue := VDefValue;
      VLanguageCode := ALangList.Code[i];
      VFileName := 'index_' + VLanguageCode + '.html';
      if AConfig.ReadString(VFileName, '') <> '' then begin
        VValue := '/' + VFileName;
      end else begin
        VFileName := 'info_' + VLanguageCode + '.txt';
        if AConfig.ReadString(VFileName, '') <> '' then begin
          VValue := '/' + VFileName;
        end;
      end;
      VValueList.Add(VValue);
    end;
    FInfoUrl := TStringByLanguageWithStaticList.Create(VValueList);
  finally
    VValueList.Free;
  end;
end;

procedure TZmpInfoGUI.LoadUIParams(
  const ALangList: ILanguageListStatic;
  const AConfig: IConfigDataProvider;
  Apnum: Integer
);
begin
  // multilanguage params
  FName := InternalMakeStringByLanguage(ALangList, AConfig, 'name', 'map#' + inttostr(Apnum));
  FParentSubMenu := InternalMakeStringByLanguage(ALangList, AConfig, 'ParentSubMenu', '');

  FHotKey := AConfig.ReadInteger('DefHotKey', 0);
  FHotKey := AConfig.ReadInteger('HotKey', FHotKey);
  FSeparator := AConfig.ReadBool('separator', false);
  FEnabled := AConfig.ReadBool('Enabled', true);
  FSortIndex := AConfig.ReadInteger('pnum', -1);
end;

{ TZmpInfo }

constructor TZmpInfo.Create(
  const AZmpConfig: IZmpConfig;
  const ALanguageManager: ILanguageManager;
  const ACoordConverterFactory: ICoordConverterFactory;
  const AContentTypeManager: IContentTypeManager;
  const ABitmapFactory: IBitmap32StaticFactory;
  const AFileName: string;
  const AConfig: IConfigDataProvider;
  Apnum: Integer
);
begin
  inherited Create;
  FFileName := AFileName;
  FZmpConfig := AZmpConfig;
  FConfig := AConfig;
  FConfigIni := FConfig.GetSubItem('params.txt');
  if FConfigIni = nil then begin
    raise EZmpIniNotFound.Create(_('Not found "params.txt" in zmp'));
  end;
  FConfigIniParams := FConfigIni.GetSubItem('PARAMS');
  if FConfigIniParams = nil then begin
    raise EZmpParamsNotFound.Create(_('Not found PARAMS section in zmp'));
  end;
  LoadConfig(ACoordConverterFactory, ALanguageManager);
  FGUI :=
    TZmpInfoGUI.Create(
      FGUID,
      ALanguageManager,
      AContentTypeManager,
      ABitmapFactory,
      FConfig,
      FConfigIni,
      FConfigIniParams,
      Apnum
    );
  FLicense := InternalMakeStringByLanguage(ALanguageManager.LanguageList, FConfigIniParams, 'License', '');
  FLayerZOrder := FConfigIniParams.ReadInteger('LayerZOrder', 0);
end;

function TZmpInfo.GetAbilities: IMapAbilitiesConfigStatic;
begin
  Result := FAbilities;
end;

function TZmpInfo.GetBanTileSamples: IBinaryDataListStatic;
begin
  Result := FBanTileSamples;
end;

function TZmpInfo.GetBinaryListByConfig(
  const AConfig: IConfigDataProvider
): IBinaryDataListStatic;
var
  VList: IStringListStatic;
  VCount: Integer;
  i: Integer;
  VItems: array of IBinaryData;
begin
  Result := nil;
  if AConfig <> nil then begin
    VList := AConfig.ReadValuesList;
    VCount := VList.Count;
    if VCount > 0 then begin
      SetLength(VItems, VCount);
      for i := 0 to VCount - 1 do begin
        VItems[i] := AConfig.ReadBinary(VList.Items[i]);
      end;
      try
        Result := TBinaryDataListStatic.Create(VItems);
      finally
        for i := 0 to VCount - 1 do begin
          VItems[i] := nil;
        end;
      end;
    end;
  end;
end;

function TZmpInfo.GetContentTypeSubst: IContentTypeSubst;
begin
  Result := FContentTypeSubst;
end;

function TZmpInfo.GetDataProvider: IConfigDataProvider;
begin
  Result := FConfig;
end;

function TZmpInfo.GetEmptyTileSamples: IBinaryDataListStatic;
begin
  Result := FEmptyTileSamples;
end;

function TZmpInfo.GetFileName: string;
begin
  Result := FFileName;
end;

function TZmpInfo.GetGeoConvert: ICoordConverter;
begin
  Result := FGeoConvert;
end;

function TZmpInfo.GetGUI: IZmpInfoGUI;
begin
  Result := FGUI;
end;

function TZmpInfo.GetGUID: TGUID;
begin
  Result := FGUID;
end;

function TZmpInfo.GetIsLayer: Boolean;
begin
  Result := FIsLayer;
end;

function TZmpInfo.GetLayerZOrder: Integer;
begin
  Result := FLayerZOrder;
end;

function TZmpInfo.GetLicense: IStringByLanguage;
begin
  Result := FLicense;
end;

function TZmpInfo.GetMapAttachmentsInfo: IMapAttachmentsInfo;
begin
  Result := FMapAttachmentsInfo;
end;

function TZmpInfo.GetStorageConfig: ISimpleTileStorageConfigStatic;
begin
  Result := FStorageConfig;
end;

function TZmpInfo.GetViewGeoConvert: ICoordConverter;
begin
  Result := FViewGeoConvert;
end;

function TZmpInfo.GetTileDownloaderConfig: ITileDownloaderConfigStatic;
begin
  Result := FTileDownloaderConfig;
end;

function TZmpInfo.GetTilePostDownloadCropConfig: ITilePostDownloadCropConfigStatic;
begin
  Result := FTilePostDownloadCropConfig;
end;

function TZmpInfo.GetTileDownloadRequestBuilderConfig: ITileDownloadRequestBuilderConfigStatic;
begin
  Result := FTileDownloadRequestBuilderConfig;
end;

function TZmpInfo.GetVersionConfig: IMapVersionInfo;
begin
  Result := FVersionConfig;
end;

procedure TZmpInfo.LoadAbilities(const AConfig: IConfigDataProvider);
var
  VIsShowOnSmMap: Boolean;
  VUseDownload: Boolean;
begin
  VIsShowOnSmMap := AConfig.ReadBool('CanShowOnSmMap', True);
  VUseDownload := AConfig.ReadBool('UseDwn', True);

  FAbilities :=
    TMapAbilitiesConfigStatic.Create(
      VIsShowOnSmMap,
      VUseDownload
    );
end;

procedure TZmpInfo.LoadAttachmentsInfo(
  const AConfig: IConfigDataProvider;
  const ALanguageManager: ILanguageManager
);
var
  VParams: IConfigDataProvider;
  VGUID: TGUID;
  VSL_Names: TStringList;
  i, VMaxSubIndex: Integer;
  VParseNumberAfter: String;
  VSL_NameInCache, VSL_Ext, VSL_DefUrlBase, VSL_ContentType: TStringList;
  VStrVal, VNameInCacheDefault: String;
  VEnabled, VUseDwn, VUseDel: Boolean;
begin
  // params in special section
  VParams := AConfig.GetSubItem('AttachmentsInfo');

  if not Assigned(VParams) then begin
    FMapAttachmentsInfo := nil;
    Exit;
  end;

  // gui params
  VGUID := LoadGUID(VParams);
  VSL_Names := InternalMakeStringListByLanguage(ALanguageManager.LanguageList, VParams, 'name', '');

  // count of sub-items for single attachment
  VMaxSubIndex := VParams.ReadInteger('MaxSubIndex', 0);
  VParseNumberAfter := VParams.ReadString('ParseNumberAfter', '');
  VUseDwn := VParams.ReadBool('UseDwn', FALSE);
  VUseDel := VParams.ReadBool('UseDel', FALSE);

  // noway
  VSL_NameInCache := nil;
  VSL_Ext := nil;
  VSL_DefUrlBase := nil;
  VSL_ContentType := nil;

  if (VMaxSubIndex >= 0) and (System.Length(VParseNumberAfter) > 0) then begin
    // make containers and obtain default values
    VSL_NameInCache := TStringList.Create;
    VNameInCacheDefault := VParams.ReadString('NameInCache', '');
    if (System.Length(VNameInCacheDefault) > 0) then begin
      VNameInCacheDefault := ExpandFileName(VNameInCacheDefault);
    end;
    VSL_NameInCache.AddObject(VNameInCacheDefault, TObject(Pointer(Ord(VParams.ReadBool('Enabled', FALSE)))));

    VSL_Ext := TStringList.Create;
    VSL_Ext.Add(LowerCase(VParams.ReadString('Ext', '')));

    VSL_DefUrlBase := TStringList.Create;
    VSL_DefUrlBase.Add(VParams.ReadString('DefUrlBase', ''));

    VSL_ContentType := TStringList.Create;
    VSL_ContentType.Add(VParams.ReadString('ContentType', ''));

    // other values (by index)
    if VMaxSubIndex > 0 then begin
      for i := 1 to VMaxSubIndex do begin
        VStrVal := ExpandFileName(VParams.ReadString('NameInCache' + IntToStr(i), VNameInCacheDefault));
        VEnabled := VParams.ReadBool('Enabled' + IntToStr(i), (VSL_NameInCache.Objects[0] <> nil));
        VSL_NameInCache.AddObject(VStrVal, TObject(Pointer(Ord(VEnabled))));

        VStrVal := LowerCase(VParams.ReadString('Ext' + IntToStr(i), VSL_Ext[0]));
        VSL_Ext.Add(VStrVal);

        VStrVal := VParams.ReadString('DefUrlBase' + IntToStr(i), VSL_DefUrlBase[0]);
        VSL_DefUrlBase.Add(VStrVal);

        VStrVal := VParams.ReadString('ContentType' + IntToStr(i), VSL_ContentType[0]);
        VSL_ContentType.Add(VStrVal);
      end;
    end;
  end;

  // make object (VSL_* will be destroyed in object's destructor)
  FMapAttachmentsInfo := TMapAttachmentsInfo.Create(VGUID,
    VMaxSubIndex,
    VParseNumberAfter,
    VSL_NameInCache,
    VSL_Ext,
    VSL_Names,
    VSL_DefUrlBase,
    VSL_ContentType,
    VUseDwn, VUseDel);
end;

procedure TZmpInfo.LoadConfig(
  const ACoordConverterFactory: ICoordConverterFactory;
  const ALanguageManager: ILanguageManager
);
begin
  FGUID := LoadGUID(FConfigIniParams);
  FIsLayer := FConfigIniParams.ReadBool('asLayer', False);
  LoadVersion(FConfigIniParams);
  LoadProjectionInfo(FConfigIni, ACoordConverterFactory);
  LoadTileRequestBuilderConfig(ACoordConverterFactory, FConfigIniParams);
  LoadTileDownloaderConfig(FConfigIniParams);
  LoadCropConfig(FConfigIniParams);
  LoadStorageConfig(FConfigIniParams);
  LoadAbilities(FConfigIniParams);
  LoadSamples(FConfig);
  LoadAttachmentsInfo(FConfigIni, ALanguageManager);
  FContentTypeSubst := TContentTypeSubstByList.Create(FConfigIniParams);
end;

procedure TZmpInfo.LoadCropConfig(const AConfig: IConfigDataProvider);
var
  VRect: TRect;
  VCutCount, VCutSize, VCutTile: TPoint;
  VCutToSkip: String;
begin
  // crop params
  VRect.Left := AConfig.ReadInteger('TileRLeft', 0);
  VRect.Top := AConfig.ReadInteger('TileRTop', 0);
  VRect.Right := AConfig.ReadInteger('TileRRight', 0);
  VRect.Bottom := AConfig.ReadInteger('TileRBottom', 0);
  // cut params
  VCutCount.X := AConfig.ReadInteger('CutCountX', 0);
  VCutCount.Y := AConfig.ReadInteger('CutCountY', 0);
  VCutSize.X := AConfig.ReadInteger('CutSizeX', 0);
  VCutSize.Y := AConfig.ReadInteger('CutSizeY', 0);
  VCutTile.X := AConfig.ReadInteger('CutTileX', 0);
  VCutTile.Y := AConfig.ReadInteger('CutTileY', 0);
  VCutToSkip := AConfig.ReadString('CutToSkip', '');
  // make
  FTilePostDownloadCropConfig := TTilePostDownloadCropConfigStatic.Create(VRect, VCutCount, VCutSize, VCutTile, VCutToSkip);
end;

function TZmpInfo.LoadGUID(const AConfig: IConfigDataProvider): TGUID;
var
  VGUIDStr: String;
begin
  Result := CGUID_Zero;
  VGUIDStr := AConfig.ReadString('GUID', '');
  if Length(VGUIDStr) > 0 then begin
    try
      Result := StringToGUID(VGUIDStr);
    except
      raise EZmpGUIDError.CreateResFmt(@SAS_ERR_MapGUIDBad, [VGUIDStr]);
    end;
    if IsEqualGUID(Result, CGUID_Zero) then begin
      raise EZmpGUIDError.CreateResFmt(@SAS_ERR_MapGUIDBad, [VGUIDStr]);
    end;
  end else begin
    raise EZmpGUIDError.CreateRes(@SAS_ERR_MapGUIDEmpty);
  end;
end;

procedure TZmpInfo.LoadProjectionInfo(
  const AConfig: IConfigDataProvider;
  const ACoordConverterFactory: ICoordConverterFactory
);
var
  VParams: IConfigDataProvider;
begin
  VParams := AConfig.GetSubItem('ViewInfo');
  if VParams <> nil then begin
    FViewGeoConvert := ACoordConverterFactory.GetCoordConverterByConfig(VParams);
  end;
  FGeoConvert := ACoordConverterFactory.GetCoordConverterByConfig(FConfigIniParams);
  if FViewGeoConvert = nil then begin
    FViewGeoConvert := FGeoConvert;
  end;
end;

procedure TZmpInfo.LoadSamples(const AConfig: IConfigDataProvider);
begin
  FEmptyTileSamples := GetBinaryListByConfig(AConfig.GetSubItem('EmptyTiles'));
  FBanTileSamples := GetBinaryListByConfig(AConfig.GetSubItem('BanTiles'));
end;

procedure TZmpInfo.LoadStorageConfig(
  const AConfig: IConfigDataProvider
);
var
  VCacheTypeCode: Integer;
  VNameInCache: string;
  VTileFileExt: string;
  VIsReadOnly: boolean;
  VAllowDelete: boolean;
  VAllowAdd: boolean;
  VAllowReplace: boolean;
  VUseMemCache: Boolean;
  VMemCacheCapacity: Integer;
  VMemCacheTTL: Cardinal;
  VMemCacheClearStrategy: Integer;
  VStorageAbilities: ITileStorageAbilities;
begin
  VNameInCache := AConfig.ReadString('NameInCache', '');
  VCacheTypeCode := AConfig.ReadInteger('CacheType', 0);
  VUseMemCache := AConfig.ReadBool('UseMemCache', FZmpConfig.UseMemCache);
  VMemCacheCapacity := AConfig.ReadInteger('MemCacheCapacity', FZmpConfig.MemCacheCapacity);
  VMemCacheTTL := AConfig.ReadInteger('MemCacheTTL', FZmpConfig.MemCacheTTL);
  VMemCacheClearStrategy := AConfig.ReadInteger('MemCacheClearStrategy', FZmpConfig.MemCacheClearStrategy);
  // c_File_Cache_Id_GE and c_File_Cache_Id_GC
  if (VCacheTypeCode = 5) or (VCacheTypeCode = 8) then begin
    VTileFileExt := '.jpg';
    VIsReadOnly := True;
    VAllowDelete := False;
    VAllowAdd := False;
    VAllowReplace := False;
  end else begin
    VTileFileExt := LowerCase(AConfig.ReadString('Ext', '.jpg'));
    VIsReadOnly := False;
    VAllowDelete := True;
    VAllowAdd := True;
    VAllowReplace := True;
  end;

  VStorageAbilities :=
    TTileStorageAbilities.Create(
      VIsReadOnly,
      VAllowAdd,
      VAllowDelete,
      VAllowReplace
    );

  FStorageConfig :=
    TSimpleTileStorageConfigStatic.Create(
      FGeoConvert,
      VCacheTypeCode,
      VNameInCache,
      VTileFileExt,
      VStorageAbilities,
      VUseMemCache,
      VMemCacheCapacity,
      VMemCacheTTL,
      VMemCacheClearStrategy
    );
end;

procedure TZmpInfo.LoadTileDownloaderConfig(const AConfig: IConfigDataProvider);
var
  VUseDownload: Boolean;
  VAllowUseCookie: Boolean;
  VIgnoreMIMEType: Boolean;
  VDefaultMIMEType: string;
  VExpectedMIMETypes: string;
  VWaitInterval: Cardinal;
  VMaxConnectToServerCount: Cardinal;
  VIteratorSubRectSize: TPoint;
  VRestartDownloaderOnMemCacheTTL: Boolean;
  fL: TStringList;
begin
  VUseDownload := AConfig.ReadBool('UseDwn', True);
  VAllowUseCookie := AConfig.ReadBool('AllowUseCookie', False);
  VIgnoreMIMEType := AConfig.ReadBool('IgnoreContentType', False);
  VDefaultMIMEType := AConfig.ReadString('DefaultContentType', 'image/jpg');
  VExpectedMIMETypes := AConfig.ReadString('ContentType', 'image/jpg');
  VWaitInterval := AConfig.ReadInteger('Sleep', 0);
  VRestartDownloaderOnMemCacheTTL := AConfig.ReadBool('RestartDownloadOnMemCacheTTL', False);
  VMaxConnectToServerCount :=
    AConfig.ReadInteger(
      'MaxConnectToServerCount',
      FZmpConfig.MaxConnectToServerCount
    );
  fL := TStringList.Create;
  try
    fL.Delimiter := ',';
    fL.StrictDelimiter := True;
    fL.DelimitedText := AConfig.ReadString('IteratorSubRectSize', '1,1');
    VIteratorSubRectSize.x := StrToInt(fL[0]);
    VIteratorSubRectSize.y := StrToInt(fL[1]);
  finally
    fL.Free
  end;
  FTileDownloaderConfig :=
    TTileDownloaderConfigStatic.Create(
      nil,
      VUseDownload,
      VAllowUseCookie,
      VWaitInterval,
      VMaxConnectToServerCount,
      VIgnoreMIMEType,
      VExpectedMIMETypes,
      VDefaultMIMEType,
      VIteratorSubRectSize,
      VRestartDownloaderOnMemCacheTTL
    );
end;

procedure TZmpInfo.LoadTileRequestBuilderConfig(
  const ACoordConverterFactory: ICoordConverterFactory;
  const AConfig: IConfigDataProvider
);
var
  VUrlBase: AnsiString;
  VRequestHead: AnsiString;
  VCoordConverter: ICoordConverter;
  VIsUseDownloader: Boolean;
  VDefaultProjConverterArgs: AnsiString;
begin
  VUrlBase := AConfig.ReadAnsiString('DefURLBase', '');
  VUrlBase := AConfig.ReadAnsiString('URLBase', VUrlBase);
  VRequestHead := AConfig.ReadAnsiString('RequestHead', '');
  VRequestHead := ALStringReplace(VRequestHead, '\r\n', #13#10, [rfIgnoreCase, rfReplaceAll]);
  VCoordConverter := ACoordConverterFactory.GetCoordConverterByConfig(AConfig);
  VIsUseDownloader := AConfig.ReadBool('IsUseDownloaderInScript', False);
  VDefaultProjConverterArgs := AConfig.ReadAnsiString('Proj4Args', '');

  FTileDownloadRequestBuilderConfig :=
    TTileDownloadRequestBuilderConfigStatic.Create(
      VUrlBase,
      VRequestHead,
      VIsUseDownloader,
      VDefaultProjConverterArgs,
      VCoordConverter
    );
end;

procedure TZmpInfo.LoadVersion(const AConfig: IConfigDataProvider);
var
  VVersion: String;
  VShowPrevVersion: Boolean;
begin
  VVersion := AConfig.ReadString('Version', '');
  VShowPrevVersion := AConfig.ReadBool('ShowPrevVersion', True);
  FVersionConfig := TMapVersionInfo.Create(VVersion, VShowPrevVersion);
end;

end.
