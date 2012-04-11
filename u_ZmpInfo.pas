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
  i_ContentTypeSubst,
  i_TileDownloadRequestBuilderConfig,
  i_TileDownloaderConfig,
  i_TilePostDownloadCropConfig,
  i_LanguageManager,
  i_StringByLanguage,
  i_CoordConverterFactory,
  i_MapAbilitiesConfig,
  i_MapAttachmentsInfo,
  i_SimpleTileStorageConfig,
  i_ZmpConfig,
  i_ZmpInfo;

type
  TZmpInfoGUI = class(TInterfacedObject, IZmpInfoGUI)
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
    FLayerZOrder: Integer;
    FEnabled: Boolean;
  private
    procedure LoadConfig(
      const ALangList: ILanguageListStatic;
      const AConfig: IConfigDataProvider;
      const AConfigIni: IConfigDataProvider;
      const AConfigIniParams: IConfigDataProvider;
      Apnum: Integer
    );
    function CreateDefaultIcon(
      Apnum: Integer
    ): IBitmap32Static;
    procedure LoadIcons(
      const AConfig : IConfigDataProvider;
      const AConfigIniParams: IConfigDataProvider;
      Apnum: Integer
    );
    procedure LoadUIParams(
      const ALangList: ILanguageListStatic;
      const AConfig : IConfigDataProvider;
      Apnum: Integer
    );
    procedure LoadInfo(
      const ALangList: ILanguageListStatic;
      const AConfig : IConfigDataProvider
    );
  protected
    function GetName: IStringByLanguage;
    function GetSortIndex: Integer;
    function GetInfoUrl: IStringByLanguage;
    function GetBmp18: IBitmap32Static;
    function GetBmp24: IBitmap32Static;
    function GetHotKey: TShortCut;
    function GetSeparator: Boolean;
    function GetLayerZOrder: Integer;
    function GetParentSubMenu: IStringByLanguage;
    function GetEnabled: Boolean;
  public
    constructor Create(
      const AGUID: TGUID;
      const ALanguageManager: ILanguageManager;
      const AConfig: IConfigDataProvider;
      const AConfigIni: IConfigDataProvider;
      const AConfigIniParams: IConfigDataProvider;
      Apnum: Integer
    );
  end;

  TZmpInfo = class(TInterfacedObject, IZmpInfo)
  private
    FGUID: TGUID;
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
    procedure LoadCropConfig(const AConfig : IConfigDataProvider);
    procedure LoadAbilities(const AConfig : IConfigDataProvider);
    procedure LoadStorageConfig(const AConfig : IConfigDataProvider);
    function LoadGUID(const AConfig : IConfigDataProvider): TGUID;
    procedure LoadVersion(const AConfig : IConfigDataProvider);
    procedure LoadAttachmentsInfo(
      const AConfig: IConfigDataProvider;
      const ALanguageManager: ILanguageManager
    );
    procedure LoadProjectionInfo(
      const AConfig : IConfigDataProvider;
      const ACoordConverterFactory: ICoordConverterFactory
    );
    procedure LoadTileRequestBuilderConfig(
      const ACoordConverterFactory: ICoordConverterFactory;
      const AConfig : IConfigDataProvider
    );
    procedure LoadTileDownloaderConfig(const AConfig: IConfigDataProvider);
  protected
    { IZmpInfo }
    function GetGUID: TGUID;
    function GetGUI: IZmpInfoGUI;
    function GetFileName: string;
    function GetVersionConfig: IMapVersionInfo;
    function GetTileDownloadRequestBuilderConfig: ITileDownloadRequestBuilderConfigStatic;
    function GetTileDownloaderConfig: ITileDownloaderConfigStatic;
    function GetTilePostDownloadCropConfig: ITilePostDownloadCropConfigStatic;
    function GetContentTypeSubst: IContentTypeSubst;
    function GetGeoConvert: ICoordConverter;
    function GetViewGeoConvert: ICoordConverter;
    function GetAbilities: IMapAbilitiesConfigStatic;
    function GetStorageConfig: ISimpleTileStorageConfigStatic;
    function GetDataProvider: IConfigDataProvider;
    function GetMapAttachmentsInfo: IMapAttachmentsInfo;
  public
    constructor Create(
      const AZmpConfig: IZmpConfig;
      const ALanguageManager: ILanguageManager;
      const ACoordConverterFactory: ICoordConverterFactory;
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
  GR32,
  gnugettext,
  i_BinaryData,
  u_Bitmap32Static,
  u_StreamReadOnlyByBinaryData,
  u_StringByLanguageWithStaticList,
  u_TileDownloadRequestBuilderConfig,
  u_TileDownloaderConfigStatic,
  u_TilePostDownloadCropConfigStatic,
  u_ContentTypeSubstByList,
  u_MapAbilitiesConfigStatic,
  u_SimpleTileStorageConfigStatic,
  u_MapVersionInfo,
  u_MapAttachmentsInfo,
  u_ResStrings;

// common subroutine
function InternalMakeStringListByLanguage(ALangList: ILanguageListStatic;
                                          AConfig: IConfigDataProvider;
                                          const AParamName: String;
                                          const ADefValue: String): TStringList;
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
      VValue := AConfig.ReadString(AParamName+'_' + VLanguageCode, VDefValue);
      Result.Add(VValue);
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function InternalMakeStringByLanguage(ALangList: ILanguageListStatic;
                                      AConfig: IConfigDataProvider;
                                      const AParamName: String;
                                      const ADefValue: String): TStringByLanguageWithStaticList;
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
  const AConfig: IConfigDataProvider;
  const AConfigIni: IConfigDataProvider;
  const AConfigIniParams: IConfigDataProvider;
  Apnum: Integer
);
var
  VLangList: ILanguageListStatic;
begin
  FGUID := AGUID;
  VLangList := ALanguageManager.LanguageList;
  LoadConfig(VLangList, AConfig, AConfigIni, AConfigIniParams, Apnum);
end;

function TZmpInfoGUI.CreateDefaultIcon(Apnum: Integer): IBitmap32Static;
var
  VBitmap: TBitmap32;
  VNameDef: string;
  VTextSize: TSize;
  VPos: TPoint;
begin
  VBitmap := TBitmap32.Create;
  try
    VNameDef :=  copy(IntToStr(Apnum), 1, 2);
    VBitmap.SetSize(32, 32);
    VBitmap.Clear(clLightGray32);
    VTextSize := VBitmap.TextExtent(VNameDef);
    VPos.X := (VBitmap.Width - VTextSize.cx) div 2;
    VPos.Y := (VBitmap.Height - VTextSize.cy) div 2;
    VBitmap.RenderText(VPos.X, VPos.Y, VNameDef, 2, clBlack32);
    Result := TBitmap32Static.CreateWithCopy(VBitmap);
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

function TZmpInfoGUI.GetLayerZOrder: Integer;
begin
  Result := FLayerZOrder;
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
  const AConfig: IConfigDataProvider;
  const AConfigIni: IConfigDataProvider;
  const AConfigIniParams: IConfigDataProvider;
  Apnum: Integer
);
begin
  LoadUIParams(ALangList, AConfigIniParams, Apnum);
  LoadIcons(AConfig, AConfigIniParams, Apnum);
  LoadInfo(ALangList, AConfig);
end;

procedure TZmpInfoGUI.LoadIcons(
  const AConfig: IConfigDataProvider;
  const AConfigIniParams: IConfigDataProvider;
  Apnum: Integer
);
procedure UpdateBMPTransp(ABitmap: TCustomBitmap32);
var
  VTranspColor: TColor32;
  VLine: PColor32Array;
  i: Integer;
  j: Integer;
begin
  VTranspColor := Color32(255, 0, 255, 255);
  for i := 0 to ABitmap.Height - 1 do begin
    VLine := ABitmap.ScanLine[i];
    for j := 0 to ABitmap.Width - 1 do begin
      if VLine[j] = VTranspColor then begin
        VLine[j] := 0;
      end;
    end;
  end;
end;
var
  VStream: TStream;
  VBitmap: TCustomBitmap32;
  VImageName: string;
  VData: IBinaryData;
begin
  VBitmap := TCustomBitmap32.Create;
  try
    try
      VImageName := '24.bmp';
      VImageName := AConfigIniParams.ReadString('BigIconName', VImageName);
      VData := AConfig.ReadBinary(VImageName);
      if VData <> nil then begin
        VStream := TStreamReadOnlyByBinaryData.Create(VData);
        try
          VBitmap.LoadFromStream(VStream);
          UpdateBMPTransp(VBitmap);
          Fbmp24 := TBitmap32Static.CreateWithCopy(VBitmap);
        finally
          VStream.Free;
        end;
      end;
    except
    end;
  finally
    VBitmap.Free;
  end;
  if FBmp24 = nil then begin
    FBmp24 := CreateDefaultIcon(Apnum);
  end;

  VBitmap := TCustomBitmap32.Create;
  try
    try
      VImageName := '18.bmp';
      VImageName := AConfigIniParams.ReadString('SmallIconName', VImageName);
      VData := AConfig.ReadBinary(VImageName);
      if VData <> nil then begin
        VStream := TStreamReadOnlyByBinaryData.Create(VData);
        try
          VBitmap.LoadFromStream(VStream);
          UpdateBMPTransp(VBitmap);
          FBmp18 := TBitmap32Static.CreateWithCopy(VBitmap);
        finally
          VStream.Free;
        end;
      end;
    except
    end;
  finally
    VBitmap.Free;
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
      VFileName := 'index_'+VLanguageCode+'.html';
      if AConfig.ReadString(VFileName, '') <> '' then begin
        VValue := '/' + VFileName;
      end else begin
        VFileName := 'info_'+VLanguageCode+'.txt';
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
  FName := InternalMakeStringByLanguage(ALangList, AConfig, 'name', 'map#'+inttostr(Apnum));
  FParentSubMenu := InternalMakeStringByLanguage(ALangList, AConfig, 'ParentSubMenu', '');

  FHotKey :=AConfig.ReadInteger('DefHotKey', 0);
  FHotKey :=AConfig.ReadInteger('HotKey', FHotKey);
  FSeparator := AConfig.ReadBool('separator', false);
  FLayerZOrder := AConfig.ReadInteger('LayerZOrder', 0);
  FEnabled := AConfig.ReadBool('Enabled', true);
  FSortIndex := AConfig.ReadInteger('pnum', -1);
end;

{ TZmpInfo }

constructor TZmpInfo.Create(
  const AZmpConfig: IZmpConfig;
  const ALanguageManager: ILanguageManager;
  const ACoordConverterFactory: ICoordConverterFactory;
  const AFileName: string;
  const AConfig: IConfigDataProvider;
  Apnum: Integer
);
begin
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
  FGUI := TZmpInfoGUI.Create(FGUID, ALanguageManager, FConfig, FConfigIni, FConfigIniParams, Apnum);
end;

function TZmpInfo.GetAbilities: IMapAbilitiesConfigStatic;
begin
  Result := FAbilities;
end;

function TZmpInfo.GetContentTypeSubst: IContentTypeSubst;
begin
  Result := FContentTypeSubst;
end;

function TZmpInfo.GetDataProvider: IConfigDataProvider;
begin
  Result := FConfig;
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
  VIsLayer: Boolean;
  VIsShowOnSmMap: Boolean;
  VIsUseStick: Boolean;
  VIsUseGenPrevious: Boolean;
  VUseDownload: Boolean;
begin
  VIsLayer := AConfig.ReadBool('asLayer', False);
  VIsShowOnSmMap := AConfig.ReadBool('CanShowOnSmMap', True);
  VIsUseStick := AConfig.ReadBool('Usestick', True);
  VIsUseGenPrevious := AConfig.ReadBool('UseGenPrevious', True);
  VUseDownload := AConfig.ReadBool('UseDwn', True);

  FAbilities :=
    TMapAbilitiesConfigStatic.Create(
      VIsLayer,
      VIsShowOnSmMap,
      VIsUseStick,
      VIsUseGenPrevious,
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
  i,VMaxSubIndex: Integer;
  VParseNumberAfter: String;
  VSL_NameInCache, VSL_Ext, VSL_DefUrlBase, VSL_ContentType: TStringList;
  VStrVal,VNameInCacheDefault: String;
  VEnabled, VUseDwn, VUseDel: Boolean;
begin
  // params in special section
  VParams := AConfig.GetSubItem('AttachmentsInfo');

  if not Assigned(VParams) then begin
    FMapAttachmentsInfo:=nil;
    Exit;
  end;
  
  // gui params
  VGUID := LoadGUID(VParams);
  VSL_Names := InternalMakeStringListByLanguage(ALanguageManager.LanguageList, VParams, 'name', '');

  // count of sub-items for single attachment
  VMaxSubIndex := VParams.ReadInteger('MaxSubIndex',0);
  VParseNumberAfter := VParams.ReadString('ParseNumberAfter','');
  VUseDwn := VParams.ReadBool('UseDwn',FALSE);
  VUseDel := VParams.ReadBool('UseDel',FALSE);

  // noway
  VSL_NameInCache:=nil;
  VSL_Ext:=nil;
  VSL_DefUrlBase:=nil;
  VSL_ContentType:=nil;

  if (VMaxSubIndex>=0) and (System.Length(VParseNumberAfter)>0) then begin
    // make containers and obtain default values
    VSL_NameInCache := TStringList.Create;
    VNameInCacheDefault := VParams.ReadString('NameInCache', '');
    if (System.Length(VNameInCacheDefault)>0) then
      VNameInCacheDefault:=ExpandFileName(VNameInCacheDefault);
    VSL_NameInCache.AddObject(VNameInCacheDefault, TObject(Pointer(Ord(VParams.ReadBool('Enabled',FALSE)))));

    VSL_Ext := TStringList.Create;
    VSL_Ext.Add(LowerCase(VParams.ReadString('Ext', '')));

    VSL_DefUrlBase := TStringList.Create;
    VSL_DefUrlBase.Add(VParams.ReadString('DefUrlBase', ''));

    VSL_ContentType := TStringList.Create;
    VSL_ContentType.Add(VParams.ReadString('ContentType', ''));

    // other values (by index)
    if VMaxSubIndex>0 then
    for i := 1 to VMaxSubIndex do begin
      VStrVal := ExpandFileName(VParams.ReadString('NameInCache'+IntToStr(i), VNameInCacheDefault));
      VEnabled := VParams.ReadBool('Enabled'+IntToStr(i),(VSL_NameInCache.Objects[0]<>nil));
      VSL_NameInCache.AddObject(VStrVal, TObject(Pointer(Ord(VEnabled))));

      VStrVal := LowerCase(VParams.ReadString('Ext'+IntToStr(i), VSL_Ext[0]));
      VSL_Ext.Add(VStrVal);

      VStrVal := VParams.ReadString('DefUrlBase'+IntToStr(i), VSL_DefUrlBase[0]);
      VSL_DefUrlBase.Add(VStrVal);

      VStrVal := VParams.ReadString('ContentType'+IntToStr(i), VSL_ContentType[0]);
      VSL_ContentType.Add(VStrVal);
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
  LoadVersion(FConfigIniParams);
  LoadProjectionInfo(FConfigIni, ACoordConverterFactory);
  LoadTileRequestBuilderConfig(ACoordConverterFactory, FConfigIniParams);
  LoadTileDownloaderConfig(FConfigIniParams);
  LoadCropConfig(FConfigIniParams);
  LoadStorageConfig(FConfigIniParams);
  LoadAbilities(FConfigIniParams);
  LoadAttachmentsInfo(FConfigIni,ALanguageManager);
  FContentTypeSubst := TContentTypeSubstByList.Create(FConfigIniParams);
end;

procedure TZmpInfo.LoadCropConfig(const AConfig: IConfigDataProvider);
var
  VRect: TRect;
begin
  VRect.Left := AConfig.ReadInteger('TileRLeft',0);
  VRect.Top := AConfig.ReadInteger('TileRTop',0);
  VRect.Right := AConfig.ReadInteger('TileRRight',0);
  VRect.Bottom := AConfig.ReadInteger('TileRBottom',0);
  FTilePostDownloadCropConfig := TTilePostDownloadCropConfigStatic.Create(VRect);
end;

function TZmpInfo.LoadGUID(const AConfig: IConfigDataProvider): TGUID;
var
  VGUIDStr: String;
begin
  VGUIDStr := AConfig.ReadString('GUID', '');
  if Length(VGUIDStr) > 0 then begin
    try
      Result := StringToGUID(VGUIDStr);
    except
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

procedure TZmpInfo.LoadStorageConfig(
  const AConfig: IConfigDataProvider
);
var
  VCacheTypeCode: Integer;
  VNameInCache: string;
  VTileFileExt: string;
  VIsStoreFileCache: Boolean;
  VIsReadOnly: boolean;
  VAllowDelete: boolean;
  VAllowAdd: boolean;
  VAllowReplace: boolean;
begin
  VNameInCache := AConfig.ReadString('NameInCache', '');
  VCacheTypeCode := AConfig.ReadInteger('CacheType', 0);
  // c_File_Cache_Id_GE and c_File_Cache_Id_GC
  if (VCacheTypeCode = 5) or (VCacheTypeCode = 8) then begin
    VTileFileExt := '.ge_image';
    VIsStoreFileCache := False;
    VIsReadOnly := True;
    VAllowDelete := False;
    VAllowAdd := False;
    VAllowReplace := False;
  end else begin
    VTileFileExt := LowerCase(AConfig.ReadString('Ext', '.jpg'));
    VIsStoreFileCache := True;
    VIsReadOnly := False;
    VAllowDelete := True;
    VAllowAdd := True;
    VAllowReplace := True;
  end;

  FStorageConfig :=
    TSimpleTileStorageConfigStatic.Create(
      FGeoConvert,
      VCacheTypeCode,
      VNameInCache,
      VTileFileExt,
      VIsStoreFileCache,
      VIsReadOnly,
      VAllowDelete,
      VAllowAdd,
      VAllowReplace
    );
end;

procedure TZmpInfo.LoadTileDownloaderConfig(const AConfig: IConfigDataProvider);
var
  VUseDownload: Boolean;
  VIgnoreMIMEType: Boolean;
  VDefaultMIMEType: string;
  VExpectedMIMETypes: string;
  VWaitInterval: Cardinal;
  VMaxConnectToServerCount: Cardinal;
  VIteratorSubRectSize: TPoint;
  fL : TStringList;
begin
  VUseDownload := AConfig.ReadBool('UseDwn', True);
  VIgnoreMIMEType := AConfig.ReadBool('IgnoreContentType', False);
  VDefaultMIMEType := AConfig.ReadString('DefaultContentType', 'image/jpg');
  VExpectedMIMETypes := AConfig.ReadString('ContentType', 'image/jpg');
  VWaitInterval := AConfig.ReadInteger('Sleep', 0);
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
    VIteratorSubRectSize.x:=StrToInt(fL[0]);
    VIteratorSubRectSize.y:=StrToInt(fL[1]);
  finally
    fL.Free
  end;
  FTileDownloaderConfig :=
    TTileDownloaderConfigStatic.Create(
      nil,
      VUseDownload,
      VWaitInterval,
      VMaxConnectToServerCount,
      VIgnoreMIMEType,
      VExpectedMIMETypes,
      VDefaultMIMEType,
      VIteratorSubRectSize
    );
end;

procedure TZmpInfo.LoadTileRequestBuilderConfig(
  const ACoordConverterFactory: ICoordConverterFactory;
  const AConfig: IConfigDataProvider
);
var
  VUrlBase: string;
  VRequestHead: string;
  VCoordConverter: ICoordConverter;
begin
  VURLBase := AConfig.ReadString('DefURLBase', '');
  VURLBase := AConfig.ReadString('URLBase', VURLBase);
  VRequestHead := AConfig.ReadString('RequestHead', '');
  VRequestHead := StringReplace(VRequestHead, '\r\n', #13#10, [rfIgnoreCase, rfReplaceAll]);
  VCoordConverter := ACoordConverterFactory.GetCoordConverterByConfig(AConfig);
  FTileDownloadRequestBuilderConfig :=
    TTileDownloadRequestBuilderConfigStatic.Create(
      VUrlBase,
      VRequestHead,
      VCoordConverter
    );
end;

procedure TZmpInfo.LoadVersion(const AConfig: IConfigDataProvider);
var
  VVersion: Variant;
begin
  VVersion := AConfig.ReadString('Version', '');
  FVersionConfig := TMapVersionInfo.Create(VVersion);
end;

end.
