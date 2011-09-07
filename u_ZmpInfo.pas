unit u_ZmpInfo;

interface

uses
  SysUtils,
  Graphics,
  Classes,
  i_CoordConverter,
  i_ConfigDataProvider,
  i_MapVersionInfo,
  i_ContentTypeSubst,
  i_TileRequestBuilderConfig,
  i_TileDownloaderConfig,
  i_LanguageManager,
  i_CoordConverterFactory,
  i_ZmpInfo;

type
  TZmpInfoGUI = class(TInterfacedObject, IZmpInfoGUI)
  private
    FGUID: TGUID;
    FNameDef: string;
    FName: string;
    FSortIndex: Integer;
    FInfoUrl: string;
    FBmp18: TBitmap;
    FBmp24: TBitmap;
    FHotKey: TShortCut;
    FSeparator: Boolean;
    FParentSubMenuDef: string;
    FParentSubMenu: string;
    FEnabled: Boolean;
  private
    procedure LoadConfig(
      AConfig: IConfigDataProvider;
      AConfigIni: IConfigDataProvider;
      AConfigIniParams: IConfigDataProvider
    );
    procedure LoadIcons(AConfig : IConfigDataProvider);
    procedure LoadUIParams(AConfig : IConfigDataProvider);
    procedure LoadInfo(AConfig : IConfigDataProvider);

    procedure LoadByLang(
      AConfig: IConfigDataProvider;
      AConfigIni: IConfigDataProvider;
      AConfigIniParams: IConfigDataProvider;
      ALanguageCode: string
    );
    procedure LoadInfoLang(AConfig : IConfigDataProvider; ALanguageCode: string);
    procedure LoadUIParamsLang(AConfig : IConfigDataProvider; ALanguageCode: string);
  protected
    function GetName: string;
    function GetSortIndex: Integer;
    function GetInfoUrl: string;
    function GetBmp18: TBitmap;
    function GetBmp24: TBitmap;
    function GetHotKey: TShortCut;
    function GetSeparator: Boolean;
    function GetParentSubMenu: string;
    function GetEnabled: Boolean;
  public
    constructor Create(
      AGUID: TGUID;
      ALanguageManager: ILanguageManager;
      AConfig: IConfigDataProvider;
      AConfigIni: IConfigDataProvider;
      AConfigIniParams: IConfigDataProvider;
      Apnum: Integer
    );
    destructor Destroy; override;
  end;

  TZmpInfo = class(TInterfacedObject, IZmpInfo)
  private
    FGUID: TGUID;
    FFileName: string;
    FVersionConfig: IMapVersionInfo;
    FTileRequestBuilderConfig: ITileRequestBuilderConfigStatic;
    FTileDownloaderConfig: ITileDownloaderConfigStatic;
    FContentTypeSubst: IContentTypeSubst;
    FGeoConvert: ICoordConverter;
    FViewGeoConvert: ICoordConverter;
    FGUI: IZmpInfoGUI;

    FConfig: IConfigDataProvider;
    FConfigIni: IConfigDataProvider;
    FConfigIniParams: IConfigDataProvider;
  private
    procedure LoadConfig(
      ACoordConverterFactory: ICoordConverterFactory
    );
    function LoadGUID(AConfig : IConfigDataProvider): TGUID;
    procedure LoadVersion(AConfig : IConfigDataProvider);
    procedure LoadProjectionInfo(
      AConfig : IConfigDataProvider;
      ACoordConverterFactory: ICoordConverterFactory
    );
    procedure LoadTileRequestBuilderConfig(AConfig : IConfigDataProvider);
    procedure LoadTileDownloaderConfig(AConfig: IConfigDataProvider);
  protected
    function GetGUID: TGUID;
    function GetGUI: IZmpInfoGUI;
    function GetFileName: string;
    function GetVersionConfig: IMapVersionInfo;
    function GetTileRequestBuilderConfig: ITileRequestBuilderConfigStatic;
    function GetTileDownloaderConfig: ITileDownloaderConfigStatic;
    function GetContentTypeSubst: IContentTypeSubst;
    function GetGeoConvert: ICoordConverter;
    function GetViewGeoConvert: ICoordConverter;
    function GetDataProvider: IConfigDataProvider;
  public
    constructor Create(
      ALanguageManager: ILanguageManager;
      ACoordConverterFactory: ICoordConverterFactory;
      AFileName: string;
      AConfig: IConfigDataProvider;
      Apnum: Integer
    );
  end;

  EZmpError = class(Exception);
  EZmpIniNotFound = class(EZmpError);
  EZmpParamsNotFound = class(EZmpError);
  EZmpGUIDError = class(EZmpError);

implementation

uses
  gnugettext,
  u_TileRequestBuilderConfig,
  u_TileDownloaderConfigStatic,
  u_ContentTypeSubstByList,
  u_MapVersionInfo,
  u_ResStrings;

{ TZmpInfoGUI }

constructor TZmpInfoGUI.Create(
  AGUID: TGUID;
  ALanguageManager: ILanguageManager;
  AConfig: IConfigDataProvider;
  AConfigIni: IConfigDataProvider;
  AConfigIniParams: IConfigDataProvider;
  Apnum: Integer
);
var
  VCurrentLanguageCode: string;
begin
  FGUID := AGUID;
  FNameDef:='map#'+inttostr(Apnum);

  VCurrentLanguageCode := ALanguageManager.GetCurrentLanguageCode;
  LoadConfig(AConfig, AConfigIni, AConfigIniParams);
  LoadByLang(AConfig, AConfigIni, AConfigIniParams, VCurrentLanguageCode);
end;

destructor TZmpInfoGUI.Destroy;
begin
  FreeAndNil(FBmp18);
  FreeAndNil(FBmp24);
  inherited;
end;

function TZmpInfoGUI.GetBmp18: TBitmap;
begin
  Result := FBmp18;
end;

function TZmpInfoGUI.GetBmp24: TBitmap;
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

function TZmpInfoGUI.GetInfoUrl: string;
begin
  Result := FInfoUrl;
end;

function TZmpInfoGUI.GetName: string;
begin
  Result := FName;
end;

function TZmpInfoGUI.GetParentSubMenu: string;
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

procedure TZmpInfoGUI.LoadByLang(
  AConfig: IConfigDataProvider;
  AConfigIni: IConfigDataProvider;
  AConfigIniParams: IConfigDataProvider;
  ALanguageCode: string
);
begin
  LoadInfoLang(AConfig, ALanguageCode);
  LoadUIParamsLang(AConfigIniParams, ALanguageCode);
end;

procedure TZmpInfoGUI.LoadConfig(
  AConfig: IConfigDataProvider;
  AConfigIni: IConfigDataProvider;
  AConfigIniParams: IConfigDataProvider
);
begin
  LoadUIParams(AConfigIniParams);
  LoadIcons(AConfig);
  LoadInfo(AConfig);
end;

procedure TZmpInfoGUI.LoadIcons(AConfig: IConfigDataProvider);
var
  VStream:TMemoryStream;
begin
  Fbmp24:=TBitmap.create;
  VStream:=TMemoryStream.Create;
  try
    try
      AConfig.ReadBinaryStream('24.bmp', VStream);
      VStream.Position:=0;
      Fbmp24.LoadFromStream(VStream);
    except
      Fbmp24.Canvas.FillRect(Fbmp24.Canvas.ClipRect);
      Fbmp24.Width:=24;
      Fbmp24.Height:=24;
      Fbmp24.Canvas.TextOut(7,3,copy(FNameDef,1,1));
    end;
  finally
    FreeAndNil(VStream);
  end;
  Fbmp18:=TBitmap.create;
  VStream:=TMemoryStream.Create;
  try
    try
      AConfig.ReadBinaryStream('18.bmp', VStream);
      VStream.Position:=0;
      Fbmp18.LoadFromStream(VStream);
    except
      Fbmp18.Canvas.FillRect(Fbmp18.Canvas.ClipRect);
      Fbmp18.Width:=18;
      Fbmp18.Height:=18;
      Fbmp18.Canvas.TextOut(3,2,copy(FName,1,1));
    end;
  finally
    FreeAndNil(VStream);
  end;
end;

procedure TZmpInfoGUI.LoadInfo(AConfig: IConfigDataProvider);
begin
  if AConfig.ReadString('index.html', '') <> '' then begin
    FInfoUrl := 'sas://ZmpInfo/' + GUIDToString(FGUID) + '/';
  end else if AConfig.ReadString('info.txt', '') <> '' then begin
    FInfoUrl := 'sas://ZmpInfo/' + GUIDToString(FGUID) + '/info.txt';
  end else begin
    FInfoUrl := '';
  end;
end;

procedure TZmpInfoGUI.LoadInfoLang(AConfig: IConfigDataProvider;
  ALanguageCode: string);
var
  VFileName: string;
begin
  VFileName := 'index_'+ALanguageCode+'.html';
  if AConfig.ReadString(VFileName, '') <> '' then begin
    FInfoUrl := 'sas://ZmpInfo/' + GUIDToString(FGUID) + '/' + VFileName;
  end else begin
    VFileName := 'info_'+ALanguageCode+'.txt';
    if AConfig.ReadString(VFileName, '') <> '' then begin
      FInfoUrl := 'sas://ZmpInfo/' + GUIDToString(FGUID) + '/' + VFileName;
    end;
  end;
end;

procedure TZmpInfoGUI.LoadUIParams(AConfig: IConfigDataProvider);
begin
  FNameDef := AConfig.ReadString('name', FNameDef);
  FHotKey :=AConfig.ReadInteger('DefHotKey', 0);
  FHotKey :=AConfig.ReadInteger('HotKey', FHotKey);
  FParentSubMenuDef := AConfig.ReadString('ParentSubMenu', '');
  FSeparator := AConfig.ReadBool('separator', false);
  FEnabled := AConfig.ReadBool('Enabled', true);
  FSortIndex := AConfig.ReadInteger('pnum', -1);
end;

procedure TZmpInfoGUI.LoadUIParamsLang(AConfig: IConfigDataProvider;
  ALanguageCode: string);
begin
  FName := AConfig.ReadString('name_' + ALanguageCode, FNameDef);
  FParentSubMenu := AConfig.ReadString('ParentSubMenu_' + ALanguageCode, FParentSubMenuDef);
end;

{ TZmpInfo }

constructor TZmpInfo.Create(
  ALanguageManager: ILanguageManager;
  ACoordConverterFactory: ICoordConverterFactory;
  AFileName: string;
  AConfig: IConfigDataProvider;
  Apnum: Integer
);
begin
  FFileName := AFileName;
  FConfig := AConfig;
  FConfigIni := FConfig.GetSubItem('params.txt');
  if FConfigIni = nil then begin
    raise EZmpIniNotFound.Create(_('Not found "params.txt" in zmp'));
  end;
  FConfigIniParams := FConfigIni.GetSubItem('PARAMS');
  if FConfigIniParams = nil then begin
    raise EZmpParamsNotFound.Create(_('Not found PARAMS section in zmp'));
  end;
  LoadConfig(ACoordConverterFactory);
  FGUI := TZmpInfoGUI.Create(FGUID, ALanguageManager, FConfig, FConfigIni, FConfigIniParams, Apnum);
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

function TZmpInfo.GetViewGeoConvert: ICoordConverter;
begin
  Result := FViewGeoConvert;
end;

function TZmpInfo.GetTileDownloaderConfig: ITileDownloaderConfigStatic;
begin
  Result := FTileDownloaderConfig;
end;

function TZmpInfo.GetTileRequestBuilderConfig: ITileRequestBuilderConfigStatic;
begin
  Result := FTileRequestBuilderConfig;
end;

function TZmpInfo.GetVersionConfig: IMapVersionInfo;
begin
  Result := FVersionConfig;
end;

procedure TZmpInfo.LoadConfig(ACoordConverterFactory: ICoordConverterFactory);
begin
  FGUID := LoadGUID(FConfigIniParams);
  LoadVersion(FConfigIniParams);
  LoadProjectionInfo(FConfigIni, ACoordConverterFactory);
  LoadTileRequestBuilderConfig(FConfigIniParams);
  LoadTileDownloaderConfig(FConfigIniParams);
  FContentTypeSubst := TContentTypeSubstByList.Create(FConfigIniParams);
end;

function TZmpInfo.LoadGUID(AConfig: IConfigDataProvider): TGUID;
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

procedure TZmpInfo.LoadProjectionInfo(AConfig: IConfigDataProvider; ACoordConverterFactory: ICoordConverterFactory);
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

procedure TZmpInfo.LoadTileDownloaderConfig(AConfig: IConfigDataProvider);
var
  VIgnoreMIMEType: Boolean;
  VDefaultMIMEType: string;
  VExpectedMIMETypes: string;
  VWaitInterval: Cardinal;
  VMaxConnectToServerCount: Cardinal;
begin
  VIgnoreMIMEType := AConfig.ReadBool('IgnoreContentType', False);
  VDefaultMIMEType := AConfig.ReadString('DefaultContentType', 'image/jpg');
  VExpectedMIMETypes := AConfig.ReadString('ContentType', 'image/jpg');
  VWaitInterval := AConfig.ReadInteger('Sleep', 0);
  VMaxConnectToServerCount := AConfig.ReadInteger('MaxConnectToServerCount', 1);

  FTileDownloaderConfig :=
    TTileDownloaderConfigStatic.Create(
      nil,
      VWaitInterval,
      VMaxConnectToServerCount,
      VIgnoreMIMEType,
      VExpectedMIMETypes,
      VDefaultMIMEType
    );
end;

procedure TZmpInfo.LoadTileRequestBuilderConfig(AConfig: IConfigDataProvider);
var
  VUrlBase: string;
  VRequestHead: string;
begin
  VURLBase := AConfig.ReadString('DefURLBase', '');
  VURLBase := AConfig.ReadString('URLBase', VURLBase);
  VRequestHead := AConfig.ReadString('RequestHead', '');
  VRequestHead := StringReplace(VRequestHead, '\r\n', #13#10, [rfIgnoreCase, rfReplaceAll]);
  FTileRequestBuilderConfig := TTileRequestBuilderConfigStatic.Create(VUrlBase, VRequestHead);
end;

procedure TZmpInfo.LoadVersion(AConfig: IConfigDataProvider);
var
  VVersion: Variant;
begin
  VVersion := AConfig.ReadString('Version', '');
  FVersionConfig := TMapVersionInfo.Create(VVersion);
end;

end.
