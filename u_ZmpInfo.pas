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
  TZmpInfo = class(TInterfacedObject, IZmpInfo)
  private
    FGUID: TGUID;
    FFileName: string;
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
    FVersionConfig: IMapVersionInfo;
    FTileRequestBuilderConfig: ITileRequestBuilderConfigStatic;
    FTileDownloaderConfig: ITileDownloaderConfigStatic;
    FContentTypeSubst: IContentTypeSubst;
    FGeoConvert: ICoordConverter;
    FViewGeoConvert: ICoordConverter;

    FConfig: IConfigDataProvider;
    FConfigIni: IConfigDataProvider;
    FConfigIniParams: IConfigDataProvider;
    FCurrentLanguageCode: string;
    FLanguageManager: ILanguageManager;
  private
    procedure LoadConfig(
      ACoordConverterFactory: ICoordConverterFactory
    );
    function LoadGUID(AConfig : IConfigDataProvider): TGUID;
    procedure LoadVersion(AConfig : IConfigDataProvider);
    procedure LoadIcons(AConfig : IConfigDataProvider);
    procedure LoadProjectionInfo(
      AConfig : IConfigDataProvider;
      ACoordConverterFactory: ICoordConverterFactory
    );
    procedure LoadUIParams(AConfig : IConfigDataProvider);
    procedure LoadInfo(AConfig : IConfigDataProvider);
    procedure LoadTileRequestBuilderConfig(AConfig : IConfigDataProvider);
    procedure LoadTileDownloaderConfig(AConfig: IConfigDataProvider);

    procedure LoadByLang(ALanguageCode: string);
    procedure LoadInfoLang(AConfig : IConfigDataProvider; ALanguageCode: string);
    procedure LoadUIParamsLang(AConfig : IConfigDataProvider; ALanguageCode: string);
  protected
    function GetGUID: TGUID;
    function GetFileName: string;
    function GetName: string;
    function GetSortIndex: Integer;
    function GetInfoUrl: string;
    function GetBmp18: TBitmap;
    function GetBmp24: TBitmap;
    function GetHotKey: TShortCut;
    function GetSeparator: Boolean;
    function GetParentSubMenu: string;
    function GetEnabled: Boolean;
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
    destructor Destroy; override;
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

{ TZmpInfo }

constructor TZmpInfo.Create(
  ALanguageManager: ILanguageManager;
  ACoordConverterFactory: ICoordConverterFactory;
  AFileName: string;
  AConfig: IConfigDataProvider;
  Apnum: Integer
);
begin
  FLanguageManager := ALanguageManager;
  FNameDef:='map#'+inttostr(Apnum);
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

  FCurrentLanguageCode := FLanguageManager.GetCurrentLanguageCode;
  LoadConfig(ACoordConverterFactory);
  LoadByLang(FCurrentLanguageCode);
end;

destructor TZmpInfo.Destroy;
begin
  FreeAndNil(FBmp18);
  FreeAndNil(FBmp24);
  inherited;
end;

function TZmpInfo.GetBmp18: TBitmap;
begin
  Result := FBmp18;
end;

function TZmpInfo.GetBmp24: TBitmap;
begin
  Result := FBmp24;
end;

function TZmpInfo.GetContentTypeSubst: IContentTypeSubst;
begin
  Result := FContentTypeSubst;
end;

function TZmpInfo.GetDataProvider: IConfigDataProvider;
begin
  Result := FConfig;
end;

function TZmpInfo.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TZmpInfo.GetFileName: string;
begin
  Result := FFileName;
end;

function TZmpInfo.GetGeoConvert: ICoordConverter;
begin
  Result := FGeoConvert;
end;

function TZmpInfo.GetGUID: TGUID;
begin
  Result := FGUID;
end;

function TZmpInfo.GetHotKey: TShortCut;
begin
  Result := FHotKey;
end;

function TZmpInfo.GetViewGeoConvert: ICoordConverter;
begin
  Result := FViewGeoConvert;
end;

function TZmpInfo.GetInfoUrl: string;
begin
  Result := FInfoUrl;
end;

function TZmpInfo.GetName: string;
begin
  Result := FName;
end;

function TZmpInfo.GetParentSubMenu: string;
begin
  Result := FParentSubMenu;
end;

function TZmpInfo.GetSeparator: Boolean;
begin
  Result := FSeparator;
end;

function TZmpInfo.GetSortIndex: Integer;
begin
  Result := FSortIndex;
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

procedure TZmpInfo.LoadByLang(ALanguageCode: string);
begin
  LoadInfoLang(FConfig, ALanguageCode);
  LoadUIParamsLang(FConfigIniParams, ALanguageCode);
end;

procedure TZmpInfo.LoadConfig(ACoordConverterFactory: ICoordConverterFactory);
begin
  FGUID := LoadGUID(FConfigIniParams);
  LoadVersion(FConfigIniParams);
  LoadUIParams(FConfigIniParams);
  LoadIcons(FConfig);
  LoadInfo(FConfig);
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

procedure TZmpInfo.LoadIcons(AConfig: IConfigDataProvider);
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

procedure TZmpInfo.LoadInfo(AConfig: IConfigDataProvider);
begin
  if AConfig.ReadString('info.txt', '') <> '' then begin
    FInfoUrl := 'sas://ZmpInfo/' + GUIDToString(FGUID) + '/';
  end else begin
    FInfoUrl := '';
  end;
end;

procedure TZmpInfo.LoadInfoLang(AConfig: IConfigDataProvider; ALanguageCode: string);
var
  VFileName: string;
begin
  VFileName := 'info_'+ALanguageCode+'.txt';
  if AConfig.ReadString(VFileName, '') <> '' then begin
    FInfoUrl := 'sas://ZmpInfo/' + GUIDToString(FGUID) + '/' + VFileName;
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

procedure TZmpInfo.LoadUIParams(AConfig: IConfigDataProvider);
begin
  FNameDef := AConfig.ReadString('name', FNameDef);
  FHotKey :=AConfig.ReadInteger('DefHotKey', 0);
  FHotKey :=AConfig.ReadInteger('HotKey', FHotKey);
  FParentSubMenuDef := AConfig.ReadString('ParentSubMenu', '');
  FSeparator := AConfig.ReadBool('separator', false);
  FEnabled := AConfig.ReadBool('Enabled', true);
  FSortIndex := AConfig.ReadInteger('pnum', -1);
end;

procedure TZmpInfo.LoadUIParamsLang(AConfig: IConfigDataProvider; ALanguageCode: string);
begin
  FName := AConfig.ReadString('name_' + ALanguageCode, FNameDef);
  FParentSubMenu := AConfig.ReadString('ParentSubMenu_' + ALanguageCode, FParentSubMenuDef);
end;

procedure TZmpInfo.LoadVersion(AConfig: IConfigDataProvider);
var
  VVersion: Variant;
begin
  VVersion := AConfig.ReadString('Version', '');
  FVersionConfig := TMapVersionInfo.Create(VVersion);
end;

end.
