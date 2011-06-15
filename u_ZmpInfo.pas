unit u_ZmpInfo;

interface

uses
  Graphics,
  Classes,
  i_CoordConverter,
  i_ConfigDataProvider,
  i_TileRequestBuilderConfig,
  i_LanguageManager,
  i_ZmpInfo;

type
  TZmpInfo = class(TInterfacedObject, IZmpInfo)
  private
    FGUID: TGUID;
    FFileName: string;
    FName: string;
    FSortIndex: Integer;
    FMapInfo: string;
    FBmp18: TBitmap;
    FBmp24: TBitmap;
    FHotKey: TShortCut;
    FSleep: Cardinal;
    FSeparator: Boolean;
    FParentSubMenu: string;
    FEnabled: Boolean;
    FTileRequestBuilderConfig: ITileRequestBuilderConfigStatic;
    FGeoConvert: ICoordConverter;
    FMainGeoConvert: ICoordConverter;

    FLanguageManager: ILanguageManager;

  private
    procedure LoadIcons(AConfig : IConfigDataProvider);
    procedure LoadProjectionInfo(AConfig : IConfigDataProvider);
    procedure LoadUrlScript(AConfig : IConfigDataProvider);
    procedure LoadWebSourceParams(AConfig : IConfigDataProvider);
    procedure LoadUIParams(AConfig : IConfigDataProvider);
    procedure LoadMapInfo(AConfig : IConfigDataProvider);
  protected
    function GetGUID: TGUID;
    function GetFileName: string;
    function GetName: string;
    function GetMapInfo: string;
    function GetBmp18: TBitmap;
    function GetBmp24: TBitmap;
    function GetHotKey: TShortCut;
    function GetSleep: Cardinal;
    function GetSeparator: Boolean;
    function GetParentSubMenu: string;
    function GetEnabled: Boolean;
    function GetTileRequestBuilderConfig: ITileRequestBuilderConfigStatic;
    function GetGeoConvert: ICoordConverter;
    function GetMainGeoConvert: ICoordConverter;
  public
    constructor Create(
      ALanguageManager: ILanguageManager;
      AGUID: TGUID;
      AConfig: IConfigDataProvider;
      Apnum: Integer
    );
  end;

implementation

uses
  SysUtils;

{ TZmpInfo }

constructor TZmpInfo.Create(ALanguageManager: ILanguageManager; AGUID: TGUID;
  AConfig: IConfigDataProvider; Apnum: Integer);
begin
  FLanguageManager := ALanguageManager;
  FGUID := AGUID;
  FName:='map#'+inttostr(Apnum);
  FFileName := AConfig.ReadString(':::FileName', FName);

end;

function TZmpInfo.GetBmp18: TBitmap;
begin
  Result := FBmp18;
end;

function TZmpInfo.GetBmp24: TBitmap;
begin
  Result := FBmp24;
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

function TZmpInfo.GetMainGeoConvert: ICoordConverter;
begin
  Result := FMainGeoConvert;
end;

function TZmpInfo.GetMapInfo: string;
begin
  Result := FMapInfo;
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

function TZmpInfo.GetSleep: Cardinal;
begin
  Result := FSleep;
end;

function TZmpInfo.GetTileRequestBuilderConfig: ITileRequestBuilderConfigStatic;
begin
  Result := FTileRequestBuilderConfig;
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
      Fbmp24.Canvas.TextOut(7,3,copy(FName,1,1));
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

procedure TZmpInfo.LoadMapInfo(AConfig: IConfigDataProvider);
begin

end;

procedure TZmpInfo.LoadProjectionInfo(AConfig: IConfigDataProvider);
begin

end;

procedure TZmpInfo.LoadUIParams(AConfig: IConfigDataProvider);
begin
  FName:=AConfig.ReadString('name',FName);
  FName:=AConfig.ReadString('name_'+FLanguageManager.GetCurrentLanguageCode,FName);
  FHotKey:=AConfig.ReadInteger('HotKey',0);
  FParentSubMenu:=AConfig.ReadString('ParentSubMenu','');
  FParentSubMenu:=AConfig.ReadString('ParentSubMenu_'+FLanguageManager.GetCurrentLanguageCode,FParentSubMenu);
  FSeparator:=AConfig.ReadBool('separator',false);
  FEnabled:=AConfig.ReadBool('Enabled',true);
  FSortIndex:=AConfig.ReadInteger('pnum',-1);
end;

procedure TZmpInfo.LoadUrlScript(AConfig: IConfigDataProvider);
begin

end;

procedure TZmpInfo.LoadWebSourceParams(AConfig: IConfigDataProvider);
begin

end;

end.
