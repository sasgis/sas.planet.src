unit u_TileStorageGEStuped;

interface

uses
  Types,
  Classes,
  GR32,
  i_ICoordConverter,
  i_ContentTypeInfo,
  i_ITileInfoBasic,
  i_IConfigDataProvider,
  u_MapTypeCacheConfig,
  u_TileStorageAbstract;

type
  TTileStorageGEStuped = class(TTileStorageAbstract)
  private
    FCoordConverter: ICoordConverter;
    FCacheConfig: TMapTypeCacheConfigAbstract;
    FMainContentType: IContentTypeInfoBasic;
  public
    constructor Create(AConfig: IConfigDataProvider);
    destructor Destroy; override;

    function GetMainContentType: IContentTypeInfoBasic; override;
    function GetAllowDifferentContentTypes: Boolean; override;

    function GetIsStoreFileCache: Boolean; override;
    function GetUseDel: boolean; override;
    function GetUseSave: boolean; override;
    function GetIsStoreReadOnly: boolean; override;
    function GetTileFileExt: string; override;
    function GetCoordConverter: ICoordConverter; override;
    function GetCacheConfig: TMapTypeCacheConfigAbstract; override;

    function GetTileFileName(AXY: TPoint; Azoom: byte; AVersion: Variant): string; override;

    function GetTileInfo(AXY: TPoint; Azoom: byte; AVersion: Variant): ITileInfoBasic; override;

    function LoadTile(AXY: TPoint; Azoom: byte; AVersion: Variant; AStream: TStream; out ATileInfo: ITileInfoBasic): Boolean; override;

    function DeleteTile(AXY: TPoint; Azoom: byte; AVersion: Variant): Boolean; override;
    function DeleteTNE(AXY: TPoint; Azoom: byte; AVersion: Variant): Boolean; override;

    procedure SaveTile(AXY: TPoint; Azoom: byte; AVersion: Variant; AStream: TStream); override;
    procedure SaveTNE(AXY: TPoint; Azoom: byte; AVersion: Variant); override;
  end;

implementation

uses
  SysUtils,
  Variants,
  u_TileInfoBasic,
  u_GlobalState,
  u_GECache;

{ TTileStorageGEStuped }

constructor TTileStorageGEStuped.Create(AConfig: IConfigDataProvider);
var
  VParams: IConfigDataProvider;
begin
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');
  FCoordConverter := GState.CoordConverterFactory.GetCoordConverterByConfig(VParams);
  FCacheConfig := TMapTypeCacheConfigGE.Create(AConfig);
  FMainContentType := GState.ContentTypeManager.GetInfo('image/jpeg');
end;

destructor TTileStorageGEStuped.Destroy;
begin
  FCoordConverter := nil;
  FreeAndNil(FCacheConfig);
  inherited;
end;

function TTileStorageGEStuped.DeleteTile(AXY: TPoint; Azoom: byte; AVersion: Variant): Boolean;
begin
  Result := False;
  Abort;
end;

function TTileStorageGEStuped.DeleteTNE(AXY: TPoint; Azoom: byte; AVersion: Variant): Boolean;
begin
  Result := False;
  Abort;
end;

function TTileStorageGEStuped.GetAllowDifferentContentTypes: Boolean;
begin
  Result := False;
end;

function TTileStorageGEStuped.GetCacheConfig: TMapTypeCacheConfigAbstract;
begin
  Result := FCacheConfig;
end;

function TTileStorageGEStuped.GetCoordConverter: ICoordConverter;
begin
  Result := FCoordConverter;
end;

function TTileStorageGEStuped.GetIsStoreFileCache: Boolean;
begin
  Result := False;
end;

function TTileStorageGEStuped.GetIsStoreReadOnly: boolean;
begin
  Result := True;
end;

function TTileStorageGEStuped.GetMainContentType: IContentTypeInfoBasic;
begin
  Result := FMainContentType;
end;

function TTileStorageGEStuped.GetTileFileExt: string;
begin
  Result := '.jpg';
end;

function TTileStorageGEStuped.GetTileFileName(AXY: TPoint; Azoom: byte; AVersion: Variant): string;
begin
  Abort;
end;

function TTileStorageGEStuped.GetTileInfo(AXY: TPoint; Azoom: byte;
  AVersion: Variant): ITileInfoBasic;
begin
  if GETileExists(FCacheConfig.BasePath+'dbCache.dat.index', AXY.X, AXY.Y, Azoom + 1, FCoordConverter) then begin
    Result := TTileInfoBasicExists.Create(0, 0, Unassigned, FMainContentType);
  end else begin
    Result := TTileInfoBasicNotExists.Create(0, Unassigned);
  end;
end;

function TTileStorageGEStuped.GetUseDel: boolean;
begin
  Result := False;
end;

function TTileStorageGEStuped.GetUseSave: boolean;
begin
  Result := False;
end;

function TTileStorageGEStuped.LoadTile(AXY: TPoint; Azoom: byte; AVersion: Variant;
  AStream: TStream; out ATileInfo: ITileInfoBasic): Boolean;
begin
  Result := False;
  Abort;
end;

procedure TTileStorageGEStuped.SaveTile(AXY: TPoint; Azoom: byte; AVersion: Variant;
  AStream: TStream);
begin
  Abort;
end;

procedure TTileStorageGEStuped.SaveTNE(AXY: TPoint; Azoom: byte; AVersion: Variant);
begin
  Abort;
end;

end.
