unit u_TileStorageGEStuped;

interface

uses
  Types,
  Classes,
  GR32,
  i_ICoordConverter,
  u_MapTypeCacheConfig,
  u_TileStorageAbstract;

type
  TTileStorageGEStuped = class(TTileStorageAbstract)
  private
    FCacheConfig: TMapTypeCacheConfigAbstract;
  public
    constructor Create(ACoordConverter: ICoordConverter);
    destructor Destroy; override;

    function GetIsStoreFileCache: Boolean; override;
    function GetUseDel: boolean; override;
    function GetUseSave: boolean; override;
    function GetIsStoreReadOnly: boolean; override;

    function ExistsTile(AXY: TPoint; Azoom: byte): Boolean; override;
    function ExistsTNE(AXY: TPoint; Azoom: byte): Boolean; override;

    function DeleteTile(AXY: TPoint; Azoom: byte): Boolean; override;
    function DeleteTNE(AXY: TPoint; Azoom: byte): Boolean; override;

    function GetTileFileName(AXY: TPoint; Azoom: byte): string; override;
    function GetTileFileExt: string; override;
    function GetCacheConfig: TMapTypeCacheConfigAbstract; override;

    function LoadTile(AXY: TPoint; Azoom: byte; AStream: TStream): Boolean; override;
    function TileLoadDate(AXY: TPoint; Azoom: byte): TDateTime; override;
    function TileSize(AXY: TPoint; Azoom: byte): integer; override;

    procedure SaveTile(AXY: TPoint; Azoom: byte; AStream: TStream); override;
    procedure SaveTNE(AXY: TPoint; Azoom: byte); override;
  end;

implementation

uses
  SysUtils,
  u_GECache;

{ TTileStorageGEStuped }

constructor TTileStorageGEStuped.Create(ACoordConverter: ICoordConverter);
begin
  inherited Create(ACoordConverter);
  FCacheConfig := TMapTypeCacheConfigGE.Create;
end;

function TTileStorageGEStuped.DeleteTile(AXY: TPoint; Azoom: byte): Boolean;
begin
  Result := False;
  Abort;
end;

function TTileStorageGEStuped.DeleteTNE(AXY: TPoint; Azoom: byte): Boolean;
begin
  Result := False;
  Abort;
end;

destructor TTileStorageGEStuped.Destroy;
begin
  FreeAndNil(FCacheConfig);
  inherited;
end;

function TTileStorageGEStuped.ExistsTile(AXY: TPoint; Azoom: byte): Boolean;
begin
  result:=GETileExists(FCacheConfig.BasePath+'dbCache.dat.index', AXY.X, AXY.Y, Azoom + 1,GeoConvert);
end;

function TTileStorageGEStuped.ExistsTNE(AXY: TPoint; Azoom: byte): Boolean;
begin
  Result := False;
end;

function TTileStorageGEStuped.GetCacheConfig: TMapTypeCacheConfigAbstract;
begin
  Result := FCacheConfig;
end;

function TTileStorageGEStuped.GetIsStoreFileCache: Boolean;
begin
  Result := False;
end;

function TTileStorageGEStuped.GetIsStoreReadOnly: boolean;
begin
  Result := True;
end;

function TTileStorageGEStuped.GetTileFileExt: string;
begin
  Result := '';
end;

function TTileStorageGEStuped.GetTileFileName(AXY: TPoint; Azoom: byte): string;
begin
  Abort;
end;

function TTileStorageGEStuped.GetUseDel: boolean;
begin
  Result := False;
end;

function TTileStorageGEStuped.GetUseSave: boolean;
begin
  Result := False;
end;

function TTileStorageGEStuped.LoadTile(AXY: TPoint; Azoom: byte;
  AStream: TStream): Boolean;
begin
  Result := False;
  Abort;
end;

procedure TTileStorageGEStuped.SaveTile(AXY: TPoint; Azoom: byte;
  AStream: TStream);
begin
  Abort;
end;

procedure TTileStorageGEStuped.SaveTNE(AXY: TPoint; Azoom: byte);
begin
  Abort;
end;

function TTileStorageGEStuped.TileLoadDate(AXY: TPoint; Azoom: byte): TDateTime;
begin
  Result := 0;
end;

function TTileStorageGEStuped.TileSize(AXY: TPoint; Azoom: byte): integer;
begin
  Result := 0;
end;

end.
