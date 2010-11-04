unit u_TileStorageGE;

interface

uses
  Types,
  Classes,
  i_ICoordConverter,
  u_MapTypeCacheConfig,
  u_TileStorageAbstract;

type
  TTileStorageGE = class(TTileStorageAbstract)
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
  SysUtils;

{ TTileStorageGEStuped }

constructor TTileStorageGE.Create(ACoordConverter: ICoordConverter);
begin
  inherited Create(ACoordConverter);
  FCacheConfig := TMapTypeCacheConfigGE.Create;
end;

function TTileStorageGE.DeleteTile(AXY: TPoint; Azoom: byte): Boolean;
begin
  Result := False;
end;

function TTileStorageGE.DeleteTNE(AXY: TPoint; Azoom: byte): Boolean;
begin
  Result := False;
end;

destructor TTileStorageGE.Destroy;
begin
  FreeAndNil(FCacheConfig);
  inherited;
end;

function TTileStorageGE.ExistsTile(AXY: TPoint; Azoom: byte): Boolean;
begin
  Result := False;
  Abort;
end;

function TTileStorageGE.ExistsTNE(AXY: TPoint; Azoom: byte): Boolean;
begin
  Result := False;
end;

function TTileStorageGE.GetCacheConfig: TMapTypeCacheConfigAbstract;
begin
  Result := FCacheConfig;
end;

function TTileStorageGE.GetIsStoreFileCache: Boolean;
begin
  Result := False;
end;

function TTileStorageGE.GetIsStoreReadOnly: boolean;
begin
  Result := True;
end;

function TTileStorageGE.GetTileFileExt: string;
begin
  Result := 'ge_tile';
end;

function TTileStorageGE.GetTileFileName(AXY: TPoint; Azoom: byte): string;
begin
  Abort;
end;

function TTileStorageGE.GetUseDel: boolean;
begin
  Result := False;
end;

function TTileStorageGE.GetUseSave: boolean;
begin
  Result := False;
end;

function TTileStorageGE.LoadTile(AXY: TPoint; Azoom: byte;
  AStream: TStream): Boolean;
begin
  Result := False;
  Abort;
end;

procedure TTileStorageGE.SaveTile(AXY: TPoint; Azoom: byte;
  AStream: TStream);
begin
  Abort;
end;

procedure TTileStorageGE.SaveTNE(AXY: TPoint; Azoom: byte);
begin
  Abort;
end;

function TTileStorageGE.TileLoadDate(AXY: TPoint; Azoom: byte): TDateTime;
begin
  Result := 0;
end;

function TTileStorageGE.TileSize(AXY: TPoint; Azoom: byte): integer;
begin
  Result := 0;
end;

end.
