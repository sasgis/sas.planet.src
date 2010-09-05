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
    FCacheConfig: TMapTypeCacheConfig;
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
    function GetCacheConfig: TMapTypeCacheConfig; override;

    function LoadTile(AXY: TPoint; Azoom: byte; AStream: TStream): Boolean; override;
    function TileLoadDate(AXY: TPoint; Azoom: byte): TDateTime; override;
    function TileSize(AXY: TPoint; Azoom: byte): integer; override;

    procedure SaveTile(AXY: TPoint; Azoom: byte; AStream: TStream); override;
    procedure SaveTNE(AXY: TPoint; Azoom: byte); override;
  end;

implementation

{ TTileStorageGEStuped }

constructor TTileStorageGEStuped.Create(ACoordConverter: ICoordConverter);
begin
  inherited Create(ACoordConverter);
//  FCacheConfig := TMapTypeCacheConfig.Create();
end;

function TTileStorageGEStuped.DeleteTile(AXY: TPoint; Azoom: byte): Boolean;
begin

end;

function TTileStorageGEStuped.DeleteTNE(AXY: TPoint; Azoom: byte): Boolean;
begin

end;

destructor TTileStorageGEStuped.Destroy;
begin

  inherited;
end;

function TTileStorageGEStuped.ExistsTile(AXY: TPoint; Azoom: byte): Boolean;
begin

end;

function TTileStorageGEStuped.ExistsTNE(AXY: TPoint; Azoom: byte): Boolean;
begin

end;

function TTileStorageGEStuped.GetCacheConfig: TMapTypeCacheConfig;
begin

end;

function TTileStorageGEStuped.GetIsStoreFileCache: Boolean;
begin

end;

function TTileStorageGEStuped.GetIsStoreReadOnly: boolean;
begin

end;

function TTileStorageGEStuped.GetTileFileExt: string;
begin

end;

function TTileStorageGEStuped.GetTileFileName(AXY: TPoint; Azoom: byte): string;
begin

end;

function TTileStorageGEStuped.GetUseDel: boolean;
begin

end;

function TTileStorageGEStuped.GetUseSave: boolean;
begin

end;

function TTileStorageGEStuped.LoadTile(AXY: TPoint; Azoom: byte;
  AStream: TStream): Boolean;
begin

end;

procedure TTileStorageGEStuped.SaveTile(AXY: TPoint; Azoom: byte;
  AStream: TStream);
begin
  inherited;

end;

procedure TTileStorageGEStuped.SaveTNE(AXY: TPoint; Azoom: byte);
begin
  inherited;

end;

function TTileStorageGEStuped.TileLoadDate(AXY: TPoint; Azoom: byte): TDateTime;
begin

end;

function TTileStorageGEStuped.TileSize(AXY: TPoint; Azoom: byte): integer;
begin

end;

end.
