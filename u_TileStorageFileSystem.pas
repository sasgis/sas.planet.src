unit u_TileStorageFileSystem;

interface

uses
  Types,
  Classes,
  GR32,
  u_MapTypeCacheConfig,
  u_TileStorageAbstract;

type
  TTileStorageFileSystem = class(TTileStorageAbstract)
  private
    FCacheConfig: TMapTypeCacheConfig;
  public
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

{ TTileStorageFileSystem }

function TTileStorageFileSystem.DeleteTile(AXY: TPoint; Azoom: byte): Boolean;
begin

end;

function TTileStorageFileSystem.DeleteTNE(AXY: TPoint; Azoom: byte): Boolean;
begin

end;

function TTileStorageFileSystem.ExistsTile(AXY: TPoint; Azoom: byte): Boolean;
begin

end;

function TTileStorageFileSystem.ExistsTNE(AXY: TPoint; Azoom: byte): Boolean;
begin

end;

function TTileStorageFileSystem.GetCacheConfig: TMapTypeCacheConfig;
begin

end;

function TTileStorageFileSystem.GetIsStoreFileCache: Boolean;
begin

end;

function TTileStorageFileSystem.GetIsStoreReadOnly: boolean;
begin

end;

function TTileStorageFileSystem.GetTileFileExt: string;
begin

end;

function TTileStorageFileSystem.GetTileFileName(AXY: TPoint; Azoom: byte): string;
begin

end;

function TTileStorageFileSystem.GetUseDel: boolean;
begin

end;

function TTileStorageFileSystem.GetUseSave: boolean;
begin

end;

function TTileStorageFileSystem.LoadTile(AXY: TPoint; Azoom: byte;
  AStream: TStream): Boolean;
begin

end;

procedure TTileStorageFileSystem.SaveTile(AXY: TPoint; Azoom: byte;
  AStream: TStream);
begin
  inherited;

end;

procedure TTileStorageFileSystem.SaveTNE(AXY: TPoint; Azoom: byte);
begin
  inherited;

end;

function TTileStorageFileSystem.TileLoadDate(AXY: TPoint; Azoom: byte): TDateTime;
begin

end;

function TTileStorageFileSystem.TileSize(AXY: TPoint; Azoom: byte): integer;
begin

end;

end.
