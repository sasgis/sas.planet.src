unit i_ITileStorage;

interface

uses
  Types,
  Classes,
  GR32,
  i_CoordConverter,
  i_ContentTypeInfo,
  i_ITileInfoBasic,
  i_ITileStorageTypeInfo,
  u_MapTypeCacheConfig;

type
  ITileStorage = interface
    ['{80A0246E-68E0-4EA0-9B0F-3338472FDB3C}']
    function GetStorageTypeInfo: ITileStorageTypeInfo;
    function GetMainContentType: IContentTypeInfoBasic;
    function GetAllowDifferentContentTypes: Boolean;

    function GetIsStoreFileCache: Boolean;
    function GetUseDel: boolean;
    function GetUseSave: boolean;
    function GetIsStoreReadOnly: boolean;
    function GetTileFileExt: string;
    function GetCoordConverter: ICoordConverter;
    function GetCacheConfig: TMapTypeCacheConfigAbstract;

    function GetTileFileName(AXY: TPoint; Azoom: byte; AVersion: Variant): string;
    function GetTileInfo(AXY: TPoint; Azoom: byte; AVersion: Variant): ITileInfoBasic;

    function LoadTile(AXY: TPoint; Azoom: byte; AVersion: Variant; AStream: TStream; out ATileInfo: ITileInfoBasic): Boolean;
    function DeleteTile(AXY: TPoint; Azoom: byte; AVersion: Variant): Boolean;
    function DeleteTNE(AXY: TPoint; Azoom: byte; AVersion: Variant): Boolean;
    procedure SaveTile(AXY: TPoint; Azoom: byte; AVersion: Variant; AStream: TStream);
    procedure SaveTNE(AXY: TPoint; Azoom: byte; AVersion: Variant);

    function LoadFillingMap(btm: TCustomBitmap32; AXY: TPoint; Azoom: byte; ASourceZoom: byte; AVersion: Variant; IsStop: PBoolean): boolean;
  end;


implementation

end.
