unit i_ZmpInfo;

interface

uses
  Graphics,
  Classes,
  i_ConfigDataProvider,
  i_CoordConverter,
  i_TileDownloaderConfig,
  i_MapVersionConfig,
  i_TileRequestBuilderConfig;

type
  IZmpInfo = interface
    ['{4AD18200-DD3B-42E4-AC57-44C12634C0EB}']
    function GetGUID: TGUID;
    property GUID: TGUID read GetGUID;

    function GetFileName: string;
    property FileName: string read GetFileName;

    function GetName: string;
    property Name: string read GetName;

    function GetInfo: string;
    property Info: string read GetInfo;

    function GetSortIndex: Integer;
    property SortIndex: Integer read GetSortIndex;

    function GetBmp18: TBitmap;
    property Bmp18: TBitmap read GetBmp18;

    function GetBmp24: TBitmap;
    property Bmp24: TBitmap read GetBmp24;

    function GetHotKey: TShortCut;
    property HotKey: TShortCut read GetHotKey;

    function GetSeparator: Boolean;
    property Separator: Boolean read GetSeparator;

    function GetParentSubMenu: string;
    property ParentSubMenu: string read GetParentSubMenu;

    function GetEnabled: Boolean;
    property Enabled: Boolean read GetEnabled;

    function GetVersionConfig: IMapVersionConfigStatic;
    property VersionConfig: IMapVersionConfigStatic read GetVersionConfig;

    function GetTileRequestBuilderConfig: ITileRequestBuilderConfigStatic;
    property TileRequestBuilderConfig: ITileRequestBuilderConfigStatic read GetTileRequestBuilderConfig;

    function GetTileDownloaderConfig: ITileDownloaderConfigStatic;
    property TileDownloaderConfig: ITileDownloaderConfigStatic read GetTileDownloaderConfig;

    function GetGeoConvert: ICoordConverter;
    property GeoConvert: ICoordConverter read GetGeoConvert;

    function GetViewGeoConvert: ICoordConverter;
    property ViewGeoConvert: ICoordConverter read GetViewGeoConvert;

    function GetDataProvider: IConfigDataProvider;
    property DataProvider: IConfigDataProvider read GetDataProvider;
  end;

implementation

end.
