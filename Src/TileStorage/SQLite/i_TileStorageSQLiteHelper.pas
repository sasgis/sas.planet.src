unit i_TileStorageSQLiteHelper;

interface

uses
  Types,
  t_TileStorageSQLite,
  t_NotifierOperationRec,
  i_MapVersionRequest,
  i_MapVersionInfo,
  i_MapVersionListStatic,
  i_TileStorage;

type
  ITileStorageSQLiteHelper = interface
    ['{581A3C7C-86C2-4B7C-9B42-FC79CEB333C8}']
    // sync
    procedure Sync;

    // load tile info
    function GetTileInfo(
      const AOper: PNotifierOperationRec;
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      const AMode: TGetTileInfoMode;
      const AUsePrevVersions: Boolean;
      const AResult: PGetTileResult
    ): Boolean;

    // load tile rect info
    function GetTileRectInfo(
      const AOper: PNotifierOperationRec;
      const AUsePrevVersions: Boolean;
      const AEnumData: TTileInfoShortEnumData
    ): Boolean;

    // delete tile
    function DeleteTile(
      const AOper: PNotifierOperationRec;
      const ADeleteTileAllData: PDeleteTileAllData
    ): Boolean;

    // save tile or tne to storage
    function SaveTile(
      const AOper: PNotifierOperationRec;
      const ASaveTileAllData: PSaveTileAllData
    ): Boolean;

    // get list of versions
    function GetListOfTileVersions(
      const AOper: PNotifierOperationRec;
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersion: IMapVersionRequest
    ): IMapVersionListStatic;
  end;

implementation

end.
