unit i_TileStorageSQLiteHandler;

interface

uses
  Types,
  t_TileStorageSQLite,
  t_NotifierOperationRec,
  i_MapVersionInfo,
  i_MapVersionListStatic;

type
  ITileStorageSQLiteHandler = interface
    ['{D35C0131-C9AD-47E2-BCBF-7589AF2C5CDE}']
    // check database available
    function Opened: Boolean;

    // select tile
    function GetTileInfo(
      const AOper: PNotifierOperationRec;
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      const AMode: TGetTileInfoModeSQLite;
      const AUsePrevVersions: Boolean;
      const AResult: PGetTileResult
    ): Boolean;

    // delete tile
    function DeleteTile(const ADeleteTileAllData: PDeleteTileAllData): Boolean;

    // save tile
    function SaveTile(const ASaveTileAllData: PSaveTileAllData): Boolean;

    // get list of versions
    function GetListOfTileVersions(
      const AOper: PNotifierOperationRec;
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): IMapVersionListStatic;

    // get information by tile rect
    function GetTileRectInfo(
      const AOper: PNotifierOperationRec;
      const AUsePrevVersions: Boolean;
      const AEnumData: TTileInfoShortEnumData
    ): Boolean;
  end;

implementation

end.
