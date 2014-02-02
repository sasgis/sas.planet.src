unit u_ExportToJnxTask;

interface

uses
  i_TileStorage,
  i_MapVersionInfo,
  i_BitmapTileSaveLoad;

type
  TExportTaskJnx = record
    FScale: Byte;
    FZoom: Byte;
    FTileStorage: ITileStorage;
    FMapVersion: IMapVersionInfo;
    FLevelDesc: String;
    FLevelName: String;
    FLevelCopyright: String;
    FSaver: IBitmapTileSaver;
    FRecompress: Boolean;
  end;

  TExportTaskJnxArray = array of TExportTaskJnx;

implementation

end.
