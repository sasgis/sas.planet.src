unit u_ExportToJnxTask;

interface

uses
  i_TileStorage,
  i_MapVersionRequest,
  i_BitmapTileSaveLoad;

type
  TExportTaskJnx = record
    FScale: Byte;
    FZoom: Byte;
    FTileStorage: ITileStorage;
    FMapVersion: IMapVersionRequest;
    FLevelDesc: String;
    FLevelName: String;
    FLevelCopyright: String;
    FSaver: IBitmapTileSaver;
    FRecompress: Boolean;
  end;

  TExportTaskJnxArray = array of TExportTaskJnx;

implementation

end.
