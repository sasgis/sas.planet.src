unit i_TileDownloadSubsystem;

interface

uses
  Types,
  i_NotifierOperation,
  i_MapVersionInfo,
  i_TileRequestTask,
  i_TileDownloaderState;

type
  ITileDownloadSubsystem = interface
    ['{06FFC386-43A0-4308-B294-58F8CF429BCB}']
    function GetRequestTask(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo;
      ACheckTileSize: Boolean
    ): ITileRequestTask;
    function GetLink(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): string;
    procedure Download(
      const ATileRequestTask: ITileRequestTask
    );

    function GetState: ITileDownloaderStateChangeble;
    property State: ITileDownloaderStateChangeble read GetState;
  end;

implementation

end.
