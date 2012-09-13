unit i_TileDownloadSubsystem;

interface

uses
  Types,
  i_NotifierOperation,
  i_TileRequestTask,
  i_TileDownloaderState;

type
  ITileDownloadSubsystem = interface
    ['{06FFC386-43A0-4308-B294-58F8CF429BCB}']
    function GetRequestTask(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const AXY: TPoint;
      AZoom: byte;
      ACheckTileSize: Boolean
    ): ITileRequestTask;
    function GetLink(
      const AXY: TPoint;
      AZoom: byte
    ): string;
    procedure Download(
      const ATileRequestTask: ITileRequestTask
    );

    function GetState: ITileDownloaderStateChangeble;
    property State: ITileDownloaderStateChangeble read GetState;
  end;

implementation

end.
