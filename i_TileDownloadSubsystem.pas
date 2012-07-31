unit i_TileDownloadSubsystem;

interface

uses
  Types,
  i_NotifierOperation,
  i_TileRequest,
  i_TileDownloaderState;

type
  ITileDownloadSubsystem = interface
    ['{06FFC386-43A0-4308-B294-58F8CF429BCB}']
    function GetRequest(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const AXY: TPoint;
      AZoom: byte;
      ACheckTileSize: Boolean
    ): ITileRequest;
    function GetLink(
      const AXY: TPoint;
      AZoom: byte
    ): string;
    procedure Download(
      const ATileRequest: ITileRequest
    );

    function GetState: ITileDownloaderStateChangeble;
    property State: ITileDownloaderStateChangeble read GetState;
  end;

implementation

end.
