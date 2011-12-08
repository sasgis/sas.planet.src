unit i_TileDownloadSubsystem;

interface

uses
  Types,
  i_OperationNotifier,
  i_TileRequest,
  i_TileDownloaderState,
  i_TileDownloaderConfig,
  i_TileDownloadRequestBuilderConfig,
  i_TileDownloader;

type
  ITileDownloadSubsystem = interface
    ['{06FFC386-43A0-4308-B294-58F8CF429BCB}']
    function GetRequest(
      ACancelNotifier: IOperationNotifier;
      AOperationID: Integer;
      AXY: TPoint;
      Azoom: byte;
      ACheckTileSize: Boolean
    ): ITileRequest;
    function GetLink(AXY: TPoint; Azoom: byte): string;
    procedure Download(
      ATileRequest: ITileRequest
    );

    function GetState: ITileDownloaderStateChangeble;
    property State: ITileDownloaderStateChangeble read GetState;
  end;

implementation

end.
