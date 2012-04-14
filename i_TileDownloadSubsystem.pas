unit i_TileDownloadSubsystem;

interface

uses
  Types,
  i_OperationNotifier,
  i_TileRequest,
  i_TileDownloaderState;

type
  ITileDownloadSubsystem = interface
    ['{06FFC386-43A0-4308-B294-58F8CF429BCB}']
    function GetRequest(
      const ACancelNotifier: IOperationNotifier;
      AOperationID: Integer;
      const AXY: TPoint;
      Azoom: byte;
      ACheckTileSize: Boolean
    ): ITileRequest;
    function GetLink(const AXY: TPoint; Azoom: byte): string;
    procedure Download(
      const ATileRequest: ITileRequest
    );

    function GetState: ITileDownloaderStateChangeble;
    property State: ITileDownloaderStateChangeble read GetState;
  end;

implementation

end.
