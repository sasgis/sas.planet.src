unit i_TileRequest;

interface

uses
  Types,
  i_Notify,
  i_OperationNotifier,
  i_MapVersionInfo;

type
  ITileRequest = interface
    ['{2E7FE7D3-1343-4823-876A-BBAD4D483728}']
    function GetTile: TPoint;
    property Tile: TPoint read GetTile;

    function GetZoom: Byte;
    property Zoom: Byte read GetZoom;

    function GetVersionInfo: IMapVersionInfo;
    property VersionInfo: IMapVersionInfo read GetVersionInfo;

    function GetStartNotifier: INotifier;
    property StartNotifier: INotifier read GetStartNotifier;

    function GetFinishNotifier: INotifier;
    property FinishNotifier: INotifier read GetFinishNotifier;

    function GetCancelNotifier: IOperationNotifier;
    property CancelNotifier: IOperationNotifier read GetCancelNotifier;

    function GetOperationID: Integer;
    property OperationID: Integer read GetOperationID;
  end;

  ITileRequestWithSizeCheck = interface(ITileRequest)
    ['{7F77BF27-E741-40BD-9F8B-D25CDC09C57B}']
  end;

implementation

end.

