unit i_TileRequest;

interface

uses
  Types,
  i_Notifier,
  i_NotifierOperation,
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

    function GetStartNotifier: INotifierInternal;
    property StartNotifier: INotifierInternal read GetStartNotifier;

    function GetFinishNotifier: INotifierInternal;
    property FinishNotifier: INotifierInternal read GetFinishNotifier;

    function GetCancelNotifier: INotifierOperation;
    property CancelNotifier: INotifierOperation read GetCancelNotifier;

    function GetOperationID: Integer;
    property OperationID: Integer read GetOperationID;
  end;

  ITileRequestWithSizeCheck = interface(ITileRequest)
    ['{7F77BF27-E741-40BD-9F8B-D25CDC09C57B}']
  end;

implementation

end.
