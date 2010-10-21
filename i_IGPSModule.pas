unit i_IGPSModule;

interface

uses
  i_JclNotify,
  i_GPS;

type
  IGPSModule = interface
    ['{8A43A950-69A9-42FC-B894-0C29E77A3415}']
    procedure Connect; safecall;
    procedure Disconnect; safecall;
    function GetIsConnected: Boolean; safecall;
    function GetPosition: IGPSPosition; safecall;

    function GetDataReciveNotifier: IJclNotifier; safecall;
    function GetConnectNotifier: IJclNotifier; safecall;
    function GetDisconnectNotifier: IJclNotifier; safecall;
    function GetTimeOutNotifier: IJclNotifier; safecall;

    property IsConnected: Boolean read GetIsConnected;
    property Position: IGPSPosition read GetPosition;
    property DataReciveNotifier: IJclNotifier read GetDataReciveNotifier;
    property ConnectNotifier: IJclNotifier read GetConnectNotifier;
    property DisconnectNotifier: IJclNotifier read GetDisconnectNotifier;
    property TimeOutNotifier: IJclNotifier read GetTimeOutNotifier;
  end;

implementation

end.
