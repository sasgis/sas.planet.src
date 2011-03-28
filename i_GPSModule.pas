unit i_GPSModule;

interface

uses
  i_JclNotify,
  i_GPS;

type
  IGPSModule = interface
    ['{B477FFBD-C36D-40C6-AF7F-B118E47A6815}']
    function GetPosition: IGPSPosition; safecall;
    property Position: IGPSPosition read GetPosition;

    function GetDataReciveNotifier: IJclNotifier; safecall;
    property DataReciveNotifier: IJclNotifier read GetDataReciveNotifier;

    function GetConnectingNotifier: IJclNotifier; safecall;
    property ConnectingNotifier: IJclNotifier read GetConnectingNotifier;

    function GetConnectedNotifier: IJclNotifier; safecall;
    property ConnectedNotifier: IJclNotifier read GetConnectedNotifier;

    function GetDisconnectingNotifier: IJclNotifier; safecall;
    property DisconnectingNotifier: IJclNotifier read GetDisconnectingNotifier;

    function GetDisconnectedNotifier: IJclNotifier; safecall;
    property DisconnectedNotifier: IJclNotifier read GetDisconnectedNotifier;

    function GetTimeOutNotifier: IJclNotifier; safecall;
    property TimeOutNotifier: IJclNotifier read GetTimeOutNotifier;

    function GetConnectErrorNotifier: IJclNotifier; safecall;
    property ConnectErrorNotifier: IJclNotifier read GetConnectErrorNotifier;
  end;

implementation

end.
