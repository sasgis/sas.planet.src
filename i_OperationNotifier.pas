unit i_OperationNotifier;

interface

uses
  i_JclNotify;

type
  IOperationNotifier = interface
    ['{96D3C3D0-7B07-4F63-AE3D-6E32516AE56B}']
    function GetCurrentOperation: Integer; stdcall;
    property CurrentOperation: Integer read GetCurrentOperation;

    function IsOperationCanceled(AID: Integer): Boolean; stdcall;

    procedure AddListener(AListener: IJclListener); stdcall;
    procedure RemoveListener(AListener: IJclListener); stdcall;
  end;

implementation

end.
