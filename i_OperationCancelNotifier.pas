unit i_OperationCancelNotifier;

interface

uses
  i_JclNotify;

type
  IOperationCancelNotifier = interface
    ['{96D3C3D0-7B07-4F63-AE3D-6E32516AE56B}']
    function GetCanceled: Boolean;
    property Canceled: Boolean read GetCanceled;

    procedure AddListener(listener: IJclListener); stdcall;
    procedure RemoveListener(listener: IJclListener); stdcall;
  end;

implementation

end.
