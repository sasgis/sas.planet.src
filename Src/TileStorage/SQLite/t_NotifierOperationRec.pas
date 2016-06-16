unit t_NotifierOperationRec;

interface

uses
  i_NotifierOperation;

type
  TNotifierOperationRec = record
    OperationID: Integer;
    CancelNotifier: INotifierOperation;
    UseExclusively: Boolean;
  public
    procedure Init(const ACancelNotifier: INotifierOperation);
    function IsOperationCancelled: Boolean;
  end;
  PNotifierOperationRec = ^TNotifierOperationRec;

implementation

{ TNotifierOperationRec }

procedure TNotifierOperationRec.Init(const ACancelNotifier: INotifierOperation);
begin
  CancelNotifier := ACancelNotifier;
  OperationID := ACancelNotifier.CurrentOperation;
end;

function TNotifierOperationRec.IsOperationCancelled: Boolean;
begin
  Result := Assigned(@Self) and Assigned(CancelNotifier) and
    CancelNotifier.IsOperationCanceled(OperationID);
end;

end.
