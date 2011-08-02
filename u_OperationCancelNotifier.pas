unit u_OperationCancelNotifier;

interface

uses
  i_JclNotify,
  i_OperationCancelNotifier;

type
  TOperationCancelNotifier = class(TInterfacedObject, IOperationCancelNotifier)
  private
    FNotifier: IJclNotifier;
    FCanceled: Boolean;
  protected
    function GetCanceled: Boolean;
    procedure AddListener(listener: IJclListener); stdcall;
    procedure RemoveListener(listener: IJclListener); stdcall;
  public
    constructor Create();
    procedure SetCanceled;
  end;

implementation

uses
  u_JclNotify;

{ TOperationCancelNotifier }

procedure TOperationCancelNotifier.AddListener(listener: IJclListener);
begin
  FNotifier.Add(listener);
end;

constructor TOperationCancelNotifier.Create;
begin
  FNotifier := TJclBaseNotifier.Create;
  FCanceled := False;
end;

function TOperationCancelNotifier.GetCanceled: Boolean;
begin
  Result := FCanceled;
end;

procedure TOperationCancelNotifier.RemoveListener(listener: IJclListener);
begin
  FNotifier.Remove(listener);
end;

procedure TOperationCancelNotifier.SetCanceled;
begin
  FCanceled := True;
  FNotifier.Notify(nil);
end;

end.
