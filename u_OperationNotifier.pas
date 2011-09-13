unit u_OperationNotifier;

interface

uses
  Windows,
  i_JclNotify,
  i_OperationNotifier;

type
  IOperationNotifierInternal = interface
    procedure NextOperation;
  end;

  TOperationNotifier = class(TInterfacedObject, IOperationNotifier, IOperationNotifierInternal)
  private
    FNotifier: IJclNotifier;
    FCurrentOperationID: Integer;
  protected
    procedure NextOperation;
  protected
    function GetCurrentOperation: Integer; stdcall;
    function IsOperationCanceled(AID: Integer): Boolean; stdcall;

    procedure AddListener(AListener: IJclListener); stdcall;
    procedure RemoveListener(AListener: IJclListener); stdcall;
  public
    constructor Create();
  end;

implementation

uses
  u_JclNotify;

{ TOperationCancelNotifier }

constructor TOperationNotifier.Create;
begin
  FNotifier := TJclBaseNotifier.Create;
  FCurrentOperationID := 0;
end;

procedure TOperationNotifier.AddListener(AListener: IJclListener);
begin
  FNotifier.Add(AListener);
end;

function TOperationNotifier.IsOperationCanceled(AID: Integer): Boolean;
begin
  Result := FCurrentOperationID <> AID;
end;

function TOperationNotifier.GetCurrentOperation: Integer;
begin
  Result := FCurrentOperationID;
end;

procedure TOperationNotifier.NextOperation;
begin
  InterlockedIncrement(FCurrentOperationID);
end;

procedure TOperationNotifier.RemoveListener(AListener: IJclListener);
begin
  FNotifier.Remove(AListener);
end;

end.
