unit u_ChangeableBase;

interface

uses
  i_Notifier,
  i_Changeable,
  u_BaseInterfacedObject;

type
  TChangeableBase = class(TBaseInterfacedObject, IChangeable)
  private
    FBeforeChangeNotifier: INotifierInternal;
    FChangeNotifier: INotifierInternal;
    FAfterChangeNotifier: INotifierInternal;
  protected
    function GetBeforeChangeNotifier: INotifier;
    function GetChangeNotifier: INotifier;
    function GetAfterChangeNotifier: INotifier;
  protected
    procedure DoBeforeChangeNotify; virtual;
    procedure DoInChangeNotify; virtual;
    procedure DoAfterChangeNotify; virtual;

    procedure DoChangeNotify; virtual;
  public
    constructor Create;
  end;

implementation

uses
  u_Notifier;

{ TChangeableBase }

constructor TChangeableBase.Create;
begin
  inherited Create;
  FBeforeChangeNotifier := TNotifierBase.Create;
  FChangeNotifier := TNotifierBase.Create;
  FAfterChangeNotifier := TNotifierBase.Create;
end;

procedure TChangeableBase.DoAfterChangeNotify;
begin
  FAfterChangeNotifier.Notify(nil);
end;

procedure TChangeableBase.DoBeforeChangeNotify;
begin
  FBeforeChangeNotifier.Notify(nil);
end;

procedure TChangeableBase.DoChangeNotify;
begin
  DoBeforeChangeNotify;
  try
    DoInChangeNotify;
  finally
    DoAfterChangeNotify;
  end;
end;

procedure TChangeableBase.DoInChangeNotify;
begin
  FChangeNotifier.Notify(nil);
end;

function TChangeableBase.GetAfterChangeNotifier: INotifier;
begin
  Result := FAfterChangeNotifier;
end;

function TChangeableBase.GetBeforeChangeNotifier: INotifier;
begin
  Result := FBeforeChangeNotifier;
end;

function TChangeableBase.GetChangeNotifier: INotifier;
begin
  Result := FChangeNotifier;
end;

end.
