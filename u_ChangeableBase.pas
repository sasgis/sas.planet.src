unit u_ChangeableBase;

interface

uses
  i_JclNotify,
  i_Changeable;

type
  TChangeableBase = class(TInterfacedObject, IChangeable)
  private
    FBeforeChangeNotifier: IJclNotifier;
    FChangeNotifier: IJclNotifier;
    FAfterChangeNotifier: IJclNotifier;
  protected
    function GetBeforeChangeNotifier: IJclNotifier;
    function GetChangeNotifier: IJclNotifier;
    function GetAfterChangeNotifier: IJclNotifier;
  protected
    procedure DoBeforeChangeNotify; virtual;
    procedure DoChangeNotify; virtual;
    procedure DoInChangeNotify; virtual;
    procedure DoAfterChangeNotify; virtual;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  u_JclNotify;

{ TChangeableBase }

constructor TChangeableBase.Create;
begin
  inherited Create;
  FBeforeChangeNotifier := TJclBaseNotifier.Create;
  FChangeNotifier := TJclBaseNotifier.Create;
  FAfterChangeNotifier := TJclBaseNotifier.Create;
end;

destructor TChangeableBase.Destroy;
begin
  FBeforeChangeNotifier := nil;
  FChangeNotifier := nil;
  FAfterChangeNotifier := nil;
  inherited;
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

function TChangeableBase.GetAfterChangeNotifier: IJclNotifier;
begin
  Result := FAfterChangeNotifier;
end;

function TChangeableBase.GetBeforeChangeNotifier: IJclNotifier;
begin
  Result := FBeforeChangeNotifier;
end;

function TChangeableBase.GetChangeNotifier: IJclNotifier;
begin
  Result := FChangeNotifier;
end;

end.
