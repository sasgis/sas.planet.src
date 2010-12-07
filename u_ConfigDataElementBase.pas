unit u_ConfigDataElementBase;

interface

uses
  Windows,
  SysUtils,
  i_JclNotify,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_IConfigDataElement;

type
  TConfigDataElementBase = class(TInterfacedObject, IConfigDataElement)
  private
    FChangeNotifier: IJclNotifier;
    FLock: TMultiReadExclusiveWriteSynchronizer;
    FStopNotifyCounter: Longint;
    FNeedNotify: Longint;
  protected
    procedure SetChanged;
    function CheckIsChangedAndReset: Boolean;
    procedure DoChangeNotify;
    procedure DoReadConfig(AConfigData: IConfigDataProvider); virtual; abstract;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); virtual; abstract;
  protected
    procedure LockRead; virtual;
    procedure LockWrite; virtual;
    procedure UnlockRead; virtual;
    procedure UnlockWrite; virtual;
    procedure ReadConfig(AConfigData: IConfigDataProvider); virtual;
    procedure WriteConfig(AConfigData: IConfigDataWriteProvider); virtual;
    procedure StopNotify; virtual;
    procedure StartNotify; virtual;
    function GetChangeNotifier: IJclNotifier; virtual;
  public
    constructor Create();
    destructor Destroy; override;
  end;

type
  TConfigDataElementBaseEmptySaveLoad = class(TConfigDataElementBase)
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  end;

implementation

uses
  u_JclNotify;

{ TConfigDataElementBase }

constructor TConfigDataElementBase.Create;
begin
  FLock := TMultiReadExclusiveWriteSynchronizer.Create;
  FChangeNotifier := TJclBaseNotifier.Create;
  FStopNotifyCounter := 0;
end;

destructor TConfigDataElementBase.Destroy;
begin
  FreeAndNil(FLock);
  FChangeNotifier := nil;
  inherited;
end;

function TConfigDataElementBase.CheckIsChangedAndReset: Boolean;
begin
  Result := InterlockedExchange(FNeedNotify, 0) <> 0;
end;

procedure TConfigDataElementBase.DoChangeNotify;
begin
  FChangeNotifier.Notify(nil);
end;

function TConfigDataElementBase.GetChangeNotifier: IJclNotifier;
begin
  Result := FChangeNotifier;
end;

procedure TConfigDataElementBase.LockRead;
begin
  FLock.BeginRead;
end;

procedure TConfigDataElementBase.LockWrite;
begin
  StopNotify;
  FLock.BeginWrite;
end;

procedure TConfigDataElementBase.ReadConfig(AConfigData: IConfigDataProvider);
begin
  LockWrite;
  try
    DoReadConfig(AConfigData);
  finally
    UnlockWrite;
  end;
end;

procedure TConfigDataElementBase.SetChanged;
begin
  InterlockedIncrement(FNeedNotify);
end;

procedure TConfigDataElementBase.StartNotify;
var
  VCouner: Longint;
begin
  VCouner := InterlockedDecrement(FStopNotifyCounter);
  if VCouner = 0 then begin
    if CheckIsChangedAndReset then begin
      DoChangeNotify;
    end;
  end;
end;

procedure TConfigDataElementBase.StopNotify;
begin
  InterlockedIncrement(FStopNotifyCounter);
end;

procedure TConfigDataElementBase.UnlockRead;
begin
  FLock.EndRead;
end;

procedure TConfigDataElementBase.UnlockWrite;
begin
  FLock.EndWrite;
  StartNotify;
end;

procedure TConfigDataElementBase.WriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  LockRead;
  try
    DoWriteConfig(AConfigData);
  finally
    UnlockRead;
  end;
end;

{ TConfigDataElementBaseEmptySaveLoad }

procedure TConfigDataElementBaseEmptySaveLoad.DoReadConfig(
  AConfigData: IConfigDataProvider);
begin
end;

procedure TConfigDataElementBaseEmptySaveLoad.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
end;

end.
