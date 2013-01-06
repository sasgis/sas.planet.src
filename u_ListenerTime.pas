unit u_ListenerTime;

interface

uses
  Classes,
  i_SimpleFlag,
  i_ListenerTime,
  u_ListenerByEvent,
  u_BaseInterfacedObject;

type
  TListenerTTLCheck = class(TBaseInterfacedObject, IListenerTimeWithUsedFlag, IListenerTime)
  private
    FOnTrimByTTL: TNotifyListenerNoMmgEvent;
    FUseFlag: ISimpleFlag;
    FLastUseTime: Cardinal;
    FTTL: Cardinal;
  private
    procedure Notification(const ANow: Cardinal);
    procedure UpdateUseTime;
  public
    constructor Create(
      AOnTrimByTTL: TNotifyListenerNoMmgEvent;
      ATTL: Cardinal
    );
  end;

  TListenerTimeCheck = class(TBaseInterfacedObject, IListenerTime)
  private
    FOnTime: TNotifyListenerNoMmgEvent;
    FCheckInterval: Cardinal;

    FNextTime: Cardinal;
  private
    procedure Notification(const ANow: Cardinal);
  public
    constructor Create(
      AOnTime: TNotifyListenerNoMmgEvent;
      ACheckInterval: Cardinal
    );
  end;

implementation

uses
  u_SimpleFlagWithInterlock;

{ TListenerTTLCheck }

constructor TListenerTTLCheck.Create(
  AOnTrimByTTL: TNotifyListenerNoMmgEvent;
  ATTL: Cardinal
);
begin
  inherited Create;
  FOnTrimByTTL := AOnTrimByTTL;
  FTTL := ATTL;

  FUseFlag := TSimpleFlagWithInterlock.Create;
  FLastUseTime := 0;
end;

procedure TListenerTTLCheck.Notification(const ANow: Cardinal);
var
  VCleanTime: Cardinal;
  VLastUseTime: Cardinal;
begin
  if FUseFlag.CheckFlagAndReset then begin
    FLastUseTime := ANow;
  end else begin
    VLastUseTime := FLastUseTime;
    if VLastUseTime <> 0 then begin
      VCleanTime := VLastUseTime + FTTL;
      if (VCleanTime <= ANow) or ((ANow < 1 shl 29) and (VCleanTime > 1 shl 30)) then begin
        FUpdateFlag.CheckFlagAndReset;
        FOnTrimByTTL;
        FLastUseTime := 0;            
      end;
    end;
  end;
end;

procedure TListenerTTLCheck.UpdateUseTime;
begin
  FUseFlag.SetFlag;
end;

{ TListenerTimeCheck }

constructor TListenerTimeCheck.Create(
  AOnTime: TNotifyListenerNoMmgEvent;
  ACheckInterval: Cardinal
);
begin
  Assert(Assigned(AOnTime));
  Assert(ACheckInterval <= 3600000);
  inherited Create;
  FOnTime := AOnTime;
  FCheckInterval := ACheckInterval;
  if FCheckInterval > 3600000  then begin
    FCheckInterval := 3600000;
  end;

  FNextTime := 0;
end;

procedure TListenerTimeCheck.Notification(const ANow: Cardinal);
var
  VNextTime: Cardinal;
begin
  VNextTime := FNextTime;
  if (VNextTime <= ANow) or ((ANow < 1 shl 29) and (VNextTime > 1 shl 30)) then begin
    if Assigned(FOnTime) then begin
      FOnTime;
    end;
    FNextTime := ANow + FCheckInterval;
  end;
end;

end.
