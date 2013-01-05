unit u_ListenerTTLCheck;

interface

uses
  Classes,
  i_SimpleFlag,
  i_ListenerTTLCheck,
  u_ListenerByEvent,
  u_BaseInterfacedObject;

type
  TListenerTTLCheck = class(TBaseInterfacedObject, IListenerTTLCheck, IListenerTimeWithUsedFlag, IListenerTime)
  private
    FOnTrimByTTL: TNotifyEvent;
    FUseFlag: ISimpleFlag;
    FLastUseTime: Cardinal;
    FTTL: Cardinal;
    FCheckInterval: Cardinal;
  private
    procedure Notification(const ANow: Cardinal);
    function CheckTTLAndGetNextCheckTime(ANow: Cardinal): Cardinal;
    procedure UpdateUseTime;
  public
    constructor Create(
      AOnTrimByTTL: TNotifyEvent;
      ATTL: Cardinal;
      ACheckInterval: Cardinal
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
  AOnTrimByTTL: TNotifyEvent;
  ATTL, ACheckInterval: Cardinal
);
begin
  inherited Create;
  FOnTrimByTTL := AOnTrimByTTL;
  FTTL := ATTL;
  FCheckInterval := ACheckInterval;

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
        FOnTrimByTTL(nil);
        FLastUseTime := 0;
      end;
    end;
  end;
end;

function TListenerTTLCheck.CheckTTLAndGetNextCheckTime(
  ANow: Cardinal): Cardinal;
var
  VCleanTime: Cardinal;
begin
  if FUseFlag.CheckFlagAndReset then begin
    FLastUseTime := ANow;
  end else begin
    if FLastUseTime <> 0 then begin
      VCleanTime := FLastUseTime + FTTL;
      if (VCleanTime <= ANow) or ((ANow < 1 shl 29) and (VCleanTime > 1 shl 30)) then begin
        FOnTrimByTTL(nil);
        FLastUseTime := 0;
      end;
    end;
  end;
  Result := ANow + FCheckInterval;
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
