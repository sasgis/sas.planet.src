unit u_ListenerTTLCheck;

interface

uses
  Classes,
  i_SimpleFlag,
  i_ListenerTTLCheck;

type
  TListenerTTLCheck = class(TInterfacedObject, IListenerTTLCheck)
  private
    FOnTrimByTTL: TNotifyEvent;
    FUseFlag: ISimpleFlag;
    FLastUseTime: Cardinal;
    FTTL: Cardinal;
    FCheckInterval: Cardinal;
  protected
    function CheckTTLAndGetNextCheckTime(ANow: Cardinal): Cardinal;
    procedure UpdateUseTime;
  public
    constructor Create(
      AOnTrimByTTL: TNotifyEvent;
      ATTL: Cardinal;
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

end.
