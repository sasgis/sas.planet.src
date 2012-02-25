unit u_TTLCheckListener;

interface

uses
  Windows,
  Classes,
  i_TTLCheckListener;

type
  TTTLCheckListener = class(TInterfacedObject, ITTLCheckListener)
  private
    FOnTrimByTTL: TNotifyEvent;
    FUseCounter: Integer;
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

{ TObjectWithTTLListener }

constructor TTTLCheckListener.Create(
  AOnTrimByTTL: TNotifyEvent;
  ATTL, ACheckInterval: Cardinal
);
begin
  FOnTrimByTTL := AOnTrimByTTL;
  FTTL := ATTL;
  FCheckInterval := ACheckInterval;
  FLastUseTime := 0;
end;

function TTTLCheckListener.CheckTTLAndGetNextCheckTime(
  ANow: Cardinal): Cardinal;
var
  VCleanTime: Cardinal;
  VCounter: Integer;
begin
  VCounter := InterlockedExchange(FUseCounter, 0);
  if VCounter > 0 then begin
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

procedure TTTLCheckListener.UpdateUseTime;
begin
  InterlockedIncrement(FUseCounter);
end;

end.
