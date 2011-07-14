unit u_KeyMovingHandler;

interface

uses
  Windows,
  GR32_Image,
  i_ViewPortState,
  i_KeyMovingConfig,
  i_MessageHandler;

type
  TKeyMovingHandler = class(TInterfacedObject, IMessageHandler)
  private
    FConfig: IKeyMovingConfig;
    FMap: TImage32;
    FViewPortState: IViewPortState;
    FKeyMovingLastTick: Int64;
    FTimeFromFirstToLast: Double;
    FWasSecondKeyPress: Boolean;
  protected
    procedure DoMessageEvent(var Msg: TMsg; var Handled: Boolean);
  public
    constructor Create(
      AMap: TImage32;
      AViewPortState: IViewPortState;
      AConfig: IKeyMovingConfig
    );
  end;


implementation

uses
  Messages,
  t_GeoTypes,
  u_GeoFun;

{ TKeyMovingHandler }

constructor TKeyMovingHandler.Create(AMap: TImage32;
  AViewPortState: IViewPortState; AConfig: IKeyMovingConfig);
begin
  FConfig := AConfig;
  FMap := AMap;
  FViewPortState := AViewPortState;
end;

procedure TKeyMovingHandler.DoMessageEvent(var Msg: TMsg; var Handled: Boolean);
var
  VMoveByDelta: Boolean;
  VPointDelta: TDoublePoint;
  VCurrTick, VFr: Int64;
  VTimeFromLast: Double;
  VStep: Double;
  VStartSpeed: Double;
  VAcelerateTime: Double;
  VMaxSpeed: Double;
  VAcelerate: Double;
begin
  case Msg.message of
    WM_KEYFIRST: begin
      VMoveByDelta := False;
      case Msg.wParam of
        VK_RIGHT,
        VK_LEFT,
        VK_DOWN,
        VK_UP: VMoveByDelta := True;
      end;
      if VMoveByDelta then begin
        Handled := True;
        QueryPerformanceCounter(VCurrTick);
        QueryPerformanceFrequency(VFr);
        if FKeyMovingLastTick = 0 then begin
          FKeyMovingLastTick := VCurrTick;
          FWasSecondKeyPress := False;
          VStep := FConfig.FirstKeyPressDelta;
        end else begin
          if not FWasSecondKeyPress then begin
            FWasSecondKeyPress := True;
            FKeyMovingLastTick := VCurrTick;
            FTimeFromFirstToLast := 0;
            VStep := 0;
          end else begin
            FConfig.LockRead;
            try
              VStartSpeed := FConfig.MinPixelPerSecond;
              VMaxSpeed := FConfig.MaxPixelPerSecond;
              VAcelerateTime := FConfig.SpeedChangeTime;
            finally
              FConfig.UnlockRead;
            end;
            VTimeFromLast := (VCurrTick - FKeyMovingLastTick) / VFr;
            if (FTimeFromFirstToLast >= VAcelerateTime) or (VAcelerateTime < 0.01) then begin
              VStep := VMaxSpeed * VTimeFromLast;
            end else begin
              VAcelerate := (VMaxSpeed - VStartSpeed) / VAcelerateTime;
              VStep := (VStartSpeed + VAcelerate * (FTimeFromFirstToLast + VTimeFromLast/2)) * VTimeFromLast;
            end;
            FKeyMovingLastTick := VCurrTick;
            FTimeFromFirstToLast := FTimeFromFirstToLast + VTimeFromLast;
          end;
        end;
        case Msg.wParam of
          VK_RIGHT: VPointDelta := DoublePoint(VStep, 0);
          VK_LEFT: VPointDelta := DoublePoint(-VStep, 0);
          VK_DOWN: VPointDelta := DoublePoint(0, VStep);
          VK_UP: VPointDelta := DoublePoint(0, -VStep);
        else
          VPointDelta := DoublePoint(0, 0);
        end;
        FMap.BeginUpdate;
        try
          FViewPortState.ChangeMapPixelByDelta(VPointDelta);
        finally
          FMap.EndUpdate;
          FMap.Changed;
        end;
      end;
    end;
    WM_KEYUP: begin
      FKeyMovingLastTick := 0;
      FTimeFromFirstToLast := 0;
    end;
  end;

end;

end.
