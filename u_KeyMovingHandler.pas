{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_KeyMovingHandler;

interface

uses
  Windows,
  Classes,
  Forms,
  t_GeoTypes,
  i_ViewPortState,
  i_KeyMovingConfig,
  i_MessageHandler,
  u_BaseInterfacedObject;

type
  TKeyMovingHandler = class(TBaseInterfacedObject, IMessageHandler)
  private
    FConfig: IKeyMovingConfig;
    FViewPortState: IViewPortState;
    FKeyMovingLastTick: Int64;
    FTimeFromFirstToLast: Double;
    FWasSecondKeyPress: Boolean;
    FMapMoveAnimtion: Boolean;
    FMoveVector: TPoint;
  private
    procedure MapMoveAnimate;
    procedure DoMessageEvent(
      var Msg: TMsg;
      var Handled: Boolean
    );
  public
    constructor Create(
      const AViewPortState: IViewPortState;
      const AConfig: IKeyMovingConfig
    );
  end;


implementation

uses
  Messages;

{ TKeyMovingHandler }

constructor TKeyMovingHandler.Create(
  const AViewPortState: IViewPortState;
  const AConfig: IKeyMovingConfig
);
begin
  inherited Create;
  FConfig := AConfig;
  FViewPortState := AViewPortState;
end;

procedure TKeyMovingHandler.DoMessageEvent(
  var Msg: TMsg;
  var Handled: Boolean
);
var
  VMoveByDelta: Boolean;
begin
  case Msg.message of
    WM_KEYFIRST: begin
      VMoveByDelta := False;
      case Msg.wParam of
        VK_RIGHT,
        VK_LEFT,
        VK_DOWN,
        VK_UP: begin
          VMoveByDelta := True;
        end;
      end;
      if VMoveByDelta then begin
        case Msg.wParam of
          VK_RIGHT: begin
            FMoveVector.x := 1;
          end;
          VK_LEFT: begin
            FMoveVector.x := -1;
          end;
          VK_DOWN: begin
            FMoveVector.y := 1;
          end;
          VK_UP: begin
            FMoveVector.y := -1;
          end;
        end;
        MapMoveAnimate;
      end;
    end;
    WM_KEYUP: begin
      case Msg.wParam of
        VK_RIGHT: begin
          FMoveVector.x := 0;
        end;
        VK_LEFT: begin
          FMoveVector.x := 0;
        end;
        VK_DOWN: begin
          FMoveVector.y := 0;
        end;
        VK_UP: begin
          FMoveVector.y := 0;
        end;
      end;
    end;
  end;

end;

procedure TKeyMovingHandler.MapMoveAnimate;
var
  VPointDelta: TDoublePoint;
  VCurrTick, VFr: Int64;
  VTimeFromLast: Double;
  VDrawTimeFromLast: Double;
  VStep: Double;
  VStartSpeed: Double;
  VAcelerateTime: Double;
  VMaxSpeed: Double;
  VAcelerate: Double;
  VAllKeyUp: Boolean;
  VZoom: byte;
begin
  if not (FMapMoveAnimtion) then begin
    FMapMoveAnimtion := True;
    try
      QueryPerformanceCounter(VCurrTick);
      QueryPerformanceFrequency(VFr);
      FWasSecondKeyPress := True;
      FKeyMovingLastTick := VCurrTick;
      FTimeFromFirstToLast := 0;
      VTimeFromLast := 0;
      VZoom := FViewPortState.GetCurrentZoom;
      FConfig.LockRead;
      try
        VStartSpeed := FConfig.MinPixelPerSecond;
        VMaxSpeed := FConfig.MaxPixelPerSecond;
        VAcelerateTime := FConfig.SpeedChangeTime;
      finally
        FConfig.UnlockRead;
      end;

      repeat
        VDrawTimeFromLast := (VCurrTick - FKeyMovingLastTick) / VFr;
        VTimeFromLast := VTimeFromLast + 0.3 * (VDrawTimeFromLast - VTimeFromLast);
        if (FTimeFromFirstToLast >= VAcelerateTime) or (VAcelerateTime < 0.01) then begin
          VStep := VMaxSpeed * VTimeFromLast;
        end else begin
          VAcelerate := (VMaxSpeed - VStartSpeed) / VAcelerateTime;
          VStep := (VStartSpeed + VAcelerate * (FTimeFromFirstToLast + VTimeFromLast / 2)) * VTimeFromLast;
        end;
        FKeyMovingLastTick := VCurrTick;
        FTimeFromFirstToLast := FTimeFromFirstToLast + VTimeFromLast;

        VPointDelta.x := FMoveVector.x * VStep;
        VPointDelta.y := FMoveVector.y * VStep;

        FViewPortState.ChangeMapPixelByDelta(VPointDelta);

        application.ProcessMessages;
        QueryPerformanceCounter(VCurrTick);
        QueryPerformanceFrequency(VFr);

        VAllKeyUp := (GetAsyncKeyState(VK_RIGHT) = 0) and
          (GetAsyncKeyState(VK_LEFT) = 0) and
          (GetAsyncKeyState(VK_DOWN) = 0) and
          (GetAsyncKeyState(VK_UP) = 0);
        if VAllKeyUp then begin
          FMoveVector := Point(0, 0);
        end;
      until ((FMoveVector.x = 0) and (FMoveVector.y = 0)) or (VZoom <> FViewPortState.GetCurrentZoom);
    finally
      FMapMoveAnimtion := false;
    end;
  end;
end;

end.
