{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_KeyMovingHandler;

interface

uses
  Windows,
  Classes,
  t_GeoTypes,
  i_ViewPortState,
  i_KeyMovingConfig,
  i_Timer,
  i_NotifierTime,
  i_ListenerTime,
  i_MessageHandler,
  u_BaseInterfacedObject;

type
  TMovmentByDirection = class
  private
    FConfig: IKeyMovingConfig;

    FCurrDirection: Integer;
    FCurrSpeed: Double;
    FLastTick: Int64;
  public
    function GetDelta(
      const ADirection: Integer;
      const AFrequency: Int64;
      const ATick: Int64
    ): Double;
  public
    constructor Create(const AConfig: IKeyMovingConfig);
  end;

  TKeyMovingHandler = class(TBaseInterfacedObject, IMessageHandler)
  private
    FConfig: IKeyMovingConfig;
    FTimer: ITimer;
    FTimerNotifier: INotifierTime;
    FViewPortState: IViewPortState;

    FTimerListener: IListenerTime;

    FVertical: TMovmentByDirection;
    FHorizontal: TMovmentByDirection;
    FKeyboardActive: Boolean;
  private
    procedure OnTime;
    procedure DoMessageEvent(
      var Msg: TMsg;
      var Handled: Boolean
    );
  public
    constructor Create(
      const AViewPortState: IViewPortState;
      const ATimer: ITimer;
      const ATimerNotifier: INotifierTime;
      const AConfig: IKeyMovingConfig
    );
    destructor Destroy; override;
  end;


implementation

uses
  SysUtils,
  Messages,
  u_ListenerTime;

{ TKeyMovingHandler }

constructor TKeyMovingHandler.Create(
  const AViewPortState: IViewPortState;
  const ATimer: ITimer;
  const ATimerNotifier: INotifierTime;
  const AConfig: IKeyMovingConfig
);
begin
  Assert(Assigned(ATimer));
  inherited Create;
  FConfig := AConfig;
  FTimer := ATimer;
  FTimerNotifier := ATimerNotifier;
  FViewPortState := AViewPortState;

  FTimerListener := TListenerTimeCheck.Create(Self.OnTime, 100);
  FTimerNotifier.Add(FTimerListener);

  FHorizontal := TMovmentByDirection.Create(FConfig);
  FVertical := TMovmentByDirection.Create(FConfig);
end;

destructor TKeyMovingHandler.Destroy;
begin
  if Assigned(FTimerNotifier) and Assigned(FTimerListener) then begin
    FTimerNotifier.Remove(FTimerListener);
    FTimerNotifier := nil;
    FTimerListener := nil;
  end;
  FreeAndNil(FVertical);
  FreeAndNil(FHorizontal);
  inherited;
end;

procedure TKeyMovingHandler.DoMessageEvent(
  var Msg: TMsg;
  var Handled: Boolean
);
begin
  case Msg.message of
    WM_KEYFIRST: begin
      case Msg.wParam of
        VK_RIGHT,
        VK_LEFT,
        VK_DOWN,
        VK_UP: begin
          FKeyboardActive := True;
          OnTime;
        end;
      end;
    end;
  end;
end;

procedure TKeyMovingHandler.OnTime;
var
  VKeyboardState: TKeyboardState;
  VCurrTick, VFr: Int64;
  VDirection: TPoint;
  VDelta: TDoublePoint;
begin
  VDirection := Point(0, 0);
  if FKeyboardActive then begin
    if GetKeyboardState(VKeyboardState) then begin
      if
        (VKeyboardState[VK_CONTROL] and $80 = 0) and
        (VKeyboardState[VK_SHIFT] and $80 = 0) and
        (VKeyboardState[VK_MENU] and $80 = 0)
      then begin
        if VKeyboardState[VK_LEFT] and $80 > 0 then begin
          Dec(VDirection.X);
        end;
        if VKeyboardState[VK_RIGHT] and $80 > 0 then begin
          Inc(VDirection.X);
        end;
        if VKeyboardState[VK_UP] and $80 > 0 then begin
          Dec(VDirection.Y);
        end;
        if VKeyboardState[VK_DOWN] and $80 > 0 then begin
          Inc(VDirection.Y);
        end;
      end;
    end;
    if (VDirection.X = 0) and (VDirection.Y = 0) then begin
      FKeyboardActive := False;
    end;
  end;
  VCurrTick := FTimer.CurrentTime;
  VFr := FTimer.Freq;
  VDelta.X := FHorizontal.GetDelta(VDirection.X, VFr, VCurrTick);
  VDelta.Y := FVertical.GetDelta(VDirection.Y, VFr, VCurrTick);
  if (Abs(VDelta.X) >= 1) or (Abs(VDelta.Y) >= 1) then begin
    FViewPortState.ChangeMapPixelByLocalDelta(VDelta);
  end;
end;

{ TMovmentByDirection }

constructor TMovmentByDirection.Create(const AConfig: IKeyMovingConfig);
begin
  inherited Create;
  FConfig := AConfig;
end;

function TMovmentByDirection.GetDelta(
  const ADirection: Integer;
  const AFrequency: Int64;
  const ATick: Int64
): Double;
var
  VTimeDelta: Double;
  VSpeedChange: Double;
  VSpeedLeft: Double;
  VAcceleration: Double;
  VConfig: IKeyMovingConfigStatic;
begin
  VConfig := FConfig.GetStatic;

  Result := 0;
  if (FCurrDirection = 0) and (ADirection = 0) then begin
    // Двигать ничего никуда не нужно
    Exit;
  end;

  if (FCurrDirection <> 0) and (ADirection = 0) then begin
    // Двигались и отпустили кнопку
    if
      ((VConfig.StopTime < 0.1) or (FLastTick = 0) or (VConfig.MaxPixelPerSecond < 1) or
      (AFrequency = 0) or (ATick = 0))
    then begin
      // Если не можем определить время или чудеса с настройками то просто останавливаемся
      FCurrDirection := 0;
      FLastTick := 0;
      FCurrSpeed := 0;
      Exit;
    end else begin
      VTimeDelta := (ATick - FLastTick) / AFrequency;
      if VTimeDelta > 10 then begin
        VTimeDelta := 10;
      end;
      VSpeedChange := VConfig.MaxPixelPerSecond / VConfig.StopTime * VTimeDelta;
      if FCurrSpeed < VSpeedChange then begin
        // Полностью остановились
        Result := FCurrDirection * FCurrSpeed * FCurrSpeed * VConfig.StopTime / VConfig.MaxPixelPerSecond / 2;
        FCurrDirection := 0;
        FLastTick := 0;
        FCurrSpeed := 0;
        Exit;
      end else begin
        // Еще есть инерция
        Result := FCurrDirection * (FCurrSpeed - VSpeedChange / 2) * VTimeDelta;
        FCurrSpeed := FCurrSpeed - VSpeedChange;
        FLastTick := ATick;
        Exit;
      end;
    end;
  end;

  if (FCurrDirection = 0) and (ADirection <> 0) then begin
    // Стояли и начали двигаться
    Result := VConfig.FirstKeyPressDelta * ADirection;
    FCurrDirection := ADirection;
    FLastTick := ATick;
    FCurrSpeed := VConfig.MinPixelPerSecond;
    Exit;
  end;

  if FCurrDirection = ADirection then begin
    // Клавиша движения все еще зажата
    if
      ((FLastTick = 0) or
      (VConfig.MaxPixelPerSecond < 1) or (VConfig.MinPixelPerSecond >= VConfig.MaxPixelPerSecond) or
      (AFrequency = 0) or (ATick = 0) or (ATick < FLastTick))
    then begin
      // Если не можем определить время или чудеса с настройками то не сдвигаем
      Result := 0;
      FLastTick := ATick;
      Exit;
    end else begin
      VTimeDelta := (ATick - FLastTick) / AFrequency;
      if VTimeDelta > 10 then begin
        VTimeDelta := 10;
      end;
      if (VConfig.SpeedChangeTime < 0.1) or (VConfig.MaxPixelPerSecond - VConfig.MinPixelPerSecond < 1) then begin
        // Если никакого ускорения не предусмотрено
        Result := ADirection * VConfig.MaxPixelPerSecond * VTimeDelta;
        FCurrSpeed := VConfig.MaxPixelPerSecond;
        FLastTick := ATick;
        Exit;
      end else begin
        VSpeedLeft := VConfig.MaxPixelPerSecond - FCurrSpeed;
        VAcceleration := (VConfig.MaxPixelPerSecond - VConfig.MinPixelPerSecond) / VConfig.SpeedChangeTime;
        VSpeedChange := VAcceleration * VTimeDelta;
        if (VSpeedLeft < 0) or (VSpeedLeft < VSpeedChange) then begin
          // Достигли максимальной скорости и дальше двигаемся с нею
            // сдвиг за время ускорения
          Result := (VConfig.MaxPixelPerSecond * VConfig.MaxPixelPerSecond - FCurrSpeed * FCurrSpeed) / 2 / VAcceleration;
          VTimeDelta := VTimeDelta - VSpeedLeft / VAcceleration;
          Result := Result + VConfig.MaxPixelPerSecond * VTimeDelta;
          Result := Result * ADirection;
          FCurrSpeed := VConfig.MaxPixelPerSecond;
          FLastTick := ATick;
          Exit;
        end else begin
          // Ускоряемся
          Result := ADirection * (FCurrSpeed + VSpeedChange / 2) * VTimeDelta;
          FCurrSpeed := FCurrSpeed + VSpeedChange;
          FLastTick := ATick;
          Exit;
        end;
      end;
    end;
  end else begin
    if
      ((VConfig.StopTime < 0.1) or (FLastTick = 0) or (VConfig.MaxPixelPerSecond < 1) or
      (AFrequency = 0) or (ATick = 0))
    then begin
      Result := VConfig.FirstKeyPressDelta * ADirection;
      FCurrDirection := ADirection;
      FLastTick := ATick;
      FCurrSpeed := VConfig.MinPixelPerSecond;
      Exit;
    end else begin
      Result := FCurrSpeed * FCurrSpeed * VConfig.StopTime / VConfig.MaxPixelPerSecond / 2;
      Result := VConfig.FirstKeyPressDelta - Result;
      if Result < 0 then begin
        Result := 0;
      end else begin
        Result := ADirection * Result;
      end;
      FCurrDirection := ADirection;
      FLastTick := ATick;
      FCurrSpeed := VConfig.MinPixelPerSecond;
      Exit;
    end;
  end;
end;

end.
