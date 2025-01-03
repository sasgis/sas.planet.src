{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_MouseState;

interface

uses
  Windows,
  Types,
  SysUtils,
  Classes,
  Controls,
  t_GeoTypes,
  i_MouseState,
  i_MouseHandler,
  i_Timer,
  u_BaseInterfacedObject;

type
  TMouseState = class(TBaseInterfacedObject, IMouseState, IMouseHandler)
  private const
    CPrevSpeedCount = 8;
  private
    FLock: IReadWriteSync;
    FTimer: ITimer;

    FMinTime: Double;
    FMaxTime: Double;
    FUsedTime: Double;

    FPrevSpeedUsed: Integer;
    FPrevPoints: array[0..CPrevSpeedCount - 1] of TPoint;
    FPrevTime: array[0..CPrevSpeedCount - 1] of Double;

    FCurentPos: TPoint;
    FCurentTime: TLargeInteger;
    FCurrentSpeed: TDoublePoint;
    FCurrentShift: TShiftState;

    FLastDownPos: array [TMouseButton] of TPoint;
    FLastDownShift: array [TMouseButton] of TShiftState;

    FLastUpPos: array [TMouseButton] of TPoint;
    FLastUpShift: array [TMouseButton] of TShiftState;

    procedure SetCurrentPos(
      const APosition: TPoint
    );
  private
    { IMouseState }
    function GetCurentPos: TPoint;
    function GetCurentSpeed: TDoublePoint;
    function GetCurrentShift: TShiftState;

    function GetLastDownShift(AButton: TMouseButton): TShiftState;
    function GetLastDownPos(AButton: TMouseButton): TPoint;
    function GetLastUpShift(AButton: TMouseButton): TShiftState;
    function GetLastUpPos(AButton: TMouseButton): TPoint;
  private
    { IMouseHandler }
    procedure OnMouseMove(
      AShift: TShiftState;
      const APosition: TPoint
    );
    procedure OnMouseDown(
      AButton: TMouseButton;
      AShift: TShiftState;
      const APosition: TPoint
    );
    procedure OnMouseUp(
      AButton: TMouseButton;
      AShift: TShiftState;
      const APosition: TPoint
    );
  public
    constructor Create(
      const ATimer: ITimer
    );
  end;

implementation

uses
  u_Synchronizer;

{ TMouseState }

constructor TMouseState.Create(
  const ATimer: ITimer
);
begin
  Assert(Assigned(ATimer));
  inherited Create;
  FTimer := ATimer;
  FLock := GSync.SyncVariable.Make(Self.ClassName);
  FCurentTime := 0;
  FMinTime := 0.001;
  FMaxTime := 3;
  FUsedTime := 0.1;
  FPrevSpeedUsed := 0;
end;

function TMouseState.GetCurentPos: TPoint;
begin
  FLock.BeginRead;
  try
    Result := FCurentPos;
  finally
    FLock.EndRead;
  end;
end;

function TMouseState.GetCurentSpeed: TDoublePoint;
begin
  FLock.BeginRead;
  try
    Result := FCurrentSpeed;
  finally
    FLock.EndRead;
  end;
end;

function TMouseState.GetCurrentShift: TShiftState;
begin
  FLock.BeginRead;
  try
    Result := FCurrentShift;
  finally
    FLock.EndRead;
  end;
end;

function TMouseState.GetLastDownPos(AButton: TMouseButton): TPoint;
begin
  FLock.BeginRead;
  try
    Result := FLastDownPos[AButton];
  finally
    FLock.EndRead;
  end;
end;

function TMouseState.GetLastDownShift(AButton: TMouseButton): TShiftState;
begin
  FLock.BeginRead;
  try
    Result := FLastDownShift[AButton];
  finally
    FLock.EndRead;
  end;
end;

function TMouseState.GetLastUpPos(AButton: TMouseButton): TPoint;
begin
  FLock.BeginRead;
  try
    Result := FLastUpPos[AButton];
  finally
    FLock.EndRead;
  end;
end;

function TMouseState.GetLastUpShift(AButton: TMouseButton): TShiftState;
begin
  FLock.BeginRead;
  try
    Result := FLastUpShift[AButton];
  finally
    FLock.EndRead;
  end;
end;

procedure TMouseState.OnMouseDown(
  AButton: TMouseButton;
  AShift: TShiftState;
  const APosition: TPoint
);
begin
  FLock.BeginWrite;
  try
    SetCurrentPos(APosition);
    FCurrentShift := AShift;

    FLastDownPos[AButton] := APosition;
    FLastDownShift[AButton] := AShift;

    FLastUpPos[AButton] := APosition;
    FLastUpShift[AButton] := AShift;
  finally
    FLock.EndWrite;
  end;
end;

procedure TMouseState.OnMouseMove(
  AShift: TShiftState;
  const APosition: TPoint
);
begin
  FLock.BeginWrite;
  try
    SetCurrentPos(APosition);
    FCurrentShift := AShift;
  finally
    FLock.EndWrite;
  end;
end;

procedure TMouseState.OnMouseUp(
  AButton: TMouseButton;
  AShift: TShiftState;
  const APosition: TPoint
);
begin
  FLock.BeginWrite;
  try
    SetCurrentPos(APosition);
    FCurrentShift := AShift;

    FLastUpPos[AButton] := APosition;
    FLastUpShift[AButton] := AShift;
  finally
    FLock.EndWrite;
  end;
end;

procedure TMouseState.SetCurrentPos(const APosition: TPoint);
var
  I: Integer;
  VCurrTime: TLargeInteger;
  VFrequency: TLargeInteger;
  VTimeFromLastMove: Double;
  VCurrentDelta: TPoint;
  VCurrentSpeed: TDoublePoint;
  VNotUsedTime: Double;
  VUsedTime: Double;
  VAvgDelta: TDoublePoint;
  VAvgSpeed: TDoublePoint;
  VFirstPoint: TPoint;
  VSecondPoint: TPoint;
begin
  VCurrTime := FTimer.CurrentTime;
  VFrequency := FTimer.Freq;
  if FCurentTime <> 0 then begin
    VTimeFromLastMove := (VCurrTime - FCurentTime) / VFrequency;
    if VTimeFromLastMove < FMaxTime then begin
      if VTimeFromLastMove > FMinTime then begin
        VCurrentDelta.X := FCurentPos.X - APosition.X;
        VCurrentDelta.Y := FCurentPos.Y - APosition.Y;

        VCurrentSpeed.X := VCurrentDelta.X / VTimeFromLastMove;
        VCurrentSpeed.Y := VCurrentDelta.Y / VTimeFromLastMove;

        VNotUsedTime := FUsedTime - VTimeFromLastMove;
        if (VNotUsedTime > 0) and (FPrevSpeedUsed > 0) then begin
          VAvgDelta.X := VCurrentDelta.X;
          VAvgDelta.Y := VCurrentDelta.Y;
          I := 0;
          while ((I < FPrevSpeedUsed) and (FPrevTime[I] < VNotUsedTime)) do begin
            VNotUsedTime := VNotUsedTime - FPrevTime[I];
            Inc(I);
          end;
          if (I < FPrevSpeedUsed) then begin
            if (VNotUsedTime > 0) then begin
              VFirstPoint := FPrevPoints[I];
              if I = 0 then begin
                VSecondPoint := FCurentPos;
              end else begin
                VSecondPoint := FPrevPoints[I - 1];
              end;
              VAvgDelta.X := VSecondPoint.X - (VSecondPoint.X - VFirstPoint.X) * VNotUsedTime / FPrevTime[I] - APosition.X;
              VAvgDelta.Y := VSecondPoint.Y - (VSecondPoint.Y - VFirstPoint.Y) * VNotUsedTime / FPrevTime[I] - APosition.Y;
              VNotUsedTime := 0;
            end else begin
              VAvgDelta.X := FPrevPoints[I].X - APosition.X;
              VAvgDelta.Y := FPrevPoints[I].Y - APosition.Y;
            end;
          end else begin
            VAvgDelta.X := FPrevPoints[I - 1].X - APosition.X;
            VAvgDelta.Y := FPrevPoints[I - 1].Y - APosition.Y;
          end;
          VUsedTime := FUsedTime - VNotUsedTime;
          VAvgSpeed.X := VAvgDelta.X / VUsedTime;
          VAvgSpeed.Y := VAvgDelta.Y / VUsedTime;
        end else begin
          VAvgSpeed := VCurrentSpeed;
        end;
        FCurrentSpeed := VAvgSpeed;
        if FPrevSpeedUsed < CPrevSpeedCount then begin
          Inc(FPrevSpeedUsed);
        end;
        for I := FPrevSpeedUsed - 1 downto 1 do begin
          FPrevPoints[I] := FPrevPoints[I - 1];
          FPrevTime[I] := FPrevTime[I - 1];
        end;
        FPrevPoints[0] := FCurentPos;
        FPrevTime[0] := VTimeFromLastMove;
      end;
    end else begin
      FCurrentSpeed.X := 0;
      FCurrentSpeed.Y := 0;
      FPrevSpeedUsed := 0;
    end;
  end else begin
    FCurrentSpeed.X := 0;
    FCurrentSpeed.Y := 0;
  end;

  FCurentPos := APosition;
  FCurentTime := VCurrTime;
end;

end.
