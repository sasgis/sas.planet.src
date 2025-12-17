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

unit u_MainFormState;

interface

{$IFDEF DEBUG}
  {.$DEFINE ENABLE_STATE_LOGGING}
{$ENDIF}

uses
  i_MainFormState,
  u_ChangeableBase;

type
  TMainFormState = class(TChangeableBase, IMainFormState)
  private
    FState: TStateEnum;
    FCount: array [TMapMovingReason] of Integer;
    {$IFDEF ENABLE_STATE_LOGGING}
    procedure LogChanges(const AProcName: string);
    {$ENDIF}
  private
    { IMainFormState }
    function GetState: TStateEnum;
    procedure SetState(const AValue: TStateEnum);

    procedure MapMovingBegin(const AReason: TMapMovingReason);
    procedure MapMovingEnd(const AReason: TMapMovingReason);
    procedure MapMovingReset;

    function GetIsMapMoving: Boolean;

    function GetIsMapZooming: Boolean;
    function GetIsMapDragging: Boolean;
    function GetIsMapPanning: Boolean;
  public
    constructor Create;
  end;

implementation

uses
  {$IFDEF ENABLE_STATE_LOGGING}
  Windows,
  SysUtils,
  {$ENDIF}
  u_Synchronizer;

{ TMainFormState }

constructor TMainFormState.Create;
begin
  inherited Create(GSync.SyncVariable.Make(Self.ClassName + 'Notifiers'));
  FState := ao_movemap;
  FillChar(FCount[Low(TMapMovingReason)], Length(FCount) * SizeOf(Integer), 0);
end;

procedure TMainFormState.MapMovingBegin(const AReason: TMapMovingReason);
begin
  Inc(FCount[AReason]);

  if FCount[AReason] = 1 then begin
    DoChangeNotify;
  end;

  {$IFDEF ENABLE_STATE_LOGGING}
  LogChanges('begin');
  {$ENDIF}
end;

procedure TMainFormState.MapMovingEnd(const AReason: TMapMovingReason);
begin
  Dec(FCount[AReason]);

  if FCount[AReason] < 0 then begin
    FCount[AReason] := 0;
  end;

  if FCount[AReason] = 0 then begin
    DoChangeNotify;
  end;

  {$IFDEF ENABLE_STATE_LOGGING}
  LogChanges('end');
  {$ENDIF}
end;

procedure TMainFormState.MapMovingReset;
var
  I: TMapMovingReason;
  VDoNotify: Boolean;
begin
  VDoNotify := False;

  for I := Low(TMapMovingReason) to High(TMapMovingReason) do begin
    if FCount[I] <> 0 then begin
      FCount[I] := 0;
      VDoNotify := True;
    end;
  end;

  if VDoNotify then begin
    DoChangeNotify;
  end;

  {$IFDEF ENABLE_STATE_LOGGING}
  LogChanges('reset');
  {$ENDIF}
end;

function TMainFormState.GetIsMapMoving: Boolean;
var
  I: TMapMovingReason;
begin
  for I := Low(TMapMovingReason) to High(TMapMovingReason) do begin
    if FCount[I] <> 0 then begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function TMainFormState.GetIsMapZooming: Boolean;
begin
  Result := FCount[mmrZooming] > 0;
end;

function TMainFormState.GetIsMapDragging: Boolean;
begin
  Result := FCount[mmrDragging] > 0;
end;

function TMainFormState.GetIsMapPanning: Boolean;
begin
  Result := FCount[mmrPanning] > 0;
end;

function TMainFormState.GetState: TStateEnum;
begin
  Result := FState;
end;

procedure TMainFormState.SetState(const AValue: TStateEnum);
begin
  if FState <> AValue then begin
    FState := AValue;
    DoChangeNotify;
  end;

  {$IFDEF ENABLE_STATE_LOGGING}
  LogChanges('state');
  {$ENDIF}
end;

{$IFDEF ENABLE_STATE_LOGGING}
procedure TMainFormState.LogChanges(const AProcName: string);
const
  cStateId: array [TStateEnum] of string = (
    'ao_movemap',
    'ao_edit_point',
    'ao_edit_line',
    'ao_edit_poly',
    'ao_calc_line',
    'ao_calc_circle',
    'ao_select_rect',
    'ao_select_poly',
    'ao_select_line'
  );
begin
  OutputDebugString(PChar(
    '[' + AProcName + '] ' +
    'State: ' + cStateId[FState] + '; ' +
    'IsMapMoving: ' + BoolToStr(GetIsMapMoving, True) + ' (' +
    'Zooming: ' + IntToStr(FCount[mmrZooming]) + '; ' +
    'Dragging: ' + IntToStr(FCount[mmrDragging]) + '; ' +
    'Panning: ' + IntToStr(FCount[mmrPanning]) + ')'
  ));
end;
{$ENDIF}

end.
