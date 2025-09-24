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
    FMapMovingCount: Integer;
    {$IFDEF ENABLE_STATE_LOGGING}
    procedure LogChanges(const AProcName: string);
    {$ENDIF}
  private
    { IMainFormState }
    function GetState: TStateEnum;
    procedure SetState(const AValue: TStateEnum);

    procedure MapMovingBegin;
    procedure MapMovingEnd;
    procedure MapMovingReset;

    function GetIsMapMoving: Boolean;
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
  FMapMovingCount := 0;
end;

procedure TMainFormState.MapMovingBegin;
begin
  Inc(FMapMovingCount);

  if FMapMovingCount = 1 then begin
    DoChangeNotify;
  end;

  {$IFDEF ENABLE_STATE_LOGGING}
  LogChanges('begin');
  {$ENDIF}
end;

procedure TMainFormState.MapMovingEnd;
begin
  Dec(FMapMovingCount);

  if FMapMovingCount < 0 then begin
    FMapMovingCount := 0;
  end;

  if FMapMovingCount = 0 then begin
    DoChangeNotify;
  end;

  {$IFDEF ENABLE_STATE_LOGGING}
  LogChanges('end');
  {$ENDIF}
end;

procedure TMainFormState.MapMovingReset;
begin
  if FMapMovingCount <> 0 then begin
    FMapMovingCount := 0;
    DoChangeNotify;
  end;

  {$IFDEF ENABLE_STATE_LOGGING}
  LogChanges('reset');
  {$ENDIF}
end;

function TMainFormState.GetIsMapMoving: Boolean;
begin
  Result := FMapMovingCount > 0;
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
    'IsMapMoving: ' + BoolToStr(FMapMovingCount > 0, True) + '; ' +
    'MapMovingCount: ' + IntToStr(FMapMovingCount)
  ));
end;
{$ENDIF}

end.
