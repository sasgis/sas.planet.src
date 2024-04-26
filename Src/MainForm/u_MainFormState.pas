{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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
    FIsMapMoving: Boolean;
    {$IFDEF ENABLE_STATE_LOGGING}
    procedure LogChanges;
    {$ENDIF}
  private
    { IMainFormState }
    function GetState: TStateEnum;
    procedure SetState(const AValue: TStateEnum);

    function GetIsMapMoving: Boolean;
    procedure SetIsMapMoving(const AValue: Boolean);
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
end;

function TMainFormState.GetIsMapMoving: Boolean;
begin
  Result := FIsMapMoving;
end;

function TMainFormState.GetState: TStateEnum;
begin
  Result := FState;
end;

procedure TMainFormState.SetIsMapMoving(const AValue: Boolean);
begin
  if FIsMapMoving <> AValue then begin
    FIsMapMoving := AValue;
    DoChangeNotify;

    {$IFDEF ENABLE_STATE_LOGGING}
    LogChanges;
    {$ENDIF}
  end;
end;

procedure TMainFormState.SetState(const AValue: TStateEnum);
begin
  if FState <> AValue then begin
    FState := AValue;
    DoChangeNotify;

    {$IFDEF ENABLE_STATE_LOGGING}
    LogChanges;
    {$ENDIF}
  end;
end;

{$IFDEF ENABLE_STATE_LOGGING}
procedure TMainFormState.LogChanges;
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
    'State: ' + cStateId[FState] + '; ' +
    'IsMapMoving: ' + BoolToStr(FIsMapMoving, True)
  ));
end;
{$ENDIF}

end.
