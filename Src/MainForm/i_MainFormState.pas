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

unit i_MainFormState;

interface

uses
  i_Changeable;

type
  TStateEnum = (
    ao_movemap,
    ao_edit_point,
    ao_edit_line,
    ao_edit_poly,
    ao_calc_line,
    ao_calc_circle,
    ao_select_rect,
    ao_select_poly,
    ao_select_line
  );

  TMapMovingReason = (
    mmrZooming,  // zooming with or without animation
    mmrDragging, // moving without animation
    mmrPanning   // moving with animation
  );

type
  IMainFormState = interface(IChangeable)
    ['{0CB21E1F-BBFC-4517-A328-40F36E6C1457}']
    function GetState: TStateEnum;
    procedure SetState(const AValue: TStateEnum);
    property State: TStateEnum read GetState write SetState;

    procedure MapMovingBegin(const AReason: TMapMovingReason);
    procedure MapMovingEnd(const AReason: TMapMovingReason);
    procedure MapMovingReset;

    function GetIsMapMoving: Boolean;
    property IsMapMoving: Boolean read GetIsMapMoving;

    function GetIsMapZooming: Boolean;
    property IsMapZooming: Boolean read GetIsMapZooming;

    function GetIsMapDragging: Boolean;
    property IsMapDragging: Boolean read GetIsMapDragging;

    function GetIsMapPanning: Boolean;
    property IsMapPanning: Boolean read GetIsMapPanning;
  end;

implementation

end.
