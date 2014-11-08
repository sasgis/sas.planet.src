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
    ao_select_rect,
    ao_select_poly,
    ao_select_line
  );

type
  IMainFormState = interface(IChangeable)
    ['{0CB21E1F-BBFC-4517-A328-40F36E6C1457}']
    function GetState: TStateEnum;
    procedure SetState(AValue: TStateEnum);
    property State: TStateEnum read GetState write SetState;
  end;

implementation

end.
