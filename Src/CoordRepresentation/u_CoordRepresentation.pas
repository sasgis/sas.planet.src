{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2021, SAS.Planet development team.                      *}
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

unit u_CoordRepresentation;

interface

uses
  gnugettext,
  t_CoordRepresentation;

type
  TDegrShowFormatCaption = array [TDegrShowFormat] of string;

function GetDegrShowFormatCaption: TDegrShowFormatCaption;

implementation

function GetDegrShowFormatCaption: TDegrShowFormatCaption;
begin
  Result[TDegrShowFormat(0)] := _('WS deg.min.sec. (W12°12''12.1234")');
  Result[TDegrShowFormat(1)] := _('WS deg.min. (W12°12.123456'')');
  Result[TDegrShowFormat(2)] := _('WS deg. (W12.12345678°)');
  Result[TDegrShowFormat(3)] := _('-- deg.min.sec. (-12°12''12.1234")');
  Result[TDegrShowFormat(4)] := _('-- deg.min. (-12°12.1234'')');
  Result[TDegrShowFormat(5)] := _('-- deg. (-12.12345678°)');
end;

end.
