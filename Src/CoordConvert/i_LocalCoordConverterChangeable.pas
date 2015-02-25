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

unit i_LocalCoordConverterChangeable;

interface

uses
  i_Changeable,
  i_LocalCoordConverter;

type
  ILocalCoordConverterChangeable = interface(IChangeable)
    ['{ED080B29-6C15-4C8E-9233-522F651004E8}']
    function GetStatic: ILocalCoordConverter;
  end;

  ILocalCoordConverterChangeableInternal = interface(ILocalCoordConverterChangeable)
    procedure SetConverter(const AValue: ILocalCoordConverter);
  end;

implementation

end.
