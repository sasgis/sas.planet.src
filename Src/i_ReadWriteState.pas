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

unit i_ReadWriteState;

interface
uses
  t_CommonTypes,
  i_Changeable;

type
  IReadWriteStateStatic = interface
    ['{E6BE17B1-BF3C-4313-B733-7B66A63EF684}']
    function GetReadAccess: TAccesState;
    property ReadAccess: TAccesState read GetReadAccess;

    function GetWriteAccess: TAccesState;
    property WriteAccess: TAccesState read GetWriteAccess;
  end;

  IReadWriteStateChangeble = interface(IChangeable)
    ['{6202AB73-00A2-4711-87F4-D195CEFD9C3F}']
    function GetStatic: IReadWriteStateStatic;
  end;

implementation

end.
