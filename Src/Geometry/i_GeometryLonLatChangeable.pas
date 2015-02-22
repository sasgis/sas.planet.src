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

unit i_GeometryLonLatChangeable;

interface

uses
  i_GeometryLonLat,
  i_Changeable;

type
  IGeometryLonLatLineChangeable = interface(IChangeable)
    ['{34134BAA-FF88-4616-89E7-6402F8572ACF}']
    function GetStatic: IGeometryLonLatLine;
  end;

  IGeometryLonLatPolygonChangeable = interface(IChangeable)
    ['{FAECBA49-B85E-49CD-B8F4-2890D9B7EA57}']
    function GetStatic: IGeometryLonLatPolygon;
  end;

implementation

end.
