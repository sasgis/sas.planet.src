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

unit i_GeometryLonLatChangeable;

interface

uses
  i_GeometryLonLat,
  i_Changeable;

type
  IGeometryLonLatMultiPointChangeable = interface(IChangeable)
    ['{3F88807D-25D7-4F18-8A0B-943CB224AAEE}']
    function GetStatic: IGeometryLonLatMultiPoint;
  end;

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
