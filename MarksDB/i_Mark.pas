{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit i_Mark;

interface

uses
  t_GeoTypes,
  i_VectorItemLonLat,
  i_VectorDataItemSimple;

type
  IMark = interface(IVectorDataItemSimple)
    ['{52794019-3681-4C92-B50F-0853D5B070DE}']
  end;

  IMarkPoint = interface(IMark)
    ['{6E8C2BA9-4A1A-49A8-98FF-8F5BFCBDB00C}']
    function GetPoint: TDoublePoint;
    property Point: TDoublePoint read GetPoint;
  end;

  IMarkLine = interface(IMark)
    ['{3C400B96-90E1-4ADD-9AA2-56199AC1910F}']
    function GetLine: ILonLatPath;
    property Line: ILonLatPath read GetLine;
  end;

  IMarkPoly = interface(IMark)
    ['{5C66FCE6-F235-4E34-B32A-AB1DD5F0C5B1}']
    function GetLine: ILonLatPolygon;
    property Line: ILonLatPolygon read GetLine;
  end;

implementation

end.
