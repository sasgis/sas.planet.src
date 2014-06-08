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

unit i_VectorDataFactory;

interface

uses
  i_Appearance,
  i_GeometryLonLat,
  i_VectorDataItemSimple;

type
  IVectorDataItemMainInfoFactory = interface
    ['{6A046EF1-D444-4996-AC8F-4645EC01FA68}']
    function BuildMainInfo(
      const AIdData: Pointer;
      const AName: string;
      const ADesc: string
    ): IVectorDataItemMainInfo;
  end;

  IVectorDataFactory = interface
    ['{F90BAFE2-B3A5-4C6B-9831-3E460F7771F6}']
    function BuildItem(
      const AMainInfo: IVectorDataItemMainInfo;
      const AAppearance: IAppearance;
      const AGeometry: IGeometryLonLat
    ): IVectorDataItemSimple;
  end;

implementation

end.
