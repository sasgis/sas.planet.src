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

unit i_VectorTileProvider;

interface

uses
  Types,
  i_ProjectionInfo,
  i_VectorItemSubset;

type
  IVectorTileProvider = interface
    ['{00ADB9F4-D421-4F71-A9B6-3F8A6E8FFCB9}']
    function GetProjectionInfo: IProjectionInfo;
    property ProjectionInfo: IProjectionInfo read GetProjectionInfo;

    function GetTile(
      const ATile: TPoint
    ): IVectorItemSubset;
  end;

implementation

end.
