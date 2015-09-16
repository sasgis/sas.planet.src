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
  i_NotifierOperation,
  i_ProjectionInfo,
  i_VectorItemSubset;

type
  IVectorTileProvider = interface
    ['{00ADB9F4-D421-4F71-A9B6-3F8A6E8FFCB9}']
    function GetProjectionInfo: IProjection;
    property ProjectionInfo: IProjection read GetProjectionInfo;

    function GetTile(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ATile: TPoint
    ): IVectorItemSubset;
  end;

  IVectorTileUniProvider = interface
    ['{6C027AA3-6DA2-4475-8179-5F80170165AD}']
    function GetTile(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AProjectionInfo: IProjection;
      const ATile: TPoint
    ): IVectorItemSubset;
  end;

implementation

end.
