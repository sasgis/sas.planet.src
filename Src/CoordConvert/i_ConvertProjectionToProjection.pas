{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit i_ConvertProjectionToProjection;

interface

uses
  Types,
  t_GeoTypes,
  i_Projection;

type
  IConvertProjectionToProjection = interface
    ['{673B639F-B634-4C17-83CD-37F776DF0816}']
    function GetSource: IProjection;
    property Source: IProjection read GetSource;

    function GetTarget: IProjection;
    property Target: IProjection read GetTarget;

    function ForwardPixelPosToPixelPosFloat(
      const APoint: TDoublePoint
    ): TDoublePoint; overload;
    function ForwardPixelPosToPixelPosFloat(
      const APoint: TPoint
    ): TDoublePoint; overload;
    function ForwardPixelRectToPixelRectFloat(
      const APoint: TDoubleRect
    ): TDoubleRect; overload;
    function ForwardPixelRectToPixelRectFloat(
      const APoint: TRect
    ): TDoubleRect; overload;

    function ForwardTilePosToTilePosFloat(
      const APoint: TDoublePoint
    ): TDoublePoint; overload;
    function ForwardTilePosToTilePosFloat(
      const APoint: TPoint
    ): TDoublePoint; overload;
    function ForwardTilePosToTileRectFloat(
      const APoint: TPoint
    ): TDoubleRect;
    function ForwardTilePosToPixeRectFloat(
      const APoint: TPoint
    ): TDoubleRect;
    function ForwardTileRectToTileRectFloat(
      const APoint: TDoubleRect
    ): TDoubleRect; overload;
    function ForwardTileRectToTileRectFloat(
      const APoint: TRect
    ): TDoubleRect; overload;


    function BackwardPixelPosToPixelPosFloat(
      const APoint: TDoublePoint
    ): TDoublePoint; overload;
    function BackwardPixelPosToPixelPosFloat(
      const APoint: TPoint
    ): TDoublePoint; overload;
    function BackwardPixelRectToPixelRectFloat(
      const APoint: TDoubleRect
    ): TDoubleRect; overload;
    function BackwardPixelRectToPixelRectFloat(
      const APoint: TRect
    ): TDoubleRect; overload;

    function BackwardTilePosToTilePosFloat(
      const APoint: TDoublePoint
    ): TDoublePoint; overload;
    function BackwardTilePosToTilePosFloat(
      const APoint: TPoint
    ): TDoublePoint; overload;
    function BackwardTilePosToTileRectFloat(
      const APoint: TPoint
    ): TDoubleRect;
    function BackwardTilePosToPixeRectFloat(
      const APoint: TPoint
    ): TDoubleRect;
    function BackwardTileRectToTileRectFloat(
      const APoint: TDoubleRect
    ): TDoubleRect; overload;
    function BackwardTileRectToTileRectFloat(
      const APoint: TRect
    ): TDoubleRect; overload;
  end;

implementation

end.
