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

unit i_NotifierTilePyramidUpdate;

interface

uses
  Types,
  i_Listener,
  i_TileKey,
  i_TileRect,
  i_CoordConverter;

type
  INotifierTilePyramidUpdate = interface
    ['{67415555-955C-4BC7-BC8F-2F9BCDD0F065}']
    function GetGeoCoder: ICoordConverter;
    property GeoCoder: ICoordConverter read GetGeoCoder;

    procedure AddListenerByRect(
      const AListener: IListener;
      const AZoom: Byte;
      const ATileRect: TRect
    );
    procedure AddListenerByZoom(
      const AListener: IListener;
      const AZoom: Byte
    );
    procedure AddListener(
      const AListener: IListener
    );
    procedure Remove(const AListener: IListener);
  end;

  INotifierTilePyramidUpdateInternal = interface
    procedure TileFullUpdateNotify;
    procedure TileUpdateNotify(const ATileKey: ITileKey); overload;
    procedure TileUpdateNotify(const ATile: TPoint; const AZoom: Byte); overload;
    procedure TileRectUpdateNotify(const ATileRect: ITileRect); overload;
    procedure TileRectUpdateNotify(const ATileRect: TRect; const AZoom: Byte); overload;
  end;

implementation

end.
