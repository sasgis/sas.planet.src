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

unit i_BitmapMarker;

interface

uses
  t_GeoTypes,
  i_Notifier,
  i_Bitmap32Static;

type
  IBitmapMarker = interface(IBitmap32Static)
    ['{03AB4233-EEEA-4AD6-A194-EFD32345056D}']
    function GetAnchorPoint: TDoublePoint;
    property AnchorPoint: TDoublePoint read GetAnchorPoint;
  end;

  IBitmapMarkerWithDirection = interface(IBitmapMarker)
    ['{A27674DB-F074-4E54-8BBA-DF29972191BF}']
    function GetDirection: Double;
    property Direction: Double read GetDirection;
  end;

  IBitmapMarkerChangeable = interface
    ['{0FD0F8A2-CB30-42A0-8622-4E8F2A37C2D6}']
    function GetStatic: IBitmapMarker;

    function GetChangeNotifier: INotifier;
    property ChangeNotifier: INotifier read GetChangeNotifier;
  end;

implementation

end.
