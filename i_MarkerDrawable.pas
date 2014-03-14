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

unit i_MarkerDrawable;

interface

uses
  GR32,
  t_GeoTypes,
  i_Changeable;

type
  IMarkerDrawable = interface
    ['{91E8968F-8563-4ED0-8774-AF844F8CA8B9}']
    function GetBoundsForPosition(const APosition: TDoublePoint): TRect;
    function DrawToBitmap(
      ABitmap: TCustomBitmap32;
      const APosition: TDoublePoint
    ): Boolean;
  end;

  IMarkerDrawableChangeable = interface(IChangeable)
    ['{0982D5D9-DE1F-4C35-84DD-E461A332D38C}']
    function GetStatic: IMarkerDrawable;
  end;

  IMarkerDrawableWithDirection = interface
    ['{76C743DE-86B4-4EF2-9451-DB22A90B8628}']
    function DrawToBitmapWithDirection(
      ABitmap: TCustomBitmap32;
      const APosition: TDoublePoint;
      const AAngle: Double
    ): Boolean;
  end;

  IMarkerDrawableWithDirectionChangeable = interface(IChangeable)
    ['{AFFC6BF0-734C-4D86-9057-EC49F4F3045B}']
    function GetStatic: IMarkerDrawableWithDirection;
  end;

implementation

end.
