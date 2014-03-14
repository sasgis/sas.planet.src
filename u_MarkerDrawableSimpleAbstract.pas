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

unit u_MarkerDrawableSimpleAbstract;

interface

uses
  GR32,
  t_GeoTypes,
  i_MarkerDrawable,
  i_MarkerSimpleConfig,
  u_BaseInterfacedObject;

type
  TMarkerDrawableSimpleBaseAbstract = class(TBaseInterfacedObject)
  private
    FConfig: IMarkerSimpleConfigStatic;
  protected
    property Config: IMarkerSimpleConfigStatic read FConfig;
  public
    constructor Create(const AConfig: IMarkerSimpleConfigStatic); virtual;
  end;

  TMarkerDrawableSimpleAbstract = class(TMarkerDrawableSimpleBaseAbstract, IMarkerDrawable)
  protected
    function GetBoundsForPosition(const APosition: TDoublePoint): TRect; virtual; abstract;
    function DrawToBitmap(
      ABitmap: TCustomBitmap32;
      const APosition: TDoublePoint
    ): Boolean; virtual; abstract;
  end;
  TMarkerDrawableSimpleAbstractClass = class of TMarkerDrawableSimpleAbstract;

  TMarkerDrawableWithDirectionSimpleAbstract = class(TMarkerDrawableSimpleBaseAbstract, IMarkerDrawableWithDirection)
  protected
    function DrawToBitmapWithDirection(
      ABitmap: TCustomBitmap32;
      const APosition: TDoublePoint;
      const AAngle: Double
    ): Boolean; virtual; abstract;
  end;
  TMarkerDrawableWithDirectionSimpleAbstractClass = class of TMarkerDrawableWithDirectionSimpleAbstract;


implementation

{ TMarkerDrawableSimpleBaseAbstract }

constructor TMarkerDrawableSimpleBaseAbstract.Create(
  const AConfig: IMarkerSimpleConfigStatic);
begin
  inherited Create;
  FConfig := AConfig;
end;

end.

