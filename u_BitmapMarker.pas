{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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

unit u_BitmapMarker;

interface

uses
  Types,
  GR32,
  t_GeoTypes,
  i_Bitmap32Static,
  i_BitmapMarker;

type
  TBitmapMarker = class(TInterfacedObject, IBitmapMarker, IBitmap32Static)
  private
    FBitmap: IBitmap32Static;
    FBitmapSize: TPoint;
    FAnchorPoint: TDoublePoint;
  protected
    function GetBitmapSize: TPoint;
    function GetBitmap: TCustomBitmap32;
    function GetAnchorPoint: TDoublePoint;
  public
    constructor Create(
      ABitmap: IBitmap32Static;
      AAnchorPoint: TDoublePoint
    );
  end;

  TBitmapMarkerWithDirection = class(TBitmapMarker, IBitmapMarkerWithDirection)
  private
    FDirection: Double;
  protected
    function GetDirection: Double;
  public
    constructor Create(
      ABitmap: IBitmap32Static;
      AAnchorPoint: TDoublePoint;
      ADirection: Double
    );
  end;


implementation

uses
  SysUtils;

{ TBitmapMarker }

constructor TBitmapMarker.Create(
  ABitmap: IBitmap32Static;
  AAnchorPoint: TDoublePoint
);
var
  VBitmap: TCustomBitmap32;
begin
  FAnchorPoint := AAnchorPoint;
  FBitmap := ABitmap;
  VBitmap := FBitmap.Bitmap;
  VBitmap.DrawMode := dmBlend;
  VBitmap.CombineMode := cmBlend;
  FBitmapSize := Point(VBitmap.Width, VBitmap.Height);
end;

function TBitmapMarker.GetAnchorPoint: TDoublePoint;
begin
  Result := FAnchorPoint;
end;

function TBitmapMarker.GetBitmap: TCustomBitmap32;
begin
  Result := FBitmap.Bitmap;
end;

function TBitmapMarker.GetBitmapSize: TPoint;
begin
  Result := FBitmapSize;
end;

{ TBitmapMarkerWithDirection }

constructor TBitmapMarkerWithDirection.Create(
  ABitmap: IBitmap32Static;
  AAnchorPoint: TDoublePoint;
  ADirection: Double
);
begin
  inherited Create(ABitmap, AAnchorPoint);
  FDirection := ADirection;
end;

function TBitmapMarkerWithDirection.GetDirection: Double;
begin
  Result := FDirection;
end;

end.
