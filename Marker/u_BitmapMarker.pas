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

unit u_BitmapMarker;

interface

uses
  Types,
  t_Hash,
  t_Bitmap32,
  t_GeoTypes,
  i_Bitmap32Static,
  i_BitmapMarker,
  u_BaseInterfacedObject;

type
  TBitmapMarker = class(TBaseInterfacedObject, IBitmapMarker, IBitmap32Static)
  private
    FBitmap: IBitmap32Static;
    FAnchorPoint: TDoublePoint;
  private
    function GetHash: THashValue;
    function GetSize: TPoint;
    function GetData: PColor32Array;
    function GetAnchorPoint: TDoublePoint;
  public
    constructor Create(
      const ABitmap: IBitmap32Static;
      const AAnchorPoint: TDoublePoint
    );
  end;

  TBitmapMarkerWithDirection = class(TBitmapMarker, IBitmapMarkerWithDirection)
  private
    FDirection: Double;
  private
    function GetDirection: Double;
  public
    constructor Create(
      const ABitmap: IBitmap32Static;
      const AAnchorPoint: TDoublePoint;
      const ADirection: Double
    );
  end;


implementation

{ TBitmapMarker }

constructor TBitmapMarker.Create(
  const ABitmap: IBitmap32Static;
  const AAnchorPoint: TDoublePoint
);
begin
  inherited Create;
  FAnchorPoint := AAnchorPoint;
  FBitmap := ABitmap;
end;

function TBitmapMarker.GetAnchorPoint: TDoublePoint;
begin
  Result := FAnchorPoint;
end;

function TBitmapMarker.GetData: PColor32Array;
begin
  Result := FBitmap.Data;
end;

function TBitmapMarker.GetHash: THashValue;
begin
  Result := FBitmap.Hash;
end;

function TBitmapMarker.GetSize: TPoint;
begin
  Result := FBitmap.Size;
end;

{ TBitmapMarkerWithDirection }

constructor TBitmapMarkerWithDirection.Create(
  const ABitmap: IBitmap32Static;
  const AAnchorPoint: TDoublePoint;
  const ADirection: Double
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
