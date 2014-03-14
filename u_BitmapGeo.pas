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

unit u_BitmapGeo;

interface

uses
  Types,
  i_Bitmap32Static,
  i_LocalCoordConverter,
  i_BitmapGeo,
  u_BaseInterfacedObject;

type
  TBitmapGeo = class(TBaseInterfacedObject, IBitmapGeo)
  private
    FBitmap: IBitmap32Static;
    FConverter: ILocalCoordConverter;
  private
    function GetBitmap: IBitmap32Static;
    function GetConverter: ILocalCoordConverter;
  public
    constructor Create(
      const ABitmap: IBitmap32Static;
      const AConverter: ILocalCoordConverter
    );
  end;

  TBitmapGeoTile = class(TBitmapGeo, IBitmapGeoTile)
  private
    FTile: TPoint;
  private
    function GetTile: TPoint;
  public
    constructor Create(
      const ATile: TPoint;
      const ABitmap: IBitmap32Static;
      const AConverter: ILocalCoordConverter
    );
  end;

implementation



{ TBitmapGeo }

constructor TBitmapGeo.Create(
  const ABitmap: IBitmap32Static;
  const AConverter: ILocalCoordConverter
);
begin
  inherited Create;
  FBitmap := ABitmap;
  FConverter := AConverter;
end;

function TBitmapGeo.GetBitmap: IBitmap32Static;
begin
  Result := FBitmap;
end;

function TBitmapGeo.GetConverter: ILocalCoordConverter;
begin
  Result := FConverter;
end;

{ TBitmapGeoTile }

constructor TBitmapGeoTile.Create(
  const ATile: TPoint;
  const ABitmap: IBitmap32Static;
  const AConverter: ILocalCoordConverter
);
begin
  inherited Create(ABitmap, AConverter);
  FTile := ATile;
end;

function TBitmapGeoTile.GetTile: TPoint;
begin
  Result := FTile;
end;

end.
