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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_BitmapLayerProviderInPolygon;

interface

uses
  i_NotifierOperation,
  i_Bitmap32Static,
  i_LocalCoordConverter,
  i_GeometryProjected,
  i_BitmapLayerProvider,
  u_BaseInterfacedObject;

type
  TBitmapLayerProviderInPolygon = class(TBaseInterfacedObject, IBitmapLayerProvider)
  private
    FSourceProvider: IBitmapLayerProvider;
    FPolyProjected: IGeometryProjectedMultiPolygon;
    FLine: IGeometryProjectedSinglePolygon;
  private
    function GetBitmapRect(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALocalConverter: ILocalCoordConverter
    ): IBitmap32Static;
  public
    constructor Create(
      const APolyProjected: IGeometryProjectedMultiPolygon;
      const ASourceProvider: IBitmapLayerProvider
    );
  end;

implementation

{ TBitmapLayerProviderInPolygon }

constructor TBitmapLayerProviderInPolygon.Create(
  const APolyProjected: IGeometryProjectedMultiPolygon;
  const ASourceProvider: IBitmapLayerProvider
);
begin
  inherited Create;
  FSourceProvider := ASourceProvider;
  FPolyProjected := APolyProjected;
  Assert(FSourceProvider <> nil);
  Assert(FPolyProjected <> nil);
  Assert(FPolyProjected.Count > 0);
  FLine := FPolyProjected.Item[0];
end;

function TBitmapLayerProviderInPolygon.GetBitmapRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALocalConverter: ILocalCoordConverter
): IBitmap32Static;
begin
  if FLine.IsRectIntersectPolygon(ALocalConverter.GetRectInMapPixelFloat) then begin
    Result :=
      FSourceProvider.GetBitmapRect(
        AOperationID,
        ACancelNotifier,
        ALocalConverter
      );
  end else begin
    Result := nil;
  end;
end;

end.
