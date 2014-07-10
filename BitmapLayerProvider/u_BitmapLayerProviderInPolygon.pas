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
    FLine: IGeometryProjectedSinglePolygon;
  private
    function GetBitmapRect(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALocalConverter: ILocalCoordConverter
    ): IBitmap32Static;
  public
    constructor Create(
      const APolyProjected: IGeometryProjectedPolygon;
      const ASourceProvider: IBitmapLayerProvider
    );
  end;

implementation

uses
  SysUtils;

{ TBitmapLayerProviderInPolygon }

constructor TBitmapLayerProviderInPolygon.Create(
  const APolyProjected: IGeometryProjectedPolygon;
  const ASourceProvider: IBitmapLayerProvider
);
var
  VMultiPolygon: IGeometryProjectedMultiPolygon;
begin
  Assert(ASourceProvider <> nil);
  Assert(APolyProjected <> nil);
  Assert(not APolyProjected.IsEmpty);
  inherited Create;
  FSourceProvider := ASourceProvider;
  if not Supports(APolyProjected, IGeometryProjectedSinglePolygon, FLine) then begin
    if Supports(APolyProjected, IGeometryProjectedMultiPolygon, VMultiPolygon) then begin
      if VMultiPolygon.Count > 0 then begin
        FLine := VMultiPolygon.Item[0];
      end;
    end else begin
      Assert(False);
      FLine := nil;
    end;
  end;
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
