{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2016, SAS.Planet development team.                      *}
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

unit u_DownloadTaskProvider;

interface

uses
  Types,
  SysUtils,
  i_MapType,
  i_TileIterator,
  i_GeometryLonLat,
  i_GeometryProjectedFactory,
  i_DownloadTaskProvider,
  u_BaseInterfacedObject;

type
  TDownloadTaskProvider = class(TBaseInterfacedObject, IDownloadTaskProvider)
  private
    FMapType: IMapType;
    FPolygon: IGeometryLonLatPolygon;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FZoomArray: TByteDynArray;
    FLastProcessedZoom: Byte;
    FLastProcessedPoint: TPoint;
  private
    { IDownloadTaskProvider }
    procedure GetTask(
      out ATilesTotal: Int64;
      out ATaskArray: TTileIteratorArray
    );
  public
    constructor Create(
      const AMapType: IMapType;
      const APolygon: IGeometryLonLatPolygon;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const AZoomArray: TByteDynArray;
      const ALastProcessedZoom: Byte;
      const ALastProcessedPoint: TPoint
    );
  end;

implementation

uses
  i_Projection,
  i_GeometryProjected,
  u_ZoomArrayFunc,
  u_TileIteratorByPolygon;

{ TDownloadTaskProvider }

constructor TDownloadTaskProvider.Create(
  const AMapType: IMapType;
  const APolygon: IGeometryLonLatPolygon;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const AZoomArray: TByteDynArray;
  const ALastProcessedZoom: Byte;
  const ALastProcessedPoint: TPoint
);
begin
  Assert(AMapType <> nil);
  Assert(APolygon <> nil);
  Assert(AVectorGeometryProjectedFactory <> nil);

  inherited Create;

  FMapType := AMapType;
  FPolygon := APolygon;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FZoomArray := GetZoomArrayCopy(AZoomArray);
  FLastProcessedZoom := ALastProcessedZoom;
  FLastProcessedPoint := ALastProcessedPoint;
end;

procedure TDownloadTaskProvider.GetTask(
  out ATilesTotal: Int64;
  out ATaskArray: TTileIteratorArray
);
var
  I: Integer;
  VZoom: Byte;
  VStartZoomIndex: Integer;
  VTaskCount: Integer;
  VTaskArray: TTileIteratorArray;
  VTileIterator: ITileIterator;
  VProjection: IProjection;
  VProjectedPolygon: IGeometryProjectedPolygon;
begin
  if not Assigned(FPolygon) then begin
    raise Exception.Create('Polygon does not exist!');
  end;

  VTaskCount := 0;
  SetLength(VTaskArray, 0);
  VStartZoomIndex := -1;

  for I := Low(FZoomArray) to High(FZoomArray) do begin
    VZoom := FZoomArray[I];

    VProjection := FMapType.ProjectionSet[VZoom];

    VProjectedPolygon :=
      FVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
        VProjection,
        FPolygon
      );

    VTileIterator :=
      TTileIteratorByPolygon.Create(
        VProjection,
        VProjectedPolygon
      );

    SetLength(VTaskArray, VTaskCount + 1);

    VTaskArray[VTaskCount] := VTileIterator;

    if VZoom = FLastProcessedZoom then begin
      VStartZoomIndex := VTaskCount;
    end;

    Inc(VTaskCount);
  end;

  if VStartZoomIndex = -1 then begin
    raise Exception.Create('Start zoom index detection error!');
  end;

  // calc tiles count
  ATilesTotal := 0;
  for I := 0 to Length(VTaskArray) - 1 do begin
    Inc(ATilesTotal, VTaskArray[I].TilesTotal);
  end;

  // skip tiles processed in last session
  if (FLastProcessedPoint.X >= 0) and (FLastProcessedPoint.Y >= 0) then begin
    VTaskArray[VStartZoomIndex].Seek(FLastProcessedPoint);
  end;

  SetLength(ATaskArray, Length(VTaskArray) - VStartZoomIndex);
  for I := VStartZoomIndex to Length(VTaskArray) - 1 do begin
    ATaskArray[I - VStartZoomIndex] := VTaskArray[I];
  end;
end;

end.
