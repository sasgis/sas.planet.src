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
  i_InterfaceListStatic,
  i_InterfaceListSimple,
  i_TileIteratorDataProvider,
  u_BaseInterfacedObject;

type
  TDownloadTaskProvider = class(TBaseInterfacedObject, IDownloadTaskProvider)
  private
    FLock: IReadWriteSync;
    FMapType: IMapType;
    FPolygon: IGeometryLonLatPolygon;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FZoomArray: TByteDynArray;
    FLastProcessedZoom: Byte;
    FLastProcessedPoint: TPoint;
    FLastProcessedCount: Int64;
    FPartsCount: Integer;
    FDataProvidersList: IInterfaceListSimple;
    FPrepared: Boolean;
    procedure _PrepareDataProviders;
  private
    { IDownloadTaskProvider }
    procedure GetTasksList(
      const AWorkerIndex: Integer;
      out ATilesTotal: Int64;
      out ATasksList: IInterfaceListStatic
    );
  public
    constructor Create(
      const AMapType: IMapType;
      const APolygon: IGeometryLonLatPolygon;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const APartsCount: Integer;
      const AZoomArray: TByteDynArray;
      const ALastProcessedZoom: Byte;
      const ALastProcessedPoint: TPoint;
      const ALastProcessedCount: Int64
    );
  end;

implementation

uses
  t_GeoTypes,
  i_Projection,
  i_GeometryProjected,
  u_GeoFunc,
  u_GeometryFunc,
  u_ZoomArrayFunc,
  u_InterfaceListSimple,
  u_TileIteratorByPolygon,
  u_TileIteratorDataProvider,
  u_Synchronizer;

{ TDownloadTaskProvider }

constructor TDownloadTaskProvider.Create(
  const AMapType: IMapType;
  const APolygon: IGeometryLonLatPolygon;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const APartsCount: Integer;
  const AZoomArray: TByteDynArray;
  const ALastProcessedZoom: Byte;
  const ALastProcessedPoint: TPoint;
  const ALastProcessedCount: Int64
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
  FLastProcessedCount := ALastProcessedCount;
  FPartsCount := APartsCount;
  FDataProvidersList := TInterfaceListSimple.Create;
  FPrepared := False;
  FLock := GSync.SyncStd.Make(Self.ClassName);
end;

procedure TDownloadTaskProvider._PrepareDataProviders;
var
  I: Integer;
  VZoom: Byte;
  VProjection: IProjection;
  VProjectedPolygon: IGeometryProjectedPolygon;
  VDataProvider: ITileIteratorDataProvider;
begin
  FLock.BeginWrite;
  try
    if FPrepared then begin
      Exit;
    end;

    if not Assigned(FPolygon) then begin
      raise Exception.Create('Polygon does not exist!');
    end;

    for I := Low(FZoomArray) to High(FZoomArray) do begin
      VZoom := FZoomArray[I];

      VProjection := FMapType.ProjectionSet[VZoom];

      VProjectedPolygon :=
        FVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
          VProjection,
          FPolygon
        );

      VDataProvider :=
        TTileIteratorDataProvider.Create(
          VProjection,
          VProjectedPolygon,
          FPartsCount
        );

      FDataProvidersList.Add(VDataProvider);
    end;

    FPrepared := True;
  finally
    FLock.EndWrite;
  end;
end;

procedure TDownloadTaskProvider.GetTasksList(
  const AWorkerIndex: Integer;
  out ATilesTotal: Int64;
  out ATasksList: IInterfaceListStatic
);
var
  I: Integer;
  VZoom: Byte;
  VTmp: TPoint;
  VStartPoint: TPoint;
  VTilesToProcess: Int64;
  VTasksList: IInterfaceListSimple;
  VTileIterator: ITileIterator;
  VSeekToLastProcessedPoint: Boolean;
  VDataProvider: ITileIteratorDataProvider;
begin
  if not FPrepared then begin
    _PrepareDataProviders;
  end;

  ATilesTotal := 0;
  VTasksList := TInterfaceListSimple.Create;

  for I := 0 to FDataProvidersList.Count - 1 do begin

    VDataProvider := FDataProvidersList.Items[I] as ITileIteratorDataProvider;
    Assert(VDataProvider <> nil);

    if AWorkerIndex >= VDataProvider.PartsCount then begin
      Continue;
    end;

    VZoom := VDataProvider.Projection.Zoom;

    VStartPoint := VDataProvider.StartPoint[AWorkerIndex];
    VTilesToProcess := VDataProvider.TilesToProcess[AWorkerIndex];

    Inc(ATilesTotal, VTilesToProcess);

    if VZoom >= FLastProcessedZoom then begin

      VSeekToLastProcessedPoint := False;
      if VZoom = FLastProcessedZoom then begin
        if (FLastProcessedPoint.X >= 0) and (FLastProcessedPoint.Y >= 0) then begin
          VStartPoint := FLastProcessedPoint;
          if FLastProcessedCount > 0 then begin
            Dec(VTilesToProcess, FLastProcessedCount);
          end;
          VSeekToLastProcessedPoint := True;
        end;
      end;

      VTileIterator :=
        TTileIteratorByPolygon.Create(
          VDataProvider.Projection,
          VDataProvider.Polygon,
          VTilesToProcess,
          VStartPoint.X,
          VStartPoint.Y
        );

      if VSeekToLastProcessedPoint then begin
        if VTileIterator.Next(VTmp) then begin
          Assert(IsPointsEqual(VTmp, FLastProcessedPoint));
        end else begin
          Assert(False);
        end;
      end;

      VTasksList.Add(VTileIterator);
    end;
  end;

  ATasksList := VTasksList.MakeStaticCopy;
end;

end.
