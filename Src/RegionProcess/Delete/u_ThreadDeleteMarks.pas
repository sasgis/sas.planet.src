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

unit u_ThreadDeleteMarks;

interface

uses

  SysUtils,
  Classes,
  c_MarkFlag,
  i_MarkDb,
  i_MarkSystem,
  i_RegionProcessProgressInfo,
  i_InterfaceListSimple,
  i_ProjectionInfo,
  i_GeometryLonLat,
  i_GeometryProjected,
  i_InterfaceListStatic,
  u_ThreadRegionProcessAbstract;

  type
  TThreadDeleteMarks = class(TThreadRegionProcessAbstract)
  private
    FpolyLL: IGeometryLonLatPolygon;
    FProjectedPolygon: IGeometryProjectedPolygon;
    FProjection: IProjectionInfo;
    FMarkSystem: IMarkSystem;
    FMarksState: Byte;
    FDelHiddenMarks: Boolean;

  protected
    procedure ProcessRegion; override;
    procedure ProgressFormUpdateOnProgress(
      const AProcessed, AToProcess, ADeleted: Int64
    );
  public
    constructor Create(
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const APolyLL: IGeometryLonLatPolygon;
      const AProjectedPolygon: IGeometryProjectedPolygon;
      const AProjection: IProjectionInfo;
      const AMarkSystem: IMarkSystem;
      const AMarksState: Byte;
      const ADelHiddenMarks: Boolean
    );
  end;

implementation

uses
  t_GeoTypes,
  i_MarkId,
  i_EnumDoublePoint,
  i_VectorItemSubset,
  u_InterfaceListSimple,
  u_GeoFunc,
  u_ResStrings;

constructor TThreadDeleteMarks.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const APolyLL: IGeometryLonLatPolygon;
  const AProjectedPolygon: IGeometryProjectedPolygon;
  const AProjection: IProjectionInfo;
  const AMarkSystem: IMarkSystem;
  const AMarksState: Byte;
  const ADelHiddenMarks: Boolean
);
begin
  inherited Create(
    AProgressInfo,
    APolyLL,
    Self.ClassName
  );
  Assert(AProgressInfo <> nil);
  Assert(APolyLL <> nil);
  Assert(AProjectedPolygon <> nil);
  Assert(AProjection <> nil);
  Assert(AMarkSystem <> nil);

  FpolyLL := APolyLL;
  FProjectedPolygon := AProjectedPolygon;
  FProjection := AProjection;
  FMarkSystem := AMarkSystem;
  FMarksState := AMarksState;
  FDelHiddenMarks := ADelHiddenMarks;
end;

procedure TThreadDeleteMarks.ProcessRegion;
var
  VTilesToProcess: Int64;
  VProcessed: Int64;
  VVectorItems: IVectorItemSubset;
  I: Integer;
  VTemp: IInterfaceListSimple;
  VMarksListToDelete: IInterfaceListStatic;
  VDoAdd: Boolean;
  VDoublePoint: TDoublePoint;
  VPoint: IGeometryLonLatPoint;
  VLine: IGeometryLonLatLine;
  VPoly: IGeometryLonLatPolygon;
  VEnum: IEnumLonLatPoint;
begin
  inherited;
  VVectorItems := FMarkSystem.MarkDb.GetMarkSubsetByCategoryListInRect(FpolyLL.Bounds.Rect, nil, FDelHiddenMarks);
  VProcessed := 0;
  if VVectorItems <> nil then begin
    VTemp := TInterfaceListSimple.Create;
    VTilesToProcess := VVectorItems.Count;
    ProgressInfo.SetCaption(
      SAS_STR_Whole + ' ' + inttostr(VVectorItems.Count)
    );
    for I := 0 to VVectorItems.Count - 1 do begin
      inc(VProcessed);
      VDoAdd := False;

      case (VVectorItems.Items[i].MainInfo as IMarkId).MarkType of
        midPoint: if (FMarksState and CPlacemarkFlag) <> 0 then VDoAdd := True;
        midLine:  if (FMarksState and CPathFlag) <> 0 then VDoAdd := True;
        midPoly:  if (FMarksState and CPolygonFlag) <> 0 then VDoAdd := True;
      else
      end;

      if VDoAdd then VDoAdd := FpolyLL.Bounds.IsContainRect(VVectorItems.Items[i].Geometry.Bounds);

      if VDoAdd then begin
        if Supports(VVectorItems.Items[i].Geometry, IGeometryLonLatPoint, VPoint) then begin
          VDoublePoint := FProjection.GeoConverter.LonLat2PixelPosFloat(VPoint.Point, FProjection.Zoom);
          VDoAdd := FProjectedPolygon.IsPointInPolygon(VDoublePoint);
        end else if Supports(VVectorItems.Items[i].Geometry, IGeometryLonLatLine, VLine) then begin
          VEnum := VLine.GetEnum;
          while VEnum.Next(VDoublePoint) and VDoAdd do begin
            if not PointIsEmpty(VDoublePoint) then begin
              VDoublePoint := FProjection.GeoConverter.LonLat2PixelPosFloat(VDoublePoint, FProjection.Zoom);
              VDoAdd := FProjectedPolygon.IsPointInPolygon(VDoublePoint);
            end;
          end;
        end else if Supports(VVectorItems.Items[i].Geometry, IGeometryLonLatPolygon, VPoly) then begin
          VEnum := VPoly.GetEnum;
          while VEnum.Next(VDoublePoint) and VDoAdd do begin
            if not PointIsEmpty(VDoublePoint) then begin
              VDoublePoint := FProjection.GeoConverter.LonLat2PixelPosFloat(VDoublePoint, FProjection.Zoom);
              VDoAdd := FProjectedPolygon.IsPointInPolygon(VDoublePoint);
            end;
          end;
        end;

        if VDoAdd then begin
          VTemp.Add(IMarkId(VVectorItems.Items[i].MainInfo));
        end;
      end;
      ProgressFormUpdateOnProgress(VProcessed, VTilesToProcess, VTemp.Count);
    end;

    if VTemp.Count > 0 then begin
      VMarksListToDelete := VTemp.MakeStaticAndClear;
      FMarkSystem.MarkDb.UpdateMarkList(VMarksListToDelete, nil)
    end;
  end;
end;

procedure TThreadDeleteMarks.ProgressFormUpdateOnProgress(
  const AProcessed, AToProcess, ADeleted: Int64
);
begin
  ProgressInfo.SetProcessedRatio(AProcessed / AToProcess);
  ProgressInfo.SetSecondLine(SAS_STR_Processed + ' ' + inttostr(AProcessed));
  ProgressInfo.SetFirstLine(SAS_STR_AllDelete + ' ' + inttostr(ADeleted) + ' ' + SAS_STR_PlaceMarks);
end;

end.
