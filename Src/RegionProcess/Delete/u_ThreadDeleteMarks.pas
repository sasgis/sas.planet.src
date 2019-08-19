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
  i_Projection,
  i_GeometryLonLat,
  i_GeometryProjected,
  i_InterfaceListStatic,
  u_GeoFunc,
  u_RegionProcessTaskAbstract;

type
  TThreadDeleteMarks = class(TRegionProcessTaskAbstract)
  private
    FpolyLL: IGeometryLonLatPolygon;
    FProjectedPolygon: IGeometryProjectedPolygon;
    FProjection: IProjection;
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
      const AProjection: IProjection;
      const AMarkSystem: IMarkSystem;
      const AMarksState: Byte;
      const ADelHiddenMarks: Boolean
    );
  end;

implementation

uses
  Math,
  t_GeoTypes,
  i_MarkId,
  i_MarkCategory,
  i_EnumDoublePoint,
  i_VectorItemSubset,
  u_InterfaceListSimple,
  u_ResStrings;

constructor TThreadDeleteMarks.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const APolyLL: IGeometryLonLatPolygon;
  const AProjectedPolygon: IGeometryProjectedPolygon;
  const AProjection: IProjection;
  const AMarkSystem: IMarkSystem;
  const AMarksState: Byte;
  const ADelHiddenMarks: Boolean
);
begin
  inherited Create(
    AProgressInfo,
    APolyLL,
    nil
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

function IsLonLatPointInProjectedPolygon(
  const AGeometry: IGeometryLonLatPoint;
  const AProjectedPolygon: IGeometryProjectedPolygon;
  const AProjection: IProjection
): Boolean;
var
  VProjectedPoint: TDoublePoint;
begin
  VProjectedPoint := AProjection.LonLat2PixelPosFloat(AGeometry.Point);
  Result := AProjectedPolygon.IsPointInPolygon(VProjectedPoint);
end;

function IsLonLatSingleLineInProjectedPolygon(
  const AGeometry: IGeometryLonLatSingleLine;
  const AProjectedPolygon: IGeometryProjectedPolygon;
  const AProjection: IProjection
): Boolean; inline;
var
  VLonlatPoint: TDoublePoint;
  VProjectedPoint: TDoublePoint;
  VEnum: IEnumLonLatPoint;
begin
  Result := True;

  VEnum := AGeometry.GetEnum;
  while VEnum.Next(VLonlatPoint) do begin
    VProjectedPoint := AProjection.LonLat2PixelPosFloat(VLonlatPoint);
    if not AProjectedPolygon.IsPointInPolygon(VProjectedPoint) then begin
      Result := False;
      Break;
    end;
  end;
end;

function IsLonLatMultiLineInProjectedPolygon(
  const AGeometry: IGeometryLonLatMultiLine;
  const AProjectedPolygon: IGeometryProjectedPolygon;
  const AProjection: IProjection
): Boolean; inline;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to AGeometry.Count - 1 do begin
    if not IsLonLatSingleLineInProjectedPolygon(AGeometry.Item[i], AProjectedPolygon, AProjection) then begin
      Result := False;
      Break;
    end;
  end;
end;

function IsLonLatLineInProjectedPolygon(
  const AGeometry: IGeometryLonLatLine;
  const AProjectedPolygon: IGeometryProjectedPolygon;
  const AProjection: IProjection
): Boolean; inline;
var
  VProjectedBounds: TDoubleRect;
  VSingleLine: IGeometryLonLatSingleLine;
  VMultiLine: IGeometryLonLatMultiLine;
begin
  Result := False;
  VProjectedBounds := AProjection.LonLatRect2PixelRectFloat(AGeometry.Bounds.Rect);
  if not IsIntersecProjectedRect(AProjectedPolygon.Bounds, VProjectedBounds) then begin
    Exit;
  end;
  if Supports(AGeometry, IGeometryLonLatSingleLine, VSingleLine) then begin
    Result := IsLonLatSingleLineInProjectedPolygon(VSingleLine, AProjectedPolygon, AProjection);
  end else if Supports(AGeometry, IGeometryLonLatMultiLine, VMultiLine) then begin
    Result := IsLonLatMultiLineInProjectedPolygon(VMultiLine, AProjectedPolygon, AProjection);
  end else begin
    Assert(False);
  end;
end;

function IsLonLatContourInProjectedPolygon(
  const AGeometry: IGeometryLonLatContour;
  const AProjectedPolygon: IGeometryProjectedPolygon;
  const AProjection: IProjection
): Boolean; inline;
var
  VLonlatPoint: TDoublePoint;
  VProjectedPoint: TDoublePoint;
  VEnum: IEnumLonLatPoint;
begin
  Result := True;
  VEnum := AGeometry.GetEnum;
  while VEnum.Next(VLonlatPoint) do begin
    VProjectedPoint := AProjection.LonLat2PixelPosFloat(VLonlatPoint);
    if not AProjectedPolygon.IsPointInPolygon(VProjectedPoint) then begin
      Result := False;
      Break;
    end;
  end;
end;

function IsLonLatSinglePolygonInProjectedPolygon(
  const AGeometry: IGeometryLonLatSinglePolygon;
  const AProjectedPolygon: IGeometryProjectedPolygon;
  const AProjection: IProjection
): Boolean; inline;
begin
  Result :=
    IsLonLatContourInProjectedPolygon(
      AGeometry.OuterBorder,
      AProjectedPolygon,
      AProjection
    );
end;

function IsLonLatMultiPolygonInProjectedPolygon(
  const AGeometry: IGeometryLonLatMultiPolygon;
  const AProjectedPolygon: IGeometryProjectedPolygon;
  const AProjection: IProjection
): Boolean; inline;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to AGeometry.Count - 1 do begin
    if IsLonLatSinglePolygonInProjectedPolygon(AGeometry.Item[i], AProjectedPolygon, AProjection) then begin
      Result := False;
      Break;
    end;
  end;
end;

function IsLonLatPolygonInProjectedPolygon(
  const AGeometry: IGeometryLonLatPolygon;
  const AProjectedPolygon: IGeometryProjectedPolygon;
  const AProjection: IProjection
): Boolean; inline;
var
  VProjectedBounds: TDoubleRect;
  VSinglePolygon: IGeometryLonLatSinglePolygon;
  VMultiPolygon: IGeometryLonLatMultiPolygon;
begin
  Result := False;
  VProjectedBounds := AProjection.LonLatRect2PixelRectFloat(AGeometry.Bounds.Rect);
  if not IsIntersecProjectedRect(AProjectedPolygon.Bounds, VProjectedBounds) then begin
    Exit;
  end;
  if Supports(AGeometry, IGeometryLonLatSinglePolygon, VSinglePolygon) then begin
    Result := IsLonLatSinglePolygonInProjectedPolygon(VSinglePolygon, AProjectedPolygon, AProjection);
  end else if Supports(AGeometry, IGeometryLonLatMultiPolygon, VMultiPolygon) then begin
    Result := IsLonLatMultiPolygonInProjectedPolygon(VMultiPolygon, AProjectedPolygon, AProjection);
  end else begin
    Assert(False);
  end;
end;

function IsLonLatGeometryInProjectedPolygon(
  const AGeometry: IGeometryLonLat;
  const AProjectedPolygon: IGeometryProjectedPolygon;
  const AProjection: IProjection
): Boolean;
var
  VPoint: IGeometryLonLatPoint;
  VLine: IGeometryLonLatLine;
  VPolygon: IGeometryLonLatPolygon;
begin
  Result := False;
  if Supports(AGeometry, IGeometryLonLatPoint, VPoint) then begin
    Result := IsLonLatPointInProjectedPolygon(VPoint, AProjectedPolygon, AProjection);
  end else if Supports(AGeometry, IGeometryLonLatLine, VLine) then begin
    Result := IsLonLatLineInProjectedPolygon(VLine, AProjectedPolygon, AProjection);
  end else if Supports(AGeometry, IGeometryLonLatPolygon, VPolygon) then begin
    Result := IsLonLatPolygonInProjectedPolygon(VPolygon, AProjectedPolygon, AProjection);
  end else begin
    Assert(False);
  end;
end;

procedure TThreadDeleteMarks.ProcessRegion;
var
  I: Integer;
  VMarksToProcess: Int64;
  VProcessed: Int64;
  VVectorItems: IVectorItemSubset;
  VTemp: IInterfaceListSimple;
  VMarksListToDelete: IInterfaceListStatic;
  VDoAdd: Boolean;
  VMarkId: IMarkId;
begin
  inherited;

  VVectorItems :=
    FMarkSystem.MarkDb.GetMarkSubsetByCategoryListInRect(
      FpolyLL.Bounds.Rect,
      nil,
      FDelHiddenMarks,
      DoublePoint(0, 0)
    );

  if VVectorItems <> nil then begin
    VProcessed := 0;
    VTemp := TInterfaceListSimple.Create;
    VMarksToProcess := VVectorItems.Count;

    ProgressInfo.SetCaption(SAS_STR_Whole + ' ' + IntToStr(VVectorItems.Count));

    for I := 0 to VVectorItems.Count - 1 do begin
      Inc(VProcessed);

      VMarkId := VVectorItems.Items[I].MainInfo as IMarkId;

      case VMarkId.MarkType of
        midPoint : VDoAdd := (FMarksState and CPlacemarkFlag) <> 0;
        midLine  : VDoAdd := (FMarksState and CPathFlag) <> 0;
        midPoly  : VDoAdd := (FMarksState and CPolygonFlag) <> 0;
      else
        VDoAdd := False;
      end;

      if VDoAdd and not FDelHiddenMarks then begin
        VDoAdd := (VMarkId.Category as IMarkCategory).Visible;
      end;

      if VDoAdd then begin
        VDoAdd := FpolyLL.Bounds.IsContainRect(VVectorItems.Items[I].Geometry.Bounds);
      end;

      if VDoAdd then begin
        VDoAdd :=
          IsLonLatGeometryInProjectedPolygon(
            VVectorItems.Items[I].Geometry,
            FProjectedPolygon,
            FProjection
          );
        if VDoAdd then begin
          VTemp.Add(IMarkId(VVectorItems.Items[I].MainInfo));
        end;
      end;

      ProgressFormUpdateOnProgress(VProcessed, VMarksToProcess, VTemp.Count);
    end;

    if VTemp.Count > 0 then begin
      VMarksListToDelete := VTemp.MakeStaticAndClear;
      FMarkSystem.MarkDb.UpdateMarkList(VMarksListToDelete, nil);
    end;
  end;
end;

procedure TThreadDeleteMarks.ProgressFormUpdateOnProgress(
  const AProcessed, AToProcess, ADeleted: Int64
);
begin
  ProgressInfo.SetProcessedRatio(AProcessed / AToProcess);
  ProgressInfo.SetSecondLine(SAS_STR_Processed + ' ' + IntToStr(AProcessed));
  ProgressInfo.SetFirstLine(SAS_STR_AllDelete + ' ' + IntToStr(ADeleted) + ' ' + SAS_STR_PlaceMarks);
end;

end.
