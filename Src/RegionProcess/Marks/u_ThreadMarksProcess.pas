{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_ThreadMarksProcess;

interface

uses
  t_MarksProcess,
  i_MarkSystem,
  i_RegionProcessProgressInfo,
  i_Projection,
  i_GeometryLonLat,
  i_GeometryProjected,
  u_GeoFunc,
  u_MarkDbGUIHelper,
  u_RegionProcessTaskAbstract;

type
  TThreadMarksProcess = class(TRegionProcessTaskAbstract)
  private
    FMarkDBGUI: TMarkDbGUIHelper;
    FLonLatPolygon: IGeometryLonLatPolygon;
    FProjectedPolygon: IGeometryProjectedPolygon;
    FProjection: IProjection;
    FMarkSystem: IMarkSystem;
    FParams: TMarksProcessTaskParams;

    procedure ProgressFormUpdate(const ACaption: string); overload;
    procedure ProgressFormUpdate(const AProcessed, AToProcess: Int64); overload;
    procedure ProgressFormUpdate(const AProcessed, AToProcess, ASelected: Int64); overload;
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      const AMarkDBGUI: TMarkDbGUIHelper;
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const ALonLatPolygon: IGeometryLonLatPolygon;
      const AProjectedPolygon: IGeometryProjectedPolygon;
      const AProjection: IProjection;
      const AMarkSystem: IMarkSystem;
      const AParams: TMarksProcessTaskParams
    );
  end;

implementation

uses
  SysUtils,
  Classes,
  t_GeoTypes,
  i_MarkId,
  i_MarkDb,
  i_MarkFactory,
  i_MarkCategory,
  i_VectorDataItemSimple,
  i_EnumDoublePoint,
  i_VectorItemSubset,
  i_InterfaceListSimple,
  i_InterfaceListStatic,
  u_Dialogs,
  u_InterfaceListSimple,
  u_ResStrings;

resourcestring
  rsThereAreNoPlacemarksToProcess = 'There are no placemarks to process!';
  rsReadingPlacemarksFromDb = 'Reading placemarks from DB...';
  rsSelectingPlacemarks = 'Selecting placemarks...';
  rsCopyingPlacemarksFmt = 'Copying %d placemarks...';
  rsMovingPlacemarksFmt = 'Moving %d placemarks...';
  rsDeletingPlacemarksFmt = 'Deleting %d placemarks...';
  rsSelectedFmt = 'Selected %d';
  rsCantCopyFmt = 'Can''t copy placemark "%s"';
  rsCantMoveFmt = 'Can''t move placemark "%s"';

{ TThreadMarksProcess }

constructor TThreadMarksProcess.Create(
  const AMarkDBGUI: TMarkDbGUIHelper;
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const ALonLatPolygon: IGeometryLonLatPolygon;
  const AProjectedPolygon: IGeometryProjectedPolygon;
  const AProjection: IProjection;
  const AMarkSystem: IMarkSystem;
  const AParams: TMarksProcessTaskParams
);
begin
  inherited Create(
    AProgressInfo,
    ALonLatPolygon,
    nil
  );

  Assert(AMarkDBGUI <> nil);
  Assert(AProgressInfo <> nil);
  Assert(ALonLatPolygon <> nil);
  Assert(AProjectedPolygon <> nil);
  Assert(AProjection <> nil);
  Assert(AMarkSystem <> nil);

  FMarkDBGUI := AMarkDBGUI;
  FLonLatPolygon := ALonLatPolygon;
  FProjectedPolygon := AProjectedPolygon;
  FProjection := AProjection;
  FMarkSystem := AMarkSystem;
  FParams := AParams;
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
  I: Integer;
begin
  Result := True;
  for I := 0 to AGeometry.Count - 1 do begin
    if not IsLonLatSingleLineInProjectedPolygon(AGeometry.Item[I], AProjectedPolygon, AProjection) then begin
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
  I: Integer;
begin
  Result := True;
  for I := 0 to AGeometry.Count - 1 do begin
    if IsLonLatSinglePolygonInProjectedPolygon(AGeometry.Item[I], AProjectedPolygon, AProjection) then begin
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

procedure TThreadMarksProcess.ProcessRegion;
var
  I: Integer;
  VTotal: Int64;
  VProcessed: Int64;
  VMarkDb: IMarkDb;
  VMarkFactory: IMarkFactory;
  VMark, VOldMark, VNewMark: IVectorDataItem;
  VVectorItems: IVectorItemSubset;
  VList: IInterfaceListSimple;
  VDoAdd: Boolean;
  VMarkId: IMarkId;
  VMarkIdList: IInterfaceListStatic;
begin
  inherited;

  ProgressFormUpdate(rsReadingPlacemarksFromDb);

  VVectorItems :=
    FMarkSystem.MarkDb.GetMarkSubsetByCategoryListInRect(
      FLonLatPolygon.Bounds.Rect,
      nil,
      FParams.IncludeHiddenMarks,
      DoublePoint(0, 0)
    );

  if VVectorItems = nil then begin
    ShowInfoMessageSync(rsThereAreNoPlacemarksToProcess);
    Exit;
  end;

  VList := TInterfaceListSimple.Create;

  VProcessed := 0;
  VTotal := VVectorItems.Count;

  ProgressFormUpdate(rsSelectingPlacemarks);

  for I := 0 to VVectorItems.Count - 1 do begin
    Inc(VProcessed);

    VMark := VVectorItems[I];
    VMarkId := VMark.MainInfo as IMarkId;

    case VMarkId.MarkType of
      midPoint : VDoAdd := mptPlacemarks in FParams.MarksTypes;
      midLine  : VDoAdd := mptPaths in FParams.MarksTypes;
      midPoly  : VDoAdd := mptPolygons in FParams.MarksTypes;
    else
      VDoAdd := False;
    end;

    if VDoAdd and not FParams.IncludeHiddenMarks then begin
      VDoAdd := (VMarkId.Category as IMarkCategory).Visible;
    end;

    if VDoAdd then begin
      VDoAdd := FLonLatPolygon.Bounds.IsContainRect(VMark.Geometry.Bounds);
    end;

    if VDoAdd then begin
      VDoAdd :=
        IsLonLatGeometryInProjectedPolygon(
          VMark.Geometry,
          FProjectedPolygon,
          FProjection
        );
      if VDoAdd then begin
        VList.Add(VMarkId);
      end;
    end;

    ProgressFormUpdate(VProcessed, VTotal, VList.Count);
  end;

  if VList.Count <= 0 then begin
    ShowInfoMessageSync(rsThereAreNoPlacemarksToProcess);
    Exit;
  end;

  VMarkIdList := VList.MakeStaticAndClear;

  case FParams.Operation of
    mpoExport: begin
      TThread.Synchronize(nil,
        procedure
        begin
          ProgressInfo.Finish;
          FMarkDBGUI.ExportMarksList(VMarkIdList);
        end
      );
    end;

    mpoCopy: begin
      VTotal := VMarkIdList.Count;
      ProgressFormUpdate(Format(rsCopyingPlacemarksFmt, [VTotal]));
      VMarkDb := FMarkSystem.MarkDb;
      VMarkFactory := FMarkSystem.MarkDb.Factory;
      for I := 0 to VMarkIdList.Count - 1 do begin
        VOldMark := VMarkDb.GetMarkByID(VMarkIdList[I] as IMarkId);
        VNewMark := VMarkFactory.ReplaceCategory(VOldMark, FParams.Category);
        if VMarkDb.UpdateMark(nil, VNewMark) = nil then begin
          ShowErrorMessageSync(Format(rsCantCopyFmt, [VOldMark.Name]));
          Exit;
        end;
        ProgressFormUpdate(I+1, VTotal);
      end;
    end;

    mpoMove: begin
      VTotal := VMarkIdList.Count;
      ProgressFormUpdate(Format(rsMovingPlacemarksFmt, [VTotal]));
      VMarkDb := FMarkSystem.MarkDb;
      VMarkFactory := FMarkSystem.MarkDb.Factory;
      for I := 0 to VMarkIdList.Count - 1 do begin
        VOldMark := VMarkDb.GetMarkByID(VMarkIdList[I] as IMarkId);
        VNewMark := VMarkFactory.ReplaceCategory(VOldMark, FParams.Category);
        if VMarkDb.UpdateMark(VOldMark, VNewMark) = nil then begin
          ShowErrorMessageSync(Format(rsCantMoveFmt, [VOldMark.Name]));
          Exit;
        end;
        ProgressFormUpdate(I+1, VTotal);
      end;
    end;

    mpoDelete: begin
      ProgressFormUpdate(Format(rsDeletingPlacemarksFmt, [VMarkIdList.Count]));
      FMarkSystem.MarkDb.UpdateMarkList(VMarkIdList, nil);
    end;
  else
    Assert(False);
  end;
end;

procedure TThreadMarksProcess.ProgressFormUpdate(const ACaption: string);
begin
  ProgressInfo.SetCaption(ACaption);
  ProgressInfo.SetFirstLine('');
  ProgressInfo.SetSecondLine('');
  ProgressInfo.SetProcessedRatio(0);
end;

procedure TThreadMarksProcess.ProgressFormUpdate(const AProcessed, AToProcess: Int64);
begin
  ProgressInfo.SetFirstLine(SAS_STR_Processed + ' ' + IntToStr(AProcessed));
  ProgressInfo.SetSecondLine('');
  ProgressInfo.SetProcessedRatio(AProcessed / AToProcess);
end;

procedure TThreadMarksProcess.ProgressFormUpdate(const AProcessed, AToProcess, ASelected: Int64);
begin
  ProgressInfo.SetFirstLine(Format(rsSelectedFmt, [ASelected]));
  ProgressInfo.SetSecondLine(SAS_STR_Processed + ' ' + IntToStr(AProcessed));
  ProgressInfo.SetProcessedRatio(AProcessed / AToProcess);
end;

end.
