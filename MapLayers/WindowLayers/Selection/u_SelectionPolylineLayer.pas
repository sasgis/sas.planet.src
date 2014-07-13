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

unit u_SelectionPolylineLayer;

interface

uses
  GR32_Image,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_InternalPerformanceCounter,
  i_GeometryLonLat,
  i_GeometryLonLatFactory,
  i_GeometryProjectedFactory,
  i_GeometryLocalFactory,
  i_LineOnMapEdit,
  i_SelectionPolylineLayerConfig,
  u_PolyLineLayerBase,
  u_MapLayerBasic;

type
  TSelectionPolylineShadowLayer = class(TPolygonLayerBase)
  private
    FLineOnMapEdit: IPathOnMapEdit;
    FConfig: ISelectionPolylineShadowLayerConfig;
    FRadius: Double;
    FVectorGeometryLonLatFactory: IGeometryLonLatFactory;
    FLine: ILonLatPathWithSelected;
    procedure OnLineChange;
  protected
    function GetLine(const ALocalConverter: ILocalCoordConverter): IGeometryLonLatPolygon; override;
  protected
    procedure DoConfigChange; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AView: ILocalCoordConverterChangeable;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AVectorGeometryLocalFactory: IGeometryLocalFactory;
      const ALineOnMapEdit: IPathOnMapEdit;
      const AConfig: ISelectionPolylineShadowLayerConfig
    );
  end;

implementation

uses
  i_DoublePointFilter,
  u_EnumDoublePointLine2Poly,
  u_ListenerByEvent;

{ TSelectionPolylineShadowLayer }

constructor TSelectionPolylineShadowLayer.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AVectorGeometryLocalFactory: IGeometryLocalFactory;
  const ALineOnMapEdit: IPathOnMapEdit;
  const AConfig: ISelectionPolylineShadowLayerConfig
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    AView,
    AVectorGeometryProjectedFactory,
    AVectorGeometryLocalFactory,
    AConfig
  );
  FConfig := AConfig;
  FLineOnMapEdit := ALineOnMapEdit;
  FVectorGeometryLonLatFactory := AVectorGeometryLonLatFactory;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnLineChange),
    FLineOnMapEdit.GetChangeNotifier
  );
end;

procedure TSelectionPolylineShadowLayer.DoConfigChange;
begin
  inherited;
  FRadius := FConfig.Radius;
end;

function TSelectionPolylineShadowLayer.GetLine(
  const ALocalConverter: ILocalCoordConverter
): IGeometryLonLatPolygon;
var
  VLine: ILonLatPathWithSelected;
  VFilter: ILonLatPointFilter;
begin
  Result := nil;
  VLine := FLine;
  if VLine <> nil then begin
    VFilter :=
      TLonLatPointFilterLine2Poly.Create(
        FRadius,
        ALocalConverter.ProjectionInfo
      );
    Result :=
      FVectorGeometryLonLatFactory.CreateLonLatMultiPolygonByLonLatPathAndFilter(
        VLine.Geometry,
        VFilter
      );
  end;
end;

procedure TSelectionPolylineShadowLayer.OnLineChange;
begin
  ViewUpdateLock;
  try
    FLine := FLineOnMapEdit.Path;
    if not FLine.Geometry.IsEmpty then begin
      SetNeedRedraw;
      Show;
    end else begin
      Hide;
    end;
    ChangedSource;
  finally
    ViewUpdateUnlock;
  end;
end;


end.
