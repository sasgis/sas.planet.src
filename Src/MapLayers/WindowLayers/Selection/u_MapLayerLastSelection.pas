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

unit u_MapLayerLastSelection;

interface

uses
  GR32_Image,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_InternalPerformanceCounter,
  i_LastSelectionLayerConfig,
  i_LastSelectionInfo,
  i_GeometryLonLat,
  i_GeometryProjectedFactory,
  u_PolyLineLayerBase;

type
  TMapLayerLastSelection = class(TPolygonLayerBase)
  private
    FConfig: ILastSelectionLayerConfig;
    FLastSelectionInfo: ILastSelectionInfo;

    FLine: IGeometryLonLatPolygon;

    procedure OnChangeSelection;
  protected
    function GetLine(const ALocalConverter: ILocalCoordConverter): IGeometryLonLatPolygon; override;
    procedure DoConfigChange; override;
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AView: ILocalCoordConverterChangeable;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const AConfig: ILastSelectionLayerConfig;
      const ALastSelectionInfo: ILastSelectionInfo
    );
  end;


implementation

uses
  u_ListenerByEvent;

{ TSelectionLayer }

constructor TMapLayerLastSelection.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const AConfig: ILastSelectionLayerConfig;
  const ALastSelectionInfo: ILastSelectionInfo
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    AView,
    AVectorGeometryProjectedFactory,
    AConfig
  );
  FConfig := AConfig;
  FLastSelectionInfo := ALastSelectionInfo;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnChangeSelection),
    FLastSelectionInfo.GetChangeNotifier
  );
end;

procedure TMapLayerLastSelection.DoConfigChange;
begin
  inherited;
  SetNeedRedraw;
  Visible := FConfig.Visible;
  if Visible then begin
    OnChangeSelection;
  end;
end;

function TMapLayerLastSelection.GetLine(
  const ALocalConverter: ILocalCoordConverter
): IGeometryLonLatPolygon;
begin
  if Visible then begin
    Result := FLine;
  end else begin
    Result := nil;
  end;
end;

procedure TMapLayerLastSelection.OnChangeSelection;
begin
  ViewUpdateLock;
  try
    if FConfig.Visible then begin
      FLine := FLastSelectionInfo.Polygon;
      if (FLine <> nil) and (not FLine.IsEmpty) then begin
        SetNeedRedraw;
        Show;
      end else begin
        Hide;
      end;
      ChangedSource;
    end;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerLastSelection.StartThreads;
begin
  inherited;
  OnChangeSelection;
end;

end.
