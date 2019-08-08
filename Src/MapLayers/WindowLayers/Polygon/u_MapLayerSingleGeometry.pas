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

unit u_MapLayerSingleGeometry;

interface

uses
  GR32,
  GR32_Image,
  i_NotifierOperation,
  i_SimpleFlag,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_ConfigDataElement,
  i_TileRect,
  i_TileRectChangeable,
  i_InternalPerformanceCounter,
  i_Projection,
  i_DoublePointsAggregator,
  i_GeometryProjectedFactory,
  i_GeometryLonLat,
  i_GeometryLonLatChangeable,
  i_GeometryProjected,
  i_PolyLineLayerConfig,
  i_PolygonLayerConfig,
  i_ProjectedDrawableElement,
  u_MapLayerBasicNoBitmap;

type
  TMapLayerSingleGeometryBase = class(TMapLayerBasicNoBitmap)
  private
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FConfig: IConfigDataElement;
    FTileRectForShow: ITileRectChangeable;

    FPrepareDrawableCounter: IInternalPerformanceCounter;
    FDrawDrawableCounter: IInternalPerformanceCounter;

    FSourceChanged: ISimpleFlag;
    FFrawableTileRect: ITileRect;
    FDrawable: IProjectedDrawableElement;

    FPreparedPointsAggreagtor: IDoublePointsAggregator;
  protected
    property Factory: IGeometryProjectedFactory read FVectorGeometryProjectedFactory;
    property PreparedPointsAggreagtor: IDoublePointsAggregator read FPreparedPointsAggreagtor;
    procedure MarkSourceChanged;
    procedure OnConfigChange;
    procedure DoConfigChange; virtual; abstract;
    procedure StartThreads; override;
    procedure PaintLayer(
      ABuffer: TBitmap32;
      const ALocalConverter: ILocalCoordConverter
    ); override;
    function PrepareDrawable(
      const AProjection: IProjection;
      const AMapRect: TRect
    ): IProjectedDrawableElement; virtual; abstract;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AView: ILocalCoordConverterChangeable;
      const ATileRectForShow: ITileRectChangeable;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const AConfig: IConfigDataElement
    );
  end;

  TMapLayerSingleLine = class(TMapLayerSingleGeometryBase)
  private
    FSource: IGeometryLonLatLineChangeable;
    FPrevLine: IGeometryLonLatLine;
    FConfig: ILineLayerConfig;

    FLineColor: TColor32;
    FLineWidth: Integer;

    procedure OnChangedSource;
  protected
    procedure DoConfigChange; override;
    function PrepareDrawable(
      const AProjection: IProjection;
      const AMapRect: TRect
    ): IProjectedDrawableElement; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AView: ILocalCoordConverterChangeable;
      const ATileRectForShow: ITileRectChangeable;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const AConfig: ILineLayerConfig;
      const ASource: IGeometryLonLatLineChangeable
    );
  end;

  TMapLayerSinglePolygon = class(TMapLayerSingleGeometryBase)
  private
    FConfig: IPolygonLayerConfig;
    FSource: IGeometryLonLatPolygonChangeable;

    FLineColor: TColor32;
    FLineWidth: Integer;
    FFillColor: TColor32;

    FPrevLine: IGeometryLonLatPolygon;
    procedure OnChangedSource;
  protected
    procedure DoConfigChange; override;
    function PrepareDrawable(
      const AProjection: IProjection;
      const AMapRect: TRect
    ): IProjectedDrawableElement; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AView: ILocalCoordConverterChangeable;
      const ATileRectForShow: ITileRectChangeable;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const AConfig: IPolygonLayerConfig;
      const ASource: IGeometryLonLatPolygonChangeable
    );
  end;

implementation

uses
  Types,
  SysUtils,
  i_InterfaceListSimple,
  u_SimpleFlagWithInterlock,
  u_InterfaceListSimple,
  u_DoublePointsAggregator,
  u_ListenerByEvent,
  u_ProjectedDrawableElementByPolygon,
  u_GeometryFunc;

{ TLineLayerBase }

constructor TMapLayerSingleGeometryBase.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
  const ATileRectForShow: ITileRectChangeable;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const AConfig: IConfigDataElement
);
begin
  Assert(Assigned(AVectorGeometryProjectedFactory));
  Assert(Assigned(AConfig));
  Assert(Assigned(ATileRectForShow));
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    AView
  );
  FConfig := AConfig;
  FTileRectForShow := ATileRectForShow;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;

  FPrepareDrawableCounter := APerfList.CreateAndAddNewCounter('PrepareDrawable');
  FDrawDrawableCounter := APerfList.CreateAndAddNewCounter('DrawDrawable');

  FSourceChanged := TSimpleFlagWithInterlock.Create;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
  FPreparedPointsAggreagtor := TDoublePointsAggregator.Create;
end;

procedure TMapLayerSingleGeometryBase.MarkSourceChanged;
begin
  FSourceChanged.SetFlag;
end;

procedure TMapLayerSingleGeometryBase.OnConfigChange;
begin
  ViewUpdateLock;
  try
    FConfig.LockRead;
    try
      DoConfigChange;
      MarkSourceChanged;
    finally
      FConfig.UnlockRead;
    end;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerSingleGeometryBase.PaintLayer(
  ABuffer: TBitmap32;
  const ALocalConverter: ILocalCoordConverter
);
var
  VCounterContext: TInternalPerformanceCounterContext;
  VTileRect: ITileRect;
  VProjection: IProjection;
  VPixelRect: TRect;
  VSourceChanged: Boolean;
begin
  inherited;
  Assert(Assigned(ALocalConverter));
  VSourceChanged := FSourceChanged.CheckFlagAndReset;
  if not VSourceChanged then begin
    if not Assigned(FFrawableTileRect) then begin
      VSourceChanged := true;
    end;
  end;
  VTileRect := FTileRectForShow.GetStatic;
  if not VSourceChanged then begin
    if not FFrawableTileRect.IsEqual(VTileRect) then begin
      VSourceChanged := True;
    end;
  end;
  if VSourceChanged then begin
    VProjection := VTileRect.Projection;
    VPixelRect := VProjection.TileRect2PixelRect(VTileRect.Rect);
    VCounterContext := FPrepareDrawableCounter.StartOperation;
    try
      FDrawable := PrepareDrawable(VProjection, VPixelRect);
    finally
      FPrepareDrawableCounter.FinishOperation(VCounterContext);
    end;
    FFrawableTileRect := VTileRect;
  end;
  if Assigned(FDrawable) then begin
    VCounterContext := FDrawDrawableCounter.StartOperation;
    try
      FDrawable.Draw(ABuffer, ALocalConverter);
    finally
      FDrawDrawableCounter.FinishOperation(VCounterContext);
    end;
  end;
end;

procedure TMapLayerSingleGeometryBase.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

{ TMapLayerSingleLine }

constructor TMapLayerSingleLine.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier, AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
  const ATileRectForShow: ITileRectChangeable;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const AConfig: ILineLayerConfig;
  const ASource: IGeometryLonLatLineChangeable
);
begin
  Assert(Assigned(ASource));
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    AView,
    ATileRectForShow,
    AVectorGeometryProjectedFactory,
    AConfig
  );
  FSource := ASource;
  FConfig := AConfig;
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnChangedSource),
    FSource.ChangeNotifier
  );
end;

procedure TMapLayerSingleLine.DoConfigChange;
begin
  FLineColor := FConfig.LineColor;
  FLineWidth := FConfig.LineWidth;
end;

procedure TMapLayerSingleLine.OnChangedSource;
var
  VLine: IGeometryLonLatLine;
begin
  ViewUpdateLock;
  try
    VLine := FSource.GetStatic;
    if Assigned(VLine) then begin
      if Assigned(FPrevLine) then begin
        if not VLine.IsSameGeometry(FPrevLine) then begin
          FPrevLine := VLine;
          MarkSourceChanged;
          SetNeedRedraw;
        end;
      end else begin
        FPrevLine := VLine;
        MarkSourceChanged;
        SetNeedRedraw;
        Show;
      end;
    end else begin
      if Assigned(FPrevLine) then begin
        FPrevLine := nil;
        MarkSourceChanged;
        Hide;
      end;
    end;
  finally
    ViewUpdateUnlock;
  end;
end;

function TMapLayerSingleLine.PrepareDrawable(
  const AProjection: IProjection;
  const AMapRect: TRect
): IProjectedDrawableElement;
var
  VLonLatLine: IGeometryLonLatLine;
  VProjectedLine: IGeometryProjectedLine;
  VPathPoints: TArrayOfFixedPoint;
  VPolygon: TArrayOfArrayOfFixedPoint;
begin
  Result := nil;
  if (AlphaComponent(FLineColor) = 0) or (FLineWidth < 1) then begin
    Exit;
  end;
  VLonLatLine := FSource.GetStatic;
  if not Assigned(VLonLatLine) then begin
    Exit;
  end;
  VProjectedLine :=
    FVectorGeometryProjectedFactory.CreateProjectedLineByLonLatPath(
      AProjection,
      VLonLatLine,
      FPreparedPointsAggreagtor
    );
  if not Assigned(VProjectedLine) then begin
    Exit;
  end;
  VPolygon :=
    ProjectedLine2ArrayOfArray(
      VProjectedLine,
      AMapRect,
      VPathPoints
    );
  if Assigned(VPolygon) then begin
    if FLineWidth = 1 then begin
      Result := TDrawableSimpleLine.Create(AProjection, AMapRect.TopLeft, VPolygon, False, FLineColor);
    end else begin
      Result := TDrawablePolygonFill.Create(AProjection, AMapRect.TopLeft, BuildPolyPolyLine(VPolygon, False, FLineWidth), FLineColor);
    end;
  end;

end;


{ TMapLayerSinglePolygon }

constructor TMapLayerSinglePolygon.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
  const ATileRectForShow: ITileRectChangeable;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const AConfig: IPolygonLayerConfig;
  const ASource: IGeometryLonLatPolygonChangeable
);
begin
  Assert(Assigned(ASource));
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    AView,
    ATileRectForShow,
    AVectorGeometryProjectedFactory,
    AConfig
  );
  FConfig := AConfig;
  FSource := ASource;
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnChangedSource),
    FSource.ChangeNotifier
  );
end;

procedure PrepareFillAndBorder(
  const ALineColor: TColor32;
  const ALineWidth: Integer;
  const AFillColor: TColor32;
  const AProjectedLine: IGeometryProjectedSinglePolygon;
  const AProjection: IProjection;
  const AMapRect: TRect;
  var ADrawableList: IInterfaceListSimple
);
var
  VPathPoints: TArrayOfFixedPoint;
  VPolygon: TArrayOfArrayOfFixedPoint;
  VFill: IProjectedDrawableElement;
  VBorder: IProjectedDrawableElement;
begin
  VPolygon := ProjectedPolygon2ArrayOfArray(AProjectedLine, AMapRect, VPathPoints);
  if Assigned(VPolygon) then begin
    if AlphaComponent(AFillColor) > 0 then begin
      VFill := TDrawablePolygonFill.Create(AProjection, AMapRect.TopLeft, VPolygon, AFillColor);
      ADrawableList.Add(VFill);
    end;
    if (ALineWidth > 0) and (AlphaComponent(ALineColor) > 0) then begin
      if ALineWidth = 1 then begin
        VBorder := TDrawableSimpleLine.Create(AProjection, AMapRect.TopLeft, VPolygon, True, ALineColor);
      end else begin
        VBorder := TDrawablePolygonFill.Create(AProjection, AMapRect.TopLeft, BuildPolyPolyLine(VPolygon, True, ALineWidth), ALineColor);
      end;
      ADrawableList.Add(VBorder);
    end;
  end;
end;

procedure TMapLayerSinglePolygon.DoConfigChange;
begin
  inherited;
  FLineColor := FConfig.LineColor;
  FLineWidth := FConfig.LineWidth;
  FFillColor := FConfig.FillColor;
end;

procedure TMapLayerSinglePolygon.OnChangedSource;
var
  VLine: IGeometryLonLatPolygon;
begin
  ViewUpdateLock;
  try
    VLine := FSource.GetStatic;
    if Assigned(VLine) then begin
      if Assigned(FPrevLine) then begin
        if not VLine.IsSameGeometry(FPrevLine) then begin
          FPrevLine := VLine;
          MarkSourceChanged;
          SetNeedRedraw;
        end;
      end else begin
        FPrevLine := VLine;
        SetNeedRedraw;
        MarkSourceChanged;
        Show;
      end;
    end else begin
      if Assigned(FPrevLine) then begin
        FPrevLine := nil;
        MarkSourceChanged;
        Hide;
      end;
    end;
  finally
    ViewUpdateUnlock;
  end;
end;

function TMapLayerSinglePolygon.PrepareDrawable(
  const AProjection: IProjection;
  const AMapRect: TRect
): IProjectedDrawableElement;
var
  VLonLatLine: IGeometryLonLatPolygon;
  VProjectedLine: IGeometryProjectedPolygon;
  VDrawableList: IInterfaceListSimple;
  VSinglePolygon: IGeometryProjectedSinglePolygon;
  VMultiPolygon: IGeometryProjectedMultiPolygon;
  i: Integer;
begin
  Result := nil;
  if ((AlphaComponent(FLineColor) = 0) or (FLineWidth < 1)) and (AlphaComponent(FFillColor) = 0) then begin
    Exit;
  end;
    VLonLatLine := FSource.GetStatic;
  if not Assigned(VLonLatLine) then begin
    Exit;
  end;
    VProjectedLine :=
      FVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
        AProjection,
        VLonLatLine,
        FPreparedPointsAggreagtor
      );
  if not Assigned(VProjectedLine) then begin
    Exit;
  end;
  VDrawableList := TInterfaceListSimple.Create;

  if Supports(VProjectedLine, IGeometryProjectedSinglePolygon, VSinglePolygon) then begin
    PrepareFillAndBorder(FLineColor, FLineWidth, FFillColor, VSinglePolygon, AProjection, AMapRect, VDrawableList);
  end else if Supports(VProjectedLine, IGeometryProjectedMultiPolygon, VMultiPolygon) then begin
    for i := 0 to VMultiPolygon.Count - 1 do begin
      VSinglePolygon := VMultiPolygon.Item[i];
      PrepareFillAndBorder(FLineColor, FLineWidth, FFillColor, VSinglePolygon, AProjection, AMapRect, VDrawableList);
    end;
  end;

  if VDrawableList.Count = 0 then begin
    Exit;
  end;
  if VDrawableList.Count = 1 then begin
    Result := IProjectedDrawableElement(VDrawableList.First);
  end else begin
    Result := TDrawableByList.Create(AProjection, VDrawableList.MakeStaticAndClear);
  end;
end;

end.
