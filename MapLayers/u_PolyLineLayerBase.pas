unit u_PolyLineLayerBase;

interface

uses
  GR32,
  GR32_Polygons,
  GR32_Transforms,
  GR32_Image,
  t_GeoTypes,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_InternalPerformanceCounter,
  i_LineOnMapEdit,
  i_ProjectionInfo,
  i_MarkerDrawable,
  i_DoublePointsAggregator,
  i_VectorGeometryProjectedFactory,
  i_VectorGeometryLocalFactory,
  i_GeometryLonLat,
  i_GeometryProjected,
  i_GeometryLocal,
  i_PolyLineLayerConfig,
  i_PolygonLayerConfig,
  u_MapLayerBasic;

type
  TLineLayerBase = class(TMapLayerBasicNoBitmap)
  private
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FVectorGeometryLocalFactory: IVectorGeometryLocalFactory;
    FConfig: ILineLayerConfig;

    FLineVisible: Boolean;
    FLineColor: TColor32;
    FLineWidth: Integer;
    FSimpleLineDraw: Boolean;

    FPreparedPointsAggreagtor: IDoublePointsAggregator;
  protected
    property Factory: IGeometryProjectedFactory read FVectorGeometryProjectedFactory;
    property PreparedPointsAggreagtor: IDoublePointsAggregator read FPreparedPointsAggreagtor;
    procedure OnConfigChange;
    procedure DoConfigChange; virtual;
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AView: ILocalCoordConverterChangeable;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const AVectorGeometryLocalFactory: IVectorGeometryLocalFactory;
      const AConfig: ILineLayerConfig
    );
  end;

  IDrawablePolygon = interface
    ['{EB682EC5-9DD3-4B9C-84AD-44A5EED26FA6}']
    procedure DrawFill(
      Bitmap: TCustomBitmap32;
      Color: TColor32;
      Transformation: TTransformation = nil
    );
    procedure DrawEdge(
      Bitmap: TCustomBitmap32;
      Color: TColor32;
      Transformation: TTransformation = nil
    );
    procedure Draw(
      Bitmap: TCustomBitmap32;
      OutlineColor, FillColor: TColor32;
      Transformation: TTransformation = nil
    );
  end;

  TDrawablePolygon32 = class(TPolygon32, IDrawablePolygon)
  public
    constructor Create; override;
    constructor CreateFromSource(ASource: TPolygon32);
  end;

  TPathLayerBase = class(TLineLayerBase)
  private
    FLine: IGeometryLonLatMultiLine;
    FProjectedLine: IGeometryProjectedMultiLine;
    FLocalLine: IGeometryLocalMultiLine;
    FPolygon: IDrawablePolygon;
  protected
    procedure ChangedSource;
    function GetLine(const ALocalConverter: ILocalCoordConverter): IGeometryLonLatMultiLine; virtual; abstract;
  protected
    procedure PaintLayer(
      ABuffer: TBitmap32;
      const ALocalConverter: ILocalCoordConverter
    ); override;
  end;

  TPolygonLayerBase = class(TLineLayerBase)
  private
    FConfig: IPolygonLayerConfig;
    FFillColor: TColor32;
    FFillVisible: Boolean;

    FLine: IGeometryLonLatMultiPolygon;
    FProjectedLine: IGeometryProjectedMultiPolygon;
    FLocalLine: IGeometryLocalMultiPolygon;
    FPolygonBorder: IDrawablePolygon;
    FPolygonFill: IDrawablePolygon;
  protected
    procedure ChangedSource;
    function GetLine(const ALocalConverter: ILocalCoordConverter): IGeometryLonLatMultiPolygon; virtual; abstract;
  protected
    procedure DoConfigChange; override;
    procedure PaintLayer(
      ABuffer: TBitmap32;
      const ALocalConverter: ILocalCoordConverter
    ); override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AView: ILocalCoordConverterChangeable;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const AVectorGeometryLocalFactory: IVectorGeometryLocalFactory;
      const AConfig: IPolygonLayerConfig
    );
  end;

  TPathEditLayer = class(TPathLayerBase)
  private
    FLineOnMapEdit: IPathOnMapEdit;
    FLine: ILonLatPathWithSelected;
    procedure OnLineChange;
  protected
    function GetLine(const ALocalConverter: ILocalCoordConverter): IGeometryLonLatMultiLine; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AView: ILocalCoordConverterChangeable;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const AVectorGeometryLocalFactory: IVectorGeometryLocalFactory;
      const ALineOnMapEdit: IPathOnMapEdit;
      const AConfig: ILineLayerConfig
    );
  end;

  TPolygonEditLayer = class(TPolygonLayerBase)
  private
    FLineOnMapEdit: IPolygonOnMapEdit;
    FLine: ILonLatPolygonWithSelected;
    procedure OnLineChange;
  protected
    function GetLine(const ALocalConverter: ILocalCoordConverter): IGeometryLonLatMultiPolygon; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AView: ILocalCoordConverterChangeable;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const AVectorGeometryLocalFactory: IVectorGeometryLocalFactory;
      const ALineOnMapEdit: IPolygonOnMapEdit;
      const AConfig: IPolygonLayerConfig
    );
  end;

  TPointsSetLayerBase = class(TMapLayerBasicNoBitmap)
  private
    FFirstPointMarker: IMarkerDrawableChangeable;
    FActivePointMarker: IMarkerDrawableChangeable;
    FNormalPointMarker: IMarkerDrawableChangeable;

    FNeedUpdatePoints: Boolean;
    FProjection: IProjectionInfo;
    FProjectedPoints: IDoublePointsAggregator;
    FActivePointIndex: Integer;
    procedure OnConfigChange;
  protected
    procedure ChangedSource;
    procedure PreparePoints(
      const AProjection: IProjectionInfo;
      out AProjectedPoints: IDoublePointsAggregator;
      out AActivePointIndex: Integer
    ); virtual; abstract;
  protected
    procedure PaintLayer(
      ABuffer: TBitmap32;
      const ALocalConverter: ILocalCoordConverter
    ); override;
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AView: ILocalCoordConverterChangeable;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const AFirstPointMarker: IMarkerDrawableChangeable;
      const AActivePointMarker: IMarkerDrawableChangeable;
      const ANormalPointMarker: IMarkerDrawableChangeable
    );
  end;

  TPathEditPointsSetLayer = class(TPointsSetLayerBase)
  private
    FLineOnMapEdit: IPathOnMapEdit;
    FLine: ILonLatPathWithSelected;
    procedure OnLineChange;
  protected
    procedure PreparePoints(
      const AProjection: IProjectionInfo;
      out AProjectedPoints: IDoublePointsAggregator;
      out AActivePointIndex: Integer
    ); override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AView: ILocalCoordConverterChangeable;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const ALineOnMapEdit: IPathOnMapEdit;
      const AFirstPointMarker: IMarkerDrawableChangeable;
      const AActivePointMarker: IMarkerDrawableChangeable;
      const ANormalPointMarker: IMarkerDrawableChangeable
    );
  end;

  TPolygonEditPointsSetLayer = class(TPointsSetLayerBase)
  private
    FLineOnMapEdit: IPolygonOnMapEdit;
    FLine: ILonLatPolygonWithSelected;
    procedure OnLineChange;
  protected
    procedure PreparePoints(
      const AProjection: IProjectionInfo;
      out AProjectedPoints: IDoublePointsAggregator;
      out AActivePointIndex: Integer
    ); override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AView: ILocalCoordConverterChangeable;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const ALineOnMapEdit: IPolygonOnMapEdit;
      const AFirstPointMarker: IMarkerDrawableChangeable;
      const AActivePointMarker: IMarkerDrawableChangeable;
      const ANormalPointMarker: IMarkerDrawableChangeable
    );
  end;

implementation

uses
  i_Listener,
  i_CoordConverter,
  i_EnumDoublePoint,
  u_DoublePointsAggregator,
  u_ListenerByEvent,
  u_EnumDoublePointMapPixelToLocalPixel,
  u_EnumDoublePointWithClip,
  u_EnumDoublePointFilterEqual;

{ TLineLayerBase }

constructor TLineLayerBase.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const AVectorGeometryLocalFactory: IVectorGeometryLocalFactory;
  const AConfig: ILineLayerConfig
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    AView
  );
  FConfig := AConfig;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FVectorGeometryLocalFactory := AVectorGeometryLocalFactory;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
  FPreparedPointsAggreagtor := TDoublePointsAggregator.Create;
end;

procedure TLineLayerBase.DoConfigChange;
begin
  FLineColor := FConfig.LineColor;
  FLineWidth := FConfig.LineWidth;
  FLineVisible := ((AlphaComponent(FLineColor) > 0) and (FLineWidth > 0));
  FSimpleLineDraw := (FLineWidth = 1);
end;

procedure TLineLayerBase.OnConfigChange;
begin
  ViewUpdateLock;
  try
    FConfig.LockRead;
    try
      DoConfigChange;
    finally
      FConfig.UnlockRead;
    end;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TLineLayerBase.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

{ TDrawablePolygon32 }

constructor TDrawablePolygon32.Create;
begin
  inherited;
  RefCounted := True;
end;

constructor TDrawablePolygon32.CreateFromSource(ASource: TPolygon32);
begin
  Create;
  Assign(ASource);
end;

{ TPathLayerSimple }

procedure TPathLayerBase.ChangedSource;
begin
  FLine := nil;
end;

procedure TPathLayerBase.PaintLayer(
  ABuffer: TBitmap32;
  const ALocalConverter: ILocalCoordConverter
);
var
  VLonLatLine: IGeometryLonLatMultiLine;
  VEnum: IEnumLocalPoint;
  VProjectedLine: IGeometryProjectedMultiLine;
  VLocalLine: IGeometryLocalMultiLine;
  VLocalRect: TRect;
  VRectWithDelta: TDoubleRect;
  VDrawablePolygon: IDrawablePolygon;
  VPolygon: TPolygon32;
  VPolygonOutline: TPolygon32;
  VPolygonGrow: TPolygon32;
  i: Integer;
  VPathFixedPoints: TArrayOfFixedPoint;
  VLine: IGeometryLocalLine;
  VIndex: Integer;
  VPoint: TDoublePoint;
begin
  if (AlphaComponent(FLineColor) = 0) or (FLineWidth < 1) then begin
    Exit;
  end;

  VLonLatLine := FLine;
  VProjectedLine := FProjectedLine;
  VLocalLine := FLocalLine;
  VDrawablePolygon := FPolygon;

  if VLonLatLine = nil then begin
    VLonLatLine := GetLine(ALocalConverter);
    FLine := VLonLatLine;
    VProjectedLine := nil;
  end;

  if VLonLatLine = nil then begin
    Exit;
  end;
  if VProjectedLine <> nil then begin
    if not ALocalConverter.ProjectionInfo.GetIsSameProjectionInfo(VProjectedLine.Projection) then begin
      VProjectedLine := nil;
    end;
  end;

  if VProjectedLine = nil then begin
    VLocalLine := nil;
    VProjectedLine := FVectorGeometryProjectedFactory.CreateProjectedPathByLonLatPath(ALocalConverter.ProjectionInfo, VLonLatLine, FPreparedPointsAggreagtor);
    FProjectedLine := VProjectedLine;
  end;

  if VProjectedLine = nil then begin
    Exit;
  end;

  if VLocalLine <> nil then begin
    if not ALocalConverter.GetIsSameConverter(VLocalLine.LocalConverter) then begin
      VLocalLine := nil;
    end;
  end;

  if VLocalLine = nil then begin
    VDrawablePolygon := nil;
    VLocalRect := ALocalConverter.GetLocalRect;
    VRectWithDelta.Left := VLocalRect.Left - 10;
    VRectWithDelta.Top := VLocalRect.Top - 10;
    VRectWithDelta.Right := VLocalRect.Right + 10;
    VRectWithDelta.Bottom := VLocalRect.Bottom + 10;

    VEnum :=
      TEnumDoublePointMapPixelToLocalPixel.Create(
        ALocalConverter,
        VProjectedLine.GetEnum
      );
    VEnum :=
      TEnumLocalPointClipByRect.Create(
        False,
        VRectWithDelta,
        VEnum
      );
    VEnum := TEnumLocalPointFilterEqual.Create(VEnum);
    VLocalLine := FVectorGeometryLocalFactory.CreateLocalPathByEnum(ALocalConverter, VEnum, FPreparedPointsAggreagtor);
    FLocalLine := VLocalLine;
  end;

  if VLocalLine = nil then begin
    Exit;
  end;

  if VDrawablePolygon = nil then begin
    VPolygon := TPolygon32.Create;
    try
      VPolygon.Closed := False;
      VPolygon.Antialiased := True;
      VPolygon.AntialiasMode := am4times;
      if VLocalLine.Count > 0 then begin
        for i := 0 to VLocalLine.Count - 1 do begin
          VLine := VLocalLine.Item[i];
          SetLength(VPathFixedPoints, VLine.Count);
          VIndex := 0;
          VEnum := VLine.GetEnum;
          while VEnum.Next(VPoint) do begin
            VPathFixedPoints[VIndex] := FixedPoint(VPoint.X, VPoint.Y);
            Inc(VIndex);
          end;
          VPolygon.AddPoints(VPathFixedPoints[0], VIndex);
          VPolygon.NewLine;
        end;
        if FLineWidth = 1 then begin
          VDrawablePolygon := TDrawablePolygon32.CreateFromSource(VPolygon);
        end else begin
          VPolygonOutline := VPolygon.Outline;
          try
            VPolygonGrow := VPolygonOutline.Grow(Fixed(FLineWidth / 2), 0.5);
            try
              VDrawablePolygon := TDrawablePolygon32.CreateFromSource(VPolygonGrow);
            finally
              VPolygonGrow.Free;
            end;
          finally
            VPolygonOutline.Free;
          end;
        end;
      end;
    finally
      VPolygon.Free;
    end;
    FPolygon := VDrawablePolygon;
  end;
  if VDrawablePolygon = nil then begin
    Exit;
  end;
  VDrawablePolygon.DrawFill(ABuffer, FLineColor);
end;

{ TPolygonLayerBase }

constructor TPolygonLayerBase.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const AVectorGeometryLocalFactory: IVectorGeometryLocalFactory;
  const AConfig: IPolygonLayerConfig
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
end;

procedure TPolygonLayerBase.ChangedSource;
begin
  FLine := nil;
end;

procedure TPolygonLayerBase.DoConfigChange;
begin
  inherited;
  FFillColor := FConfig.FillColor;
  FFillVisible := (AlphaComponent(FFillColor) > 0);
end;

procedure TPolygonLayerBase.PaintLayer(
  ABuffer: TBitmap32;
  const ALocalConverter: ILocalCoordConverter
);
var
  VLonLatLine: IGeometryLonLatMultiPolygon;
  VEnum: IEnumLocalPoint;
  VProjectedLine: IGeometryProjectedMultiPolygon;
  VLocalLine: IGeometryLocalMultiPolygon;
  VLocalRect: TRect;
  VRectWithDelta: TDoubleRect;
  VDrawablePolygonFill: IDrawablePolygon;
  VDrawablePolygonBorder: IDrawablePolygon;
  VPolygon: TPolygon32;
  VPolygonOutline: TPolygon32;
  VPolygonGrow: TPolygon32;
  i: Integer;
  VPathFixedPoints: TArrayOfFixedPoint;
  VLine: IGeometryLocalPolygon;
  VIndex: Integer;
  VPoint: TDoublePoint;
begin
  if not FFillVisible and not FLineVisible then begin
    Exit;
  end;

  VLonLatLine := FLine;
  VProjectedLine := FProjectedLine;
  VLocalLine := FLocalLine;
  VDrawablePolygonFill := FPolygonFill;
  VDrawablePolygonBorder := FPolygonBorder;
  if VLonLatLine = nil then begin
    VLonLatLine := GetLine(ALocalConverter);
    FLine := VLonLatLine;
    VProjectedLine := nil;
  end;

  if VLonLatLine = nil then begin
    Exit;
  end;

  if VProjectedLine <> nil then begin
    if not ALocalConverter.ProjectionInfo.GetIsSameProjectionInfo(VProjectedLine.Projection) then begin
      VProjectedLine := nil;
    end;
  end;

  if VProjectedLine = nil then begin
    VLocalLine := nil;
    VProjectedLine := FVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(ALocalConverter.ProjectionInfo, VLonLatLine, FPreparedPointsAggreagtor);
    FProjectedLine := VProjectedLine;
  end;

  if VProjectedLine = nil then begin
    Exit;
  end;

  if VLocalLine <> nil then begin
    if not ALocalConverter.GetIsSameConverter(VLocalLine.LocalConverter) then begin
      VLocalLine := nil;
    end;
  end;

  if VLocalLine = nil then begin
    VDrawablePolygonFill := nil;
    VDrawablePolygonBorder := nil;
    VLocalRect := ALocalConverter.GetLocalRect;
    VRectWithDelta.Left := VLocalRect.Left - 10;
    VRectWithDelta.Top := VLocalRect.Top - 10;
    VRectWithDelta.Right := VLocalRect.Right + 10;
    VRectWithDelta.Bottom := VLocalRect.Bottom + 10;

    VEnum :=
      TEnumDoublePointMapPixelToLocalPixel.Create(
        ALocalConverter,
        VProjectedLine.GetEnum
      );
    VEnum :=
      TEnumLocalPointClipByRect.Create(
        True,
        VRectWithDelta,
        VEnum
      );
    VEnum := TEnumLocalPointFilterEqual.Create(VEnum);
    VLocalLine := FVectorGeometryLocalFactory.CreateLocalPolygonByEnum(ALocalConverter, VEnum, FPreparedPointsAggreagtor);
    FLocalLine := VLocalLine;
  end;

  if VLocalLine = nil then begin
    Exit;
  end;

  if (VDrawablePolygonFill = nil) then begin
    VPolygon := TPolygon32.Create;
    try
      VPolygon.Closed := True;
      VPolygon.Antialiased := True;
      VPolygon.AntialiasMode := am4times;
      if VLocalLine.Count > 0 then begin
        for i := 0 to VLocalLine.Count - 1 do begin
          VLine := VLocalLine.Item[i];
          SetLength(VPathFixedPoints, VLine.Count + 1);
          VIndex := 0;
          VEnum := VLine.GetEnum;
          while VEnum.Next(VPoint) do begin
            VPathFixedPoints[VIndex] := FixedPoint(VPoint.X, VPoint.Y);
            Inc(VIndex);
          end;
          VPolygon.AddPoints(VPathFixedPoints[0], VIndex);
          VPolygon.NewLine;
        end;
        VDrawablePolygonFill := TDrawablePolygon32.CreateFromSource(VPolygon);
        VDrawablePolygonBorder := nil;
        if not FSimpleLineDraw then begin
          VPolygonOutline := VPolygon.Outline;
          try
            VPolygonGrow := VPolygonOutline.Grow(Fixed(FLineWidth / 2), 0.5);
            try
              VDrawablePolygonBorder := TDrawablePolygon32.CreateFromSource(VPolygonGrow);
            finally
              VPolygonGrow.Free;
            end;
          finally
            VPolygonOutline.Free;
          end;
        end;
      end;
    finally
      VPolygon.Free;
    end;
    FPolygonFill := VDrawablePolygonFill;
    FPolygonBorder := VDrawablePolygonBorder;
  end;
  if (VDrawablePolygonFill = nil) then begin
    Exit;
  end;
  if FFillVisible then begin
    if FLineVisible and FSimpleLineDraw then begin
      VDrawablePolygonFill.Draw(ABuffer, FLineColor, FFillColor);
    end else begin
      VDrawablePolygonFill.DrawFill(ABuffer, FFillColor);
    end;
  end else begin
    if FLineVisible and FSimpleLineDraw then begin
      VDrawablePolygonFill.DrawEdge(ABuffer, FLineColor);
    end;
  end;

  if VDrawablePolygonBorder <> nil then begin
    VDrawablePolygonBorder.DrawFill(ABuffer, FLineColor);
  end;
end;

{ TPathEditLayer }

constructor TPathEditLayer.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const AVectorGeometryLocalFactory: IVectorGeometryLocalFactory;
  const ALineOnMapEdit: IPathOnMapEdit;
  const AConfig: ILineLayerConfig
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
  FLineOnMapEdit := ALineOnMapEdit;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnLineChange),
    FLineOnMapEdit.GetChangeNotifier
  );
end;

function TPathEditLayer.GetLine(
  const ALocalConverter: ILocalCoordConverter
): IGeometryLonLatMultiLine;
begin
  Result := FLine;
end;

procedure TPathEditLayer.OnLineChange;
begin
  ViewUpdateLock;
  try
    FLine := FLineOnMapEdit.Path;
    if FLine.Count > 0 then begin
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

{ TPolygonEditLayer }

constructor TPolygonEditLayer.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const AVectorGeometryLocalFactory: IVectorGeometryLocalFactory;
  const ALineOnMapEdit: IPolygonOnMapEdit;
  const AConfig: IPolygonLayerConfig
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
  FLineOnMapEdit := ALineOnMapEdit;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnLineChange),
    FLineOnMapEdit.GetChangeNotifier
  );
end;

function TPolygonEditLayer.GetLine(
  const ALocalConverter: ILocalCoordConverter
): IGeometryLonLatMultiPolygon;
begin
  Result := FLine;
end;

procedure TPolygonEditLayer.OnLineChange;
begin
  ViewUpdateLock;
  try
    FLine := FLineOnMapEdit.Polygon;
    if FLine.Count > 0 then begin
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

{ TPointsSetLayerBase }

procedure TPointsSetLayerBase.ChangedSource;
begin
  FNeedUpdatePoints := True;
end;

constructor TPointsSetLayerBase.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const AFirstPointMarker: IMarkerDrawableChangeable;
  const AActivePointMarker: IMarkerDrawableChangeable;
  const ANormalPointMarker: IMarkerDrawableChangeable
);
var
  VListener: IListener;
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    AView
  );
  FFirstPointMarker := AFirstPointMarker;
  FActivePointMarker := AActivePointMarker;
  FNormalPointMarker := ANormalPointMarker;

  VListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  LinksList.Add(
    VListener,
    FFirstPointMarker.GetChangeNotifier
  );
  LinksList.Add(
    VListener,
    FActivePointMarker.GetChangeNotifier
  );
  LinksList.Add(
    VListener,
    FNormalPointMarker.GetChangeNotifier
  );
end;

procedure TPointsSetLayerBase.OnConfigChange;
begin
  ViewUpdateLock;
  try
    SetNeedRedraw
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TPointsSetLayerBase.PaintLayer(
  ABuffer: TBitmap32;
  const ALocalConverter: ILocalCoordConverter
);
var
  VProjection: IProjectionInfo;
  VPoints: IDoublePointsAggregator;
  VActiveIndex: Integer;
  VNeedUpdatePoints: Boolean;
  VLocalRect: TRect;
  VBitmapSize: TPoint;
  VPosOnMap: TDoublePoint;
  VPosOnBitmap: TDoublePoint;
  i: Integer;
  VFirstPointMarker: IMarkerDrawable;
  VActivePointMarker: IMarkerDrawable;
  VNormalPointMarker: IMarkerDrawable;
begin
  inherited;
  VFirstPointMarker := FFirstPointMarker.GetStatic;
  VActivePointMarker := FActivePointMarker.GetStatic;
  VNormalPointMarker := FNormalPointMarker.GetStatic;

  VProjection := FProjection;
  VPoints := FProjectedPoints;
  VActiveIndex := FActivePointIndex;
  VNeedUpdatePoints := FNeedUpdatePoints;
  if not VNeedUpdatePoints then begin
    if (VProjection = nil) or (VPoints = nil) then begin
      VNeedUpdatePoints := True;
    end else begin
      if not ALocalConverter.ProjectionInfo.GetIsSameProjectionInfo(VProjection) then begin
        VNeedUpdatePoints := True;
      end;
    end;
  end;
  if VNeedUpdatePoints then begin
    VProjection := ALocalConverter.ProjectionInfo;
    PreparePoints(VProjection, VPoints, VActiveIndex);
    FProjectedPoints := VPoints;
    FProjection := VProjection;
    FActivePointIndex := VActiveIndex;
    FNeedUpdatePoints := False;
  end;

  if VPoints = nil then begin
    Exit;
  end;

  if VPoints.Count > 0 then begin
    VLocalRect := ALocalConverter.GetLocalRect;
    VBitmapSize.X := VLocalRect.Right - VLocalRect.Left;
    VBitmapSize.Y := VLocalRect.Bottom - VLocalRect.Top;
    VPosOnMap := VPoints.Points[0];
    VPosOnBitmap := ALocalConverter.MapPixelFloat2LocalPixelFloat(VPosOnMap);
    if VActiveIndex = 0 then begin
      VActivePointMarker.DrawToBitmap(ABuffer, VPosOnBitmap);
    end else begin
      VFirstPointMarker.DrawToBitmap(ABuffer, VPosOnBitmap);
    end;
    for i := 1 to VPoints.Count - 1 do begin
      VPosOnMap := VPoints.Points[i];
      VPosOnBitmap := ALocalConverter.MapPixelFloat2LocalPixelFloat(VPosOnMap);
      if VActiveIndex = i then begin
        VActivePointMarker.DrawToBitmap(ABuffer, VPosOnBitmap);
      end else begin
        VNormalPointMarker.DrawToBitmap(ABuffer, VPosOnBitmap);
      end;
    end;
  end;
end;

procedure TPointsSetLayerBase.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

{ TPathEditPointsSetLayer }

constructor TPathEditPointsSetLayer.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const ALineOnMapEdit: IPathOnMapEdit;
  const AFirstPointMarker: IMarkerDrawableChangeable;
  const AActivePointMarker: IMarkerDrawableChangeable;
  const ANormalPointMarker: IMarkerDrawableChangeable
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    AView,
    AVectorGeometryProjectedFactory,
    AFirstPointMarker,
    AActivePointMarker,
    ANormalPointMarker
  );
  FLineOnMapEdit := ALineOnMapEdit;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnLineChange),
    FLineOnMapEdit.GetChangeNotifier
  );
end;

procedure TPathEditPointsSetLayer.OnLineChange;
begin
  ViewUpdateLock;
  try
    FLine := FLineOnMapEdit.Path;
    if FLine.Count > 0 then begin
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

procedure TPathEditPointsSetLayer.PreparePoints(
  const AProjection: IProjectionInfo;
  out AProjectedPoints: IDoublePointsAggregator;
  out AActivePointIndex: Integer
);
var
  VLine: ILonLatPathWithSelected;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VLonLatPoint: TDoublePoint;
  VPrevPoint: TDoublePoint;
  VProjectedPoint: TDoublePoint;
  i, j: Integer;
  VSigleLine: IGeometryLonLatLine;
  VIndex: Integer;
  VSegmentIndex: Integer;
  VPointIndex: Integer;
begin
  AProjectedPoints := nil;
  AActivePointIndex := -1;
  VLine := FLine;
  if VLine <> nil then begin
    if VLine.Count > 0 then begin
      AProjectedPoints := TDoublePointsAggregator.Create;
      VConverter := AProjection.GeoConverter;
      VZoom := AProjection.Zoom;
      VSegmentIndex := VLine.GetSelectedSegmentIndex;
      VPointIndex := VLine.GetSelectedPointIndex;
      VIndex := 0;
      for i := 0 to VLine.Count - 1 do begin
        VSigleLine := VLine.Item[i];
        for j := 0 to VSigleLine.Count - 1 do begin
          VLonLatPoint := VSigleLine.Points[j];
          VConverter.CheckLonLatPos(VLonLatPoint);
          VProjectedPoint := VConverter.LonLat2PixelPosFloat(VLonLatPoint, VZoom);
          if (VSegmentIndex = i) and (VPointIndex = j) then begin
            AProjectedPoints.Add(VProjectedPoint);
            VPrevPoint := VProjectedPoint;
            AActivePointIndex := VIndex;
            Inc(VIndex);
          end else begin
            if VIndex = 0 then begin
              AProjectedPoints.Add(VProjectedPoint);
              VPrevPoint := VProjectedPoint;
              Inc(VIndex);
            end else begin
              if (abs(VProjectedPoint.X - VPrevPoint.X) > 1) or (abs(VProjectedPoint.Y - VPrevPoint.Y) > 1) then begin
                AProjectedPoints.Add(VProjectedPoint);
                VPrevPoint := VProjectedPoint;
                Inc(VIndex);
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

{ TPolygonEditPointsSetLayer }

constructor TPolygonEditPointsSetLayer.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const ALineOnMapEdit: IPolygonOnMapEdit;
  const AFirstPointMarker: IMarkerDrawableChangeable;
  const AActivePointMarker: IMarkerDrawableChangeable;
  const ANormalPointMarker: IMarkerDrawableChangeable
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    AView,
    AVectorGeometryProjectedFactory,
    AFirstPointMarker,
    AActivePointMarker,
    ANormalPointMarker
  );
  FLineOnMapEdit := ALineOnMapEdit;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnLineChange),
    FLineOnMapEdit.GetChangeNotifier
  );
end;

procedure TPolygonEditPointsSetLayer.OnLineChange;
begin
  ViewUpdateLock;
  try
    FLine := FLineOnMapEdit.Polygon;
    if FLine.Count > 0 then begin
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

procedure TPolygonEditPointsSetLayer.PreparePoints(
  const AProjection: IProjectionInfo;
  out AProjectedPoints: IDoublePointsAggregator;
  out AActivePointIndex: Integer
);
var
  VLine: ILonLatPolygonWithSelected;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VLonLatPoint: TDoublePoint;
  VPrevPoint: TDoublePoint;
  VProjectedPoint: TDoublePoint;
  i, j: Integer;
  VSigleLine: IGeometryLonLatPolygon;
  VIndex: Integer;
  VSegmentIndex: Integer;
  VPointIndex: Integer;
begin
  AProjectedPoints := nil;
  AActivePointIndex := -1;
  VLine := FLine;
  if VLine <> nil then begin
    if VLine.Count > 0 then begin
      AProjectedPoints := TDoublePointsAggregator.Create;
      VConverter := AProjection.GeoConverter;
      VZoom := AProjection.Zoom;
      VSegmentIndex := VLine.GetSelectedSegmentIndex;
      VPointIndex := VLine.GetSelectedPointIndex;
      VIndex := 0;
      for i := 0 to VLine.Count - 1 do begin
        VSigleLine := VLine.Item[i];
        for j := 0 to VSigleLine.Count - 1 do begin
          VLonLatPoint := VSigleLine.Points[j];
          VConverter.CheckLonLatPos(VLonLatPoint);
          VProjectedPoint := VConverter.LonLat2PixelPosFloat(VLonLatPoint, VZoom);
          if (VSegmentIndex = i) and (VPointIndex = j) then begin
            AProjectedPoints.Add(VProjectedPoint);
            VPrevPoint := VProjectedPoint;
            AActivePointIndex := VIndex;
            Inc(VIndex);
          end else begin
            if VIndex = 0 then begin
              AProjectedPoints.Add(VProjectedPoint);
              VPrevPoint := VProjectedPoint;
              Inc(VIndex);
            end else begin
              if (abs(VProjectedPoint.X - VPrevPoint.X) > 1) or (abs(VProjectedPoint.Y - VPrevPoint.Y) > 1) then begin
                AProjectedPoints.Add(VProjectedPoint);
                VPrevPoint := VProjectedPoint;
                Inc(VIndex);
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

end.
