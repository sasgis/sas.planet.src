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
  GR32_Polygons,
  GR32_Transforms,
  GR32_Image,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_InternalPerformanceCounter,
  i_Projection,
  i_DoublePointsAggregator,
  i_GeometryProjectedFactory,
  i_GeometryLonLat,
  i_GeometryLonLatChangeable,
  i_GeometryProjected,
  i_PolyLineLayerConfig,
  i_PolygonLayerConfig,
  i_InterfaceListStatic,
  u_BaseInterfacedObject,
  u_MapLayerBasicNoBitmap;

type
  TMapLayerSingleGeometryBase = class(TMapLayerBasicNoBitmap)
  private
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
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

  TDrawablePolygonByList = class(TBaseInterfacedObject, IDrawablePolygon)
  private
    FList: IInterfaceListStatic;
  private
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
  public
    constructor Create(const AList: IInterfaceListStatic);
  end;

  TMapLayerSingleLine = class(TMapLayerSingleGeometryBase)
  private
    FSource: IGeometryLonLatLineChangeable;
    FPrevLine: IGeometryLonLatLine;
    FProjection: IProjection;
    FProjectedLine: IGeometryProjectedLine;
    FLocalConverter: ILocalCoordConverter;
    FPolygon: IDrawablePolygon;
    procedure OnChangedSource;
  protected
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
      const AConfig: ILineLayerConfig;
      const ASource: IGeometryLonLatLineChangeable
    );
  end;

  TMapLayerSinglePolygon = class(TMapLayerSingleGeometryBase)
  private
    FConfig: IPolygonLayerConfig;
    FSource: IGeometryLonLatPolygonChangeable;

    FFillColor: TColor32;
    FFillVisible: Boolean;

    FPrevLine: IGeometryLonLatPolygon;
    FProjection: IProjection;
    FProjectedLine: IGeometryProjectedPolygon;
    FLocalConverter: ILocalCoordConverter;
    FPolygonBorder: IDrawablePolygon;
    FPolygonFill: IDrawablePolygon;
    procedure OnChangedSource;
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
      const AConfig: IPolygonLayerConfig;
      const ASource: IGeometryLonLatPolygonChangeable
    );
  end;

implementation

uses
  SysUtils,
  i_InterfaceListSimple,
  u_InterfaceListSimple,
  u_DoublePointsAggregator,
  u_ListenerByEvent,
  u_GeometryFunc;

{ TLineLayerBase }

constructor TMapLayerSingleGeometryBase.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const AConfig: ILineLayerConfig
);
begin
  Assert(Assigned(AVectorGeometryProjectedFactory));
  Assert(Assigned(AConfig));
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    AView
  );
  FConfig := AConfig;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
  FPreparedPointsAggreagtor := TDoublePointsAggregator.Create;
end;

procedure TMapLayerSingleGeometryBase.DoConfigChange;
begin
  FLineColor := FConfig.LineColor;
  FLineWidth := FConfig.LineWidth;
  FLineVisible := ((AlphaComponent(FLineColor) > 0) and (FLineWidth > 0));
  FSimpleLineDraw := (FLineWidth = 1);
end;

procedure TMapLayerSingleGeometryBase.OnConfigChange;
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

procedure TMapLayerSingleGeometryBase.StartThreads;
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

{ TMapLayerSingleLine }

constructor TMapLayerSingleLine.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier, AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
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
    AVectorGeometryProjectedFactory,
    AConfig
  );
  FSource := ASource;
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnChangedSource),
    FSource.ChangeNotifier
  );
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
          FPrevLine := nil;
          SetNeedRedraw;
        end;
      end else begin
        SetNeedRedraw;
        Show;
      end;
    end else begin
      if Assigned(FPrevLine) then begin
        FPrevLine := nil;
        Hide;
      end;
    end;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerSingleLine.PaintLayer(
  ABuffer: TBitmap32;
  const ALocalConverter: ILocalCoordConverter
);
var
  VLonLatLine: IGeometryLonLatLine;
  VProjection: IProjection;
  VProjectedLine: IGeometryProjectedLine;
  VLocalConverter: ILocalCoordConverter;
  VDrawablePolygon: IDrawablePolygon;
  VPolygon: TPolygon32;
  VPolygonOutline: TPolygon32;
  VPolygonGrow: TPolygon32;
  VPathFixedPoints: TArrayOfFixedPoint;
begin
  if (AlphaComponent(FLineColor) = 0) or (FLineWidth < 1) then begin
    Exit;
  end;

  VLonLatLine := FPrevLine;
  VProjection := FProjection;
  VProjectedLine := FProjectedLine;
  VLocalConverter := FLocalConverter;
  VDrawablePolygon := FPolygon;

  if not Assigned(VLonLatLine) then begin
    VLonLatLine := FSource.GetStatic;
    FPrevLine := VLonLatLine;
    VProjectedLine := nil;
    VProjection := nil;
  end;

  if not Assigned(VLonLatLine) then begin
    Exit;
  end;
  if Assigned(VProjectedLine) then begin
    if not ALocalConverter.Projection.IsSame(VProjection) then begin
      VProjectedLine := nil;
    end;
  end;

  if not Assigned(VProjectedLine) then begin
    VDrawablePolygon := nil;
    VLocalConverter := nil;
    VProjection := ALocalConverter.Projection;
    VProjectedLine :=
      FVectorGeometryProjectedFactory.CreateProjectedLineByLonLatPath(
        VProjection,
        VLonLatLine,
        FPreparedPointsAggreagtor
      );
    FProjectedLine := VProjectedLine;
    FProjection := VProjection;
  end;

  if not Assigned(VProjectedLine) then begin
    Exit;
  end;

  if Assigned(VDrawablePolygon) then begin
    if not ALocalConverter.GetIsSameConverter(VLocalConverter) then begin
      VDrawablePolygon := nil;
      VLocalConverter := nil;
    end;
  end;

  if not Assigned(VDrawablePolygon) then begin
    VPolygon := nil;
    try
      ProjectedLine2GR32Polygon(
        VProjectedLine,
        ALocalConverter,
        am4times,
        VPathFixedPoints,
        VPolygon
      );
      if Assigned(VPolygon) then begin
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
    FLocalConverter := VLocalConverter;
  end;
  if not Assigned(VDrawablePolygon) then begin
    Exit;
  end;
  VDrawablePolygon.DrawFill(ABuffer, FLineColor);
end;

{ TMapLayerSinglePolygon }

constructor TMapLayerSinglePolygon.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
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

procedure PrepareFillAndBorderForSinglePolygon(
  const ASimpleLineDraw: Boolean;
  const ALineWidth: Integer;
  const AProjectedLine: IGeometryProjectedSinglePolygon;
  const ALocalConverter: ILocalCoordConverter;
  var ADrawablePolygonFill: IDrawablePolygon;
  var ADrawablePolygonBorder: IDrawablePolygon
);
var
  VPolygonGrow: TPolygon32;
  VPolygonOutline: TPolygon32;
  VPathFixedPoints: TArrayOfFixedPoint;
  VPolygon: TPolygon32;
begin
  VPolygon := nil;
  try
    ProjectedPolygon2GR32Polygon(AProjectedLine, ALocalConverter, am4times, VPathFixedPoints, VPolygon);
    if Assigned(VPolygon) then
    begin
      ADrawablePolygonFill := TDrawablePolygon32.CreateFromSource(VPolygon);
      ADrawablePolygonBorder := nil;
      if not ASimpleLineDraw then
      begin
        VPolygonOutline := VPolygon.Outline;
        try
          VPolygonGrow := VPolygonOutline.Grow(Fixed(ALineWidth / 2), 0.5);
          try
            ADrawablePolygonBorder := TDrawablePolygon32.CreateFromSource(VPolygonGrow);
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
end;

procedure PrepareFillAndBorder(
  const ASimpleLineDraw: Boolean;
  const ALineWidth: Integer;
  const AProjectedLine: IGeometryProjectedPolygon;
  const ALocalConverter: ILocalCoordConverter;
  var ADrawablePolygonFill: IDrawablePolygon;
  var ADrawablePolygonBorder: IDrawablePolygon
);
var
  VSinglePolygon: IGeometryProjectedSinglePolygon;
  VMultiPolygon: IGeometryProjectedMultiPolygon;
  i: Integer;
  VFillList: IInterfaceListSimple;
  VBorderList: IInterfaceListSimple;
begin
  if Supports(AProjectedLine, IGeometryProjectedSinglePolygon, VSinglePolygon) then begin
    PrepareFillAndBorderForSinglePolygon(
      ASimpleLineDraw,
      ALineWidth,
      VSinglePolygon,
      ALocalConverter,
      ADrawablePolygonFill,
      ADrawablePolygonBorder
    );
  end else if Supports(AProjectedLine, IGeometryProjectedMultiPolygon, VMultiPolygon) then begin
    if VMultiPolygon.Count = 0 then begin
      ADrawablePolygonFill := nil;
      ADrawablePolygonBorder := nil;
    end else if VMultiPolygon.Count = 1 then begin
      VSinglePolygon := VMultiPolygon.Item[0];
      PrepareFillAndBorderForSinglePolygon(
        ASimpleLineDraw,
        ALineWidth,
        VSinglePolygon,
        ALocalConverter,
        ADrawablePolygonFill,
        ADrawablePolygonBorder
      );
    end else begin
      VFillList := TInterfaceListSimple.Create;
      VBorderList := TInterfaceListSimple.Create;
      for i := 0 to VMultiPolygon.Count - 1 do begin
        VSinglePolygon := VMultiPolygon.Item[i];
        PrepareFillAndBorderForSinglePolygon(
          ASimpleLineDraw,
          ALineWidth,
          VSinglePolygon,
          ALocalConverter,
          ADrawablePolygonFill,
          ADrawablePolygonBorder
        );
        if Assigned(ADrawablePolygonFill) then begin
          VFillList.Add(ADrawablePolygonFill);
        end;
        if Assigned(ADrawablePolygonBorder) then begin
          VBorderList.Add(ADrawablePolygonBorder);
        end;
      end;
      if VFillList.Count = 0 then begin
        ADrawablePolygonFill := nil;
      end else if VFillList.Count = 1 then begin
        ADrawablePolygonFill := IDrawablePolygon(VFillList.Items[0]);
      end else begin
        ADrawablePolygonFill := TDrawablePolygonByList.Create(VFillList.MakeStaticAndClear);
      end;
      if VBorderList.Count = 0 then begin
        ADrawablePolygonBorder := nil;
      end else if VBorderList.Count = 1 then begin
        ADrawablePolygonBorder := IDrawablePolygon(VBorderList.Items[0]);
      end else begin
        ADrawablePolygonBorder := TDrawablePolygonByList.Create(VBorderList.MakeStaticAndClear);
      end;
    end;
  end;
end;

procedure TMapLayerSinglePolygon.DoConfigChange;
begin
  inherited;
  FFillColor := FConfig.FillColor;
  FFillVisible := (AlphaComponent(FFillColor) > 0);
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
          FPrevLine := nil;
          SetNeedRedraw;
        end;
      end else begin
        SetNeedRedraw;
        Show;
      end;
    end else begin
      if Assigned(FPrevLine) then begin
        FPrevLine := nil;
        Hide;
      end;
    end;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerSinglePolygon.PaintLayer(
  ABuffer: TBitmap32;
  const ALocalConverter: ILocalCoordConverter
);
var
  VLonLatLine: IGeometryLonLatPolygon;
  VProjection: IProjection;
  VProjectedLine: IGeometryProjectedPolygon;
  VDrawablePolygonFill: IDrawablePolygon;
  VDrawablePolygonBorder: IDrawablePolygon;
begin
  if not FFillVisible and not FLineVisible then begin
    Exit;
  end;

  VLonLatLine := FPrevLine;
  VProjection := FProjection;
  VProjectedLine := FProjectedLine;
  VDrawablePolygonFill := FPolygonFill;
  VDrawablePolygonBorder := FPolygonBorder;
  if not Assigned(VLonLatLine) then begin
    VLonLatLine := FSource.GetStatic;
    FPrevLine := VLonLatLine;
    VProjectedLine := nil;
    VProjection := nil;
  end;

  if not Assigned(VLonLatLine) then begin
    Exit;
  end;

  if Assigned(VProjectedLine) then begin
    if not ALocalConverter.Projection.IsSame(VProjection) then begin
      VProjection := nil;
      VProjectedLine := nil;
    end;
  end;

  if not Assigned(VProjectedLine) then begin
    VDrawablePolygonFill := nil;
    VDrawablePolygonBorder := nil;
    VProjection := ALocalConverter.Projection;
    VProjectedLine :=
      FVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
        VProjection,
        VLonLatLine,
        FPreparedPointsAggreagtor
      );
    FProjection := VProjection;
    FProjectedLine := VProjectedLine;
  end;

  if not Assigned(VProjectedLine) then begin
    Exit;
  end;

  if Assigned(VDrawablePolygonFill) or Assigned(VDrawablePolygonBorder) then begin
    if not ALocalConverter.GetIsSameConverter(FLocalConverter) then begin
      VDrawablePolygonFill := nil;
      VDrawablePolygonBorder := nil;
    end;
  end;

  if not Assigned(VDrawablePolygonFill) then begin
    PrepareFillAndBorder(FSimpleLineDraw, FLineWidth, VProjectedLine, ALocalConverter, VDrawablePolygonFill, VDrawablePolygonBorder);
    FPolygonFill := VDrawablePolygonFill;
    FPolygonBorder := VDrawablePolygonBorder;
    FLocalConverter := ALocalConverter;
  end;
  if not Assigned(VDrawablePolygonFill) then begin
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

  if Assigned(VDrawablePolygonBorder) then begin
    VDrawablePolygonBorder.DrawFill(ABuffer, FLineColor);
  end;
end;

{ TDrawablePolygonByList }

constructor TDrawablePolygonByList.Create(const AList: IInterfaceListStatic);
begin
  Assert(Assigned(AList));
  Assert(AList.Count > 1);
  inherited Create;
  FList := AList;
end;

procedure TDrawablePolygonByList.Draw(
  Bitmap: TCustomBitmap32;
  OutlineColor, FillColor: TColor32;
  Transformation: TTransformation
);
var
  i: Integer;
  VItem: IDrawablePolygon;
begin
  for i := 0 to FList.Count - 1 do begin
    VItem := IDrawablePolygon(FList.Items[i]);
    VItem.Draw(Bitmap, OutlineColor, FillColor, Transformation);
  end;
end;

procedure TDrawablePolygonByList.DrawEdge(
  Bitmap: TCustomBitmap32;
  Color: TColor32;
  Transformation: TTransformation
);
var
  i: Integer;
  VItem: IDrawablePolygon;
begin
  for i := 0 to FList.Count - 1 do begin
    VItem := IDrawablePolygon(FList.Items[i]);
    VItem.DrawEdge(Bitmap, Color, Transformation);
  end;
end;

procedure TDrawablePolygonByList.DrawFill(
  Bitmap: TCustomBitmap32;
  Color: TColor32;
  Transformation: TTransformation
);
var
  i: Integer;
  VItem: IDrawablePolygon;
begin
  for i := 0 to FList.Count - 1 do begin
    VItem := IDrawablePolygon(FList.Items[i]);
    VItem.DrawFill(Bitmap, Color, Transformation);
  end;
end;

end.
