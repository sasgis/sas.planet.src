unit u_MapFillingLayer;

interface

uses
  Windows,
  Classes,
  SysUtils,
  SyncObjs,
  GR32,
  GR32_Image,
  GR32_Layers,
  t_GeoTypes,
  u_MapLayerBasic,
  uMapType;

type
  TMapFillingLayer = class(TMapLayerBasic)
  protected
    FThread: TThread;
    FSourceMapType: TMapType;
    FSourceZoom: Byte;
    procedure DoRedraw; override;
  public
    constructor Create(AParentMap: TImage32; ACenter: TPoint);
    destructor Destroy; override;
    procedure SetSourceMap(AMapType: TMapType; AZoom: Byte);
  end;

implementation

type
  TMapFillingThread = class(TThread)
  private
    FLayer:TMapFillingLayer;
    FStopThread: TEvent;
    FDrowActive: TEvent;
    FNeedRedrow: Boolean;
    FCSChangeScene: TCriticalSection;
  protected
    procedure Execute; override;
    procedure UpdateLayer;
    procedure BuildBitmap;
  public
    constructor Create(ALayer: TMapFillingLayer);
    destructor Destroy; override;
    procedure PrepareToChangeScene;
    procedure ChangeScene;
    procedure FinishThread;
  end;


{ TFillingMapLayer }

constructor TMapFillingLayer.Create(AParentMap: TImage32; ACenter: TPoint);
begin
  inherited Create(AParentMap, ACenter);
  FLayer.Bitmap.DrawMode:=dmBlend;
  FThread := TMapFillingThread.Create(Self);
end;

destructor TMapFillingLayer.Destroy;
begin
  FreeAndNil(FThread);
  inherited;
end;

procedure TMapFillingLayer.DoRedraw;
begin
  inherited;

end;

procedure TMapFillingLayer.SetSourceMap(AMapType: TMapType; AZoom: Byte);
begin

end;

{ TFillingMapThread }

procedure TMapFillingThread.BuildBitmap;
var
  VZoom: Byte;
  VZoomSource: Byte;
  VSourceMapType: TMapType;
  VBmp: TBitmap32;

  {
    Прямоугольник пикселей растра в координатах текущей основной карты
  }
  VBitmapOnMapPixelRect: TRect;

  {
    Географические координаты растра
  }
  VSourceLonLatRect: TExtendedRect;

  {
    Прямоугольник пикселов текущего зума, покрывающий растр, в кооординатах
    карты для которой строится слой заполнения
  }
  VPixelSourceRect: TRect;

  {
    Прямоугольник тайлов текущего зума, покрывающий растр, в кооординатах
    карты, для которой строится слой заполнения
  }
  VTileSourceRect: TRect;
  {
    Текущий тайл в кооординатах карты, для которой строится слой заполнения
  }
  VTile: TPoint;
  {
    Прямоугольник пикслов текущего тайла в кооординатах карты,
    для которой строится слой заполнения
  }
  VCurrTilePixelRectSource: TRect;
  {
    Прямоугольник пикслов текущего тайла в кооординатах текущей карты
  }
  VCurrTilePixelRect: TRect;
  {
    Прямоугольник пикслов текущего тайла в кооординатах текущего растра
  }
  VCurrTilePixelRectAtBitmap: TRect;

  {
    Прямоугольник тайла подлежащий отображению на текущий растр
  }
  VTilePixelsToDraw: TRect;


  VSourceGeoConvert: ICoordConverter;
  VGeoConvert: ICoordConverter;
  i, j: integer;
begin
  VBmp := TBitmap32.Create;
  try
    VZoom := FLayer.Zoom;
    VZoomSource := FLayer.FSourceZoom;
    VSourceMapType := FLayer.FSourceMapType;
    VSourceGeoConvert := VSourceMapType.GeoConvert;
    VGeoConvert := FLayer.GeoConvert;
    VBitmapOnMapPixelRect.TopLeft := FLayer.BitmapPixel2MapPixel(Point(0, 0));
    VBitmapOnMapPixelRect.BottomRight := FLayer.BitmapPixel2MapPixel(FLayer.GetBitmapSizeInPixel);
    if not FNeedRedrow then begin
      VGeoConvert.CheckPixelRect(VBitmapOnMapPixelRect, VZoom, False);
      VSourceLonLatRect := VGeoConvert.PixelRect2LonLatRect(VBitmapOnMapPixelRect, VZoom);
      VPixelSourceRect := VSourceGeoConvert.LonLatRect2PixelRect(VSourceLonLatRect, VZoom);
      VTileSourceRect := VSourceGeoConvert.PixelRect2TileRect(VPixelSourceRect, VZoom);

      for i := VTileSourceRect.Left to VTileSourceRect.Right do begin
        if FNeedRedrow then break;
        VTile.X := i;
        for j:= VTileSourceRect.Top to VTileSourceRect.Bottom do begin
          VTile.Y := j;
          VCurrTilePixelRectSource := VSourceGeoConvert.TilePos2PixelRect(VTile, VZoom);
          VTilePixelsToDraw.TopLeft := Point(0, 0);
          VTilePixelsToDraw.Right := VCurrTilePixelRectSource.Right - VCurrTilePixelRectSource.Left + 1;
          VTilePixelsToDraw.Bottom := VCurrTilePixelRectSource.Bottom - VCurrTilePixelRectSource.Top + 1;

          if VCurrTilePixelRectSource.Left < VPixelSourceRect.Left then begin
            VTilePixelsToDraw.Left := VPixelSourceRect.Left - VCurrTilePixelRectSource.Left;
            VCurrTilePixelRectSource.Left := VPixelSourceRect.Left;
          end;

          if VCurrTilePixelRectSource.Top < VPixelSourceRect.Top then begin
            VTilePixelsToDraw.Top := VPixelSourceRect.Top - VCurrTilePixelRectSource.Top;
            VCurrTilePixelRectSource.Top := VPixelSourceRect.Top;
          end;

          if VCurrTilePixelRectSource.Right > VPixelSourceRect.Right then begin
            VTilePixelsToDraw.Right := VPixelSourceRect.Right - VCurrTilePixelRectSource.Left + 1;
            VCurrTilePixelRectSource.Right := VPixelSourceRect.Right;
          end;

          if VCurrTilePixelRectSource.Bottom < VPixelSourceRect.Bottom then begin
            VTilePixelsToDraw.Bottom := VPixelSourceRect.Bottom - VCurrTilePixelRectSource.Top;
            VCurrTilePixelRectSource.Bottom := VPixelSourceRect.Bottom;
          end;

          VCurrTilePixelRect := VSourceGeoConvert.Pos2OtherMap(VCurrTilePixelRectSource, VZoom, VGeoConvert);

          if FNeedRedrow then break;
          VCurrTilePixelRectAtBitmap.TopLeft := FLayer.MapPixel2BitmapPixel(VCurrTilePixelRect.TopLeft);
          VCurrTilePixelRectAtBitmap.TopLeft := FLayer.MapPixel2BitmapPixel(VCurrTilePixelRect.TopLeft);
          if FNeedRedrow then break;
          if VSourceMapType.LoadFillingMap(VBmp, VTile, VZoom, VZoomSource, @FNeedRedrow) then begin
            FLayer.FLayer.Bitmap.Lock;
            try
              FLayer.FLayer.Bitmap.Draw(VCurrTilePixelRectAtBitmap, VTilePixelsToDraw, Vbmp);
            finally
              FLayer.FLayer.Bitmap.UnLock;
            end;
          end;
          Synchronize(UpdateLayer);
        end;
      end;
    end;
  finally
    VBmp.Free;
  end;
end;

procedure TMapFillingThread.ChangeScene;
begin
  FCSChangeScene.Enter;
  try
    FDrowActive.SetEvent;
  finally
    FCSChangeScene.Leave;
  end;
end;

constructor TMapFillingThread.Create(ALayer: TMapFillingLayer);
begin
  inherited Create(false);
  FLayer := ALayer;
  FStopThread := TEvent.Create(nil, True, False, '');
  FDrowActive := TEvent.Create(nil, True, False, '');
  FCSChangeScene := TCriticalSection.Create;
  FNeedRedrow := False;
end;

destructor TMapFillingThread.Destroy;
begin
  FinishThread;
  FreeAndNil(FStopThread);
  FreeAndNil(FDrowActive);
  FreeAndNil(FCSChangeScene);
  inherited;
  FLayer := nil;
end;

procedure TMapFillingThread.Execute;
var
  VHandles: array [0..1] of THandle;
  VWaitResult: DWORD;
begin
  inherited;
  VHandles[0] := FDrowActive.Handle;
  VHandles[1] := FStopThread.Handle;
  while not Terminated do begin
    VWaitResult := WaitForMultipleObjects(Length(VHandles), @VHandles[0], False, 0);
    case VWaitResult of
      WAIT_OBJECT_0: begin
        FCSChangeScene.Enter;
        try
          FDrowActive.ResetEvent;
          FNeedRedrow := false;
        finally
          FCSChangeScene.Leave;
        end;
        BuildBitmap;
      end;
    end;
  end;
end;

procedure TMapFillingThread.FinishThread;
begin
  FNeedRedrow := True;
  Terminate;
  FStopThread.SetEvent;
end;

procedure TMapFillingThread.PrepareToChangeScene;
begin
  FCSChangeScene.Enter;
  try
    FNeedRedrow := True;
    FDrowActive.ResetEvent;
  finally
    FCSChangeScene.Leave;
  end;
end;

procedure TMapFillingThread.UpdateLayer;
begin
  if not FNeedRedrow then begin
    FLayer.FLayer.Update;
  end;
end;

end.
