unit u_MapFillingLayer;

interface

uses
  Windows,
  Types,
  Classes,
  SysUtils,
  SyncObjs,
  GR32,
  GR32_Image,
  GR32_Layers,
  t_GeoTypes,
  i_ICoordConverter,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_JclNotify,
  u_MapViewPortState,
  u_MapLayerBasic,
  uMapType;

type
  TMapFillingLayer = class(TMapLayerBasic)
  protected
    FThread: TThread;
    FSourceMapType: TMapType;
    FSourceSelected: TMapType;
    FSourceZoom: integer;
    FMainMapChangeListener: IJclListener;
    FSourceMapChangeNotifier: IJclNotifier;
    procedure DoRedraw; override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
    destructor Destroy; override;
    procedure StartThreads; override;
    procedure SendTerminateToThreads; override;
    procedure LoadConfig(AConfigProvider: IConfigDataProvider); override;
    procedure SaveConfig(AConfigProvider: IConfigDataWriteProvider); override;
    procedure SetSourceMap(AMapType: TMapType; AZoom: integer);
    procedure SetScreenCenterPos(const AScreenCenterPos: TPoint; const AZoom: byte; AGeoConvert: ICoordConverter); override;
    procedure Hide; override;
    procedure Redraw; override;
    property SourceSelected: TMapType read FSourceSelected;
    property SourceZoom: integer read FSourceZoom;
    property SourceMapChangeNotifier: IJclNotifier read FSourceMapChangeNotifier;
  end;

implementation

uses
  Graphics,
  u_GlobalState,
  u_JclNotify,
  u_WindowLayerBasic,
  u_TileIteratorAbstract,
  u_TileIteratorStuped;

type
  TFillingMapListener = class(TJclBaseListener)
  private
    FOwnerItem: TMapFillingLayer;
  public
    constructor Create(AOwnerItem: TMapFillingLayer);
  end;

{ TFillingMapListener }

constructor TFillingMapListener.Create(
  AOwnerItem: TMapFillingLayer);
begin
  FOwnerItem := AOwnerItem;
end;

type
  TFillingMapMainMapChangeListener = class(TFillingMapListener)
  public
    procedure Notification(msg: IJclNotificationMessage); override;
  end;

{ TFillingMapMainMapChangeListener }

procedure TFillingMapMainMapChangeListener.Notification(
  msg: IJclNotificationMessage);
begin
  FOwnerItem.Redraw;
end;



type
  TMapFillingThread = class(TThread)
  private
    FLayer: TMapFillingLayer;
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

constructor TMapFillingLayer.Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
begin
  inherited;
  FLayer.Bitmap.DrawMode := dmBlend;
  FThread := TMapFillingThread.Create(Self);
  FMainMapChangeListener := TFillingMapMainMapChangeListener.Create(Self);
  FSourceMapChangeNotifier := TJclBaseNotifier.Create;
end;

destructor TMapFillingLayer.Destroy;
begin
  FSourceMapChangeNotifier := nil;
  FreeAndNil(FThread);
  FSourceMapType := nil;
  FMainMapChangeListener := nil;
  inherited;
end;

procedure TMapFillingLayer.DoRedraw;
begin
  if (FSourceMapType <> nil) and (FZoom <= FSourceZoom) and (FGeoConvert <> nil) then begin
    inherited;
    TMapFillingThread(FThread).PrepareToChangeScene;
    if FSourceSelected = nil then begin
      FSourceMapType := GState.ViewState.GetCurrentMap;
    end;
    FLayer.Bitmap.Clear(clBlack);
    TMapFillingThread(FThread).ChangeScene;
  end;
end;

procedure TMapFillingLayer.Hide;
begin
  inherited;
  TMapFillingThread(FThread).PrepareToChangeScene;
end;

procedure TMapFillingLayer.LoadConfig(AConfigProvider: IConfigDataProvider);
var
  VConfigProvider: IConfigDataProvider;
  VGUID: TGUID;
  VGUIDString: string;
  VFillingmaptype: TMapType;
  VZoom: Integer;
begin
  inherited;
  VConfigProvider := AConfigProvider.GetSubItem('FillingMap');
  if VConfigProvider <> nil then begin
    try
      VGUIDString := VConfigProvider.ReadString('Map','');
      if VGUIDString <> '' then begin
        VGUID := StringToGUID(VGUIDString);
        VFillingmaptype:=GState.GetMapFromID(VGUID);
      end else begin
        VFillingmaptype := nil;
      end;
    except
      VFillingmaptype := nil;
    end;
    VZoom := VConfigProvider.ReadInteger('Zoom', -1);
  end else begin
    VFillingmaptype := nil;
    VZoom := -1;
  end;
  SetSourceMap(VFillingmaptype, Vzoom);
end;

procedure TMapFillingLayer.Redraw;
begin
  if (FSourceMapType <> nil) and (FGeoConvert <> nil) and (FZoom <= FSourceZoom) then begin
    if not FLayer.Visible then begin
      FLayer.Visible := true;
    end;
  end else begin
    FLayer.Visible := false;
  end;
  inherited;

end;

procedure TMapFillingLayer.SaveConfig(
  AConfigProvider: IConfigDataWriteProvider);
var
  VConfigProvider: IConfigDataWriteProvider;
begin
  inherited;
  VConfigProvider := AConfigProvider.GetOrCreateSubItem('FillingMap');

  VConfigProvider.WriteInteger('Zoom', Self.SourceZoom);
  if Self.SourceSelected = nil then begin
    VConfigProvider.WriteString('Map','')
  end else begin
    VConfigProvider.WriteString('Map', Self.SourceSelected.GUIDString);
  end;
end;

procedure TMapFillingLayer.SendTerminateToThreads;
begin
  inherited;
  FThread.Terminate;
end;

procedure TMapFillingLayer.SetScreenCenterPos(
  const AScreenCenterPos: TPoint; const AZoom: byte;
  AGeoConvert: ICoordConverter);
var
  VFullRedraw: Boolean;
begin
  VFullRedraw := False;
  if (FGeoConvert = nil) or ((FGeoConvert.GetProjectionEPSG() <> 0) and (FGeoConvert.GetProjectionEPSG <> AGeoConvert.GetProjectionEPSG)) then begin
    VFullRedraw := True;
  end;
  if FZoom <> AZoom then begin
    VFullRedraw := True;
  end;
  if (FScreenCenterPos.X <> AScreenCenterPos.X) or (FScreenCenterPos.Y <> AScreenCenterPos.Y) then begin
    if not VFullRedraw then begin
      if IsNeedFullRedraw(AScreenCenterPos) then begin
        VFullRedraw := True;
      end else begin
        FScreenCenterPos := AScreenCenterPos;
      end;
    end;
  end;

  FScale := 1;
  FCenterMove := Point(0, 0);
  FFreezeInCenter := True;

  if VFullRedraw then begin
    TMapFillingThread(FThread).PrepareToChangeScene;
    FGeoConvert := AGeoConvert;
    FZoom := AZoom;
    FScreenCenterPos := AScreenCenterPos;
    if FSourceSelected = nil then begin
      FSourceMapType := GState.ViewState.GetCurrentMap;
    end;
    Redraw;
  end else begin
    RedrawPartial(AScreenCenterPos);
    FScreenCenterPos := AScreenCenterPos;
  end;
  Resize;
end;

procedure TMapFillingLayer.SetSourceMap(AMapType: TMapType; AZoom: integer);
var
  VFullRedraw: Boolean;
begin
  VFullRedraw := false;
  if (AMapType <> nil) then begin
    if (FSourceSelected <> AMapType) then begin
      VFullRedraw := True;
    end;
    GState.ViewState.MapChangeNotifier.Remove(FMainMapChangeListener);
  end else begin
    if (FSourceMapType <> GState.ViewState.GetCurrentMap) then begin
      VFullRedraw := True;
    end;
    GState.ViewState.MapChangeNotifier.Add(FMainMapChangeListener);
  end;
  if FSourceZoom <> AZoom then begin
    VFullRedraw := True;
  end;
  if VFullRedraw then begin
    if AMapType <> nil then begin
      FSourceMapType := AMapType;
      FSourceSelected := AMapType;
    end else begin
      FSourceSelected := AMapType;
      FSourceMapType := GState.ViewState.GetCurrentMap;
    end;
    FSourceZoom := AZoom;
    Redraw;
    FSourceMapChangeNotifier.Notify(nil);
  end;
  Resize;
end;

procedure TMapFillingLayer.StartThreads;
begin
  inherited;
  FThread.Resume;
end;

{ TFillingMapThread }

procedure TMapFillingThread.BuildBitmap;
var
  VZoom: Byte;
  VZoomSource: Byte;
  VSourceMapType: TMapType;
  VBmp: TCustomBitmap32;

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
  VTileIterator: TTileIteratorAbstract;
begin
  VBmp := TCustomBitmap32.Create;
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
      VTileIterator := TTileIteratorStuped.Create(VZoom, VSourceLonLatRect, VSourceGeoConvert);
      while VTileIterator.Next do begin
        if FNeedRedrow then begin
          break;
        end;
        VTile.X := VTileIterator.Current.X;
        VTile.Y := VTileIterator.Current.Y;
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

        if VCurrTilePixelRectSource.Bottom > VPixelSourceRect.Bottom then begin
          VTilePixelsToDraw.Bottom := VPixelSourceRect.Bottom - VCurrTilePixelRectSource.Top + 1;
          VCurrTilePixelRectSource.Bottom := VPixelSourceRect.Bottom;
        end;

        VCurrTilePixelRect.TopLeft := VSourceGeoConvert.PixelPos2OtherMap(VCurrTilePixelRectSource.TopLeft, VZoom, VGeoConvert);
        VCurrTilePixelRect.BottomRight := VSourceGeoConvert.PixelPos2OtherMap(VCurrTilePixelRectSource.BottomRight, VZoom, VGeoConvert);

        if FNeedRedrow then begin
          break;
        end;
        VCurrTilePixelRectAtBitmap.TopLeft := FLayer.MapPixel2BitmapPixel(VCurrTilePixelRect.TopLeft);
        VCurrTilePixelRectAtBitmap.BottomRight := FLayer.MapPixel2BitmapPixel(VCurrTilePixelRect.BottomRight);
        Inc(VCurrTilePixelRectAtBitmap.Bottom);
        Inc(VCurrTilePixelRectAtBitmap.Right);
        if FNeedRedrow then begin
          break;
        end;
        if VSourceMapType.LoadFillingMap(VBmp, VTile, VZoom, VZoomSource, @FNeedRedrow) then begin
          FLayer.FLayer.Bitmap.Lock;
          try
            FLayer.FLayer.Bitmap.Draw(VCurrTilePixelRectAtBitmap, VTilePixelsToDraw, Vbmp);
          finally
            FLayer.FLayer.Bitmap.UnLock;
          end;
        end;
      end;
    end;
    if not FNeedRedrow then begin
      Synchronize(UpdateLayer);
    end;
  finally
    FreeAndNil(VTileIterator);
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
  inherited Create(True);
  Priority := tpLowest;
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
    VWaitResult := WaitForMultipleObjects(Length(VHandles), @VHandles[0], False, INFINITE);
    case VWaitResult of
      WAIT_OBJECT_0:
      begin
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
