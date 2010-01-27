unit u_MapFillingLayer;

interface

uses
  Windows,
  Classes,
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
  end;


{ TFillingMapLayer }

constructor TMapFillingLayer.Create(AParentMap: TImage32; ACenter: TPoint);
begin
  inherited Create(AParentMap, ACenter);
  FLayer.Bitmap.DrawMode:=dmBlend;
end;

destructor TMapFillingLayer.Destroy;
begin

  inherited;
end;

procedure TMapFillingLayer.DoRedraw;
begin
  inherited;

end;

{ TFillingMapThread }

procedure TMapFillingThread.BuildBitmap;
var
  VZoom: Byte;
  VZoomSource: Byte;
  VMapType: TMapType;
  VTile: TPoint;
  VBmp: TBitmap32;
  VBitmapOnMapPixelRect: TRect;
  VPixelSourceRect: TRect;
  VTileSourceRect: TRect;

begin
  VBmp := TBitmap32.Create;
  try
    VZoom := FLayer.Zoom;
    VZoomSource := FLayer.FSourceZoom;
    VMapType := FLayer.FSourceMapType;



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

  inherited;
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
          FNeedRedrow := false;
        finally
          FCSChangeScene.Leave;
        end;
      end;
    else
      Break;
    end;
    BuildBitmap;
  end;
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
