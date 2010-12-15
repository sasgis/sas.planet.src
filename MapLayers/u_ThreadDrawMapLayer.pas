unit u_ThreadDrawMapLayer;

interface

uses
  Classes,
  Types,
  SyncObjs,
  GR32,
  i_ILocalCoordConverter;

type
  TThreadDrawMapLayer = class(TThread)
  private
    FStopThread: TEvent;
    FDrowActive: TEvent;
    FDrawCS: TCriticalSection;
    FDrowStopCounter: Longint;
  protected
    FNeedStopDraw: Boolean;
    FBitmap: TCustomBitmap32;
    FBitmapSize: TPoint;
    FConverter: ILocalCoordConverter;
    procedure Execute; override;
    procedure DrawBitmap; virtual; abstract;
    procedure ResizeBitmap;
  public
    constructor Create(ABitmap: TCustomBitmap32);
    destructor Destroy; override;
    procedure Terminate; reintroduce;
    procedure ChangePosOrSize(ANewSize: TPoint; AConverter: ILocalCoordConverter);
    procedure StartDraw;
    procedure StopDraw;
  end;

implementation

{ TThreadDrawMapLayer }

procedure TThreadDrawMapLayer.ChangePosOrSize(ANewSize: TPoint;
  AConverter: ILocalCoordConverter);
begin
  StopDraw;
  try
    FBitmapSize := ANewSize;
    FConverter := AConverter;
  finally
    StartDraw;
  end;
end;

constructor TThreadDrawMapLayer.Create(ABitmap: TCustomBitmap32);
begin
  FBitmap := ABitmap;
  FBitmapSize := Point(FBitmap.Width, FBitmap.Height);
  FConverter := nil;
  FStopThread := TEvent.Create(nil, True, False, '');
  FDrowActive := TEvent.Create(nil, True, False, '');
  FDrawCS := TCriticalSection.Create;
end;

destructor TThreadDrawMapLayer.Destroy;
begin
  Terminate;
  inherited;
end;

procedure TThreadDrawMapLayer.Execute;
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
        FDrawCS.Acquire;
        try
          if FConverter <> nil then begin
            ResizeBitmap;
            DrawBitmap;
          end;
        finally
          FDrawCS.Release;
        end;
      end;
    end;
  end;
end;

procedure TThreadDrawMapLayer.ResizeBitmap;
begin
  FBitmap.Lock;
  try
    if (FBitmap.Width <> FBitmapSize.X) or (FBitmap.Height <> FBitmapSize.Y) then begin
      FBitmap.SetSize(FBitmapSize.X, FBitmapSize.Y);
    end;
  finally
    FBitmap.Unlock;
  end;
end;

procedure TThreadDrawMapLayer.StartDraw;
var
  VCouner: Longint;
begin
  VCouner := InterlockedDecrement(FDrowStopCounter);
  if VCouner = 0 then begin
    FNeedStopDraw := False;
    FDrowActive.SetEvent;
  end;
end;

procedure TThreadDrawMapLayer.StopDraw;
begin
  FNeedStopDraw := True;
  FDrowActive.ResetEvent;
  FDrawCS.Acquire;
  InterlockedIncrement(FDrowStopCounter);
  FDrawCS.Release;
end;

procedure TThreadDrawMapLayer.Terminate;
begin
  StopDraw;
  inherited Terminate;
  FStopThread.SetEvent;
end;

end.
