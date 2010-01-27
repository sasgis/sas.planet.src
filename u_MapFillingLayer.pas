unit u_MapFillingLayer;

interface

uses
  Classes,
  SyncObjs,
  GR32,
  GR32_Image,
  GR32_Layers,
  t_GeoTypes,
  u_MapLayerBasic,
  uMapType;

type
  TMapFillingThread = class(TThread)
  private
    FLayer:TBitmapLayer;
    FStopThread: TEvent;
    FDrowActive: TEvent;
    FNeedRedrow: Boolean;
    FCSChangeScene: TCriticalSection;
  protected
    procedure Execute; override;
  public
    constructor Create(ALayer: TBitmapLayer);
    destructor Destroy; override;
    procedure PrepareToChangeScene;
    procedure ChangeScene;
  end;

  TMapFillingLayer = class(TMapLayerBasic)
  protected
    FThread: TMapFillingThread;

    procedure ResizeBitmap; override;
    procedure DoRedraw; override;
  public
    constructor Create(AParentMap: TImage32; ACenter: TPoint);
    destructor Destroy; override;
  end;

implementation

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

procedure TMapFillingThread.ChangeScene;
begin

end;

constructor TMapFillingThread.Create(ALayer: TBitmapLayer);
begin
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
begin
  inherited;

end;

procedure TMapFillingThread.PrepareToChangeScene;
begin

end;

end.
 