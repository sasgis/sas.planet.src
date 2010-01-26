unit u_MapFillingLayer;

interface

uses
  Classes,
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
  protected
    procedure Execute; override;
  public
    constructor Create(ALayer: TBitmapLayer);
    destructor Destroy; override;
  end;

  TMapFillingLayer = class(TMapLayerBasic)
  protected
    FThread: TMapFillingThread;
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

constructor TMapFillingThread.Create(ALayer: TBitmapLayer);
begin

end;

destructor TMapFillingThread.Destroy;
begin

  inherited;
end;

procedure TMapFillingThread.Execute;
begin
  inherited;

end;

end.
 