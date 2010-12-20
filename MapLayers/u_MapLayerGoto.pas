unit u_MapLayerGoto;

interface

uses
  Windows,
  Types,
  GR32,
  GR32_Image,
  t_GeoTypes,
  u_MapViewPortState,
  u_MapLayerBasic;

type
  TGotoLayer = class(TMapLayerFixedWithBitmap)
  protected
    FHideAfterTime: Cardinal;
    FBitmapSize: TPoint;
    FGoToSelIcon: TCustomBitmap32;
    procedure DoUpdateLayerLocation(ANewLocation: TFloatRect); override;
    function GetBitmapSizeInPixel: TPoint; override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
    destructor Destroy; override;
    procedure ShowGotoIcon(APoint: TDoublePoint);
  end;



implementation

uses
  SysUtils,
  u_GlobalState,
  u_WindowLayerBasic;

{ TGotoLayer }

constructor TGotoLayer.Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
begin
  inherited;
  FGoToSelIcon := TCustomBitmap32.Create;
  FGoToSelIcon.DrawMode := dmBlend;
  GState.LoadBitmapFromRes('ICONIII', FGoToSelIcon);
  FBitmapSize.X := FGoToSelIcon.Width;
  FBitmapSize.Y := FGoToSelIcon.Height;
  FLayer.Bitmap.Assign(FGoToSelIcon);
  FFixedOnBitmap.X := 7;
  FFixedOnBitmap.Y := 6;
end;

destructor TGotoLayer.Destroy;
begin
  FreeAndNil(FGoToSelIcon);
  inherited;
end;

procedure TGotoLayer.DoUpdateLayerLocation(ANewLocation: TFloatRect);
var
  VCurrTime: Cardinal;
begin
  if FHideAfterTime <> 0 then begin
    VCurrTime := GetTickCount;
    if (VCurrTime < FHideAfterTime) then begin
      if (VCurrTime < FHideAfterTime) then begin
        inherited;
      end else begin
        Visible := False;
      end;
    end else begin
      Visible := False;
    end;
  end else begin
    Visible := False;
  end;
end;

function TGotoLayer.GetBitmapSizeInPixel: TPoint;
begin
  Result := FBitmapSize;
end;

procedure TGotoLayer.ShowGotoIcon(APoint: TDoublePoint);
begin
  FFixedLonLat := APoint;
  FHideAfterTime := GetTickCount + 100000;
  Visible := True;
  UpdateLayerLocation(GetMapLayerLocationRect);
end;

end.
