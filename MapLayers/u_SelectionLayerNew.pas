unit u_SelectionLayerNew;

interface

uses
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  u_MapViewPortState,
  u_MapLayerScaledBase;

type
  TSelectionLayerNew = class(TMapLayerScaledBase)
  protected
    procedure DoRedraw; override;
    procedure PaintLayer(Sender: TObject; Buffer: TBitmap32);
    function LonLatArrayToVisualFloatArray(APolygon: TExtendedPointArray): TExtendedPointArray;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
    procedure LoadConfig(AConfigProvider: IConfigDataProvider); override;
    procedure SaveConfig(AConfigProvider: IConfigDataWriteProvider); override;
  end;


implementation

uses
  i_ICoordConverter,
  Ugeofun;

{ TSelectionLayer }

constructor TSelectionLayerNew.Create(AParentMap: TImage32;
  AViewPortState: TMapViewPortState);
begin
  inherited;
  FLayerPositioned.OnPaint := PaintLayer;
end;

procedure TSelectionLayerNew.DoRedraw;
begin
  inherited;
  FLayerPositioned.Update;
end;

procedure TSelectionLayerNew.LoadConfig(AConfigProvider: IConfigDataProvider);
var
  VConfigProvider: IConfigDataProvider;
begin
  inherited;
  VConfigProvider := AConfigProvider.GetSubItem('VIEW');
  if VConfigProvider <> nil then begin
    Visible := VConfigProvider.ReadBool('ShowLastSelection',false);
  end;
end;

function TSelectionLayerNew.LonLatArrayToVisualFloatArray(
  APolygon: TExtendedPointArray): TExtendedPointArray;
var
  i: Integer;
  VPointsCount: Integer;
  VViewRect: TExtendedRect;
begin
  VPointsCount := Length(APolygon);
  SetLength(Result, VPointsCount);
  FViewPortState.LockRead;
  try
    for i := 0 to VPointsCount - 1 do begin
      Result[i] := FViewPortState.LonLat2VisiblePixel(APolygon[i]);
    end;
    VViewRect := ExtendedRect(FViewPortState.GetViewRectInVisualPixel);
  finally
    FViewPortState.UnLockRead;
  end;
  for i := 0 to VPointsCount - 1 do begin
    Result[i]
  end;
end;

procedure TSelectionLayerNew.PaintLayer(Sender: TObject; Buffer: TBitmap32);
begin

end;

procedure TSelectionLayerNew.SaveConfig(AConfigProvider: IConfigDataWriteProvider);
var
  VConfigProvider: IConfigDataWriteProvider;
begin
  inherited;
  VConfigProvider := AConfigProvider.GetOrCreateSubItem('VIEW');
  VConfigProvider.WriteBool('ShowLastSelection', Visible);
end;

end.
