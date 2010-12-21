unit u_MapMarksLayer;

interface

uses
  GR32,
  GR32_Image,
  Ugeofun,
  u_MapViewPortState,
  u_MapLayerBasic;

type
  TMapMarksLayer = class(TMapLayerBasic)
  protected
    procedure DoRedraw; override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
    procedure MouseOnMyReg(var APWL: TResObj; xy: TPoint);
  end;

implementation

uses
  Types,
  Graphics,
  Classes,
  SysUtils,
  t_GeoTypes,
  t_CommonTypes,
  u_GlobalState,
  i_ICoordConverter,
  i_IBitmapLayerProvider,
  i_ILocalCoordConverter,
  u_MarksSimple,
  Unit1,
  u_WindowLayerBasic;

{ TMapMarksLayer }

constructor TMapMarksLayer.Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
begin
  inherited;
  FLayer.Bitmap.Font.Name := 'Tahoma';
  FLayer.Bitmap.Font.Style := [];
end;

procedure TMapMarksLayer.DoRedraw;
var
  VBitmapSize: TPoint;
  VRect: TRect;
  VProv: IBitmapLayerProvider;
begin
  inherited;
  if (GState.show_point <> mshNone) then begin
    VProv := GState.MarksBitmapProvider;
    FLayer.Bitmap.DrawMode:=dmBlend;
    FLayer.Bitmap.CombineMode:=cmMerge;
    FLayer.Bitmap.Clear(clBlack);
    VBitmapSize := MapViewSize;

    VRect.TopLeft := FBitmapCoordConverter.LocalPixel2MapPixel(Point(0, 0));
    VRect.BottomRight := FBitmapCoordConverter.LocalPixel2MapPixel(VBitmapSize);
    VProv.GetBitmapRect(FLayer.Bitmap, FBitmapCoordConverter.GetGeoConverter, VRect, FBitmapCoordConverter.GetZoom);
  end;
end;

procedure TMapMarksLayer.MouseOnMyReg(var APWL: TResObj; xy: TPoint);
var
  j:integer;
  arLL: TPointArray;
  poly: TDoublePointArray;
  VLonLatRect: TDoubleRect;
  VRect: TRect;
  VConverter: ICoordConverter;
  VMarkLonLatRect: TDoubleRect;
  VPixelPos: TPoint;
  VZoom: Byte;
  VMarksIterator: TMarksIteratorBase;
  VMark: TMarkFull;
  VMapRect: TDoubleRect;
  VLocalConverter: ILocalCoordConverter;
begin
  if GState.show_point = mshNone then exit;

  VRect.Left := xy.X - 8;
  VRect.Top := xy.Y - 16;
  VRect.Right := xy.X + 8;
  VRect.Bottom := xy.Y + 16;

  VLocalConverter := FVisualCoordConverter;
  VConverter := VLocalConverter.GetGeoConverter;
  VZoom := VLocalConverter.GetZoom;

  VMapRect := FVisualCoordConverter.LocalRect2MapRectFloat(VRect);
  VConverter.CheckPixelPosFloatStrict(VMapRect.TopLeft, VZoom, False);
  VConverter.CheckPixelPosFloatStrict(VMapRect.BottomRight, VZoom, False);
  VLonLatRect := VConverter.PixelRectFloat2LonLatRect(VMapRect, VZoom);
  VPixelPos := FVisualCoordConverter.LocalPixel2MapPixel(xy);

  VMarksIterator := FMain.GetMarksIterator(VZoom, VLonLatRect, GState.show_point);
  try
    While VMarksIterator.Next do begin
      VMark := VMarksIterator.Current;
      VMarkLonLatRect := VMark.LLRect;

      if((VLonLatRect.Right>VMarkLonLatRect.Left)and(VLonLatRect.Left<VMarkLonLatRect.Right)and
      (VLonLatRect.Bottom<VMarkLonLatRect.Top)and(VLonLatRect.Top>VMarkLonLatRect.Bottom))then begin
        if VMark.IsPoint then begin
          APWL.name:=VMark.name;
          APWL.descr:=VMark.Desc;
          APWL.numid:=IntToStr(VMark.id);
          APWL.find:=true;
          APWL.type_:=ROTpoint;
          exit;
        end else begin
          poly := VMark.Points;
          arLL := VConverter.LonLatArray2PixelArray(poly, VZoom);
          if VMark.IsLine then begin
            j:=1;
            while (j<length(poly)) do begin
              if CursorOnLinie(VPixelPos.x,VPixelPos.Y,arLL[j-1].x,arLL[j-1].y,arLL[j].x,arLL[j].y,(VMark.Scale1 div 2)+1)
              then begin
                APWL.name:=VMark.name;
                APWL.descr:=VMark.Desc;
                APWL.numid:=IntToStr(VMark.id);
                APWL.find:=true;
                APWL.type_:=ROTline;
                exit;
              end;
              inc(j);
            end
          end else begin
            if (PtInRgn(arLL,VPixelPos)) then begin
              if ((not(APWL.find))or((PolygonSquare(arLL)<APWL.S)and(APWL.S <> 0))) then begin
                APWL.S:=PolygonSquare(arLL);
                APWL.name:=VMark.name;
                APWL.descr:=VMark.Desc;
                APWL.numid:=IntToStr(VMark.id);
                APWL.find:=true;
                APWL.type_:=ROTPoly;
              end;
            end;
          end;
        end;
      end;
    end;
  finally
    VMarksIterator.Free;
 end;
end;

end.
