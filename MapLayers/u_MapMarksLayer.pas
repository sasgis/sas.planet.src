unit u_MapMarksLayer;

interface

uses
  GR32,
  GR32_Image,
  i_IUsedMarksConfig,
  i_MarksSimple,
  Ugeofun,
  u_MapViewPortState,
  u_MarksDbGUIHelper,
  u_MapLayerBasic;

type
  TMapMarksLayer = class(TMapLayerBasic)
  private
    FConfig: IUsedMarksConfig;
    FConfigStatic: IUsedMarksConfigStatic;
    FMarkDBGUI: TMarksDbGUIHelper;
    FMarksSubset: IMarksSubset;
    procedure OnConfigChange(Sender: TObject);
    function GetMarksSubset: IMarksSubset;
  protected
    procedure DoRedraw; override;
  public
    constructor Create(
      AParentMap: TImage32;
      AViewPortState: TMapViewPortState;
      AConfig: IUsedMarksConfig;
      AMarkDBGUI: TMarksDbGUIHelper
    );
    procedure MouseOnMyReg(var APWL: TResObj; xy: TPoint);
    property Visible: Boolean read GetVisible write SetVisible;
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
  u_NotifyEventListener;

{ TMapMarksLayer }

constructor TMapMarksLayer.Create(
  AParentMap: TImage32;
  AViewPortState: TMapViewPortState;
  AConfig: IUsedMarksConfig;
  AMarkDBGUI: TMarksDbGUIHelper
);
begin
  inherited Create(AParentMap, AViewPortState);
  FConfig := AConfig;
  FMarkDBGUI := AMarkDBGUI;

  LinksList.Add(
    TNotifyEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
end;

procedure TMapMarksLayer.DoRedraw;
var
  VRect: TRect;
  VProv: IBitmapLayerProvider;
begin
  inherited;


  if (GState.show_point <> mshNone) then begin
    VProv := GState.MarksBitmapProvider;
    VRect := FBitmapCoordConverter.GetRectInMapPixel;
    FLayer.BeginUpdate;
    try
      FLayer.Bitmap.DrawMode:=dmBlend;
      FLayer.Bitmap.CombineMode:=cmMerge;
      FLayer.Bitmap.Clear(clBlack);
      VProv.GetBitmapRect(FLayer.Bitmap, FBitmapCoordConverter.GetGeoConverter, VRect, FBitmapCoordConverter.GetZoom);
    finally
      FLayer.EndUpdate;
      FLayer.Changed;
    end;
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
//  VMarksIterator: TMarksIteratorBase;
  VMark: IMarkFull;
  VMapRect: TDoubleRect;
  VLocalConverter: ILocalCoordConverter;
begin
  if GState.show_point = mshNone then exit;

  VRect.Left := xy.X - 8;
  VRect.Top := xy.Y - 16;
  VRect.Right := xy.X + 8;
  VRect.Bottom := xy.Y + 16;

  VLocalConverter := FBitmapCoordConverter;
  VConverter := VLocalConverter.GetGeoConverter;
  VZoom := VLocalConverter.GetZoom;

  VMapRect := FVisualCoordConverter.LocalRect2MapRectFloat(VRect);
  VConverter.CheckPixelRectFloat(VMapRect, VZoom);
  VLonLatRect := VConverter.PixelRectFloat2LonLatRect(VMapRect, VZoom);
  VPixelPos := FVisualCoordConverter.LocalPixel2MapPixel(xy);
//
//  VMarksIterator := FMain.GetMarksIterator(VZoom, VLonLatRect, GState.show_point);
//  try
//    While VMarksIterator.Next do begin
//      VMark := VMarksIterator.Current;
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
//    end;
//  finally
//    VMarksIterator.Free;
// end;
end;
function TMapMarksLayer.GetMarksSubset: IMarksSubset;
var
  VList: TList;
  VConverter: ILocalCoordConverter;
  VZoom: Byte;
  VMapPixelRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
  VGeoConverter: ICoordConverter;
begin
  VList := nil;
  if FConfigStatic.IsUseMarks then begin
    VConverter := FBitmapCoordConverter;
    if VConverter <> nil then begin
      VZoom := VConverter.GetZoom;
      if not FConfigStatic.IgnoreCategoriesVisible then begin
        VList := FMarkDBGUI.GetVisibleCateroriesIDList(VZoom);
      end;
      try
        if (VList <> nil) and (VList.Count = 0) then begin
          Result := nil;
        end else begin
          VGeoConverter := VConverter.GetGeoConverter;
          VMapPixelRect := VConverter.GetRectInMapPixelFloat;
          VGeoConverter.CheckPixelRectFloat(VMapPixelRect, VZoom);
          VLonLatRect := VGeoConverter.PixelRectFloat2LonLatRect(VMapPixelRect, VZoom);
          Result := FMarkDBGUI.MarksDB.MarksDb.GetMarksSubset(VLonLatRect, VList, FConfigStatic.IgnoreMarksVisible);
        end;
      finally
        VList.Free;
      end;
    end;
  end else begin
    Result := nil;
  end;
end;

procedure TMapMarksLayer.OnConfigChange(Sender: TObject);
begin
  FConfigStatic := FConfig.GetStatic;
  FMarksSubset := GetMarksSubset;
  if FMarksSubset <> nil then begin
    if not FMarksSubset.IsEmpty then begin
      Redraw;
      Show
    end else begin
      Hide;
    end;
  end else begin
    Hide
  end;
end;

end.
