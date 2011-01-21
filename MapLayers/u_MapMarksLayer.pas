unit u_MapMarksLayer;

interface

uses
  GR32,
  GR32_Image,
  i_IUsedMarksConfig,
  i_MarksSimple,
  Ugeofun,
  u_MapViewPortState,
  i_ILocalCoordConverter,
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
    procedure StartThreads; override;
  public
    constructor Create(
      AParentMap: TImage32;
      AViewPortState: TMapViewPortState;
      AConfig: IUsedMarksConfig;
      AMarkDBGUI: TMarksDbGUIHelper
    );
    procedure MouseOnMyReg(var APWL: TResObj; xy: TPoint);
  end;

implementation

uses
  ActiveX,
  Types,
  Graphics,
  Classes,
  SysUtils,
  t_GeoTypes,
  t_CommonTypes,
  u_GlobalState,
  i_ICoordConverter,
  i_IBitmapLayerProvider,
  u_MapMarksBitmapLayerProviderByMarksSubset,
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
  VProv: IBitmapLayerProvider;
  VMarksSubset: IMarksSubset;
begin
  inherited;
  FMarksSubset := GetMarksSubset;
  VMarksSubset := FMarksSubset;
  if (VMarksSubset <> nil) and (not VMarksSubset.IsEmpty) then begin
    VProv := TMapMarksBitmapLayerProviderByMarksSubset.Create(VMarksSubset);
    FLayer.BeginUpdate;
    try
      FLayer.Bitmap.DrawMode:=dmBlend;
      FLayer.Bitmap.CombineMode:=cmMerge;
      FLayer.Bitmap.Clear(0);
      VProv.GetBitmapRect(FLayer.Bitmap, FBitmapCoordConverter);
    finally
      FLayer.EndUpdate;
      FLayer.Changed;
    end;
  end else begin
    FLayer.BeginUpdate;
    try
      FLayer.Bitmap.Clear(0);
    finally
      FLayer.EndUpdate;
      FLayer.Changed;
    end;
  end;
end;

procedure TMapMarksLayer.MouseOnMyReg(var APWL: TResObj; xy: TPoint);
var
  j:integer;
  VLineOnBitmap: TPointArray;
  VLonLatRect: TDoubleRect;
  VRect: TRect;
  VConverter: ICoordConverter;
  VMarkLonLatRect: TDoubleRect;
  VPixelPos: TPoint;
  VZoom: Byte;
  VMark: IMarkFull;
  VMapRect: TDoubleRect;
  VLocalConverter: ILocalCoordConverter;
  VMarksSubset: IMarksSubset;
  VMarksEnum: IEnumUnknown;
  VSquare:Double;
  i: Cardinal;
begin
  VMarksSubset := FMarksSubset;
  if VMarksSubset <> nil then begin
    if not VMarksSubset.IsEmpty then begin
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
      VMarksEnum := VMarksSubset.GetEnum;
      while VMarksEnum.Next(1, VMark, @i) = S_OK do begin
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
            VLineOnBitmap := VConverter.LonLatArray2PixelArray(VMark.Points, VZoom);
            if VMark.IsLine then begin
              for j := 1 to Length(VLineOnBitmap) - 1 do begin
                if
                  CursorOnLinie(
                    VPixelPos.x,
                    VPixelPos.Y,
                    VLineOnBitmap[j-1].x,
                    VLineOnBitmap[j-1].y,
                    VLineOnBitmap[j].x,
                    VLineOnBitmap[j].y,
                    (VMark.Scale1 div 2)+1
                  )
                then begin
                  APWL.name:=VMark.name;
                  APWL.descr:=VMark.Desc;
                  APWL.numid:=IntToStr(VMark.id);
                  APWL.find:=true;
                  APWL.type_:=ROTline;
                  exit;
                end;
              end;
            end else begin
              if (PtInRgn(VLineOnBitmap,VPixelPos)) then begin
                if ((not(APWL.find))or(APWL.S <> 0)) then begin
                  VSquare := PolygonSquare(VLineOnBitmap);
                  if (not APWL.find) or (VSquare<APWL.S) then begin
                    APWL.S:=VSquare;
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
      end;
    end;
  end;
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
        VList := FMarkDBGUI.MarksDB.GetVisibleCateroriesIDList(VZoom);
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
  if FConfigStatic.IsUseMarks then begin
    Redraw;
    Show;
  end else begin
    Hide;
    FMarksSubset := nil;
  end;
end;

procedure TMapMarksLayer.StartThreads;
begin
  inherited;
  OnConfigChange(nil);
end;

end.
