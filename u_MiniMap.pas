unit u_MiniMap;

interface

uses
  Windows,
  Controls,
  Classes,
  Graphics,
  GR32,
  GR32_Image,
  GR32_Layers,
  UMapType;

type
  TMiniMap = class
  private
    FParentMap: TImage32;
    procedure LoadBitmaps;
  public
    size_dw,zooming,m_dwn:boolean;
    width,height,dx,dy:integer;
    pos:TPoint;
    alpha,zoom,z1mz2:integer;
    maptype: TMapType;
    PlusButton,MinusButton,SmMapBitmap,SmMapLayerBitmap:TBitmap32;
    LayerMinMap: TBitmapLayer;
    defoultMap:TBitmap32;
    constructor Create(AParentMap: TImage32);
    destructor Destroy; override;
    procedure sm_im_reset(x,y:integer; MainMapPos: TPoint);
    procedure sm_im_reset_type2(x,y:integer);
    procedure SetMiniMapVisible(visible:boolean; MainMapPos: TPoint);
  end;

implementation

uses
  Types,
  SysUtils,
  GR32_Polygons,
  pngimage,
  UImgfun,
  u_GlobalState;

const

  zoom_Sizes:array [1..24] of longint = (256,512,1024,2048,4096,8192,16384,32768,65536,
                                   131072,262144,524288,1048576,2097152,4194304,
                                   8388608,16777216,33554432,67108864,134217728,
                                   268435456,536870912,1073741824,2147483647);

{ TMiniMap }

constructor TMiniMap.Create(AParentMap: TImage32);
begin
  FParentMap := AParentMap;

  width:=GState.MainIni.readInteger('VIEW','SmMapW',160);
  height:=GState.MainIni.readInteger('VIEW','SmMapH',160);
  z1mz2:=GState.MainIni.readInteger('VIEW','SmMapDifference',4);
  Alpha:=GState.MainIni.readInteger('VIEW','SmMapAlpha',220);

  LayerMinMap := TBitmapLayer.Create(FParentMap.Layers);
  SmMapBitmap := TBitmap32.Create;

  LayerMinMap.bitmap.Font.Charset := RUSSIAN_CHARSET;
  LayerMinMap.Cursor := crHandPoint;
  LayerMinMap.bitmap.DrawMode := dmBlend;
  LoadBitmaps;
end;

destructor TMiniMap.Destroy;
begin
  FreeAndNil(SmMapBitmap);
  FreeAndNil(DefoultMap);
  FreeAndNil(PlusButton);
  FreeAndNil(MinusButton);
  inherited;
end;


procedure TMiniMap.LoadBitmaps;
var
  b: TPNGObject;
begin
  b := TPNGObject.Create;
  try
    b.LoadFromResourceName(HInstance, 'MAINMAP');
    DefoultMap := TBitmap32.Create;
    DefoultMap.Assign(b);
    b.LoadFromResourceName(HInstance, 'ICONI');
    PlusButton := TBitmap32.Create;
    PNGintoBitmap32(PlusButton,b);
    PlusButton.DrawMode:=dmTransparent;
    b.LoadFromResourceName(HInstance, 'ICONII');
    MinusButton := TBitmap32.Create;
    MinusButton.DrawMode:=dmTransparent;
    PNGintoBitmap32(MinusButton,b);
  finally
    FreeAndNil(b);
  end;
end;

procedure TMiniMap.SetMiniMapVisible(visible: boolean; MainMapPos: TPoint);
begin
 LayerMinMap.Visible:= visible;
 sm_im_reset(width div 2,height div 2, MainMapPos);
end;

procedure TMiniMap.sm_im_reset(x, y: integer; MainMapPos: TPoint);
var Polygon: TPolygon32;
    iLay:integer;
    btm:TBitmap32;
begin
  if LayerMinMap.Visible=false then exit;
  if GState.ShowStatusBar then begin
    LayerMinMap.location:=floatrect(bounds(FParentMap.Width-width-5,FParentMap.Height-height-17 - 5,width+5,height+5));
  end else begin
    LayerMinMap.location:=floatrect(bounds(FParentMap.Width-width-5,FParentMap.Height-height - 5,width+5,height+5));
  end;
  LayerMinMap.Bitmap.Width:=width+5;
  LayerMinMap.Bitmap.Height:=height+5;
  SmMapBitmap.Resampler:=CreateResampler(GState.Resampling);
  zoom:=GState.zoom_size-z1mz2;
  if zoom<1 then zoom:=1;
  if zoom>1 then begin
    if ((x=width div 2)and(y=height div 2))and(not size_dw) then begin
      sm_im_reset_type2(MainMapPos.x,MainMapPos.y);
    end;
    dx:=round(width*(FParentMap.Width/(1 shl (GState.zoom_size - zoom + 8))));
    dy:=round(height*(FParentMap.Height/(1 shl (GState.zoom_size - zoom + 8))));
    pos:=Point(x,y);
  end else begin
    btm:=TBitmap32.Create;
    if (maptype = nil) then begin
      if not(GState.sat_map_both.LoadTile(SmMapBitmap,128,128,1,true)) then begin
        SmMapBitmap.Assign(DefoultMap);
      end;
    end else begin
      if not(maptype.LoadTile(SmMapBitmap,128,128,1,true)) then begin
        SmMapBitmap.Assign(DefoultMap);
      end;
    end;
    for iLay:=0 to length(GState.MapType)-1 do begin
      if (GState.MapType[iLay].asLayer)and(GState.MapType[iLay].ShowOnSmMap) then begin
        GState.MapType[iLay].LoadTile(btm,128,128,1,false);
        btm.DrawMode:=dmBlend;
        SmMapBitmap.Draw(bounds(0,0,SmMapBitmap.width,SmMapBitmap.height),bounds(0,0,256,256),btm);
      end;
    end;
    if (x=width div 2)and(y=height div 2) then begin
      pos:=Point(round(MainMapPos.x*(width/zoom_Sizes[GState.zoom_size])),round(MainMapPos.y*(height/zoom_Sizes[GState.zoom_size])));
    end else begin
      pos:=Point(x,y);
    end;
    dx:=round(width*(FParentMap.Width/zoom_Sizes[GState.zoom_size]));
    dy:=round(height*(FParentMap.Height/zoom_Sizes[GState.zoom_size]));
    btm.Free;
  end;
  LayerMinMap.bitmap.Draw(bounds(5,5,width,height),bounds(0,0,256,256),SmMapBitmap);

  gamma(LayerMinMap.bitmap);

  Polygon := TPolygon32.Create;
  try
    Polygon.Antialiased:=true;
    Polygon.Add(FixedPoint((pos.x-dx div 2)+4-2,(pos.y-dy div 2)+4-2));
    Polygon.Add(FixedPoint((pos.x-dx div 2)+dx+4+2,(pos.y-dy div 2)+4-2));
    Polygon.Add(FixedPoint((pos.x-dx div 2)+dx+4+2,(pos.y-dy div 2)+dy+4+2));
    Polygon.Add(FixedPoint((pos.x-dx div 2)+4-2,(pos.y-dy div 2)+dy+4+2));
    with Polygon.Outline do try
      with Grow(Fixed(3.2 / 2), 0.5) do try
        FillMode := pfWinding;
        DrawFill(LayerMinMap.bitmap,SetAlpha(clNavy32,(GState.zoom_size-zoom)*43));
      finally
        Free;
      end;
    finally
      Free;
    end;
    Polygon.DrawFill(LayerMinMap.bitmap,SetAlpha(clWhite32,(GState.zoom_size-zoom)*35));
  finally
    Polygon.Free;
  end;
  Polygon := TPolygon32.Create;
  try
    Polygon.Antialiased:=False;
    Polygon.Add(FixedPoint(0,height+5));
    Polygon.Add(FixedPoint(0,0));
    Polygon.Add(FixedPoint(width+5,0));
    Polygon.Add(FixedPoint(width+5,5));
    Polygon.Add(FixedPoint(5,5));
    Polygon.Add(FixedPoint(5,height+5));
    Polygon.Draw(LayerMinMap.bitmap, clBlack32, clLightGray32);
  finally
    Polygon.Free;
  end;

  LayerMinMap.bitmap.Pixel[2,((height+5) div 2)-6]:=clBlack;
  LayerMinMap.bitmap.Pixel[2,((height+5) div 2)-2]:=clBlack;
  LayerMinMap.bitmap.Pixel[2,((height+5) div 2)+2]:=clBlack;
  LayerMinMap.bitmap.Pixel[2,((height+5) div 2)+6]:=clBlack;
  LayerMinMap.bitmap.ResetAlpha(alpha);
  if z1mz2>1 then LayerMinMap.bitmap.Draw(6,6,PlusButton);
  if zoom>1 then LayerMinMap.bitmap.Draw(19,6,MinusButton);
end;

procedure TMiniMap.sm_im_reset_type2(x, y: integer);
var bm:TBitmap32;
    pos_sm,d:TPoint;
    x128,y128,ilay:integer;
    m_t:TMapType;
begin
  bm := TBitmap32.Create;
  bm.DrawMode:=dmOpaque;
  try
    bm.Width := 256;
    bm.Height := 256;
    SmMapBitmap.Width := 256;
    SmMapBitmap.Height := 256;
    SmMapBitmap.Clear(Color32(GState.BGround));
    pos_sm := Point(x shr (GState.zoom_size-zoom),y shr (GState.zoom_size-zoom));
    if maptype = nil then begin
      m_t:=GState.sat_map_both;
    end else begin
      m_t:=maptype;
    end;
    Pos_sm := GState.sat_map_both.GeoConvert.Pos2OtherMap(Pos_sm, (zoom - 1) + 8, m_t.GeoConvert);
    d := Point((pos_sm.X-128),(pos_sm.y-128));
    if d.x < 0 then d.x := 256+d.x;
    if d.y < 0 then d.y := 256+d.y;
    d := Point((d.x mod 256),(d.y mod 256));

    x128 := -128;
    while (x128<=128) do begin
      y128:=-128;
      if (GState.CiclMap)or((pos_sm.X+x128<=zoom_Sizes[zoom])and(pos_sm.X+x128>=0)) then begin
        while (y128<=128) do begin
          if (pos_sm.y+y128<=zoom_Sizes[zoom])and(pos_sm.y+y128>=0) then begin
            bm.Clear(Color32(GState.BGround));
            if (m_t.tileexists(pos_sm.X+x128,pos_sm.y+y128,zoom)) then begin
              if not(m_t.LoadTile(bm,pos_sm.X+x128,pos_sm.y+y128,zoom,true)) then begin
                bm.Clear(Color32(GState.BGround));
              end;
            end else begin
              m_t.LoadTileFromPreZ(bm,pos_sm.x+x128,pos_sm.y+y128,zoom,true);
            end;
            SmMapBitmap.Draw((128+x128)-d.x,(128+y128)-d.y,bm);
          end;
          inc(y128,256);
        end;
      end;
      inc(x128,256);
    end;
    for iLay := 0 to length(GState.MapType)-1 do begin
      if (GState.MapType[iLay].asLayer)and(GState.MapType[iLay].ShowOnSmMap) then begin
        pos_sm := Point(X shr (GState.zoom_size-zoom),y shr (GState.zoom_size-zoom));
        Pos_sm := GState.sat_map_both.GeoConvert.Pos2OtherMap(Pos_sm, (zoom - 1) + 8, GState.MapType[iLay].GeoConvert);
        d := Point((pos_sm.X-128),(pos_sm.y-128));
        if d.x < 0 then d.x := 256 + d.x;
        if d.y < 0 then d.y := 256 + d.y;
        d := Point((d.x mod 256),(d.y mod 256));
        x128 := -128;
        while (x128<=128) do begin
          y128:=-128;
          if (GState.CiclMap)or((pos_sm.X+x128<=zoom_Sizes[zoom])and(pos_sm.X+x128>=0)) then begin
            while (y128<=128) do  begin
              if (pos_sm.y+y128<=zoom_Sizes[zoom])and(pos_sm.y+y128>=0) then begin
                bm.Clear(Color32(GState.BGround));
                bm.Draw(0,0,bounds((128+x128)-d.x,(128+y128)-d.y,256,256),SmMapBitmap);
                if (not((pos_sm.Y-y128<0)or(pos_sm.Y+y128>zoom_Sizes[zoom])) )
                  and (GState.MapType[iLay].TileExists(pos_sm.X+x128,pos_sm.y+y128,zoom)) then
                begin
                  GState.MapType[iLay].LoadTile(bm,pos_sm.X+x128,pos_sm.y+y128,zoom,true);
                end;
                bm.DrawMode:=dmBlend;
                SmMapBitmap.Draw((128+x128)-d.x,(128+y128)-d.y,bm);
              end;
              inc(y128,256);
            end;
          end;
          inc(x128,256);
        end;
      end;
    end;
  finally
    bm.Free;
  end;
end;

end.
