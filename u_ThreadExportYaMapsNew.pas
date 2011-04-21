unit u_ThreadExportYaMapsNew;

interface

uses
  Windows,
  Types,
  SysUtils,
  Classes,
  GR32,
  u_MapType,
  u_ResStrings,
  u_YaMobileWrite,
  t_GeoTypes,
  u_ThreadExportAbstract;

type
  TThreadExportYaMapsNew = class(TThreadExportAbstract)
  private
    FMapTypeArr: array of TMapType;
    FIsReplace: boolean;
    FExportPath: string;
    csat, cmap: byte;
  protected
    procedure ProcessRegion; override;
    function GetFilePath(Ax,Ay,Azoom,Aid:integer): string;
  public
    constructor Create(
      APath: string;
      APolygon: TArrayOfDoublePoint;
      Azoomarr: array of boolean;
      Atypemaparr: array of TMapType;
      Areplace: boolean
    );
  end;

implementation

uses
  c_CoordConverter,
  i_CoordConverter,
  i_TileIterator,
  u_TileIteratorStuped,
  i_BitmapTileSaveLoad,
  u_BitmapTileVampyreSaver,
  u_GlobalState;

constructor TThreadExportYaMapsNew.Create(
  APath: string;
  APolygon: TArrayOfDoublePoint;
  Azoomarr: array of boolean;
  Atypemaparr: array of TMapType;
  Areplace: boolean
);
var
  i: integer;
begin
  inherited Create(APolygon, Azoomarr);
  FExportPath := APath;
  FIsReplace := AReplace;
  setlength(FMapTypeArr, length(Atypemaparr));
  for i := 1 to length(Atypemaparr) do begin
    FMapTypeArr[i - 1] := Atypemaparr[i - 1];
  end;
end;

function TThreadExportYaMapsNew.GetFilePath(Ax,Ay,Azoom,Aid:integer): string;
 function heightTreeForZooms(i:integer):integer;
 var tilesInZoom,j,heightZoom:integer;
 begin
    heightZoom:=0;
    tilesInZoom:=4 shl (i shl 1);
    j:=1;
    while j<tilesInZoom do begin
      j:=j shl 8;
      inc(heightZoom);
    end;
    result:=heightZoom;
 end;

var path:string;
    xNode,yNode:integer;
    xChild,yChild:integer;
    fileX,fileY:integer;
    sizeNode:integer;
    i:integer;
    heightTreeForZoom:integer;
    block127:integer;
begin
  heightTreeForZoom:=heightTreeForZooms(Azoom);

  path:=FExportPath;
  path:=path+inttostr(Aid)+PathDelim;
  path:=path+inttostr(Azoom)+PathDelim;

  xNode:=0;
  yNode:=0;
  sizeNode:=1 shl ((4 * (heightTreeForZoom - 1) - 1));

  xChild:=0;
  yChild:=0;

  for i:=0 to heightTreeForZoom - 3 do begin
    xChild:=(AX-xNode)div sizeNode;
    yChild:=(AY-yNode)div sizeNode;
    xNode:=xNode+sizeNode*xChild;
    yNode:=yNode+sizeNode*yChild;
    sizeNode:=sizeNode shr 4;
    if (i < heightTreeForZoom - 3) then begin
      path:=path+IntToHex(xChild,1);
      path:=path+IntToHex(yChild,1);
      path:=path+PathDelim;
    end;
  end;
  path:=path+IntToHex(xChild,1);
  path:=path+IntToHex(yChild,1);
 
  fileX:=AX-xNode;
  fileY:=AY-yNode;

  block127:=((fileX shr 7) shl 1) or (fileY shr 7);

  if ((fileX shr 7)>0) then begin
    fileX:=fileX-128;
  end;
  if ((fileY shr 7)>0) then begin
    fileY:=fileY-128;
  end;

 path:=path+IntToHex(block127,1);
 result:=path;
end;

procedure TThreadExportYaMapsNew.ProcessRegion;
var
  i, j, xi, yi, hxyi, sizeim: integer;
  VZoom: Byte;
  bmp32, bmp322, bmp32crop: TCustomBitmap32;
  TileStream: TMemoryStream;
  tc: cardinal;
  VGeoConvert: ICoordConverter;
  JPGSaver, PNGSaver: IBitmapTileSaver;
  VTile: TPoint;
  VMapType: TMapType;
  VSaver: IBitmapTileSaver;
  Vmt: Byte;
  VTileIterators: array of ITileIterator;
begin
  inherited;
  if (FMapTypeArr[0] = nil) and (FMapTypeArr[1] = nil) and (FMapTypeArr[2] = nil) then begin
    exit;
  end;
  bmp32 := TCustomBitmap32.Create;
  bmp322 := TCustomBitmap32.Create;
  try
    hxyi := 1;
    sizeim := 128;
    JPGSaver := TVampyreBasicBitmapTileSaverJPG.create(cSat);
    PNGSaver := TVampyreBasicBitmapTileSaverPNGPalette.create(cMap);
    TileStream := TMemoryStream.Create;
    try
      bmp32.DrawMode := dmBlend;
      bmp322.DrawMode := dmBlend;
      bmp32crop := TCustomBitmap32.Create;
      bmp32crop.Width := sizeim;
      bmp32crop.Height := sizeim;
      VGeoConvert := GState.CoordConverterFactory.GetCoordConverterByCode(CYandexProjectionEPSG, CTileSplitQuadrate256x256);
      FTilesToProcess := 0;
      SetLength(VTileIterators,Length(FZooms));

      for i := 0 to Length(FZooms) - 1 do begin
        VZoom := FZooms[i];
        VTileIterators[i] := TTileIteratorStuped.Create(VZoom, FPolygLL, VGeoConvert);
        for j := 0 to 2 do begin
          if (FMapTypeArr[j] <> nil) and (not ((j = 0) and (FMapTypeArr[2] <> nil))) then begin
            FTilesToProcess := FTilesToProcess + VTileIterators[i].TilesTotal;
          end;
        end;
      end;
      try
        FTilesProcessed := 0;

        ProgressFormUpdateCaption(SAS_STR_ExportTiles, SAS_STR_AllSaves + ' ' + inttostr(FTilesToProcess) + ' ' + SAS_STR_files);
        ProgressFormUpdateOnProgress;

        tc := GetTickCount;
        for i := 0 to Length(FZooms) - 1 do begin
          VZoom := FZooms[i];
          while VTileIterators[i].Next(VTile) do begin
            if IsCancel then begin
              exit;
            end;
            for j := 0 to 2 do begin
              VMapType := FMapTypeArr[j];
              if (VMapType <> nil) and (not ((j = 0) and (FMapTypeArr[2] <> nil))) then begin
                bmp322.Clear;
                if (j = 2) and (FMapTypeArr[0] <> nil) then begin
                  FMapTypeArr[0].LoadTileUni(bmp322, VTile, VZoom, False, VGeoConvert, False, False, True);
                end;
                bmp32.Clear;
                if VMapType.LoadTileUni(bmp32, VTile, VZoom, False, VGeoConvert, False, False, True) then begin
                  if (j = 2) and (FMapTypeArr[0] <> nil) then begin
                    bmp322.Draw(0, 0, bmp32);
                    bmp32.Draw(0, 0, bmp322);
                  end;
                  if (j = 2) or (j = 0) then begin
                    VSaver := JPGSaver;
                    Vmt := 2;
                  end else begin
                    VSaver := PNGSaver;
                    Vmt := 1;
                  end;
                  for xi := 0 to hxyi do begin
                    for yi := 0 to hxyi do begin
                      bmp32crop.Clear;
                      bmp32crop.Draw(0, 0, bounds(sizeim * xi, sizeim * yi, sizeim, sizeim), bmp32);
                      TileStream.Clear;
                      VSaver.SaveToStream(bmp32crop, TileStream);
                      //запись тайла
                   end;
                  end;
                end;
                inc(FTilesProcessed);
                if (GetTickCount - tc > 1000) then begin
                  tc := GetTickCount;
                  ProgressFormUpdateOnProgress;
                end;
              end;
            end;
          end;
        end;
      finally
        for i := 0 to Length(FZooms)-1 do begin
          VTileIterators[i] := nil;
        end;
      end;
      ProgressFormUpdateOnProgress
    finally
      TileStream.Free;
    end;
  finally
    bmp32.Free;
    bmp322.Free;
  end;
end;



end.
