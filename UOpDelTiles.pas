unit UOpDelTiles;

interface

uses
  Windows,
  SysUtils,
  Classes,
  t_GeoTypes,
  UMapType,
  u_ThreadRegionProcessAbstract,
  UResStrings;

type
  TThreadDeleteTiles = class(TThreadRegionProcessAbstract)
  private
    FZoom:byte;
    FMapType:TMapType;
    FDeletedCount:integer;
    DelBytes:boolean;
    DelBytesNum:integer;
  protected
    procedure ProcessRegion; override;
    procedure ProgressFormUpdateOnProgress;
  public
    constructor Create(
      APolyLL: TExtendedPointArray;
      Azoom: byte;
      Atypemap: TMapType;
      ADelByte:boolean;
      ADelBytesNum:integer
    );
  end;

implementation

uses
  Ugeofun;

constructor TThreadDeleteTiles.Create(
  APolyLL: TExtendedPointArray;
  Azoom:byte;
  Atypemap:TMapType;
  ADelByte:boolean;
  ADelBytesNum:integer
);
begin
  inherited Create(APolyLL);
  FDeletedCount:=0;
  FZoom:=Azoom;
  FMapType:=Atypemap;
  DelBytes:=ADelByte;
  DelBytesNum := ADelBytesNum;
end;

procedure TThreadDeleteTiles.ProcessRegion;
var
  i,j:integer;
  VTile: TPoint;
  polyg:TPointArray;
  max,min:TPoint;
begin
  polyg := FMapType.GeoConvert.LonLatArray2PixelArray(FPolygLL, (FZoom - 1));
  FTilesToProcess:=GetDwnlNum(min,max,Polyg,true);
  ProgressFormUpdateCaption(
    '',
    SAS_STR_Deleted+' '+inttostr(FTilesToProcess)+' '+SAS_STR_files+' (x'+inttostr(FZoom)+')'
  );
  i:=min.x;
  while (i<max.X)and(not IsCancel) do begin
    VTile.X := i shr 8;
    j:=min.Y;
    while (j<max.y)and(not IsCancel) do  begin
      VTile.Y := j shr 8;
      if RgnAndRgn(Polyg,i,j,false) then begin
        if (not DelBytes or (DelBytesNum=FMapType.TileSize(VTile, FZoom - 1))) then begin
          if FMapType.DeleteTile(VTile, FZoom - 1) then begin
            inc(FDeletedCount);
          end;
          ProgressFormUpdateOnProgress;
        end;
        inc(FTilesProcessed);
      end;
      inc(j,256);
    end;
    inc(i,256);
  end;
end;

procedure TThreadDeleteTiles.ProgressFormUpdateOnProgress;
begin
  ProgressFormUpdateProgressLine0AndLine1(
    round((FTilesProcessed / FTilesToProcess) * 100),
    SAS_STR_AllDelete+' '+inttostr(FDeletedCount)+' '+SAS_STR_files,
    SAS_STR_Processed+' '+inttostr(FTilesProcessed)
  );
end;

end.
