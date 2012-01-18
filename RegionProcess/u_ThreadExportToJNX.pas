unit u_ThreadExportToJNX;

interface

uses
  SysUtils,
  Classes,
  JNXlib,
  GR32,
  t_GeoTypes,
  i_CoordConverterFactory,
  u_MapType,
  u_ResStrings,
  u_ThreadExportAbstract;

type
  TThreadExportToJnx = class(TThreadExportAbstract)
  private
    FMapType: TMapType;
    FTargetFile: string;
    FCoordConverterFactory: ICoordConverterFactory;
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      ACoordConverterFactory: ICoordConverterFactory;
      ATargetFile: string;
      APolygon: TArrayOfDoublePoint;
      Azoomarr: array of boolean;
      AMapType: TMapType
    );
  end;

implementation

uses
  Types,
  c_CoordConverter,
  i_CoordConverter,
  i_TileIterator,
  i_BitmapTileSaveLoad,
  u_BitmapTileVampyreSaver,
  u_TileIteratorStuped;

constructor TThreadExportToJnx.Create(
  ACoordConverterFactory: ICoordConverterFactory;
  ATargetFile: string;
  APolygon: TArrayOfDoublePoint;
  Azoomarr: array of boolean;
  AMapType: TMapType
);
begin
  inherited Create(APolygon, Azoomarr);
  FTargetFile := ATargetFile;
  FMapType := AMapType;
  FCoordConverterFactory := ACoordConverterFactory;
end;

procedure TThreadExportToJnx.ProcessRegion;
var
  i: integer;
  VBmp: TCustomBitmap32;
  VZoom: Byte;
  VTile: TPoint;
  VTileIterators: array of ITileIterator;
  VTileIterator: ITileIterator;
  VSaver: IBitmapTileSaver;
  VMemStream: TMemoryStream;
  VGeoConvert: ICoordConverter;
  VStringStream: TStringStream;
  VTileIndexPerZoom: Integer;
  VWriter: TJNXWriter;
  VTileBounds: TJNXRect;
  VTopLeft: TDoublePoint;
  VBottomRight: TDoublePoint;
begin
  inherited;
  FTilesToProcess := 0;
  VSaver := TVampyreBasicBitmapTileSaverJPG.Create(95);
  VGeoConvert := FCoordConverterFactory.GetCoordConverterByCode(CGELonLatProjectionEPSG, CTileSplitQuadrate256x256);
  SetLength(VTileIterators, Length(FZooms));
  for i := 0 to Length(FZooms) - 1 do begin
    VZoom := FZooms[i];
    VTileIterators[i] := TTileIteratorStuped.Create(VZoom, FPolygLL, VGeoConvert);
    FTilesToProcess := FTilesToProcess + VTileIterators[i].TilesTotal;
  end;

  VWriter := TJNXWriter.Create(FTargetFile);
  try
    VWriter.Levels := Length(FZooms);
    for i := 0 to Length(FZooms) - 1 do begin
      VWriter.LevelScale[i] := DigitalGlobeZoomToScale(FZooms[i]);
      VWriter.TileCount[i]  := VTileIterators[i].TilesTotal;
    end;

    try
      ProgressFormUpdateCaption(
        SAS_STR_ExportTiles,
        SAS_STR_AllSaves + ' ' + inttostr(FTilesToProcess) + ' ' + SAS_STR_Files
      );
      VMemStream := TMemoryStream.Create;
      VStringStream := TStringStream.Create('');
      VBmp := TCustomBitmap32.Create;
      try
        FTilesProcessed := 0;
        ProgressFormUpdateOnProgress;
        for i := 0 to Length(FZooms) - 1 do begin
          VZoom := FZooms[i];
          VTileIndexPerZoom := 0;
          VTileIterator := VTileIterators[i];
          while VTileIterator.Next(VTile) do begin
            if CancelNotifier.IsOperationCanceled(OperationID) then begin
              exit;
            end;
            VBmp.Clear;
            if FMapType.LoadTileUni(VBmp, VTile, VZoom, VGeoConvert, False, False, True) then begin
              VMemStream.Clear;
              VMemStream.Position := 0;
              VSaver.SaveToStream(VBmp, VMemStream);

              VTopLeft := VGeoConvert.TilePos2LonLat(VTile, VZoom);
              VBottomRight := VGeoConvert.TilePos2LonLat( Point(VTile.X + 1, VTile.Y - 1), VZoom);

              VTileBounds := JNXRect(
                WGS84CoordToJNX(VBottomRight.Y),
                WGS84CoordToJNX(VBottomRight.X),
                WGS84CoordToJNX(VTopLeft.Y),
                WGS84CoordToJNX(VTopLeft.X)
              );

              VMemStream.Position := 0;
              VStringStream.CopyFrom(VMemStream, 0);

              VWriter.WriteTile(
                I,
                VTileIndexPerZoom,
                256,
                256,
                VTileBounds,
                VStringStream.DataString
              );

            end;
            Inc(VTileIndexPerZoom);
            inc(FTilesProcessed);
            if FTilesProcessed mod 100 = 0 then begin
              ProgressFormUpdateOnProgress;
            end;
          end;
        end;
      finally
        VMemStream.Free;
        VStringStream.Free;
        VBmp.Free;
      end;
    finally
      for i := 0 to Length(FZooms) - 1 do begin
        VTileIterators[i] := nil;
      end;
      VTileIterators := nil;
    end;
    ProgressFormUpdateOnProgress;
  finally
    VWriter.Free;
  end;
end;

end.
