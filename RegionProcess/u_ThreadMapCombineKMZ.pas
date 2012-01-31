unit u_ThreadMapCombineKMZ;

interface

uses
  SysUtils,
  Classes,
  GR32,
  i_GlobalViewMainConfig,
  i_BitmapLayerProvider,
  i_VectorItemLonLat,
  i_VectorItemProjected,
  i_LocalCoordConverterFactorySimpe,
  u_MapType,
  t_GeoTypes,
  i_BitmapPostProcessingConfig,
  u_ResStrings,
  u_ThreadMapCombineBase,
  Imaging,
  ImagingTypes,
  ImagingJpeg;

type
  TThreadMapCombineKMZ = class(TThreadMapCombineBase)
  private
    FQuality: Integer;
  protected
    procedure SaveRect; override;
  public
    constructor Create(
      AViewConfig: IGlobalViewMainConfig;
      AMarksImageProvider: IBitmapLayerProvider;
      ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
      AMapCalibrationList: IInterfaceList;
      AFileName: string;
      APolygon: ILonLatPolygon;
      AProjectedPolygon: IProjectedPolygon;
      ASplitCount: TPoint;
      Atypemap: TMapType;
      AHtypemap: TMapType;
      AusedReColor: Boolean;
      ARecolorConfig: IBitmapPostProcessingConfigStatic;
      AQuality: Integer
    );
  end;

implementation

uses
  KAZip,
  i_CoordConverter,
  i_LocalCoordConverter,
  i_BitmapTileSaveLoad,
  u_BitmapTileVampyreSaver,
  u_GeoFun,
  u_GeoToStr;

constructor TThreadMapCombineKMZ.Create(
  AViewConfig: IGlobalViewMainConfig;
  AMarksImageProvider: IBitmapLayerProvider;
  ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
  AMapCalibrationList: IInterfaceList;
  AFileName: string;
  APolygon: ILonLatPolygon;
  AProjectedPolygon: IProjectedPolygon;
  ASplitCount: TPoint;
  Atypemap, AHtypemap: TMapType;
  AusedReColor: Boolean;
  ARecolorConfig: IBitmapPostProcessingConfigStatic;
  AQuality: Integer
);
var
  nim: TPoint;
begin
  inherited Create(
    AViewConfig,
    AMarksImageProvider,
    ALocalConverterFactory,
    AMapCalibrationList,
    AFileName,
    APolygon,
    AProjectedPolygon,
    ASplitCount,
    Atypemap,
    AHtypemap,
    AusedReColor,
    ARecolorConfig
  );
  FQuality := AQuality;
  nim.X := ((FMapPieceSize.X-1) div 1024) + 1;
  nim.Y := ((FMapPieceSize.Y-1) div 1024) + 1;
  if ((nim.X * nim.Y) > 100) then begin
    ShowMessageSync(SAS_MSG_GarminMax1Mp);
  end;
end;

procedure TThreadMapCombineKMZ.SaveRect;
var
  iWidth, iHeight: integer;
  i, j: integer;
  VFileName: string;

  kmlm, jpgm: TMemoryStream;
  VLLRect: TDoubleRect;
  str: UTF8String;
  VNameInKmz: String;
  nim: TPoint;

  Zip: TKaZip;

  VPixelRect: TRect;
  VBmp: TCustomBitmap32;
  VLocalConverter: ILocalCoordConverter;
  VLocalRect: TRect;
  JPGSaver: IBitmapTileSaver;
  VKmzFileNameOnly: string;
begin
  nim.X := ((FMapPieceSize.X-1) div 1024) + 1;
  nim.Y := ((FMapPieceSize.Y-1) div 1024) + 1;
  FTilesProcessed := 0;
  FTilesToProcess := nim.X * nim.Y;
  iWidth := FMapPieceSize.X div (nim.X);
  iHeight := FMapPieceSize.y div (nim.Y);

  JPGSaver := TVampyreBasicBitmapTileSaverJPG.create(FQuality);

  VKmzFileNameOnly := ExtractFileName(FCurrentFileName);
  Zip := TKaZip.Create(nil);
  try
    Zip.FileName := FCurrentFileName;
    Zip.CreateZip(FCurrentFileName);
    Zip.CompressionType := ctFast;
    Zip.Active := true;

    kmlm := TMemoryStream.Create;
    try
      str := ansiToUTF8('<?xml version="1.0" encoding="UTF-8"?>' + #13#10 + '<kml xmlns="http://earth.google.com/kml/2.2">' + #13#10 + '<Folder>' + #13#10 + '<name>' + VKmzFileNameOnly + '</name>' + #13#10);
      VBmp := TCustomBitmap32.Create;
      try
        VBmp.SetSize(iWidth, iHeight);
        VLocalRect.Left := 0;
        VLocalRect.Top := 0;
        VLocalRect.Right := iWidth;
        VLocalRect.Bottom := iHeight;
        for i := 1 to nim.X do begin
          for j := 1 to nim.Y do begin
            jpgm := TMemoryStream.Create;
            try
              if CancelNotifier.IsOperationCanceled(OperationID) then begin
                break;
              end;
              VPixelRect.Left := FCurrentPieceRect.Left + iWidth * (i - 1);
              VPixelRect.Right := FCurrentPieceRect.Left + iWidth * i;
              VPixelRect.Top := FCurrentPieceRect.Top + iHeight * (j - 1);
              VPixelRect.Bottom := FCurrentPieceRect.Top + iHeight * j;
              if Line.IsRectIntersectPolygon(DoubleRect(VPixelRect)) then begin
                VLocalConverter := ConverterFactory.CreateConverter(VLocalRect, Zoom, MainGeoConverter, DoublePoint(1, 1), DoublePoint(VPixelRect.TopLeft));

                VBmp.Clear(BackGroundColor);
                PrepareTileBitmap(VBmp, VLocalConverter);
                if CancelNotifier.IsOperationCanceled(OperationID) then begin
                  break;
                end;
                JPGSaver.SaveToStream(VBmp, jpgm);

                VFileName := ChangeFileExt(VKmzFileNameOnly, inttostr(i) + inttostr(j) + '.jpg');
                VNameInKmz := 'files/' + VFileName;
                str := str + ansiToUTF8('<GroundOverlay>' + #13#10 + '<name>' + VFileName + '</name>' + #13#10 + '<drawOrder>75</drawOrder>' + #13#10);
                str := str + ansiToUTF8('<Icon><href>' + VNameInKmz + '</href>' + '<viewBoundScale>0.75</viewBoundScale></Icon>' + #13#10);
                VLLRect := MainGeoConverter.PixelRect2LonLatRect(VPixelRect, Zoom);
                str := str + ansiToUTF8('<LatLonBox>' + #13#10);
                str := str + ansiToUTF8('<north>' + R2StrPoint(VLLRect.Top) + '</north>' + #13#10);
                str := str + ansiToUTF8('<south>' + R2StrPoint(VLLRect.Bottom) + '</south>' + #13#10);
                str := str + ansiToUTF8('<east>' + R2StrPoint(VLLRect.Right) + '</east>' + #13#10);
                str := str + ansiToUTF8('<west>' + R2StrPoint(VLLRect.Left) + '</west>' + #13#10);
                str := str + ansiToUTF8('</LatLonBox>' + #13#10 + '</GroundOverlay>' + #13#10);
                jpgm.Position := 0;
                Zip.AddStream(VNameInKmz, jpgm);
              end;
            finally
              jpgm.Free;
            end;
            Inc(FTilesProcessed);
            ProgressFormUpdateOnProgress;
          end;
        end;
      finally
        VBmp.Free;
      end;
      str := str + ansiToUTF8('</Folder>' + #13#10 + '</kml>');
      kmlm.Write(str[1], length(str));
      kmlm.Position := 0;
      Zip.AddStream('doc.kml', kmlm);
      Zip.Active := false;
      Zip.Close;
    finally
      kmlm.Free;
    end;
  finally
    Zip.Free;
  end;
end;

end.
