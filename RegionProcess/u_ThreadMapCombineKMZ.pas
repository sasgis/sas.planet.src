unit u_ThreadMapCombineKMZ;

interface

uses
  SysUtils,
  Classes,
  GR32,
  i_LocalCoordConverter,
  i_OperationNotifier,
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
    procedure SaveRect(
      AOperationID: Integer;
      ACancelNotifier: IOperationNotifier;
      AFileName: string;
      AImageProvider: IBitmapLayerProvider;
      ALocalConverter: ILocalCoordConverter;
      AConverterFactory: ILocalCoordConverterFactorySimpe
    ); override;
  public
    constructor Create(
      APolygon: ILonLatPolygon;
      ATargetConverter: ILocalCoordConverter;
      AImageProvider: IBitmapLayerProvider;
      ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
      AMapCalibrationList: IInterfaceList;
      AFileName: string;
      ASplitCount: TPoint;
      AQuality: Integer
    );
  end;

implementation

uses
  KAZip,
  i_CoordConverter,
  i_BitmapTileSaveLoad,
  u_BitmapTileVampyreSaver,
  u_GeoFun,
  u_GeoToStr;

constructor TThreadMapCombineKMZ.Create(
  APolygon: ILonLatPolygon;
  ATargetConverter: ILocalCoordConverter;
  AImageProvider: IBitmapLayerProvider;
  ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
  AMapCalibrationList: IInterfaceList;
  AFileName: string;
  ASplitCount: TPoint;
  AQuality: Integer
);
var
  nim: TPoint;
  VMapSize: TPoint;
  VMapPieceSize: TPoint;
begin
  inherited Create(
    APolygon,
    ATargetConverter,
    AImageProvider,
    ALocalConverterFactory,
    AMapCalibrationList,
    AFileName,
    ASplitCount
  );
  FQuality := AQuality;
  VMapSize := ATargetConverter.GetLocalRectSize;
  VMapPieceSize.X := VMapSize.X div ASplitCount.X;
  VMapPieceSize.Y := VMapSize.Y div ASplitCount.Y;
  nim.X := ((VMapPieceSize.X-1) div 1024) + 1;
  nim.Y := ((VMapPieceSize.Y-1) div 1024) + 1;
  if ((nim.X * nim.Y) > 100) then begin
    ShowMessageSync(SAS_MSG_GarminMax1Mp);
  end;
end;

procedure TThreadMapCombineKMZ.SaveRect(
  AOperationID: Integer;
  ACancelNotifier: IOperationNotifier;
  AFileName: string;
  AImageProvider: IBitmapLayerProvider;
  ALocalConverter: ILocalCoordConverter;
  AConverterFactory: ILocalCoordConverterFactorySimpe
);
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
  VCurrentPieceRect: TRect;
  VGeoConverter: ICoordConverter;
  VMapPieceSize: TPoint;
begin
  VGeoConverter := ALocalConverter.GeoConverter;
  VCurrentPieceRect := ALocalConverter.GetRectInMapPixel;
  VMapPieceSize := ALocalConverter.GetLocalRectSize;
  nim.X := ((VMapPieceSize.X-1) div 1024) + 1;
  nim.Y := ((VMapPieceSize.Y-1) div 1024) + 1;
  FTilesProcessed := 0;
  FTilesToProcess := nim.X * nim.Y;
  iWidth := VMapPieceSize.X div (nim.X);
  iHeight := VMapPieceSize.y div (nim.Y);

  JPGSaver := TVampyreBasicBitmapTileSaverJPG.create(FQuality);

  VKmzFileNameOnly := ExtractFileName(AFileName);
  Zip := TKaZip.Create(nil);
  try
    Zip.FileName := AFileName;
    Zip.CreateZip(AFileName);
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
              VPixelRect.Left := VCurrentPieceRect.Left + iWidth * (i - 1);
              VPixelRect.Right := VCurrentPieceRect.Left + iWidth * i;
              VPixelRect.Top := VCurrentPieceRect.Top + iHeight * (j - 1);
              VPixelRect.Bottom := VCurrentPieceRect.Top + iHeight * j;
              VLocalConverter :=
                AConverterFactory.CreateConverterNoScale(
                  VLocalRect,
                  ALocalConverter.Zoom,
                  VGeoConverter,
                  DoublePoint(VPixelRect.TopLeft)
                );
              if AImageProvider.GetBitmapRect(AOperationID, ACancelNotifier, VBmp, VLocalConverter) then begin
                if CancelNotifier.IsOperationCanceled(OperationID) then begin
                  break;
                end;
                JPGSaver.SaveToStream(VBmp, jpgm);

                VFileName := ChangeFileExt(VKmzFileNameOnly, inttostr(i) + inttostr(j) + '.jpg');
                VNameInKmz := 'files/' + VFileName;
                str := str + ansiToUTF8('<GroundOverlay>' + #13#10 + '<name>' + VFileName + '</name>' + #13#10 + '<drawOrder>75</drawOrder>' + #13#10);
                str := str + ansiToUTF8('<Icon><href>' + VNameInKmz + '</href>' + '<viewBoundScale>0.75</viewBoundScale></Icon>' + #13#10);
                VLLRect := VGeoConverter.PixelRect2LonLatRect(VPixelRect, ALocalConverter.Zoom);
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
