unit u_ThreadMapCombineKMZ;

interface

uses
  SysUtils,
  Classes,
  GR32,
  i_LocalCoordConverter,
  i_OperationNotifier,
  i_RegionProcessProgressInfo,
  i_BitmapLayerProvider,
  i_VectorItemLonLat,
  i_LocalCoordConverterFactorySimpe,
  t_GeoTypes,
  u_ThreadMapCombineBase;

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
      ACancelNotifier: IOperationNotifier;
      AOperationID: Integer;
      AProgressInfo: IRegionProcessProgressInfo;
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
  i_Bitmap32Static,
  i_BinaryData,
  i_CoordConverter,
  i_BitmapTileSaveLoad,
  u_BitmapTileVampyreSaver,
  u_StreamReadOnlyByBinaryData,
  u_GeoToStr;

constructor TThreadMapCombineKMZ.Create(
  ACancelNotifier: IOperationNotifier;
  AOperationID: Integer;
  AProgressInfo: IRegionProcessProgressInfo;
  APolygon: ILonLatPolygon;
  ATargetConverter: ILocalCoordConverter;
  AImageProvider: IBitmapLayerProvider;
  ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
  AMapCalibrationList: IInterfaceList;
  AFileName: string;
  ASplitCount: TPoint;
  AQuality: Integer
);
begin
  inherited Create(
    ACancelNotifier,
    AOperationID,
    AProgressInfo,
    APolygon,
    ATargetConverter,
    AImageProvider,
    ALocalConverterFactory,
    AMapCalibrationList,
    AFileName,
    ASplitCount
  );
  FQuality := AQuality;
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

  kmlm: TMemoryStream;
  VLLRect: TDoubleRect;
  str: UTF8String;
  VNameInKmz: String;
  nim: TPoint;

  Zip: TKaZip;

  VPixelRect: TRect;
  VLocalConverter: ILocalCoordConverter;
  VLocalRect: TRect;
  JPGSaver: IBitmapTileSaver;
  VKmzFileNameOnly: string;
  VCurrentPieceRect: TRect;
  VGeoConverter: ICoordConverter;
  VMapPieceSize: TPoint;
  VTilesProcessed: Integer;
  VTilesToProcess: Integer;
  VBitmapTile: IBitmap32Static;
  VData: IBinaryData;
  VDataStream: TStreamReadOnlyByBinaryData;
begin
  VGeoConverter := ALocalConverter.GeoConverter;
  VCurrentPieceRect := ALocalConverter.GetRectInMapPixel;
  VMapPieceSize := ALocalConverter.GetLocalRectSize;
  nim.X := ((VMapPieceSize.X-1) div 1024) + 1;
  nim.Y := ((VMapPieceSize.Y-1) div 1024) + 1;
  VTilesProcessed := 0;
  VTilesToProcess := nim.X * nim.Y;
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
        VLocalRect.Left := 0;
        VLocalRect.Top := 0;
        VLocalRect.Right := iWidth;
        VLocalRect.Bottom := iHeight;
        for i := 1 to nim.X do begin
          for j := 1 to nim.Y do begin
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
                  VPixelRect.TopLeft
                );
              VBitmapTile := AImageProvider.GetBitmapRect(AOperationID, ACancelNotifier, VLocalConverter);
              if VBitmapTile <> nil then begin
                if CancelNotifier.IsOperationCanceled(OperationID) then begin
                  break;
                end;
                VData := JPGSaver.Save(VBitmapTile);

                if VData <> nil then begin
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

                  VDataStream := TStreamReadOnlyByBinaryData.Create(VData);
                  try
                    Zip.AddStream(VNameInKmz, VDataStream);
                  finally
                    VDataStream.Free;
                  end;
                end;
              end;
            ProgressFormUpdateOnProgress(VTilesProcessed/VTilesToProcess);
          end;
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
