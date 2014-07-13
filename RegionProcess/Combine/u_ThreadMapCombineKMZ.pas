{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_ThreadMapCombineKMZ;

interface

uses
  SysUtils,
  Classes,
  Types,
  i_LocalCoordConverter,
  i_NotifierOperation,
  i_RegionProcessProgressInfo,
  i_BitmapLayerProvider,
  i_MapCalibration,
  i_GeometryLonLat,
  i_BitmapTileSaveLoadFactory,
  i_ArchiveReadWriteFactory,
  i_LocalCoordConverterFactorySimpe,
  t_GeoTypes,
  u_ThreadMapCombineBase;

type
  TThreadMapCombineKMZ = class(TThreadMapCombineBase)
  private
    FQuality: Integer;
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
    FArchiveReadWriteFactory: IArchiveReadWriteFactory;
  protected
    procedure SaveRect(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AFileName: string;
      const AImageProvider: IBitmapLayerProvider;
      const ALocalConverter: ILocalCoordConverter;
      const AConverterFactory: ILocalCoordConverterFactorySimpe
    ); override;
  public
    constructor Create(
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const APolygon: IGeometryLonLatPolygon;
      const ATargetConverter: ILocalCoordConverter;
      const AImageProvider: IBitmapLayerProvider;
      const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
      const AMapCalibrationList: IMapCalibrationList;
      const AFileName: string;
      const ASplitCount: TPoint;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
      AQuality: Integer
    );
  end;

implementation

uses
  i_Bitmap32Static,
  i_BinaryData,
  i_CoordConverter,
  i_BitmapTileSaveLoad,
  i_ArchiveReadWrite,
  u_BinaryDataByMemStream,
  u_GeoToStrFunc;

constructor TThreadMapCombineKMZ.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const APolygon: IGeometryLonLatPolygon;
  const ATargetConverter: ILocalCoordConverter;
  const AImageProvider: IBitmapLayerProvider;
  const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
  const AMapCalibrationList: IMapCalibrationList;
  const AFileName: string;
  const ASplitCount: TPoint;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
  AQuality: Integer
);
begin
  inherited Create(
    AProgressInfo,
    APolygon,
    ATargetConverter,
    AImageProvider,
    ALocalConverterFactory,
    AMapCalibrationList,
    AFileName,
    ASplitCount,
    Self.ClassName
  );
  FQuality := AQuality;
  FBitmapTileSaveLoadFactory := ABitmapTileSaveLoadFactory;
  FArchiveReadWriteFactory := AArchiveReadWriteFactory;
end;

procedure TThreadMapCombineKMZ.SaveRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AFileName: string;
  const AImageProvider: IBitmapLayerProvider;
  const ALocalConverter: ILocalCoordConverter;
  const AConverterFactory: ILocalCoordConverterFactorySimpe
);
var
  iWidth, iHeight: integer;
  i, j: integer;
  VFileName: string;
  kmlm: TMemoryStream;
  VLLRect: TDoubleRect;
  VStr: UTF8String;
  VNameInKmz: String;
  nim: TPoint;
  VZip: IArchiveWriter;
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
begin
  VGeoConverter := ALocalConverter.GeoConverter;
  VCurrentPieceRect := ALocalConverter.GetRectInMapPixel;
  VMapPieceSize := ALocalConverter.GetLocalRectSize;
  nim.X := ((VMapPieceSize.X - 1) div 1024) + 1;
  nim.Y := ((VMapPieceSize.Y - 1) div 1024) + 1;
  VTilesProcessed := 0;
  VTilesToProcess := nim.X * nim.Y;
  iWidth := VMapPieceSize.X div (nim.X);
  iHeight := VMapPieceSize.y div (nim.Y);

  JPGSaver := FBitmapTileSaveLoadFactory.CreateJpegSaver(FQuality);
  VZip := FArchiveReadWriteFactory.CreateZipWriterByName(AFileName);
  VKmzFileNameOnly := ExtractFileName(AFileName);
  kmlm := TMemoryStream.Create;
  try
    VStr := ansiToUTF8('<?xml version="1.0" encoding="UTF-8"?>' + #13#10 + '<kml xmlns="http://earth.google.com/kml/2.2">' + #13#10 + '<Folder>' + #13#10 + '<name>' + VKmzFileNameOnly + '</name>' + #13#10);
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
            VStr := VStr + ansiToUTF8('<GroundOverlay>' + #13#10 + '<name>' + VFileName + '</name>' + #13#10 + '<drawOrder>75</drawOrder>' + #13#10);
            VStr := VStr + ansiToUTF8('<Icon><href>' + VNameInKmz + '</href>' + '<viewBoundScale>0.75</viewBoundScale></Icon>' + #13#10);
            VLLRect := VGeoConverter.PixelRect2LonLatRect(VPixelRect, ALocalConverter.Zoom);
            VStr := VStr + ansiToUTF8('<LatLonBox>' + #13#10);
            VStr := VStr + ansiToUTF8('<north>' + R2StrPoint(VLLRect.Top) + '</north>' + #13#10);
            VStr := VStr + ansiToUTF8('<south>' + R2StrPoint(VLLRect.Bottom) + '</south>' + #13#10);
            VStr := VStr + ansiToUTF8('<east>' + R2StrPoint(VLLRect.Right) + '</east>' + #13#10);
            VStr := VStr + ansiToUTF8('<west>' + R2StrPoint(VLLRect.Left) + '</west>' + #13#10);
            VStr := VStr + ansiToUTF8('</LatLonBox>' + #13#10 + '</GroundOverlay>' + #13#10);

            VZip.AddFile(VData, VNameInKmz, Now);
          end;
        end;
        ProgressFormUpdateOnProgress(VTilesProcessed / VTilesToProcess);
      end;
    end;
    VStr := VStr + ansiToUTF8('</Folder>' + #13#10 + '</kml>');
    kmlm.Write(VStr[1], length(VStr));
    kmlm.Position := 0;

    VData := TBinaryDataByMemStream.CreateFromStream(kmlm);
    VZip.AddFile(VData, 'doc.kml', Now);
  finally
    kmlm.Free;
  end;
end;

end.
