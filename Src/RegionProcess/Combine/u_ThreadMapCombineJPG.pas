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

unit u_ThreadMapCombineJPG;

interface

uses
  Types,
  SysUtils,
  Classes,
  LibJpegWrite,
  t_Bitmap32,
  i_NotifierOperation,
  i_RegionProcessProgressInfo,
  i_BitmapLayerProvider,
  i_MapCalibration,
  i_GeometryLonLat,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe,
  i_ImageLineProvider,
  u_ThreadMapCombineBase;

type
  TThreadMapCombineJPG = class(TThreadMapCombineBase)
  private
    FWidth: Integer;
    FHeight: Integer;
    FQuality: Integer;
    FBgColor: TColor32;
    FLineProvider: IImageLineProvider;
    FSaveGeoRefInfoToExif: Boolean;
    function GetLine(
      Sender: TObject;
      ALineNumber: Integer;
      ALineSize: Cardinal;
      out Abort: Boolean
    ): PByte;
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
      const ABgColor: TColor32;
      const AQuality: Integer;
      const ASaveGeoRefInfoToExif: Boolean
    );
  end;

implementation

uses
  Exif,
  t_GeoTypes,
  i_CoordConverter,
  u_ImageLineProvider,
  u_ResStrings;

{ TThreadMapCombineJPG }

constructor TThreadMapCombineJPG.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const APolygon: IGeometryLonLatPolygon;
  const ATargetConverter: ILocalCoordConverter;
  const AImageProvider: IBitmapLayerProvider;
  const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
  const AMapCalibrationList: IMapCalibrationList;
  const AFileName: string;
  const ASplitCount: TPoint;
  const ABgColor: TColor32;
  const AQuality: Integer;
  const ASaveGeoRefInfoToExif: Boolean
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
  FBgColor := ABgColor;
  FQuality := AQuality;
  FSaveGeoRefInfoToExif := ASaveGeoRefInfoToExif;
end;

procedure TThreadMapCombineJPG.SaveRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AFileName: string;
  const AImageProvider: IBitmapLayerProvider;
  const ALocalConverter: ILocalCoordConverter;
  const AConverterFactory: ILocalCoordConverterFactorySimpe
);
const
  JPG_MAX_HEIGHT = 65536;
  JPG_MAX_WIDTH = 65536;
var
  VJpegWriter: TJpegWriter;
  VStream: TFileStream;
  VCurrentPieceRect: TRect;
  VGeoConverter: ICoordConverter;
  VMapPieceSize: TPoint;
  VExif: TExifSimple;
  VCenterLonLat: TDoublePoint;
  VUseBGRAColorSpace: Boolean;
begin
  VGeoConverter := ALocalConverter.GeoConverter;
  VCurrentPieceRect := ALocalConverter.GetRectInMapPixel;
  VMapPieceSize := ALocalConverter.GetLocalRectSize;
  VCenterLonLat := ALocalConverter.GetCenterLonLat;

  VUseBGRAColorSpace := True; // Available for libjpeg-turbo only

  if VUseBGRAColorSpace then begin
    FLineProvider :=
      TImageLineProviderBGRA.Create(
        AImageProvider,
        ALocalConverter,
        AConverterFactory,
        FBgColor
      );
  end else begin
    FLineProvider :=
      TImageLineProviderRGB.Create(
        AImageProvider,
        ALocalConverter,
        AConverterFactory,
        FBgColor
      );
  end;

  FWidth := VMapPieceSize.X;
  FHeight := VMapPieceSize.Y;
  if (FWidth >= JPG_MAX_WIDTH) or (FHeight >= JPG_MAX_HEIGHT) then begin
    raise Exception.CreateFmt(SAS_ERR_ImageIsTooBig, ['JPG', FWidth, JPG_MAX_WIDTH, FHeight, JPG_MAX_HEIGHT, 'JPG']);
  end;
  VStream := TFileStream.Create(AFileName, fmCreate);
  try
    VJpegWriter := TJpegWriter.Create(VStream, VUseBGRAColorSpace);
    try
      VJpegWriter.Width := FWidth;
      VJpegWriter.Height := FHeight;
      VJpegWriter.Quality := FQuality;
      VJpegWriter.AddCommentMarker('Created with SAS.Planet' + #0);
      if FSaveGeoRefInfoToExif then begin
        VExif := TExifSimple.Create(VCenterLonLat.Y, VCenterLonLat.X);
        try
          VJpegWriter.AddExifMarker(VExif.Stream);
        finally
          VExif.Free;
        end;
      end;
      VJpegWriter.Compress(Self.GetLine);
    finally
      VJpegWriter.Free;
    end;
  finally
    VStream.Free;
  end;
end;

function TThreadMapCombineJPG.GetLine(
  Sender: TObject;
  ALineNumber: Integer;
  ALineSize: Cardinal;
  out Abort: Boolean
): PByte;
begin
  if ALineNumber mod 256 = 0 then begin
    ProgressFormUpdateOnProgress(ALineNumber / FHeight);
  end;
  Result := FLineProvider.GetLine(OperationID, CancelNotifier, ALineNumber);
  Abort := (Result = nil) or CancelNotifier.IsOperationCanceled(OperationID);
end;

end.
