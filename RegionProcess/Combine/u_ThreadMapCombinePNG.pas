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

unit u_ThreadMapCombinePNG;

interface

uses
  SysUtils,
  Classes,
  GR32,
  i_ImageLineProvider,
  i_NotifierOperation,
  i_RegionProcessProgressInfo,
  i_BitmapLayerProvider,
  i_MapCalibration,
  i_LocalCoordConverter,
  i_GeometryLonLat,
  i_LocalCoordConverterFactorySimpe,
  u_ThreadMapCombineBase;

type
  TThreadMapCombinePNG = class(TThreadMapCombineBase)
  private
    FBgColor: TColor32;
    FWithAlpha: Boolean;
    FLineProvider: IImageLineProvider;
    FOperationID: Integer;
    FCancelNotifier: INotifierOperation;
  private
    function GetLineCallBack(
      const ARowNumber: Integer;
      const ALineSize: Integer;
      const AUserInfo: Pointer
    ): Pointer;
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
      const APolygon: IGeometryLonLatMultiPolygon;
      const ATargetConverter: ILocalCoordConverter;
      const AImageProvider: IBitmapLayerProvider;
      const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
      const AMapCalibrationList: IMapCalibrationList;
      const AFileName: string;
      const ASplitCount: TPoint;
      ABgColor: TColor32;
      AWithAlpha: Boolean
    );
  end;

implementation

uses
  LibPngWriter,
  i_CoordConverter,
  u_ImageLineProvider,
  u_ResStrings;

{ TThreadMapCombinePNG }

constructor TThreadMapCombinePNG.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const APolygon: IGeometryLonLatMultiPolygon;
  const ATargetConverter: ILocalCoordConverter;
  const AImageProvider: IBitmapLayerProvider;
  const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
  const AMapCalibrationList: IMapCalibrationList;
  const AFileName: string;
  const ASplitCount: TPoint;
  ABgColor: TColor32;
  AWithAlpha: Boolean
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
  FWithAlpha := AWithAlpha;
end;

procedure TThreadMapCombinePNG.SaveRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AFileName: string;
  const AImageProvider: IBitmapLayerProvider;
  const ALocalConverter: ILocalCoordConverter;
  const AConverterFactory: ILocalCoordConverterFactorySimpe
);
const
  PNG_MAX_HEIGHT = 65536;
  PNG_MAX_WIDTH = 65536;
var
  VDest: TFileStream;
  VBitsPerPix: Integer;
  VCurrentPieceRect: TRect;
  VGeoConverter: ICoordConverter;
  VMapPieceSize: TPoint;
  VPngWriter: TLibPngWriter;
begin
  FOperationID := AOperationID;
  FCancelNotifier := ACancelNotifier;

  VGeoConverter := ALocalConverter.GeoConverter;
  VCurrentPieceRect := ALocalConverter.GetRectInMapPixel;
  VMapPieceSize := ALocalConverter.GetLocalRectSize;

  if (VMapPieceSize.X >= PNG_MAX_WIDTH) or (VMapPieceSize.Y >= PNG_MAX_HEIGHT) then begin
    raise Exception.CreateFmt(
      SAS_ERR_ImageIsTooBig,
      ['PNG', VMapPieceSize.X, PNG_MAX_WIDTH, VMapPieceSize.Y, PNG_MAX_HEIGHT, 'PNG']
    );
  end;

  if FWithAlpha then begin
    VBitsPerPix := 32;
    FLineProvider :=
      TImageLineProviderRGBA.Create(
        AImageProvider,
        ALocalConverter,
        AConverterFactory,
        FBgColor
      );
  end else begin
    VBitsPerPix := 24;
    FLineProvider :=
      TImageLineProviderRGB.Create(
        AImageProvider,
        ALocalConverter,
        AConverterFactory,
        FBgColor
      );
  end;

  VDest := TFileStream.Create(AFileName, fmCreate);
  try
    VPngWriter := TLibPngWriter.Create;
    try
      VPngWriter.Write(
        VDest,
        VMapPieceSize.X,
        VMapPieceSize.Y,
        VBitsPerPix,
        Self.GetLineCallBack
      );
    finally
      VPngWriter.Free;
    end;
  finally
    VDest.Free;
  end;
end;

function TThreadMapCombinePNG.GetLineCallBack(
  const ARowNumber: Integer;
  const ALineSize: Integer;
  const AUserInfo: Pointer
): Pointer;
begin
  Result := FLineProvider.GetLine(FOperationID, FCancelNotifier, ARowNumber);
end;

end.
