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
  Types,
  t_Bitmap32,
  i_ImageLineProvider,
  i_NotifierOperation,
  i_RegionProcessProgressInfo,
  i_BitmapLayerProvider,
  i_MapCalibration,
  i_ProjectionInfo,
  i_GeometryLonLat,
  u_ThreadMapCombineBase;

type
  TThreadMapCombinePNG = class(TThreadMapCombineBase)
  private
    FWidth: Integer;
    FHeight: Integer;
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
      const AImageProvider: IBitmapTileUniProvider;
      const AProjection: IProjectionInfo;
      const AMapRect: TRect
    ); override;
  public
    constructor Create(
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const APolygon: IGeometryLonLatPolygon;
      const AProjection: IProjectionInfo;
      const AMapRect: TRect;
      const AImageProvider: IBitmapTileUniProvider;
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
  u_GeoFunc,
  u_ResStrings;

{ TThreadMapCombinePNG }

constructor TThreadMapCombinePNG.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const APolygon: IGeometryLonLatPolygon;
  const AProjection: IProjectionInfo;
  const AMapRect: TRect;
  const AImageProvider: IBitmapTileUniProvider;
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
    AProjection,
    AMapRect,
    AImageProvider,
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
  const AImageProvider: IBitmapTileUniProvider;
  const AProjection: IProjectionInfo;
  const AMapRect: TRect
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

  VGeoConverter := AProjection.GeoConverter;
  VCurrentPieceRect := AMapRect;
  VMapPieceSize := RectSize(VCurrentPieceRect);

  FWidth := VMapPieceSize.X;
  FHeight := VMapPieceSize.Y;

  if (FWidth >= PNG_MAX_WIDTH) or (FHeight >= PNG_MAX_HEIGHT) then begin
    raise Exception.CreateFmt(
      SAS_ERR_ImageIsTooBig,
      ['PNG', FWidth, PNG_MAX_WIDTH, FHeight, PNG_MAX_HEIGHT, 'PNG']
    );
  end;

  if FWithAlpha then begin
    VBitsPerPix := 32;
    FLineProvider :=
      TImageLineProviderRGBA.Create(
        AImageProvider,
        AProjection,
        AMapRect,
        FBgColor
      );
  end else begin
    VBitsPerPix := 24;
    FLineProvider :=
      TImageLineProviderRGB.Create(
        AImageProvider,
        AProjection,
        AMapRect,
        FBgColor
      );
  end;

  VDest := TFileStream.Create(AFileName, fmCreate);
  try
    VPngWriter := TLibPngWriter.Create;
    try
      VPngWriter.Write(
        VDest,
        FWidth,
        FHeight,
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
  if ARowNumber mod 256 = 0 then begin
    ProgressFormUpdateOnProgress(ARowNumber / FHeight);
  end;
  if not CancelNotifier.IsOperationCanceled(OperationID) then begin
    Result := FLineProvider.GetLine(FOperationID, FCancelNotifier, ARowNumber);
  end else begin
    Result := nil;
  end;
end;

end.
