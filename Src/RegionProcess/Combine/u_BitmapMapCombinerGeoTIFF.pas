{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2016, SAS.Planet development team.                      *}
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

unit u_BitmapMapCombinerGeoTIFF;

interface

uses
  SysUtils,
  Classes,
  Types,
  GeoTiffWriter,
  t_GeoTIFF,
  i_ImageLineProvider,
  i_NotifierOperation,
  i_BitmapTileProvider,
  i_BitmapMapCombiner,
  u_BaseInterfacedObject;

type
  TBitmapMapCombinerGeoTIFF = class(TBaseInterfacedObject, IBitmapMapCombiner)
  private
    FProgressUpdate: IBitmapCombineProgressUpdate;
    FWidth: Integer;
    FHeight: Integer;
    FWithAlpha: Boolean;
    FFileFormat: TGeoTiffFileFormat;
    FCompression: TGeoTiffCompression;
    FLineProvider: IImageLineProvider;
    FOperationID: Integer;
    FCancelNotifier: INotifierOperation;
  private
    function _GetTiffType: TTiffType;
    function GetLineCallBack(
      const ARowNumber: Integer;
      const ALineSize: Integer;
      const AUserInfo: Pointer
    ): Pointer;
  private
    procedure SaveRect(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AFileName: string;
      const AImageProvider: IBitmapTileProvider;
      const AMapRect: TRect
    );
  public
    constructor Create(
      const AProgressUpdate: IBitmapCombineProgressUpdate;
      const AWithAlpha: Boolean = True;
      const AFileFormat: TGeoTiffFileFormat = gtfOld;
      const ACompression: TGeoTiffCompression = gtcLZW
    );
  end;

implementation

uses
  c_CoordConverter,
  i_Projection,
  u_CalcWFileParams,
  u_ImageLineProvider,
  u_GeoFunc,
  u_ResStrings;

{ TBitmapMapCombinerGeoTIFF }

constructor TBitmapMapCombinerGeoTIFF.Create(
  const AProgressUpdate: IBitmapCombineProgressUpdate;
  const AWithAlpha: Boolean;
  const AFileFormat: TGeoTiffFileFormat;
  const ACompression: TGeoTiffCompression
);
begin
  inherited Create;
  FProgressUpdate := AProgressUpdate;
  FWithAlpha := AWithAlpha;
  FFileFormat := AFileFormat;
  FCompression := ACompression;
end;

function TBitmapMapCombinerGeoTIFF._GetTiffType: TTiffType;
const
  cOldTiffMaxFileSize = 4 * 1024 * 1024; // 4000 MB
var
  VSize: Double;
  VCompressionCoeff: Single;
  VBytesPerPix: Integer;
begin
  if FCompression = gtcNone then begin
    VCompressionCoeff := 1.0;
  end else begin
    VCompressionCoeff := 2.5; // just try guess compression ratio
  end;

  case FFileFormat of
    gtfOld: Result := ttOldTiff;
    gtfBig: Result := ttBigTiff;
  else
    begin
      if FWithAlpha then begin
        VBytesPerPix := 4;
      end else begin
        VBytesPerPix := 3;
      end;
      VSize := FWidth * FHeight * VBytesPerPix / (VCompressionCoeff * 1024);
      if Round(VSize) >= cOldTiffMaxFileSize then begin
        Result := ttBigTiff;
      end else begin
        Result := ttOldTiff;
      end;
    end;
  end;
end;

procedure TBitmapMapCombinerGeoTIFF.SaveRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AFileName: string;
  const AImageProvider: IBitmapTileProvider;
  const AMapRect: TRect
);
var
  VDest: TFileStream;
  VCurrentPieceRect: TRect;
  VMapPieceSize: TPoint;
  VProjection: IProjection;
  VCellIncrementX, VCellIncrementY, VOriginX, VOriginY: Double;
  VGeoTiffWriter: TGeoTiffWriter;
  VTiffType: TTiffType;
  VCompression: TTiffCompression;
  VTiePoints: TTiePoints;
  VPixScale: TPixScale;
  VEPSG: Integer;
begin
  FOperationID := AOperationID;
  FCancelNotifier := ACancelNotifier;

  VCurrentPieceRect := AMapRect;
  VMapPieceSize := RectSize(VCurrentPieceRect);

  FWidth := VMapPieceSize.X;
  FHeight := VMapPieceSize.Y;

  VProjection := AImageProvider.Projection;

  FillChar(VTiePoints[0], Length(VTiePoints)*SizeOf(VTiePoints[0]), 0);
  FillChar(VPixScale[0], Length(VPixScale)*SizeOf(VPixScale[0]), 0);

  CalculateWFileParams(
    VProjection.PixelPos2LonLat(VCurrentPieceRect.TopLeft),
    VProjection.PixelPos2LonLat(VCurrentPieceRect.BottomRight),
    VMapPieceSize.X, VMapPieceSize.Y, VProjection.ProjectionType,
    VCellIncrementX, VCellIncrementY, VOriginX, VOriginY
  );

  VTiePoints[3] := VOriginX;
  VTiePoints[4] := VOriginY;

  VPixScale[0] := VCellIncrementX;
  VPixScale[1] := -VCellIncrementY;

  VEPSG := VProjection.ProjectionType.ProjectionEPSG;

  case FCompression of
    gtcZIP: VCompression := tcZip;
    gtcLZW: VCompression := tcLZW;
    gtcJPEG: VCompression := tcJPG;
  else
    VCompression := tcNone;
  end;

  VTiffType := _GetTiffType;

  if FWithAlpha then begin
    FLineProvider :=
      TImageLineProviderRGBA.Create(
        AImageProvider,
        AMapRect
      );
  end else begin
    FLineProvider :=
      TImageLineProviderRGB.Create(
        AImageProvider,
        AMapRect
      );
  end;

  VDest := TFileStream.Create(AFileName, fmCreate);
  try
    VGeoTiffWriter := TGeoTiffWriter.Create('SAS.Planet');
    try
      VGeoTiffWriter.Write(
        VTiffType,
        VDest,
        FWidth,
        FHeight,
        VCompression,
        Self.GetLineCallBack,
        nil,
        FWithAlpha,
        True, // store proj info
        VEPSG,
        (VEPSG = CGELonLatProjectionEPSG), // is geographic proj
        VTiePoints,
        VPixScale
      );
    finally
      VGeoTiffWriter.Free;
    end;
  finally
    VDest.Free;
  end;
end;

function TBitmapMapCombinerGeoTIFF.GetLineCallBack(
  const ARowNumber: Integer;
  const ALineSize: Integer;
  const AUserInfo: Pointer
): Pointer;
begin
  if ARowNumber mod 256 = 0 then begin
    FProgressUpdate.Update(ARowNumber / FHeight);
  end;
  if not FCancelNotifier.IsOperationCanceled(FOperationID) then begin
    Result := FLineProvider.GetLine(FOperationID, FCancelNotifier, ARowNumber);
  end else begin
    Result := nil;
  end;
end;

end.
