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
  i_ImageLineProvider,
  i_NotifierOperation,
  i_BitmapTileProvider,
  i_BitmapMapCombiner,
  u_BaseInterfacedObject;

type
  TBitmapMapCombinerPNG = class(TBaseInterfacedObject, IBitmapMapCombiner)
  private
    FProgressUpdate: IBitmapCombineProgressUpdate;
    FWidth: Integer;
    FHeight: Integer;
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
      AWithAlpha: Boolean
    );
  end;

implementation

uses
  LibPngWriter,
  u_ImageLineProvider,
  u_GeoFunc,
  u_ResStrings;

{ TThreadMapCombinePNG }

constructor TBitmapMapCombinerPNG.Create(
  const AProgressUpdate: IBitmapCombineProgressUpdate;
  AWithAlpha: Boolean
);
begin
  inherited Create;
  FProgressUpdate := AProgressUpdate;
  FWithAlpha := AWithAlpha;
end;

procedure TBitmapMapCombinerPNG.SaveRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AFileName: string;
  const AImageProvider: IBitmapTileProvider;
  const AMapRect: TRect
);
const
  PNG_MAX_HEIGHT = 65536;
  PNG_MAX_WIDTH = 65536;
var
  VDest: TFileStream;
  VBitsPerPix: Integer;
  VCurrentPieceRect: TRect;
  VMapPieceSize: TPoint;
  VPngWriter: TLibPngWriter;
begin
  FOperationID := AOperationID;
  FCancelNotifier := ACancelNotifier;

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
        AMapRect
      );
  end else begin
    VBitsPerPix := 24;
    FLineProvider :=
      TImageLineProviderRGB.Create(
        AImageProvider,
        AMapRect
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

function TBitmapMapCombinerPNG.GetLineCallBack(
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
