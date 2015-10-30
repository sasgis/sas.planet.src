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

unit u_BitmapMapCombinerECW;

interface

uses
  SysUtils,
  Types,
  i_NotifierOperation,
  i_BitmapTileProvider,
  i_Projection,
  i_ImageLineProvider,
  i_BitmapMapCombiner,
  u_ECWWrite,
  u_BaseInterfacedObject;

type
  TBitmapMapCombinerECW = class(TBaseInterfacedObject, IBitmapMapCombiner)
  private
    FProgressUpdate: IBitmapCombineProgressUpdate;
    FImageLineProvider: IImageLineProvider;
    FLinesCount: Integer;
    FQuality: Integer;
    FOperationID: Integer;
    FCancelNotifier: INotifierOperation;

    function ReadLine(
      ALine: Integer;
      var LineR, LineG, LineB: PLineRGB
    ): Boolean;
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
      AQuality: Integer
    );
  end;

implementation

uses
  LibECW,
  ALString,
  t_ECW,
  u_CalcWFileParams,
  u_ImageLineProvider,
  u_GeoFunc,
  u_ResStrings;

constructor TBitmapMapCombinerECW.Create(
  const AProgressUpdate: IBitmapCombineProgressUpdate;
  AQuality: Integer
);
begin
  inherited Create;
  FProgressUpdate := AProgressUpdate;
  FQuality := AQuality;
end;

function TBitmapMapCombinerECW.ReadLine(
  ALine: Integer;
  var LineR, LineG,
  LineB: PLineRGB
): Boolean;
type
  TBGR = packed record
    B: Byte;
    G: Byte;
    R: Byte;
  end;
  PArrayBGR = ^TArrayBGR;
  TArrayBGR = array [0..0] of TBGR;
var
  VRGB: PArrayBGR;
  i: Integer;
  VWidth: Integer;
begin
  VWidth := FImageLineProvider.ImageSize.X;
  VRGB := FImageLineProvider.GetLine(FOperationID, FCancelNotifier, ALine);
  for i := 0 to VWidth - 1 do begin
    LineR[i] := VRGB[i].R;
    LineG[i] := VRGB[i].G;
    LineB[i] := VRGB[i].B;
  end;
  if ALine mod 256 = 0 then begin
    FProgressUpdate.Update(ALine / FLinesCount);
  end;
  Result := True;
end;

procedure TBitmapMapCombinerECW.SaveRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AFileName: string;
  const AImageProvider: IBitmapTileProvider;
  const AMapRect: TRect
);
var
  VEPSG: Integer;
  Datum, Proj: AnsiString;
  Units: TCellSizeUnits;
  CellIncrementX, CellIncrementY, OriginX, OriginY: Double;
  errecw: integer;
  VECWWriter: TECWWrite;
  VCurrentPieceRect: TRect;
  VProjection: IProjection;
  VMapPieceSize: TPoint;
begin
  FOperationID := AOperationID;
  FCancelNotifier := ACancelNotifier;

  VECWWriter := TECWWrite.Create;
  try
    FImageLineProvider :=
      TImageLineProviderBGR.Create(
        AImageProvider,
        AMapRect
      );
    VProjection := AImageProvider.Projection;
    VCurrentPieceRect := AMapRect;
    VMapPieceSize := RectSize(AMapRect);
    FLinesCount := VMapPieceSize.Y;
    VEPSG := VProjection.ProjectionType.Datum.EPSG;
    if VEPSG > 0 then begin
      Datum := 'EPSG:' + ALIntToStr(VEPSG);
    end else begin
      Datum := 'RAW';
    end;
    VEPSG := VProjection.ProjectionType.ProjectionEPSG;
    if VEPSG > 0 then begin
      Proj := 'EPSG:' + ALIntToStr(VEPSG);
    end else begin
      Proj := 'RAW';
    end;
    Units := GetUnitsByProjectionEPSG(VEPSG);
    CalculateWFileParams(
      VProjection.PixelPos2LonLat(VCurrentPieceRect.TopLeft),
      VProjection.PixelPos2LonLat(VCurrentPieceRect.BottomRight),
      VMapPieceSize.X, VMapPieceSize.Y, VProjection.ProjectionType,
      CellIncrementX, CellIncrementY, OriginX, OriginY
    );
    errecw :=
      VECWWriter.Encode(
        FOperationID,
        FCancelNotifier,
        AFileName,
        VMapPieceSize.X,
        VMapPieceSize.Y,
        101 - FQuality,
        COMPRESS_HINT_BEST,
        ReadLine,
        Datum,
        Proj,
        Units,
        CellIncrementX,
        CellIncrementY,
        OriginX,
        OriginY
      );
    if (errecw > 0) and (errecw <> 52) then begin
      raise Exception.Create(SAS_ERR_Save + ' ' + SAS_ERR_Code + inttostr(errecw));
    end;
  finally
    FreeAndNil(VECWWriter);
  end;
end;

end.
