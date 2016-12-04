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

unit u_BitmapMapCombinerECWJP2;

interface

{$IFDEF DEBUG}
  {$DEFINE SHOW_COMPRESSION_STAT}
{$ENDIF}

uses
  SysUtils,
  Types,
  i_NotifierOperation,
  i_BitmapTileProvider,
  i_Projection,
  i_ImageLineProvider,
  i_BitmapMapCombiner,
  u_ECWJP2Write,
  u_BaseInterfacedObject;

type
  TBitmapMapCombinerECWJP2 = class(TBaseInterfacedObject, IBitmapMapCombiner)
  private
    FCompressionRatio: Single;
    FEncodeInfo: TECWCompressionStatistics;
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

    {$IFDEF SHOW_COMPRESSION_STAT}
    procedure ShowCompressionStat;
    {$ENDIF}
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
      const AQuality: Integer
    );
  end;

implementation

uses
  {$IFDEF SHOW_COMPRESSION_STAT}
  Classes,
  Dialogs,
  {$ENDIF}
  libecwj2,
  ALString,
  t_ECW,
  c_CoordConverter,
  u_CalcWFileParams,
  u_ImageLineProvider,
  u_GeoFunc,
  u_ResStrings;

constructor TBitmapMapCombinerECWJP2.Create(
  const AProgressUpdate: IBitmapCombineProgressUpdate;
  const AQuality: Integer
);
begin
  inherited Create;
  FProgressUpdate := AProgressUpdate;
  FQuality := AQuality;
end;

function TBitmapMapCombinerECWJP2.ReadLine(
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

procedure TBitmapMapCombinerECWJP2.SaveRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AFileName: string;
  const AImageProvider: IBitmapTileProvider;
  const AMapRect: TRect
);
const
  NCS_SUCCESS = 0;
  NCS_USER_CANCELLED_COMPRESSION = 52;
var
  VEPSG: Integer;
  Datum, Proj: AnsiString;
  Units: TCellSizeUnits;
  CellIncrementX, CellIncrementY, OriginX, OriginY: Double;
  VErrorCode: Integer;
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
    if VEPSG = CGELonLatProjectionEPSG then begin
      Proj := 'GEODETIC';
      Datum := 'WGS84';
      Units := CELL_UNITS_DEGREES;
    end else begin
      if VEPSG > 0 then begin
        Proj := 'EPSG:' + ALIntToStr(VEPSG);
      end else begin
        Proj := 'RAW';
      end;
      Units := GetUnitsByProjectionEPSG(VEPSG);
    end;

    CalculateWFileParams(
      CalculatePixelLonLat(VProjection, VCurrentPieceRect.TopLeft),
      CalculatePixelLonLat(VProjection, VCurrentPieceRect.BottomRight),
      VMapPieceSize.X, VMapPieceSize.Y, VProjection.ProjectionType,
      CellIncrementX, CellIncrementY, OriginX, OriginY
    );

    FCompressionRatio := 101 - FQuality;

    VErrorCode :=
      VECWWriter.Encode(
        FOperationID,
        FCancelNotifier,
        AFileName,
        VMapPieceSize.X,
        VMapPieceSize.Y,
        FCompressionRatio,
        COMPRESS_HINT_BEST,
        ReadLine,
        Datum,
        Proj,
        Units,
        CellIncrementX,
        CellIncrementY,
        OriginX,
        OriginY,
        @FEncodeInfo
      );
    if (VErrorCode > NCS_SUCCESS) and (VErrorCode <> NCS_USER_CANCELLED_COMPRESSION) then begin
      raise Exception.Create(
        SAS_ERR_Save + ' ' + SAS_ERR_Code + ' [' + IntToStr(VErrorCode) + '] ' +
        VECWWriter.ErrorCodeToString(VErrorCode)
      );
    end else begin
      {$IFDEF SHOW_COMPRESSION_STAT}
      FProgressUpdate.Update(1); // 100%
      TThread.Synchronize(nil, ShowCompressionStat);
      {$ENDIF}
    end;
  finally
    FreeAndNil(VECWWriter);
  end;
end;

{$IFDEF SHOW_COMPRESSION_STAT}
procedure TBitmapMapCombinerECWJP2.ShowCompressionStat;

  function _SizeToStr(const ASizeInKb: Double): string;
  begin
    if ASizeInKb > 1048576 then begin
      Result := FormatFloat('0.0', ASizeInKb / 1048576) + ' GB';
    end else begin
      if ASizeInKb > 1024 then begin
        Result := FormatFloat('0.0', ASizeInKb / 1024) + ' MB';
      end else begin
        Result := FormatFloat('0.0', ASizeInKb) + ' KB';
      end;
    end;
  end;

  function _SecondsToTime(const Seconds: Double): TDateTime;
  const
    MilliSecsPerDay = 86400000;
  begin
    Result := Round(Seconds * 1000) / MilliSecsPerDay;
  end;

var
  VEncodeInfoMsg: string;
begin
  VEncodeInfoMsg := Format(
    'Compression finished sucessfull at %s sec.' + #13#10 + #13#10 +
    'Target compression ratio: %2.f' + #13#10 +
    'Actual compression ratio: %2.f' + #13#10 +
    'Compression speed: %4.f MB/sec' + #13#10 +
    'Output file size: %s',
    [FormatDateTime('hh:nn:ss.zzz', _SecondsToTime(FEncodeInfo.CompressionSeconds)),
     FCompressionRatio,
     FEncodeInfo.ActualCompression,
     FEncodeInfo.CompressionMBSec,
     _SizeToStr(FEncodeInfo.OutputSize / 1024)
    ]
  );
  MessageDlg(VEncodeInfoMsg, mtInformation, [mbOK], 0);
end;
{$ENDIF}

end.
