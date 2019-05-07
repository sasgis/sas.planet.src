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
  i_InternalPerformanceCounter,
  i_BitmapMapCombiner,
  i_RegionProcessParamsFrame,
  u_BitmapMapCombinerFactoryBase;

type
  TBitmapMapCombinerFactoryECW = class(TBitmapMapCombinerFactoryBase)
  private
    FSaveRectCounter: IInternalPerformanceCounter;
    FPrepareDataCounter: IInternalPerformanceCounter;
    FGetLineCounter: IInternalPerformanceCounter;
  protected
    function PrepareMapCombiner(
      const AParams: IRegionProcessParamsFrameMapCombine;
      const AProgressInfo: IBitmapCombineProgressUpdate
    ): IBitmapMapCombiner; override;
  public
    constructor Create(
      const ACounterList: IInternalPerformanceCounterList
    );
  end;

type
  TBitmapMapCombinerFactoryJP2 = class(TBitmapMapCombinerFactoryBase)
  private
    FSaveRectCounter: IInternalPerformanceCounter;
    FPrepareDataCounter: IInternalPerformanceCounter;
    FGetLineCounter: IInternalPerformanceCounter;
    FLossless: Boolean;
  protected
    function PrepareMapCombiner(
      const AParams: IRegionProcessParamsFrameMapCombine;
      const AProgressInfo: IBitmapCombineProgressUpdate
    ): IBitmapMapCombiner; override;
  public
    constructor Create(
      const ACounterList: IInternalPerformanceCounterList;
      const ALossless: Boolean
    );
  end;

implementation

uses
  SysUtils,
  Types,
  {$IFDEF SHOW_COMPRESSION_STAT}
  Classes,
  Dialogs,
  UITypes,
  {$ENDIF}
  gnugettext,
  libecwj2,
  ALString,
  t_ECW,
  t_CommonTypes,
  t_MapCombineOptions,
  c_CoordConverter,
  i_NotifierOperation,
  i_BitmapTileProvider,
  i_Projection,
  i_ImageLineProvider,
  u_BaseInterfacedObject,
  u_CalcWFileParams,
  u_ECWJP2Write,
  u_ImageLineProvider,
  u_GeoFunc,
  u_StrFunc,
  u_ResStrings;

type
  TBitmapMapCombinerECWJP2 = class(TBaseInterfacedObject, IBitmapMapCombiner)
  private
    FCompressionRatio: Single;
    FEncodeInfo: TECWCompressionStatistics;
    FProgressUpdate: IBitmapCombineProgressUpdate;
    FSaveRectCounter: IInternalPerformanceCounter;
    FPrepareDataCounter: IInternalPerformanceCounter;
    FGetLineCounter: IInternalPerformanceCounter;
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
      const ASaveRectCounter: IInternalPerformanceCounter;
      const APrepareDataCounter: IInternalPerformanceCounter;
      const AGetLineCounter: IInternalPerformanceCounter;
      const AQuality: Integer
    );
  end;

constructor TBitmapMapCombinerECWJP2.Create(
  const AProgressUpdate: IBitmapCombineProgressUpdate;
  const ASaveRectCounter: IInternalPerformanceCounter;
  const APrepareDataCounter: IInternalPerformanceCounter;
  const AGetLineCounter: IInternalPerformanceCounter;
  const AQuality: Integer
);
begin
  inherited Create;
  FProgressUpdate := AProgressUpdate;
  FSaveRectCounter := ASaveRectCounter;
  FPrepareDataCounter := APrepareDataCounter;
  FGetLineCounter := AGetLineCounter;
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
  VOutputFileName: AnsiString;
  VContext: TInternalPerformanceCounterContext;
begin
  if IsAnsi(AFileName) then begin
    VOutputFileName := AnsiString(AFileName);
  end else begin
    raise Exception.CreateFmt(
      'ECW/JP2K encoder does not support unicode file names: %s', [AFileName]
    );
  end;

  FOperationID := AOperationID;
  FCancelNotifier := ACancelNotifier;

  VContext := FSaveRectCounter.StartOperation;
  try
    VECWWriter := TECWWrite.Create;
    try
      FImageLineProvider :=
        TImageLineProviderBGR.Create(
          FPrepareDataCounter,
          FGetLineCounter,
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
        VProjection.PixelPos2LonLat(VCurrentPieceRect.TopLeft),
        VProjection.PixelPos2LonLat(VCurrentPieceRect.BottomRight),
        VMapPieceSize.X, VMapPieceSize.Y, VProjection.ProjectionType,
        CellIncrementX, CellIncrementY, OriginX, OriginY
      );

      FCompressionRatio := 101 - FQuality;

      VErrorCode :=
        VECWWriter.Encode(
          FOperationID,
          FCancelNotifier,
          VOutputFileName,
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
  finally
    FSaveRectCounter.FinishOperation(VContext);
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

{ TBitmapMapCombinerFactoryECW }

constructor TBitmapMapCombinerFactoryECW.Create(
  const ACounterList: IInternalPerformanceCounterList
);
var
  VCounterList: IInternalPerformanceCounterList;
begin
  inherited Create(
    Point(128, 128),
    Point(MaxInt, MaxInt),
    stsAnsi,
    'ecw',
    gettext_NoExtract('ECW (Enhanced Compression Wavelet)'),
    [mcQuality]
  );
  VCounterList := ACounterList.CreateAndAddNewSubList('ECW');
  FSaveRectCounter := VCounterList.CreateAndAddNewCounter('SaveRect');
  FPrepareDataCounter := VCounterList.CreateAndAddNewCounter('PrepareData');
  FGetLineCounter := VCounterList.CreateAndAddNewCounter('GetLine');
end;

function TBitmapMapCombinerFactoryECW.PrepareMapCombiner(
  const AParams: IRegionProcessParamsFrameMapCombine;
  const AProgressInfo: IBitmapCombineProgressUpdate
): IBitmapMapCombiner;
begin
  Result :=
    TBitmapMapCombinerECWJP2.Create(
      AProgressInfo,
      FSaveRectCounter,
      FPrepareDataCounter,
      FGetLineCounter,
      AParams.CustomOptions.Quality
    );
end;

{ TBitmapMapCombinerFactoryJP2 }

constructor TBitmapMapCombinerFactoryJP2.Create(
  const ACounterList: IInternalPerformanceCounterList;
  const ALossless: Boolean
);
var
  VCaption: string;
  VOptions: TMapCombineOptionsSet;
  VCounterList: IInternalPerformanceCounterList;
begin
  FLossless := ALossless;

  VCaption := 'JPEG2000';
  VOptions := [mcQuality];

  if FLossless then begin
    VCaption := VCaption + ' (Lossless Compression)';
    VOptions := [];
  end;

  inherited Create(
    Point(2, 2),
    Point(MaxInt, MaxInt),
    stsAnsi,
    'jp2',
    gettext_NoExtract(VCaption),
    VOptions
  );
  VCounterList := ACounterList.CreateAndAddNewSubList('JPEG2000');
  FSaveRectCounter := VCounterList.CreateAndAddNewCounter('SaveRect');
  FPrepareDataCounter := VCounterList.CreateAndAddNewCounter('PrepareData');
  FGetLineCounter := VCounterList.CreateAndAddNewCounter('GetLine');
end;

function TBitmapMapCombinerFactoryJP2.PrepareMapCombiner(
  const AParams: IRegionProcessParamsFrameMapCombine;
  const AProgressInfo: IBitmapCombineProgressUpdate
): IBitmapMapCombiner;
var
  VQuality: Integer;
begin
  if FLossless then begin
    VQuality := 100;
  end else begin
    VQuality := AParams.CustomOptions.Quality;
  end;
  Result :=
    TBitmapMapCombinerECWJP2.Create(
      AProgressInfo,
      FSaveRectCounter,
      FPrepareDataCounter,
      FGetLineCounter,
      VQuality
    );
end;

end.
