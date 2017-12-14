{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2017, SAS.Planet development team.                      *}
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

unit u_ImageLineProviderMultiThread;

interface

uses
  Types,
  Classes,
  SyncObjs,
  t_Bitmap32,
  i_InternalPerformanceCounter,
  i_NotifierOperation,
  i_Bitmap32Static,
  i_Projection,
  i_ImageLineProvider,
  i_BitmapTileProvider,
  u_BaseInterfacedObject;

type
  TImageLineProviderMultiThreadAbstract = class(TBaseInterfacedObject, IImageLineProvider)
  private
    FPrepareDataCounter: IInternalPerformanceCounter;
    FGetLineCounter: IInternalPerformanceCounter;
    FMapRect: TRect;
    FThreadCount: Integer;
    FFullTileRect: TRect;
    FImageProvider: IBitmapTileProvider;
    FBytesPerPixel: Integer;

    FProjection: IProjection;
    FStartEvents: array of TEvent;
    FThreadColPos: array of Integer;
    FFinishEvents: array of TEvent;
    FFinishEventHandles: array of THandle;
    FThreads: array of TThread;

    FPreparedTileRect: TRect;
    FPreparedMapRect: TRect;
    FPreparedData: array of Pointer;
    FOperationID: Integer;
    FCancelNotifier: INotifierOperation;

    function GetLocalLine(ALine: Integer): Pointer;
    procedure AddTile(
      const ABitmap: IBitmap32Static;
      const ATile: TPoint
    );
    procedure PrepareBufferMem(ARect: TRect);
    procedure ClearBuffer;
    function GetMapRectForLine(ALine: Integer): TRect;
    procedure PrepareBufferData(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AMapRect: TRect
    );
    procedure PrepareBufferDataPart(const AIndex: Integer);
  protected
    procedure PreparePixleLine(
      ASource: PColor32;
      ATarget: Pointer;
      ACount: Integer
    ); virtual; abstract;
  private
    function GetImageSize: TPoint;
    function GetBytesPerPixel: Integer;
    function GetLine(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      ALine: Integer
    ): Pointer;
  public
    constructor Create(
      const APrepareDataCounter: IInternalPerformanceCounter;
      const AGetLineCounter: IInternalPerformanceCounter;
      const AImageProvider: IBitmapTileProvider;
      const AThreadCount: Integer;
      const AMapRect: TRect;
      ABytesPerPixel: Integer
    );
    destructor Destroy; override;
  end;

  TImageLineProviderNoAlfaMultiThread = class(TImageLineProviderMultiThreadAbstract)
  public
    constructor Create(
      const APrepareDataCounter: IInternalPerformanceCounter;
      const AGetLineCounter: IInternalPerformanceCounter;
      const AImageProvider: IBitmapTileProvider;
      const AThreadCount: Integer;
      const AMapRect: TRect
    );
  end;

  TImageLineProviderWithAlfaMultiThread = class(TImageLineProviderMultiThreadAbstract)
  public
    constructor Create(
      const APrepareDataCounter: IInternalPerformanceCounter;
      const AGetLineCounter: IInternalPerformanceCounter;
      const AImageProvider: IBitmapTileProvider;
      const AThreadCount: Integer;
      const AMapRect: TRect
    );
  end;

  TImageLineProviderRGBMultiThread = class(TImageLineProviderNoAlfaMultiThread)
  protected
    procedure PreparePixleLine(
      ASource: PColor32;
      ATarget: Pointer;
      ACount: Integer
    ); override;
  end;

  TImageLineProviderBGRMultiThread = class(TImageLineProviderNoAlfaMultiThread)
  protected
    procedure PreparePixleLine(
      ASource: PColor32;
      ATarget: Pointer;
      ACount: Integer
    ); override;
  end;

  TImageLineProviderRGBAMultiThread = class(TImageLineProviderWithAlfaMultiThread)
  protected
    procedure PreparePixleLine(
      ASource: PColor32;
      ATarget: Pointer;
      ACount: Integer
    ); override;
  end;

  TImageLineProviderBGRAMultiThread = class(TImageLineProviderWithAlfaMultiThread)
  protected
    procedure PreparePixleLine(
      ASource: PColor32;
      ATarget: Pointer;
      ACount: Integer
    ); override;
  end;

implementation

uses
  Windows,
  Math,
  SysUtils,
  {$IFDEF EUREKALOG}
  ExceptionLog,
  ECore,
  {$ENDIF}
  t_GeoTypes,
  u_GeoFunc,
  u_ReadableThreadNames,
  u_TileIteratorByRect;

type
  TPartProcessMethod = procedure(const Index: Integer) of object;
type
  TWorkingThread = class(TThread)
  private
    FIndex: Integer;
    FExec: TPartProcessMethod;
    FStartEvent: TEvent;
    FFinishEvent: TEvent;
    FDebugName: string;
  protected
    procedure Execute; override;
  public
    constructor Create(
      const AIndex: Integer;
      const AStartEvent: TEvent;
      const AFinishEvent: TEvent;
      APriority: TThreadPriority;
      const ADebugName: string;
      AExec: TPartProcessMethod
    );
  end;

{ TWorkingThread }

constructor TWorkingThread.Create(
  const AIndex: Integer;
  const AStartEvent, AFinishEvent: TEvent;
  APriority: TThreadPriority;
  const ADebugName: string;
  AExec: TPartProcessMethod
);
begin
  inherited Create(False);
  FIndex := AIndex;
  FDebugName := ADebugName;
  Self.Priority := APriority;
  FExec := AExec;
  FStartEvent := AStartEvent;
  FFinishEvent := AFinishEvent;
end;

procedure TWorkingThread.Execute;
begin
  {$IFDEF EUREKALOG}
  try
  {$ENDIF}
    inherited;
    SetCurrentThreadName(FDebugName);
    if Terminated then begin
      Exit;
    end;
    while not Terminated do begin
      FStartEvent.WaitFor(INFINITE);
      if not Terminated then begin
        FExec(FIndex);
        FStartEvent.ResetEvent;
        FFinishEvent.SetEvent;
      end;
    end;
  {$IFDEF EUREKALOG}
  except
    ForceApplicationTermination(tbTerminate);
    ShowLastExceptionData;
  end;
  {$ENDIF}
end;

{ TImageLineProviderMultiThreadAbstract }

constructor TImageLineProviderMultiThreadAbstract.Create(
  const APrepareDataCounter: IInternalPerformanceCounter;
  const AGetLineCounter: IInternalPerformanceCounter;
  const AImageProvider: IBitmapTileProvider;
  const AThreadCount: Integer;
  const AMapRect: TRect;
  ABytesPerPixel: Integer
);
var
  VTileRectSize: TPoint;
  i: Integer;
begin
  Assert(Assigned(AImageProvider));
  Assert(AThreadCount > 1);
  Assert(AThreadCount <= MAXIMUM_WAIT_OBJECTS);
  Assert(AImageProvider.Projection.CheckPixelRect(AMapRect));
  inherited Create;
  FPrepareDataCounter := APrepareDataCounter;
  FGetLineCounter := AGetLineCounter;
  FImageProvider := AImageProvider;
  FThreadCount := AThreadCount;
  FMapRect := AMapRect;
  FBytesPerPixel := ABytesPerPixel;

  FProjection := FImageProvider.Projection;
  FFullTileRect := FProjection.PixelRect2TileRect(FMapRect);
  VTileRectSize := RectSize(FFullTileRect);
  Assert(VTileRectSize.X > FThreadCount);

  SetLength(FThreadColPos, FThreadCount + 1);
  SetLength(FStartEvents, FThreadCount);
  SetLength(FFinishEvents, FThreadCount);
  SetLength(FFinishEventHandles, FThreadCount);
  SetLength(FThreads, FThreadCount);

  for i := 0 to FThreadCount - 1 do begin
    FThreadColPos[i] := FFullTileRect.Left + Trunc(Int64(i) * VTileRectSize.X / FThreadCount);
    FStartEvents[i] := TEvent.Create;
    FFinishEvents[i] := TEvent.Create;
    FFinishEventHandles[i] := FFinishEvents[i].Handle;
    FThreads[i] :=
      TWorkingThread.Create(
        i,
        FStartEvents[i],
        FFinishEvents[i],
        tpLower,
        'ImageLineProviderMultiThread_' + IntToStr(i),
        Self.PrepareBufferDataPart
      );
  end;
  FThreadColPos[FThreadCount] := FFullTileRect.Right;
end;

destructor TImageLineProviderMultiThreadAbstract.Destroy;
var
  i: Integer;
begin
  for i := 0 to FThreadCount - 1 do begin
    FThreads[i].Terminate;
  end;
  for i := 0 to FThreadCount - 1 do begin
    FStartEvents[i].SetEvent;
  end;
  ClearBuffer;
  for i := 0 to FThreadCount - 1 do begin
    FThreads[i].WaitFor;
    FreeAndNil(FStartEvents[i]);
    FreeAndNil(FFinishEvents[i]);
    FreeAndNil(FThreads[i]);
  end;
  inherited;
end;

procedure TImageLineProviderMultiThreadAbstract.AddTile(
  const ABitmap: IBitmap32Static;
  const ATile: TPoint
);
var
  i: Integer;
  VTileMapRect: TRect;
  VTileSize: TPoint;
  VCopyRectSize: TPoint;
  VCopyMapRect: TRect;
  VCopyRectAtSource: TRect;
  VCopyRectAtTarget: TRect;
  VSourceLine: PColor32;
begin
  Assert(Assigned(ABitmap));
  Assert(PtInRect(FPreparedTileRect, ATile));
  VTileMapRect := FProjection.TilePos2PixelRect(ATile);
  VTileSize := ABitmap.Size;
  Assert(IsPointsEqual(VTileSize, RectSize(VTileMapRect)));
  IntersectRect(VCopyMapRect, VTileMapRect, FPreparedMapRect);

  VCopyRectSize := RectSize(VCopyMapRect);
  VCopyRectAtTarget := RectMove(VCopyMapRect, FPreparedMapRect.TopLeft);
  VCopyRectAtSource := RectMove(VCopyMapRect, VTileMapRect.TopLeft);

  for i := 0 to VCopyRectSize.Y - 1 do begin
    VSourceLine := @ABitmap.Data[VCopyRectAtSource.Left + (i + VCopyRectAtSource.Top) * VTileSize.X];
    PreparePixleLine(
      VSourceLine,
      Pointer(Cardinal(FPreparedData[i + VCopyRectAtTarget.Top]) + Cardinal(VCopyRectAtTarget.Left * FBytesPerPixel)),
      VCopyRectSize.X
    );
  end;
end;

procedure TImageLineProviderMultiThreadAbstract.ClearBuffer;
var
  i: Integer;
begin
  for i := 0 to Length(FPreparedData) - 1 do begin
    if FPreparedData[i] <> nil then begin
      FreeMem(FPreparedData[i]);
      FPreparedData[i] := nil;
    end;
  end;
  FPreparedData := nil;
end;

function TImageLineProviderMultiThreadAbstract.GetBytesPerPixel: Integer;
begin
  Result := FBytesPerPixel;
end;

function TImageLineProviderMultiThreadAbstract.GetImageSize: TPoint;
begin
  Result := RectSize(FMapRect);
end;

function TImageLineProviderMultiThreadAbstract.GetLine(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  ALine: Integer
): Pointer;
var
  VMapLine: Integer;
  VContext: TInternalPerformanceCounterContext;
begin
  Assert(ALine >= 0);
  VMapLine := FMapRect.Top + ALine;
  Assert(VMapLine < FMapRect.Bottom);
  Assert(VMapLine >= FMapRect.Top);
  if not IsRectEmpty(FPreparedMapRect) then begin
    if (VMapLine < FPreparedMapRect.Top) or (VMapLine >= FPreparedMapRect.Bottom) then begin
      FPreparedMapRect := Rect(0, 0, 0, 0);
    end;
  end;

  if IsRectEmpty(FPreparedMapRect) then begin
    VContext := FPrepareDataCounter.StartOperation;
    try
      FPreparedMapRect := GetMapRectForLine(ALine);
      PrepareBufferData(AOperationID, ACancelNotifier, FPreparedMapRect);
    finally
      FPrepareDataCounter.FinishOperation(VContext);
    end;
  end;
  VContext := FGetLineCounter.StartOperation;
  try
    Result := GetLocalLine(ALine);
  finally
    FGetLineCounter.FinishOperation(VContext);
  end;
end;

function TImageLineProviderMultiThreadAbstract.GetLocalLine(ALine: Integer): Pointer;
var
  VMapLine: Integer;
begin
  Assert(ALine >= 0);
  VMapLine := FMapRect.Top + ALine;
  Assert(VMapLine < FMapRect.Bottom);
  Assert(VMapLine >= FPreparedMapRect.Top);
  Assert(VMapLine < FPreparedMapRect.Bottom);
  Result := FPreparedData[VMapLine - FPreparedMapRect.Top];
end;

procedure TImageLineProviderMultiThreadAbstract.PrepareBufferData(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AMapRect: TRect
);
var
  i: Integer;
begin
  PrepareBufferMem(AMapRect);
  FPreparedTileRect := FProjection.PixelRect2TileRect(AMapRect);
  if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
    Exit;
  end;
  FOperationID := AOperationID;
  FCancelNotifier := ACancelNotifier;
  for i := 0 to FThreadCount - 1 do begin
    FStartEvents[i].SetEvent;
  end;
  // Start PrepareBufferDataPart for all parts
  WaitForMultipleObjects(FThreadCount, @FFinishEventHandles[0], True, INFINITE);
  for i := 0 to FThreadCount - 1 do begin
    FFinishEvents[i].ResetEvent;
  end;
  FCancelNotifier := nil;
end;

procedure TImageLineProviderMultiThreadAbstract.PrepareBufferDataPart(
  const AIndex: Integer
);
var
  VTile: TPoint;
  VIterator: TTileIteratorByRectRecord;
  VLeft, VRight: Integer;
  VTileRect: TRect;
begin
  VLeft := FThreadColPos[AIndex];
  VRight := FThreadColPos[AIndex + 1];

  VTileRect := Rect(VLeft, FPreparedTileRect.Top, VRight, FPreparedTileRect.Bottom);
  VIterator.Init(VTileRect);
  while VIterator.Next(VTile) do begin
    AddTile(
      FImageProvider.GetTile(FOperationID, FCancelNotifier, VTile),
      VTile
    );
  end;
end;

procedure TImageLineProviderMultiThreadAbstract.PrepareBufferMem(ARect: TRect);
var
  VLinesExists: Integer;
  VLinesNeed: Integer;
  VWidth: Integer;
  i: Integer;
begin
  VWidth := ARect.Right - ARect.Left;
  VLinesNeed := ARect.Bottom - ARect.Top;
  VLinesExists := Length(FPreparedData);
  if VLinesExists < VLinesNeed then begin
    SetLength(FPreparedData, VLinesNeed);
    for i := VLinesExists to VLinesNeed - 1 do begin
      GetMem(FPreparedData[i], (VWidth + 1) * FBytesPerPixel);
    end;
  end;
end;

function TImageLineProviderMultiThreadAbstract.GetMapRectForLine(ALine: Integer): TRect;
var
  VMapLine: Integer;
  VTilePos: TPoint;
  VPixelRect: TRect;
begin
  Assert(ALine >= 0);
  VMapLine := FMapRect.Top + ALine;
  Assert(VMapLine < FMapRect.Bottom);
  VTilePos := PointFromDoublePoint(FProjection.PixelPos2TilePosFloat(Point(FMapRect.Left, VMapLine)), prToTopLeft);
  VPixelRect := FProjection.TilePos2PixelRect(VTilePos);
  Result := Rect(FMapRect.Left, VPixelRect.Top, FMapRect.Right, VPixelRect.Bottom);
end;

{ TImageLineProviderNoAlfaMultiThread }

constructor TImageLineProviderNoAlfaMultiThread.Create(
  const APrepareDataCounter: IInternalPerformanceCounter;
  const AGetLineCounter: IInternalPerformanceCounter;
  const AImageProvider: IBitmapTileProvider;
  const AThreadCount: Integer;
  const AMapRect: TRect
);
begin
  inherited Create(
    APrepareDataCounter,
    AGetLineCounter,
    AImageProvider,
    AThreadCount,
    AMapRect,
    3
  );
end;

{ TImageLineProviderWithAlfaMultiThread }

constructor TImageLineProviderWithAlfaMultiThread.Create(
  const APrepareDataCounter: IInternalPerformanceCounter;
  const AGetLineCounter: IInternalPerformanceCounter;
  const AImageProvider: IBitmapTileProvider;
  const AThreadCount: Integer;
  const AMapRect: TRect
);
begin
  inherited Create(
    APrepareDataCounter,
    AGetLineCounter,
    AImageProvider,
    AThreadCount,
    AMapRect,
    4
  );
end;

type
  TBGR = packed record
    B: Byte;
    G: Byte;
    R: Byte;
  end;

  TRGB = packed record
    R: Byte;
    G: Byte;
    B: Byte;
  end;

  TRGBA = packed record
    R: Byte;
    G: Byte;
    B: Byte;
    A: Byte;
  end;



{ TImageLineProviderRGB }

procedure TImageLineProviderRGBMultiThread.PreparePixleLine(
  ASource: PColor32;
  ATarget: Pointer;
  ACount: Integer
);
var
  i: Integer;
  VSource: PColor32Entry;
  VTarget: ^TRGB;
begin
  Assert(Assigned(ASource));
  VSource := PColor32Entry(ASource);
  VTarget := ATarget;
  for i := 0 to ACount - 1 do begin
    VTarget.B := VSource.B;
    VTarget.G := VSource.G;
    VTarget.R := VSource.R;
    Inc(VSource);
    Inc(VTarget);
  end;
end;

{ TImageLineProviderBGRMultiThread }

procedure TImageLineProviderBGRMultiThread.PreparePixleLine(
  ASource: PColor32;
  ATarget: Pointer;
  ACount: Integer
);
var
  i: Integer;
  VSource: PColor32Entry;
  VTarget: ^TBGR;
begin
  Assert(Assigned(ASource));
  VSource := PColor32Entry(ASource);
  VTarget := ATarget;
  for i := 0 to ACount - 1 do begin
    VTarget.B := VSource.B;
    VTarget.G := VSource.G;
    VTarget.R := VSource.R;
    Inc(VSource);
    Inc(VTarget);
  end;
end;

{ TImageLineProviderARGBMultiThread }

procedure TImageLineProviderRGBAMultiThread.PreparePixleLine(
  ASource: PColor32;
  ATarget: Pointer;
  ACount: Integer
);
var
  i: Integer;
  VSource: PColor32Entry;
  VTarget: ^TRGBA;
begin
  Assert(Assigned(ASource));
  VSource := PColor32Entry(ASource);
  VTarget := ATarget;
  for i := 0 to ACount - 1 do begin
    VTarget.B := VSource.B;
    VTarget.G := VSource.G;
    VTarget.R := VSource.R;
    VTarget.A := VSource.A;
    Inc(VSource);
    Inc(VTarget);
  end;
end;

{ TImageLineProviderBGRAMultiThread }

procedure TImageLineProviderBGRAMultiThread.PreparePixleLine(
  ASource: PColor32;
  ATarget: Pointer;
  ACount: Integer
);
begin
  Assert(Assigned(ASource));
  Move(ASource^, ATarget^, ACount * SizeOf(ASource^));
end;

end.
