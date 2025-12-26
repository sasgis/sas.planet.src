{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_BitmapTileMatrixChangeableByVectorMatrix;

interface

{$I DebugLog.inc}

uses
  SysUtils,
  i_BackgroundTask,
  i_ThreadConfig,
  i_InternalPerformanceCounter,
  i_NotifierOperation,
  i_Listener,
  i_SimpleFlag,
  i_ImageResamplerFactoryChangeable,
  i_Bitmap32BufferFactory,
  i_HashFunction,
  i_TileRect,
  i_BitmapTileMatrix,
  i_BitmapTileMatrixBuilder,
  i_BitmapTileMatrixChangeable,
  i_VectorTileMatrixChangeable,
  i_VectorTileRendererChangeable,
  i_HashTileMatrixBuilder,
  u_ChangeableBase;

type
  TBitmapTileMatrixChangeableByVectorMatrix = class(TChangeableWithSimpleLockBase, IBitmapTileMatrixChangeable)
  private
    FAppStartedNotifier: INotifierOneOperation;
    FAppClosingNotifier: INotifierOneOperation;
    FSourceTileMatrix: IVectorTileMatrixChangeable;
    FTileRenderer: IVectorTileRendererChangeable;
    FDebugName: string;

    FDrawTask: IBackgroundTask;
    FOneTilePrepareCounter: IInternalPerformanceCounter;
    FMatrixChangeRectCounter: IInternalPerformanceCounter;
    FUpdateResultCounter: IInternalPerformanceCounter;

    FSourceTileMatrixListener: IListener;
    FTileRendererListener: IListener;
    FAppStartedListener: IListener;
    FAppClosingListener: IListener;
    FIsNeedFullUpdate: ISimpleFlag;

    FPrepareStateChangeable: IBitmapTileMatrixStateChangeableInternal;

    FPreparedHashMatrix: IHashTileMatrixBuilder;
    FPreparedBitmapMatrix: IBitmapTileMatrixBuilder;

    FResult: IBitmapTileMatrix;

    procedure OnAppStarted;
    procedure OnAppClosing;
    procedure OnSourceTileMatrixChange;
    procedure OnTileRendererChange;

    procedure DoUpdateResultAndNotify;
    procedure OnPrepareTileMatrix(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation
    );
  private
    { IBitmapTileMatrixChangeable }
    function GetPrepareStateChangeable: IBitmapTileMatrixStateChangeable;
    function GetStatic: IBitmapTileMatrix;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      const ASourceTileMatrix: IVectorTileMatrixChangeable;
      const ATileRenderer: IVectorTileRendererChangeable;
      const AImageResampler: IImageResamplerFactoryChangeable;
      const AIsReprojectTiles: Boolean;
      const ABitmapFactory: IBitmap32StaticFactory;
      const AHashFunction: IHashFunction;
      const AThreadConfig: IThreadConfig;
      const ADebugName: string
    );
    destructor Destroy; override;
  end;

implementation

uses
  Types,
  t_Hash,
  i_TileIterator,
  i_Bitmap32Static,
  i_Projection,
  i_VectorTileRenderer,
  i_VectorTileMatrix,
  i_VectorItemSubset,
  {$IFDEF ENABLE_BITMAP_TILE_MATRIX_LOGGING}
  u_DebugLogger,
  {$ENDIF}
  u_SimpleFlagWithInterlock,
  u_ListenerByEvent,
  u_TileIteratorSpiralByRect,
  u_HashTileMatrixBuilder,
  u_BitmapTileMatrixBuilder,
  u_BitmapTileMatrixStateChangeable,
  u_BackgroundTask;

{ TBitmapTileMatrixChangeableByVectorMatrix }

constructor TBitmapTileMatrixChangeableByVectorMatrix.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  const ASourceTileMatrix: IVectorTileMatrixChangeable;
  const ATileRenderer: IVectorTileRendererChangeable;
  const AImageResampler: IImageResamplerFactoryChangeable;
  const AIsReprojectTiles: Boolean;
  const ABitmapFactory: IBitmap32StaticFactory;
  const AHashFunction: IHashFunction;
  const AThreadConfig: IThreadConfig;
  const ADebugName: string
);
begin
  Assert(Assigned(AAppStartedNotifier));
  Assert(Assigned(AAppClosingNotifier));
  Assert(Assigned(ATileRenderer));
  Assert(Assigned(ASourceTileMatrix));

  inherited Create;

  FAppStartedNotifier := AAppStartedNotifier;
  FAppClosingNotifier := AAppClosingNotifier;
  FTileRenderer := ATileRenderer;
  FSourceTileMatrix := ASourceTileMatrix;

  FDebugName := ADebugName;
  if FDebugName = '' then begin
    FDebugName := Self.ClassName;
  end;

  FIsNeedFullUpdate := TSimpleFlagWithInterlock.Create;
  FSourceTileMatrixListener := TNotifyNoMmgEventListener.Create(Self.OnSourceTileMatrixChange);
  FTileRendererListener := TNotifyNoMmgEventListener.Create(Self.OnTileRendererChange);

  FOneTilePrepareCounter := APerfList.CreateAndAddNewCounter('OneTilePrepare');
  FUpdateResultCounter := APerfList.CreateAndAddNewCounter('UpdateResult');
  FMatrixChangeRectCounter := APerfList.CreateAndAddNewCounter('MatrixChangeRect');

  FPrepareStateChangeable := TBitmapTileMatrixStateChangeable.Create(FDebugName);

  FPreparedHashMatrix := THashTileMatrixBuilder.Create(AHashFunction);
  FPreparedBitmapMatrix :=
    TBitmapTileMatrixBuilder.Create(
      AImageResampler,
      AIsReprojectTiles,
      ABitmapFactory,
      AHashFunction
    );

  FDrawTask :=
    TBackgroundTask.Create(
      AAppClosingNotifier,
      OnPrepareTileMatrix,
      AThreadConfig,
      FDebugName
    );

  FAppStartedListener := TNotifyNoMmgEventListener.Create(Self.OnAppStarted);
  FAppStartedNotifier.Add(FAppStartedListener);
  if FAppStartedNotifier.IsExecuted then begin
    OnAppStarted;
  end;

  FAppClosingListener := TNotifyNoMmgEventListener.Create(Self.OnAppClosing);
  FAppClosingNotifier.Add(FAppClosingListener);
  if FAppClosingNotifier.IsExecuted then begin
    OnAppClosing;
  end;
end;

destructor TBitmapTileMatrixChangeableByVectorMatrix.Destroy;
begin
  if Assigned(FSourceTileMatrix) and Assigned(FSourceTileMatrixListener) then begin
    FSourceTileMatrix.ChangeNotifier.Remove(FSourceTileMatrixListener);
    FSourceTileMatrixListener := nil;
  end;
  if Assigned(FTileRenderer) and Assigned(FTileRendererListener) then begin
    FTileRenderer.ChangeNotifier.Remove(FTileRendererListener);
    FTileRendererListener := nil;
  end;
  if Assigned(FAppStartedNotifier) and Assigned(FAppStartedListener) then begin
    FAppStartedNotifier.Remove(FAppStartedListener);
    FAppStartedNotifier := nil;
  end;
  if Assigned(FAppClosingNotifier) and Assigned(FAppClosingListener) then begin
    FAppClosingNotifier.Remove(FAppClosingListener);
    FAppClosingNotifier := nil;
  end;
  inherited;
end;

function TBitmapTileMatrixChangeableByVectorMatrix.GetPrepareStateChangeable: IBitmapTileMatrixStateChangeable;
begin
  Result := FPrepareStateChangeable as IBitmapTileMatrixStateChangeable;
end;

function TBitmapTileMatrixChangeableByVectorMatrix.GetStatic: IBitmapTileMatrix;
begin
  CS.BeginRead;
  try
    Result := FResult;
  finally
    CS.EndRead;
  end;
end;

procedure TBitmapTileMatrixChangeableByVectorMatrix.OnAppClosing;
begin
  FDrawTask.Terminate;
end;

procedure TBitmapTileMatrixChangeableByVectorMatrix.OnAppStarted;
begin
  if Assigned(FSourceTileMatrix) and Assigned(FSourceTileMatrixListener) then begin
    FSourceTileMatrix.ChangeNotifier.Add(FSourceTileMatrixListener);
  end;
  if Assigned(FTileRenderer) and Assigned(FTileRendererListener) then begin
    FTileRenderer.ChangeNotifier.Add(FTileRendererListener);
  end;
  FDrawTask.Start;
  FDrawTask.StartExecute;
end;

procedure TBitmapTileMatrixChangeableByVectorMatrix.OnSourceTileMatrixChange;
begin
  {$IFDEF ENABLE_BITMAP_TILE_MATRIX_LOGGING}
  GLog.Write(Self, '%s: OnSourceTileMatrixChange', [FDebugName]);
  {$ENDIF}
  FDrawTask.StopExecute;
  FDrawTask.StartExecute;
end;

procedure TBitmapTileMatrixChangeableByVectorMatrix.OnTileRendererChange;
begin
  {$IFDEF ENABLE_BITMAP_TILE_MATRIX_LOGGING}
  GLog.Write(Self, '%s: OnTileRendererChange', [FDebugName]);
  {$ENDIF}
  FDrawTask.StopExecute;
  FIsNeedFullUpdate.SetFlag;
  FDrawTask.StartExecute;
end;

procedure TBitmapTileMatrixChangeableByVectorMatrix.OnPrepareTileMatrix(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation
);
var
  VRenderer: IVectorTileRenderer;
  VTileRect: ITileRect;
  VTileIterator: ITileIterator;
  VSourceMatrix: IVectorTileMatrix;
  VTile: TPoint;
  VProjection: IProjection;
  VCounterContext: TInternalPerformanceCounterContext;
  VBitmap: IBitmap32Static;
  VSourceItem: IVectorItemSubset;
  VSourceHash: THashValue;
  VTileRectChanged: Boolean;
  VIsNeedFullRedraw: Boolean;
begin
  FPrepareStateChangeable.State := psBusy;
  VRenderer := FTileRenderer.GetStatic;
  if Assigned(VRenderer) then begin
    VSourceMatrix := FSourceTileMatrix.GetStatic;
    if Assigned(VSourceMatrix) then begin
      VTileRect := VSourceMatrix.TileRect;
      VTileRectChanged := not VTileRect.IsEqual(FPreparedBitmapMatrix.TileRect);
      VIsNeedFullRedraw := FIsNeedFullUpdate.CheckFlagAndReset;

      if VTileRectChanged then begin
        VCounterContext := FMatrixChangeRectCounter.StartOperation;
        try
          if VIsNeedFullRedraw then begin
            FPreparedHashMatrix.SetRectWithReset(VTileRect, 0);
          end else begin
            FPreparedHashMatrix.SetRect(VTileRect, 0);
          end;
          FPreparedBitmapMatrix.SetRect(VTileRect);
        finally
          FMatrixChangeRectCounter.FinishOperation(VCounterContext);
        end;
        DoUpdateResultAndNotify;
        if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
          FPrepareStateChangeable.State := psCancelled;
          Exit;
        end;
      end else begin
        if VIsNeedFullRedraw then begin
          FPreparedHashMatrix.SetRectWithReset(VTileRect, 0);
        end;
      end;
      VProjection := VTileRect.Projection;
      VTileIterator := TTileIteratorSpiralByRect.Create(VTileRect);
      while VTileIterator.Next(VTile) do begin
        VSourceItem := VSourceMatrix.GetElementByTile(VTile);
        if Assigned(VSourceItem) then begin
          VSourceHash := VSourceItem.Hash;
          if FPreparedHashMatrix.Tiles[VTile] <> VSourceHash then begin
            VCounterContext := FOneTilePrepareCounter.StartOperation;
            try
              VBitmap := VRenderer.RenderVectorTile(AOperationID, ACancelNotifier, VProjection, VTile, VSourceItem);
            finally
              FOneTilePrepareCounter.FinishOperation(VCounterContext);
            end;
            if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
              FPrepareStateChangeable.State := psCancelled;
              Exit;
            end;
            FPreparedBitmapMatrix.Tiles[VTile] := VBitmap;
            FPreparedHashMatrix.Tiles[VTile] := VSourceHash;
            DoUpdateResultAndNotify;
            if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
              FPrepareStateChangeable.State := psCancelled;
              Exit;
            end;
          end;
        end else begin
          if Assigned(FPreparedBitmapMatrix.Tiles[VTile]) then begin
            FPreparedBitmapMatrix.Tiles[VTile] := nil;
            FPreparedHashMatrix.Tiles[VTile] := 0;
            DoUpdateResultAndNotify;
            if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
              FPrepareStateChangeable.State := psCancelled;
              Exit;
            end;
          end;
        end;
      end;
    end else begin
      FPreparedHashMatrix.SetRectWithReset(nil, 0);
      FPreparedBitmapMatrix.SetRectWithReset(nil);
      DoUpdateResultAndNotify;
    end;
  end else begin
    FPreparedHashMatrix.SetRectWithReset(nil, 0);
    FPreparedBitmapMatrix.SetRectWithReset(nil);
    DoUpdateResultAndNotify;
  end;
  FPrepareStateChangeable.State := psComplete;
end;

procedure TBitmapTileMatrixChangeableByVectorMatrix.DoUpdateResultAndNotify;
var
  VCounterContext: TInternalPerformanceCounterContext;
  VResult: IBitmapTileMatrix;
  VChanged: Boolean;
begin
  VCounterContext := FUpdateResultCounter.StartOperation;
  try
    VResult := FPreparedBitmapMatrix.MakeStatic;
    CS.BeginWrite;
    try
      if Assigned(VResult) then begin
        if Assigned(FResult) then begin
          if VResult.Hash <> FResult.Hash then begin
            FResult := VResult;
            VChanged := True;
          end else begin
            VChanged := False;
          end;
        end else begin
          FResult := VResult;
          VChanged := True;
        end;
      end else begin
        if Assigned(FResult) then begin
          FResult := nil;
          VChanged := True;
        end else begin
          VChanged := False;
        end;
      end;
    finally
      CS.EndWrite;
    end;
    if VChanged then begin
      {$IFDEF ENABLE_BITMAP_TILE_MATRIX_LOGGING}
      GLog.Write(Self, '%s: DoChangeNotify', [FDebugName]);
      {$ENDIF}
      DoChangeNotify;
    end;
  finally
    FUpdateResultCounter.FinishOperation(VCounterContext);
  end;
end;

end.
