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

unit u_BitmapTileMatrixChangeableByVectorMatrix;

interface

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

    FPreparedHashMatrix: IHashTileMatrixBuilder;

    FSourceTileMatrixListener: IListener;
    FTileRendererListener: IListener;
    FAppStartedListener: IListener;
    FAppClosingListener: IListener;
    FIsNeedFullUpdate: ISimpleFlag;

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
    function GetStatic: IBitmapTileMatrix;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      const ASourceTileMatrix: IVectorTileMatrixChangeable;
      const ATileRenderer: IVectorTileRendererChangeable;
      const AImageResampler: IImageResamplerFactoryChangeable;
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
  i_CoordConverter,
  i_ProjectionInfo,
  i_VectorTileRenderer,
  i_VectorTileMatrix,
  i_VectorItemSubset,
  u_SimpleFlagWithInterlock,
  u_ListenerByEvent,
  u_TileIteratorSpiralByRect,
  u_HashTileMatrixBuilder,
  u_BitmapTileMatrixBuilder,
  u_BackgroundTask;

{ TBitmapTileMatrixChangeableByVectorMatrix }

constructor TBitmapTileMatrixChangeableByVectorMatrix.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  const ASourceTileMatrix: IVectorTileMatrixChangeable;
  const ATileRenderer: IVectorTileRendererChangeable;
  const AImageResampler: IImageResamplerFactoryChangeable;
  const ABitmapFactory: IBitmap32StaticFactory;
  const AHashFunction: IHashFunction;
  const AThreadConfig: IThreadConfig;
  const ADebugName: string
);
var
  VDebugName: string;
begin
  Assert(Assigned(AAppStartedNotifier));
  Assert(Assigned(AAppClosingNotifier));
  Assert(Assigned(ATileRenderer));
  Assert(Assigned(ASourceTileMatrix));
  VDebugName := ADebugName;
  if VDebugName = '' then begin
    VDebugName := Self.ClassName;
  end;
  inherited Create;

  FAppStartedNotifier := AAppStartedNotifier;
  FAppClosingNotifier := AAppClosingNotifier;
  FTileRenderer := ATileRenderer;
  FSourceTileMatrix := ASourceTileMatrix;
  FDebugName := VDebugName;

  FIsNeedFullUpdate := TSimpleFlagWithInterlock.Create;
  FSourceTileMatrixListener := TNotifyNoMmgEventListener.Create(Self.OnSourceTileMatrixChange);
  FTileRendererListener := TNotifyNoMmgEventListener.Create(Self.OnTileRendererChange);

  FOneTilePrepareCounter := APerfList.CreateAndAddNewCounter('OneTilePrepare');
  FUpdateResultCounter := APerfList.CreateAndAddNewCounter('UpdateResult');
  FMatrixChangeRectCounter := APerfList.CreateAndAddNewCounter('MatrixChangeRect');

  FPreparedHashMatrix := THashTileMatrixBuilder.Create(AHashFunction);
  FPreparedBitmapMatrix :=
    TBitmapTileMatrixBuilder.Create(
      AImageResampler,
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
  FAppClosingListener := TNotifyNoMmgEventListener.Create(Self.OnAppClosing);
  FAppStartedNotifier.Add(FAppStartedListener);
  if FAppStartedNotifier.IsExecuted then begin
    OnAppStarted;
  end;
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
  FDrawTask.StopExecute;
  FDrawTask.StartExecute;
end;

procedure TBitmapTileMatrixChangeableByVectorMatrix.OnTileRendererChange;
begin
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
  VProjection: IProjectionInfo;
  VConverter: ICoordConverter;
  VCounterContext: TInternalPerformanceCounterContext;
  VBitmap: IBitmap32Static;
  VSourceItem: IVectorItemSubset;
  VSourceHash: THashValue;
  VTileRectChanged: Boolean;
  VIsNeedFullRedraw: Boolean;
begin
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
          Exit;
        end;
      end else begin
        if VIsNeedFullRedraw then begin
          FPreparedHashMatrix.SetRectWithReset(VTileRect, 0);
        end;
      end;
      VProjection := VTileRect.ProjectionInfo;
      VConverter := VProjection.GeoConverter;
      VTileIterator := TTileIteratorSpiralByRect.Create(VTileRect.Rect);
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
              Exit;
            end;
            FPreparedBitmapMatrix.Tiles[VTile] := VBitmap;
            FPreparedHashMatrix.Tiles[VTile] := VSourceHash;
            DoUpdateResultAndNotify;
            if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
              Exit;
            end;
          end;
        end else begin
          if Assigned(FPreparedBitmapMatrix.Tiles[VTile]) then begin
            FPreparedBitmapMatrix.Tiles[VTile] := nil;
            FPreparedHashMatrix.Tiles[VTile] := 0;
            DoUpdateResultAndNotify;
            if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
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
      DoChangeNotify;
    end;
  finally
    FUpdateResultCounter.FinishOperation(VCounterContext);
  end;
end;

end.
