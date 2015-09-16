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

unit u_VectorTileMatrixChangeableByVectorSubsetChangeable;

interface

uses
  Types,
  SysUtils,
  i_BackgroundTask,
  i_ThreadConfig,
  i_InternalPerformanceCounter,
  i_NotifierOperation,
  i_Listener,
  i_SimpleFlag,
  i_HashFunction,
  i_TileRect,
  i_TileRectChangeable,
  i_VectorTileMatrix,
  i_VectorTileMatrixBuilder,
  i_VectorItemSubsetChangeable,
  i_HashTileMatrixBuilder,
  i_VectorItemSubsetBuilder,
  i_VectorTileMatrixChangeable,
  u_ChangeableBase;

type
  TVectorTileMatrixChangeableByVectorSubsetChangeable = class(TChangeableWithSimpleLockBase, IVectorTileMatrixChangeable)
  private
    FAppStartedNotifier: INotifierOneOperation;
    FAppClosingNotifier: INotifierOneOperation;
    FTileRect: ITileRectChangeable;
    FVectorSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FItemSelectOversize: TRect;
    FSource: IVectorItemSubsetChangeable;
    FDebugName: string;

    FDrawTask: IBackgroundTask;
    FOneTilePrepareCounter: IInternalPerformanceCounter;
    FMatrixChangeRectCounter: IInternalPerformanceCounter;
    FUpdateResultCounter: IInternalPerformanceCounter;

    FSourceCounter: ICounter;
    FTileRectPrev: ITileRect;

    FPreparedHashMatrix: IHashTileMatrixBuilder;

    FPosChangeListener: IListener;
    FSourceListener: IListener;
    FAppStartedListener: IListener;
    FAppClosingListener: IListener;

    FVisible: Boolean;
    FPreparedVectorMatrix: IVectorTileMatrixBuilder;
    FResult: IVectorTileMatrix;

    procedure OnAppStarted;
    procedure OnAppClosing;
    procedure OnSourceChange;
    procedure OnPosChange;

    procedure DoUpdateResultAndNotify;
    procedure OnPrepareTileMatrix(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation
    );
  private
    function GetStatic: IVectorTileMatrix;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      const ATileRect: ITileRectChangeable;
      const AHashFunction: IHashFunction;
      const AVectorSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const ASource: IVectorItemSubsetChangeable;
      const AThreadConfig: IThreadConfig;
      const AItemSelectOversize: TRect;
      const ADebugName: string
    );
    destructor Destroy; override;
  end;

implementation

uses
  t_Hash,
  i_TileIterator,
  i_VectorItemSubset,
  i_ProjectionInfo,
  i_VectorTileProvider,
  u_SimpleFlagWithInterlock,
  u_ListenerByEvent,
  u_TileIteratorSpiralByRect,
  u_HashTileMatrixBuilder,
  u_VectorTileMatrixBuilder,
  u_VectorTileProviderByFixedSubset,
  u_BackgroundTask;

{ TBitmapTileMatrixChangeableWithThread }

constructor TVectorTileMatrixChangeableByVectorSubsetChangeable.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier, AAppClosingNotifier: INotifierOneOperation;
  const ATileRect: ITileRectChangeable;
  const AHashFunction: IHashFunction;
  const AVectorSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const ASource: IVectorItemSubsetChangeable;
  const AThreadConfig: IThreadConfig;
  const AItemSelectOversize: TRect;
  const ADebugName: string
);
var
  VDebugName: string;
begin
  Assert(Assigned(AAppStartedNotifier));
  Assert(Assigned(AAppClosingNotifier));
  Assert(Assigned(ATileRect));
  Assert(Assigned(AVectorSubsetBuilderFactory));
  Assert(Assigned(ASource));
  Assert(AItemSelectOversize.Left >= 0);
  Assert(AItemSelectOversize.Left < 4096);
  Assert(AItemSelectOversize.Top >= 0);
  Assert(AItemSelectOversize.Top < 4096);
  Assert(AItemSelectOversize.Right >= 0);
  Assert(AItemSelectOversize.Right < 4096);
  Assert(AItemSelectOversize.Bottom >= 0);
  Assert(AItemSelectOversize.Bottom < 4096);
  VDebugName := ADebugName;
  if VDebugName = '' then begin
    VDebugName := Self.ClassName;
  end;
  inherited Create;

  FAppStartedNotifier := AAppStartedNotifier;
  FAppClosingNotifier := AAppClosingNotifier;
  FTileRect := ATileRect;
  FVectorSubsetBuilderFactory := AVectorSubsetBuilderFactory;
  FSource := ASource;
  FItemSelectOversize := AItemSelectOversize;
  FDebugName := VDebugName;

  FPosChangeListener := TNotifyNoMmgEventListener.Create(Self.OnPosChange);
  FSourceListener := TNotifyNoMmgEventListener.Create(Self.OnSourceChange);

  FOneTilePrepareCounter := APerfList.CreateAndAddNewCounter('OneTilePrepare');
  FUpdateResultCounter := APerfList.CreateAndAddNewCounter('UpdateResult');
  FMatrixChangeRectCounter := APerfList.CreateAndAddNewCounter('MatrixChangeRect');

  FSourceCounter := TCounterInterlock.Create;
  FPreparedHashMatrix := THashTileMatrixBuilder.Create(AHashFunction);
  FPreparedVectorMatrix :=
    TVectorTileMatrixBuilder.Create(
      AVectorSubsetBuilderFactory,
      AItemSelectOversize,
      AHashFunction
    );
  FVisible := False;

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

destructor TVectorTileMatrixChangeableByVectorSubsetChangeable.Destroy;
begin
  if Assigned(FTileRect) and Assigned(FPosChangeListener) then begin
    FTileRect.ChangeNotifier.Remove(FPosChangeListener);
    FPosChangeListener := nil;
  end;
  if Assigned(FSource) and Assigned(FSourceListener) then begin
    FSource.ChangeNotifier.Remove(FSourceListener);
    FSourceListener := nil;
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

function TVectorTileMatrixChangeableByVectorSubsetChangeable.GetStatic: IVectorTileMatrix;
begin
  CS.BeginRead;
  try
    Result := FResult;
  finally
    CS.EndRead;
  end;
end;

procedure TVectorTileMatrixChangeableByVectorSubsetChangeable.OnAppClosing;
begin
  FDrawTask.Terminate;
end;

procedure TVectorTileMatrixChangeableByVectorSubsetChangeable.OnAppStarted;
begin
  FDrawTask.Start;
  if Assigned(FSource) and Assigned(FSourceListener) then begin
    FSource.ChangeNotifier.Add(FSourceListener);
  end;
  FDrawTask.StartExecute;
end;

procedure TVectorTileMatrixChangeableByVectorSubsetChangeable.OnSourceChange;
begin
  FDrawTask.StopExecute;
  FDrawTask.StartExecute;
end;

procedure TVectorTileMatrixChangeableByVectorSubsetChangeable.OnPosChange;
begin
  FDrawTask.StopExecute;
  FDrawTask.StartExecute;
end;

procedure TVectorTileMatrixChangeableByVectorSubsetChangeable.OnPrepareTileMatrix(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation
);
var
  VSource: IVectorItemSubset;
  VTileRect: ITileRect;
  VTileIterator: ITileIterator;
  VTile: TPoint;
  VProjection: IProjection;
  VCounterContext: TInternalPerformanceCounterContext;
  VVectorTile: IVectorItemSubset;
  VSourceHash: THashValue;
  VTileRectChanged: Boolean;
  VProvider: IVectorTileUniProvider;
begin
  VSource := FSource.GetStatic;
  if not Assigned(VSource) then begin
    if FVisible then begin
      FTileRect.ChangeNotifier.Remove(FPosChangeListener);
      FPreparedHashMatrix.SetRectWithReset(nil, 0);
      FPreparedVectorMatrix.SetRectWithReset(nil);
      FVisible := False;
      FTileRectPrev := nil;
      DoUpdateResultAndNotify;
    end;
  end else begin
    VTileRect := FTileRect.GetStatic;
    if Assigned(VTileRect) then begin
      VTileRectChanged := not VTileRect.IsEqual(FTileRectPrev);
      if not FVisible then begin
        FTileRect.ChangeNotifier.Add(FPosChangeListener);
        FVisible := True;
      end;
      if VTileRectChanged then begin
        FTileRectPrev := VTileRect;
        VCounterContext := FMatrixChangeRectCounter.StartOperation;
        try
          FPreparedHashMatrix.SetRect(VTileRect, 0);
          FPreparedVectorMatrix.SetRect(VTileRect);
        finally
          FMatrixChangeRectCounter.FinishOperation(VCounterContext);
        end;
        DoUpdateResultAndNotify;
      end;
      if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
        Exit;
      end;
      VSourceHash := VSource.Hash;
      VProvider :=
        TVectorTileProviderByFixedSubset.Create(
          FVectorSubsetBuilderFactory,
          FItemSelectOversize,
          VSource
        );
      VProjection := VTileRect.ProjectionInfo;
      VTileIterator := TTileIteratorSpiralByRect.Create(VTileRect);
      while VTileIterator.Next(VTile) do begin
        if FPreparedHashMatrix.Tiles[VTile] <> VSourceHash then begin
          VCounterContext := FOneTilePrepareCounter.StartOperation;
          try
            VVectorTile := VProvider.GetTile(AOperationID, ACancelNotifier, VProjection, VTile);
          finally
            FOneTilePrepareCounter.FinishOperation(VCounterContext);
          end;
          if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
            Exit;
          end;
          FPreparedVectorMatrix.Tiles[VTile] := VVectorTile;
          FPreparedHashMatrix.Tiles[VTile] := VSourceHash;
          DoUpdateResultAndNotify;
          if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
            Exit;
          end;
        end;
      end;
    end;
  end;
end;

procedure TVectorTileMatrixChangeableByVectorSubsetChangeable.DoUpdateResultAndNotify;
var
  VCounterContext: TInternalPerformanceCounterContext;
  VResult: IVectorTileMatrix;
  VChanged: Boolean;
begin
  VCounterContext := FUpdateResultCounter.StartOperation;
  try
    VResult := FPreparedVectorMatrix.MakeStatic;
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
