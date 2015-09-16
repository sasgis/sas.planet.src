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

unit u_VectorTileMatrixChangeableForVectorLayers;

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
  i_VectorTileProvider,
  i_VectorTileProviderChangeable,
  i_ObjectWithListener,
  i_HashTileMatrix,
  i_HashTileMatrixBuilder,
  i_VectorItemSubsetBuilder,
  i_VectorTileMatrixChangeable,
  u_ChangeableBase;

type
  TVectorTileMatrixChangeableForVectorLayers = class(TChangeableWithSimpleLockBase, IVectorTileMatrixChangeable)
  private
    FAppStartedNotifier: INotifierOneOperation;
    FAppClosingNotifier: INotifierOneOperation;
    FTileRect: ITileRectChangeable;
    FVectorSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FTileProvider: IVectorTileUniProviderChangeable;
    FSourcUpdateNotyfier: IObjectWithListener;
    FDebugName: string;

    FDrawTask: IBackgroundTask;
    FOneTilePrepareCounter: IInternalPerformanceCounter;
    FMatrixChangeRectCounter: IInternalPerformanceCounter;
    FUpdateResultCounter: IInternalPerformanceCounter;

    FSourceCounter: ICounter;
    FTileRectPrev: ITileRect;
    FSourceHashMatrix: IHashTileMatrixBuilder;
    FSourceHashMatrixCS: IReadWriteSync;

    FPreparedHashMatrix: IHashTileMatrixBuilder;

    FPosChangeListener: IListener;
    FLayerProviderListener: IListener;
    FRectUpdateListener: IListener;
    FAppStartedListener: IListener;
    FAppClosingListener: IListener;

    FVisible: Boolean;
    FPreparedVectorMatrix: IVectorTileMatrixBuilder;
    FResult: IVectorTileMatrix;

    procedure OnAppStarted;
    procedure OnAppClosing;
    procedure OnLayerProviderChange;
    procedure OnPosChange;
    procedure OnRectUpdate(const AMsg: IInterface);

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
      const ATileProvider: IVectorTileUniProviderChangeable;
      const ASourcUpdateNotyfier: IObjectWithListener;
      const AThreadConfig: IThreadConfig;
      const AOversize: TRect;
      const ADebugName: string
    );
    destructor Destroy; override;
  end;

implementation

uses
  Math,
  t_Hash,
  t_GeoTypes,
  i_TileIterator,
  i_VectorItemSubset,
  i_ProjectionInfo,
  i_LonLatRect,
  u_SimpleFlagWithInterlock,
  u_ListenerByEvent,
  u_TileIteratorByRect,
  u_TileIteratorSpiralByRect,
  u_HashTileMatrixBuilder,
  u_VectorTileMatrixBuilder,
  u_BackgroundTask,
  u_GeoFunc,
  u_Synchronizer;

{ TBitmapTileMatrixChangeableWithThread }

constructor TVectorTileMatrixChangeableForVectorLayers.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier, AAppClosingNotifier: INotifierOneOperation;
  const ATileRect: ITileRectChangeable;
  const AHashFunction: IHashFunction;
  const AVectorSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const ATileProvider: IVectorTileUniProviderChangeable;
  const ASourcUpdateNotyfier: IObjectWithListener;
  const AThreadConfig: IThreadConfig;
  const AOversize: TRect;
  const ADebugName: string
);
var
  VDebugName: string;
begin
  Assert(Assigned(AAppStartedNotifier));
  Assert(Assigned(AAppClosingNotifier));
  Assert(Assigned(ATileRect));
  Assert(Assigned(AVectorSubsetBuilderFactory));
  Assert(Assigned(ATileProvider));
  VDebugName := ADebugName;
  if VDebugName = '' then begin
    VDebugName := Self.ClassName;
  end;
  inherited Create;

  FAppStartedNotifier := AAppStartedNotifier;
  FAppClosingNotifier := AAppClosingNotifier;
  FTileRect := ATileRect;
  FVectorSubsetBuilderFactory := AVectorSubsetBuilderFactory;
  FTileProvider := ATileProvider;
  FSourcUpdateNotyfier := ASourcUpdateNotyfier;
  FDebugName := VDebugName;

  FSourceHashMatrixCS := GSync.SyncVariable.Make(FDebugName + '\SourceUpdates');

  FPosChangeListener := TNotifyNoMmgEventListener.Create(Self.OnPosChange);
  FLayerProviderListener := TNotifyNoMmgEventListener.Create(Self.OnLayerProviderChange);

  FOneTilePrepareCounter := APerfList.CreateAndAddNewCounter('OneTilePrepare');
  FUpdateResultCounter := APerfList.CreateAndAddNewCounter('UpdateResult');
  FMatrixChangeRectCounter := APerfList.CreateAndAddNewCounter('MatrixChangeRect');
  if Assigned(FSourcUpdateNotyfier) then begin
    FRectUpdateListener := TNotifyEventListener.Create(Self.OnRectUpdate);
  end;

  FSourceCounter := TCounterInterlock.Create;
  FSourceHashMatrix := THashTileMatrixBuilder.Create(AHashFunction);
  FPreparedHashMatrix := THashTileMatrixBuilder.Create(AHashFunction);
  FPreparedVectorMatrix :=
    TVectorTileMatrixBuilder.Create(
      AVectorSubsetBuilderFactory,
      AOversize,
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

destructor TVectorTileMatrixChangeableForVectorLayers.Destroy;
begin
  if Assigned(FTileRect) and Assigned(FPosChangeListener) then begin
    FTileRect.ChangeNotifier.Remove(FPosChangeListener);
    FPosChangeListener := nil;
  end;
  if Assigned(FTileProvider) and Assigned(FLayerProviderListener) then begin
    FTileProvider.ChangeNotifier.Remove(FLayerProviderListener);
    FLayerProviderListener := nil;
  end;
  if Assigned(FSourcUpdateNotyfier) then begin
    FSourcUpdateNotyfier.RemoveListener;
    FSourcUpdateNotyfier := nil;
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

function TVectorTileMatrixChangeableForVectorLayers.GetStatic: IVectorTileMatrix;
begin
  CS.BeginRead;
  try
    Result := FResult;
  finally
    CS.EndRead;
  end;
end;

procedure TVectorTileMatrixChangeableForVectorLayers.OnAppClosing;
begin
  FDrawTask.Terminate;
end;

procedure TVectorTileMatrixChangeableForVectorLayers.OnAppStarted;
begin
  FDrawTask.Start;
  if Assigned(FTileProvider) and Assigned(FLayerProviderListener) then begin
    FTileProvider.ChangeNotifier.Add(FLayerProviderListener);
  end;
  FDrawTask.StartExecute;
end;

procedure TVectorTileMatrixChangeableForVectorLayers.OnLayerProviderChange;
begin
  FDrawTask.StopExecute;
  FSourceHashMatrixCS.BeginWrite;
  try
    FSourceHashMatrix.Reset(FSourceCounter.Inc);
  finally
    FSourceHashMatrixCS.EndWrite;
  end;
  FDrawTask.StartExecute;
end;

procedure TVectorTileMatrixChangeableForVectorLayers.OnPosChange;
begin
  FDrawTask.StopExecute;
  FDrawTask.StartExecute;
end;

procedure TVectorTileMatrixChangeableForVectorLayers.OnPrepareTileMatrix(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation
);
var
  VProvider: IVectorTileUniProvider;
  VTileRect: ITileRect;
  VTileIterator: ITileIterator;
  VSourceHashMatrix: IHashTileMatrix;
  VTile: TPoint;
  VProjection: IProjection;
  VCounterContext: TInternalPerformanceCounterContext;
  VVectorTile: IVectorItemSubset;
  VSourceHash: THashValue;
  VTileRectChanged: Boolean;
begin
  VProvider := FTileProvider.GetStatic;
  if not Assigned(VProvider) then begin
    if FVisible then begin
      FTileRect.ChangeNotifier.Remove(FPosChangeListener);
      if Assigned(FSourcUpdateNotyfier) then begin
        FSourcUpdateNotyfier.RemoveListener;
      end;
      FSourceHashMatrixCS.BeginWrite;
      try
        FSourceHashMatrix.SetRectWithReset(nil, 0);
      finally
        FSourceHashMatrixCS.EndWrite
      end;
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

      FSourceHashMatrixCS.BeginWrite;
      try
        if VTileRectChanged then begin
          FSourceHashMatrix.SetRect(VTileRect, FSourceCounter.Inc);
        end;
        VSourceHashMatrix := FSourceHashMatrix.MakeStatic;
      finally
        FSourceHashMatrixCS.EndWrite
      end;
      if not FVisible then begin
        FTileRect.ChangeNotifier.Add(FPosChangeListener);
        FVisible := True;
      end;
      if VTileRectChanged then begin
        FTileRectPrev := VTileRect;
        if Assigned(FSourcUpdateNotyfier) then begin
          FSourcUpdateNotyfier.SetListener(FRectUpdateListener, VTileRect);
        end;
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
      VProjection := VTileRect.ProjectionInfo;
      VTileIterator := TTileIteratorSpiralByRect.Create(VTileRect);
      while VTileIterator.Next(VTile) do begin
        VSourceHash := FSourceHashMatrix.Tiles[VTile];
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
    end else begin
      if Assigned(FSourcUpdateNotyfier) then begin
        FSourcUpdateNotyfier.RemoveListener;
      end;
    end;
  end;
end;

procedure TVectorTileMatrixChangeableForVectorLayers.OnRectUpdate(
  const AMsg: IInterface
);
var
  VTileRectUpdated: TRect;
  VLonLatRectUpdated: ILonLatRect;
  VLonLatRectAtMap: TDoubleRect;
  VProjection: IProjection;
  VTileRectToUpdate: TRect;
  VTileRect: ITileRect;
  VCounter: Integer;
  VIterator: TTileIteratorByRectRecord;
  VTile: TPoint;
  VChanged: Boolean;
begin
  VChanged := False;
  FSourceHashMatrixCS.BeginWrite;
  try
    VTileRect := FSourceHashMatrix.TileRect;
    if Assigned(VTileRect) then begin
      if Supports(AMsg, ILonLatRect, VLonLatRectUpdated) then begin
        VLonLatRectAtMap := VLonLatRectUpdated.Rect;
        VProjection := VTileRect.ProjectionInfo;
        VProjection.ProjectionType.ValidateLonLatRect(VLonLatRectAtMap);
        VTileRectUpdated := RectFromDoubleRect(VProjection.LonLatRect2TileRectFloat(VLonLatRectAtMap), rrOutside);

        if Types.IntersectRect(VTileRectToUpdate, VTileRectUpdated, VTileRect.Rect) then begin
          VCounter := FSourceCounter.Inc;
          VIterator.Init(VTileRectToUpdate);
          while VIterator.Next(VTile) do begin
            FSourceHashMatrix.Tiles[VTile] := VCounter;
          end;
          VChanged := True;
        end;
      end else begin
        VCounter := FSourceCounter.Inc;
        FSourceHashMatrix.Reset(VCounter);
        VChanged := True;
      end;
    end;
  finally
    FSourceHashMatrixCS.EndWrite;
  end;
  if VChanged then begin
    FDrawTask.StartExecute;
  end;
end;

procedure TVectorTileMatrixChangeableForVectorLayers.DoUpdateResultAndNotify;
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
