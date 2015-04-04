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

unit u_BitmapTileMatrixChangeableComposite;

interface

uses
  Types,
  SysUtils,
  t_Hash,
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
  i_Bitmap32Static,
  i_HashTileMatrixBuilder,
  i_InterfaceListStatic,
  i_InterfaceListSimple,
  i_TileRectChangeable,
  i_BitmapTileMatrix,
  i_BitmapTileMatrixBuilder,
  i_BitmapTileMatrixChangeable,
  u_ChangeableBase;

type
  TBitmapTileMatrixChangeableComposite = class(TChangeableWithSimpleLockBase, IBitmapTileMatrixChangeable)
  private
    FAppStartedNotifier: INotifierOneOperation;
    FAppClosingNotifier: INotifierOneOperation;
    FHashFunction: IHashFunction;
    FBitmapFactory: IBitmap32StaticFactory;
    FTileRect: ITileRectChangeable;
    FSourceTileMatrixList: IInterfaceListStatic;
    FDebugName: string;

    FDrawTask: IBackgroundTask;
    FOneTilePrepareCounter: IInternalPerformanceCounter;
    FSourceDataGetCounter: IInternalPerformanceCounter;
    FHashCheckCounter: IInternalPerformanceCounter;
    FMatrixChangeRectCounter: IInternalPerformanceCounter;
    FUpdateResultCounter: IInternalPerformanceCounter;

    FPreparedHashMatrix: IHashTileMatrixBuilder;

    FSourceTileMatrixListener: IListener;
    FTileRectListener: IListener;
    FAppStartedListener: IListener;
    FAppClosingListener: IListener;
    FIsNeedFullUpdate: ISimpleFlag;

    FPreparedBitmapMatrix: IBitmapTileMatrixBuilder;

    FResult: IBitmapTileMatrix;

    procedure OnAppStarted;
    procedure OnAppClosing;
    procedure OnSourceTileMatrixChange;
    procedure OnTileRectChange;

    procedure DoUpdateResultAndNotify;
    function PrepareBitmap(
      const ATile: TPoint;
      const ASourceMatrixList: IInterfaceListSimple;
      const AExistedSourceHash: THashValue;
      var AResult: IBitmap32Static;
      var ASourceHash: THashValue
    ): Boolean;
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
      const ATileRect: ITileRectChangeable;
      const ASourceTileMatrixList: IInterfaceListStatic;
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
  GR32,
  i_TileIterator,
  i_CoordConverter,
  i_ProjectionInfo,
  u_InterfaceListSimple,
  u_SimpleFlagWithInterlock,
  u_ListenerByEvent,
  u_TileIteratorSpiralByRect,
  u_HashTileMatrixBuilder,
  u_BitmapTileMatrixBuilder,
  u_Bitmap32ByStaticBitmap,
  u_BitmapFunc,
  u_BackgroundTask;

{ TBitmapTileMatrixChangeableByVectorMatrix }

constructor TBitmapTileMatrixChangeableComposite.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  const ATileRect: ITileRectChangeable;
  const ASourceTileMatrixList: IInterfaceListStatic;
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
  Assert(Assigned(ATileRect));
  Assert(Assigned(AHashFunction));
  Assert(Assigned(ASourceTileMatrixList));
  Assert(ASourceTileMatrixList.Count > 1);
  VDebugName := ADebugName;
  if VDebugName = '' then begin
    VDebugName := Self.ClassName;
  end;
  inherited Create;

  FAppStartedNotifier := AAppStartedNotifier;
  FAppClosingNotifier := AAppClosingNotifier;
  FHashFunction := AHashFunction;
  FBitmapFactory := ABitmapFactory;
  FTileRect := ATileRect;
  FSourceTileMatrixList := ASourceTileMatrixList;
  FDebugName := VDebugName;

  FIsNeedFullUpdate := TSimpleFlagWithInterlock.Create;
  FTileRectListener := TNotifyNoMmgEventListener.Create(Self.OnTileRectChange);
  FSourceTileMatrixListener := TNotifyNoMmgEventListener.Create(Self.OnSourceTileMatrixChange);

  FOneTilePrepareCounter := APerfList.CreateAndAddNewCounter('OneTilePrepare');
  FUpdateResultCounter := APerfList.CreateAndAddNewCounter('UpdateResult');
  FMatrixChangeRectCounter := APerfList.CreateAndAddNewCounter('MatrixChangeRect');
  FSourceDataGetCounter := APerfList.CreateAndAddNewCounter('SourceDataGet');
  FHashCheckCounter := APerfList.CreateAndAddNewCounter('HashCheck');

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

destructor TBitmapTileMatrixChangeableComposite.Destroy;
var
  i: Integer;
begin
  if Assigned(FTileRect) and Assigned(FTileRectListener) then begin
    FTileRect.ChangeNotifier.Remove(FTileRectListener);
    FTileRectListener := nil;
  end;
  if Assigned(FSourceTileMatrixList) and Assigned(FSourceTileMatrixListener) then begin
    for i := 0 to FSourceTileMatrixList.Count - 1 do begin
      IBitmapTileMatrixChangeable(FSourceTileMatrixList.Items[i]).ChangeNotifier.Remove(FSourceTileMatrixListener);
    end;
    FSourceTileMatrixListener := nil;
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

function TBitmapTileMatrixChangeableComposite.GetStatic: IBitmapTileMatrix;
begin
  CS.BeginRead;
  try
    Result := FResult;
  finally
    CS.EndRead;
  end;
end;

procedure TBitmapTileMatrixChangeableComposite.OnAppClosing;
begin
  FDrawTask.Terminate;
end;

procedure TBitmapTileMatrixChangeableComposite.OnAppStarted;
var
  i: Integer;
begin
  if Assigned(FTileRect) and Assigned(FTileRectListener) then begin
    FTileRect.ChangeNotifier.Add(FTileRectListener);
  end;
  for i := 0 to FSourceTileMatrixList.Count - 1 do begin
    IBitmapTileMatrixChangeable(FSourceTileMatrixList.Items[i]).ChangeNotifier.Add(FSourceTileMatrixListener);
  end;
  FDrawTask.Start;
  FDrawTask.StartExecute;
end;

procedure TBitmapTileMatrixChangeableComposite.OnSourceTileMatrixChange;
begin
  FDrawTask.StopExecute;
  FDrawTask.StartExecute;
end;

procedure TBitmapTileMatrixChangeableComposite.OnTileRectChange;
begin
  FDrawTask.StopExecute;
  FDrawTask.StartExecute;
end;

function TBitmapTileMatrixChangeableComposite.PrepareBitmap(
  const ATile: TPoint;
  const ASourceMatrixList: IInterfaceListSimple;
  const AExistedSourceHash: THashValue;
  var AResult: IBitmap32Static;
  var ASourceHash: THashValue
): Boolean;
var
  VCounterContext: TInternalPerformanceCounterContext;
  i:  Integer;
  VSourceItem: IBitmap32Static;
  VBitmapGR32: TBitmap32ByStaticBitmap;
  VTileCount: Integer;
begin
  Result := True;
  AResult := nil;
  ASourceHash := 0;
  VCounterContext := FHashCheckCounter.StartOperation;
  try
    VTileCount := 0;
    for i := 0 to ASourceMatrixList.Count - 1 do begin
      VSourceItem := IBitmapTileMatrix(ASourceMatrixList.Items[i]).GetElementByTile(ATile);
      if Assigned(VSourceItem) then begin
        if VTileCount = 0 then begin
          AResult := VSourceItem;
          ASourceHash := VSourceItem.Hash;
        end else begin
          FHashFunction.UpdateHashByHash(ASourceHash, VSourceItem.Hash);
        end;
        Inc(VTileCount);
      end;
    end;
  finally
    FHashCheckCounter.FinishOperation(VCounterContext);
  end;
  if AExistedSourceHash = ASourceHash then begin
    Result := False;
    Exit;
  end;
  if VTileCount = 0 then begin
    AResult := nil;
    Exit
  end;
  if VTileCount = 1 then begin
    Exit
  end;

  VCounterContext := FOneTilePrepareCounter.StartOperation;
  try
    VBitmapGR32 := TBitmap32ByStaticBitmap.Create(FBitmapFactory);
    try
      VTileCount := 0;
      for i := 0 to ASourceMatrixList.Count - 1 do begin
        VSourceItem := IBitmapTileMatrix(ASourceMatrixList.Items[i]).GetElementByTile(ATile);
        if Assigned(VSourceItem) then begin
          if VTileCount = 0 then begin
            AssignStaticToBitmap32(VBitmapGR32, VSourceItem);
          end else begin
            BlockTransferFull(
              VBitmapGR32,
              0, 0,
              VSourceItem,
              dmBlend
            );
          end;
          Inc(VTileCount);
        end;
      end;
      AResult := VBitmapGR32.MakeAndClear;
    finally
      VBitmapGR32.Free;
    end;
  finally
    FOneTilePrepareCounter.FinishOperation(VCounterContext);
  end;
end;

procedure TBitmapTileMatrixChangeableComposite.OnPrepareTileMatrix(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation
);
var
  VTileRect: ITileRect;
  VTileIterator: ITileIterator;
  VTile: TPoint;
  VProjection: IProjectionInfo;
  VConverter: ICoordConverter;
  VCounterContext: TInternalPerformanceCounterContext;
  VBitmap: IBitmap32Static;
  VSourceHash: THashValue;
  VTileRectChanged: Boolean;
  VSourceMatrix: IBitmapTileMatrix;
  VSourceMatrixList: IInterfaceListSimple;
  i: Integer;
  VAllSourceReady: Boolean;
begin
  VTileRect := FTileRect.GetStatic;
  if Assigned(VTileRect) then begin
    VProjection := VTileRect.ProjectionInfo;
    VTileRectChanged := not VTileRect.IsEqual(FPreparedBitmapMatrix.TileRect);
    if VTileRectChanged then begin
      VCounterContext := FMatrixChangeRectCounter.StartOperation;
      try
        FPreparedHashMatrix.SetRect(VTileRect, 0);
        FPreparedBitmapMatrix.SetRect(VTileRect);
      finally
        FMatrixChangeRectCounter.FinishOperation(VCounterContext);
      end;
      DoUpdateResultAndNotify;
      if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
        Exit;
      end;
    end;
    VCounterContext := FSourceDataGetCounter.StartOperation;
    try
      VAllSourceReady := True;
      VSourceMatrixList := TInterfaceListSimple.Create;
      VSourceMatrixList.Capacity := FSourceTileMatrixList.Count;
      for i := 0 to FSourceTileMatrixList.Count - 1 do begin
        VSourceMatrix := IBitmapTileMatrixChangeable(FSourceTileMatrixList.Items[i]).GetStatic;
        if Assigned(VSourceMatrix) then begin
          if VProjection.GetIsSameProjectionInfo(VSourceMatrix.TileRect.ProjectionInfo) then begin
            VSourceMatrixList.Add(VSourceMatrix);
          end else begin
            VAllSourceReady := False;
          end;
        end;
      end;
    finally
      FSourceDataGetCounter.FinishOperation(VCounterContext);
    end;

    if VSourceMatrixList.Count > 0 then begin
      VConverter := VProjection.GeoConverter;
      VTileIterator := TTileIteratorSpiralByRect.Create(VTileRect);
      if not VAllSourceReady then begin
        while VTileIterator.Next(VTile) do begin
          if Assigned(FPreparedBitmapMatrix.Tiles[VTile]) then begin
            Continue;
          end;
          if PrepareBitmap(VTile, VSourceMatrixList, FPreparedHashMatrix.Tiles[VTile], VBitmap, VSourceHash) then begin
            FPreparedBitmapMatrix.Tiles[VTile] := VBitmap;
            FPreparedHashMatrix.Tiles[VTile] := VSourceHash;
            DoUpdateResultAndNotify;
            if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
              Exit;
            end;
          end;
        end;
        VTileIterator.Reset;
      end;
      if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
        Exit;
      end;
      while VTileIterator.Next(VTile) do begin
        if PrepareBitmap(VTile, VSourceMatrixList, FPreparedHashMatrix.Tiles[VTile], VBitmap, VSourceHash) then begin
          FPreparedBitmapMatrix.Tiles[VTile] := VBitmap;
          FPreparedHashMatrix.Tiles[VTile] := VSourceHash;
          DoUpdateResultAndNotify;
          if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
            Exit;
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

procedure TBitmapTileMatrixChangeableComposite.DoUpdateResultAndNotify;
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
