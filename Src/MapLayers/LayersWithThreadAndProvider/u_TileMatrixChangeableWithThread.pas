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

unit u_TileMatrixChangeableWithThread;

interface

uses
  SysUtils,
  i_BackgroundTask,
  i_ThreadConfig,
  i_InternalPerformanceCounter,
  i_NotifierOperation,
  i_Listener,
  i_ListenerNotifierLinksList,
  i_SimpleFlag,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_BitmapLayerProvider,
  i_BitmapLayerProviderChangeable,
  i_ObjectWithListener,
  i_TileMatrix,
  i_TileMatrixChangeable,
  u_ChangeableBase;

type
  TTileMatrixChangeableWithThread = class(TChangeableBase, ITileMatrixChangeable)
  private
    FAppStartedNotifier: INotifierOneOperation;
    FAppClosingNotifier: INotifierOneOperation;
    FTileMatrixFactory: ITileMatrixFactory;
    FPosition: ILocalCoordConverterChangeable;
    FLayerProvider: IBitmapLayerProviderChangeable;
    FSourcUpdateNotyfier: IObjectWithListener;
    FDebugName: string;

    FLinksList: IListenerNotifierLinksList;
    FDrawTask: IBackgroundTask;
    FDelicateRedrawFlag: ISimpleFlag;
    FRectUpdateListener: IListener;
    FAppStartedListener: IListener;
    FAppClosingListener: IListener;

    FTileMatrix: ITileMatrix;
    FTileMatrixCS: IReadWriteSync;

    FOneTilePrepareCounter: IInternalPerformanceCounter;
    FTileMatrixUpdateCounter: IInternalPerformanceCounter;
    procedure OnPosChange;
    procedure OnLayerProviderChange;
    procedure OnRectUpdate(const AMsg: IInterface);
    procedure OnAppStarted;
    procedure OnAppClosing;

    procedure OnPrepareTileMatrix(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation
    );

    procedure PrepareTileMatrixItems(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ATileMatrix: ITileMatrix;
      const ALayerProvider: IBitmapLayerProvider
    );
    procedure SetMatrixNotReady(const ATileMatrix: ITileMatrix);
  private
    function GetStatic: ITileMatrix;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      const APosition: ILocalCoordConverterChangeable;
      const ATileMatrixFactory: ITileMatrixFactory;
      const ALayerProvider: IBitmapLayerProviderChangeable;
      const ASourcUpdateNotyfier: IObjectWithListener;
      const AThreadConfig: IThreadConfig;
      const ADebugName: string
    );
    destructor Destroy; override;
  end;

implementation

uses
  Types,
  t_GeoTypes,
  i_TileIterator,
  i_Bitmap32Static,
  i_CoordConverter,
  i_LonLatRect,
  u_SimpleFlagWithInterlock,
  u_ListenerNotifierLinksList,
  u_ListenerByEvent,
  u_TileIteratorSpiralByRect,
  u_BackgroundTask,
  u_GeoFunc,
  u_Synchronizer;

{ TTileMatrixChangeableWithThread }

constructor TTileMatrixChangeableWithThread.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier, AAppClosingNotifier: INotifierOneOperation;
  const APosition: ILocalCoordConverterChangeable;
  const ATileMatrixFactory: ITileMatrixFactory;
  const ALayerProvider: IBitmapLayerProviderChangeable;
  const ASourcUpdateNotyfier: IObjectWithListener;
  const AThreadConfig: IThreadConfig;
  const ADebugName: string
);
var
  VDebugName: string;
begin
  Assert(Assigned(AAppStartedNotifier));
  Assert(Assigned(AAppClosingNotifier));
  Assert(Assigned(APosition));
  Assert(Assigned(ATileMatrixFactory));
  Assert(Assigned(ALayerProvider));
  inherited Create(GSync.SyncVariable.Make(Self.ClassName + 'Notifiers'));

  FAppStartedNotifier := AAppStartedNotifier;
  FAppClosingNotifier := AAppClosingNotifier;
  FPosition := APosition;
  FTileMatrixFactory := ATileMatrixFactory;
  FLayerProvider := ALayerProvider;
  FSourcUpdateNotyfier := ASourcUpdateNotyfier;

  FLinksList := TListenerNotifierLinksList.Create;
  FTileMatrixCS := GSync.SyncVariable.Make(Self.ClassName);
  FOneTilePrepareCounter := APerfList.CreateAndAddNewCounter('OneTilePrepare');
  FTileMatrixUpdateCounter := APerfList.CreateAndAddNewCounter('TileMatrixUpdate');
  if Assigned(FSourcUpdateNotyfier) then begin
    FRectUpdateListener := TNotifyEventListener.Create(Self.OnRectUpdate);
  end;

  FDelicateRedrawFlag := TSimpleFlagWithInterlock.Create;

  FDebugName := ADebugName;
  VDebugName := ADebugName;
  if VDebugName = '' then begin
    VDebugName := Self.ClassName;
  end;

  FDrawTask :=
    TBackgroundTask.Create(
      AAppClosingNotifier,
      OnPrepareTileMatrix,
      AThreadConfig,
      VDebugName
    );

  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnPosChange),
    FPosition.ChangeNotifier
  );
  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnLayerProviderChange),
    FLayerProvider.ChangeNotifier
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

destructor TTileMatrixChangeableWithThread.Destroy;
begin
  FLinksList := nil;
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

function TTileMatrixChangeableWithThread.GetStatic: ITileMatrix;
begin
  FTileMatrixCS.BeginRead;
  try
    Result := FTileMatrix;
  finally
    FTileMatrixCS.EndRead;
  end;
end;

procedure TTileMatrixChangeableWithThread.OnAppClosing;
begin
  FLinksList.DeactivateLinks;
  FDrawTask.Terminate;
end;

procedure TTileMatrixChangeableWithThread.OnAppStarted;
begin
  FLinksList.ActivateLinks;
  FDrawTask.Start;
  FDrawTask.StartExecute;
end;

procedure TTileMatrixChangeableWithThread.OnLayerProviderChange;
var
  VTileMatrix: ITileMatrix;
begin
  FDrawTask.StopExecute;
  VTileMatrix := GetStatic;
  SetMatrixNotReady(VTileMatrix);
  FDrawTask.StartExecute;
end;

procedure TTileMatrixChangeableWithThread.OnPosChange;
begin
  FDrawTask.StopExecute;
  FDrawTask.StartExecute;
end;

procedure TTileMatrixChangeableWithThread.OnPrepareTileMatrix(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation
);
var
  VProvider: IBitmapLayerProvider;
  VLayerConverter: ILocalCoordConverter;
  VTileMatrix: ITileMatrix;
  VUpdated: Boolean;
  VNeedRedraw: Boolean;
begin
  VUpdated := False;
  VProvider := FLayerProvider.GetStatic;
  if not Assigned(VProvider) then begin
    FTileMatrixCS.BeginWrite;
    try
      VUpdated := Assigned(FTileMatrix);
      FTileMatrix := nil;
    finally
      FTileMatrixCS.EndWrite;
    end;
  end;

  VLayerConverter := nil;
  if Assigned(VProvider) then begin
    VLayerConverter := FPosition.GetStatic;
    if not Assigned(VLayerConverter) then begin
      FTileMatrixCS.BeginWrite;
      try
        VUpdated := Assigned(FTileMatrix);
        FTileMatrix := nil;
      finally
        FTileMatrixCS.EndWrite;
      end;
    end;
  end;

  VTileMatrix := nil;
  if Assigned(VLayerConverter) then begin
    VTileMatrix := GetStatic;
    if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
      Exit;
    end;
    if not Assigned(VTileMatrix) or not VTileMatrix.LocalConverter.GetIsSameConverter(VLayerConverter) then begin
      VTileMatrix := FTileMatrixFactory.BuildNewMatrix(VTileMatrix, VLayerConverter);
      if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
        Exit;
      end;
    end;
    FTileMatrixCS.BeginWrite;
    try
      VUpdated := FTileMatrix <> VTileMatrix;
      FTileMatrix := VTileMatrix;
    finally
      FTileMatrixCS.EndWrite;
    end;
  end;
  if Assigned(FSourcUpdateNotyfier) then begin
    if Assigned(VTileMatrix) then begin
      FSourcUpdateNotyfier.SetListener(FRectUpdateListener, VTileMatrix.LocalConverter);
    end else begin
      FSourcUpdateNotyfier.RemoveListener;
    end;
  end;
  if VUpdated then begin
    DoChangeNotify;
  end;
  FDelicateRedrawFlag.CheckFlagAndReset;
  if Assigned(VTileMatrix) then begin
    VNeedRedraw := True;
    while VNeedRedraw do begin
      if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
        Exit;
      end;
      PrepareTileMatrixItems(
        AOperationID,
        ACancelNotifier,
        VTileMatrix,
        VProvider
      );
      VNeedRedraw := FDelicateRedrawFlag.CheckFlagAndReset;
    end;
  end;
end;

procedure TTileMatrixChangeableWithThread.OnRectUpdate(const AMsg: IInterface);
var
  VLonLatRect: ILonLatRect;
  VTileMatrix: ITileMatrix;
  VTileRect: TRect;
  VMapLonLatRect: TDoubleRect;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VTileRectToUpdate: TRect;
  i, j: Integer;
  VTile: TPoint;
  VElement: ITileMatrixElement;
begin
  if Supports(AMsg, ILonLatRect, VLonLatRect) then begin
    VTileMatrix := GetStatic;
    if VTileMatrix <> nil then begin
      VMapLonLatRect := VLonLatRect.Rect;
      VConverter := VTileMatrix.LocalConverter.GeoConverter;
      VZoom := VTileMatrix.LocalConverter.Zoom;
      VConverter.ValidateLonLatRect(VMapLonLatRect);
      VTileRect := RectFromDoubleRect(VConverter.LonLatRect2TileRectFloat(VMapLonLatRect, VZoom), rrOutside);
      if Types.IntersectRect(VTileRectToUpdate, VTileRect, VTileMatrix.TileRect) then begin
        for i := VTileRectToUpdate.Top to VTileRectToUpdate.Bottom - 1 do begin
          VTile.Y := i;
          for j := VTileRectToUpdate.Left to VTileRectToUpdate.Right - 1 do begin
            VTile.X := j;
            VElement := VTileMatrix.GetElementByTile(VTile);
            if VElement <> nil then begin
              VElement.IncExpectedID;
              FDelicateRedrawFlag.SetFlag;
              FDrawTask.StartExecute;
            end;
          end;
        end;
      end;
    end;
  end else begin
    VTileMatrix := GetStatic;
    if VTileMatrix <> nil then begin
      SetMatrixNotReady(VTileMatrix);
    end;
    FDelicateRedrawFlag.SetFlag;
    FDrawTask.StartExecute;
  end;
end;

procedure TTileMatrixChangeableWithThread.PrepareTileMatrixItems(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ATileMatrix: ITileMatrix;
  const ALayerProvider: IBitmapLayerProvider
);
var
  VTileIterator: ITileIterator;
  VTile: TPoint;
  VElement: ITileMatrixElement;
  VBitmap: IBitmap32Static;
  VCounterContext: TInternalPerformanceCounterContext;
  VId: Integer;
begin
  Assert(Assigned(ATileMatrix));
  Assert(Assigned(ALayerProvider));
  VTileIterator := TTileIteratorSpiralByRect.Create(ATileMatrix.TileRect);
  while VTileIterator.Next(VTile) do begin
    VElement := ATileMatrix.GetElementByTile(VTile);
    Assert(Assigned(VElement));
    if VElement <> nil then begin
      VId := VElement.ExpectedID;
      if VElement.ReadyID <> VId then begin
        VCounterContext := FOneTilePrepareCounter.StartOperation;
        try
          VBitmap := ALayerProvider.GetBitmapRect(AOperationID, ACancelNotifier, VElement.LocalConverter);
          VElement.UpdateBitmap(VId, VBitmap);
        finally
          FOneTilePrepareCounter.FinishOperation(VCounterContext);
        end;
        DoChangeNotify;
      end;
    end;
    if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
      Exit;
    end;
  end;
end;

procedure TTileMatrixChangeableWithThread.SetMatrixNotReady(
  const ATileMatrix: ITileMatrix
);
var
  i, j: Integer;
  VTileRect: TRect;
  VElement: ITileMatrixElement;
begin
  if ATileMatrix <> nil then begin
    VTileRect := ATileMatrix.TileRect;
    for i := 0 to VTileRect.Right - VTileRect.Left - 1 do begin
      for j := 0 to VTileRect.Bottom - VTileRect.Top - 1 do begin
        VElement := ATileMatrix.Items[i, j];
        if VElement <> nil then begin
          VElement.IncExpectedID;
        end;
      end;
    end;
  end;
end;

end.
