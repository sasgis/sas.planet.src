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

unit u_VectorItemSubsetChangeableForMarksLayer;

interface

uses
  SysUtils,
  Types,
  t_GeoTypes,
  i_VectorItemSubset,
  i_VectorItemSubsetChangeable,
  i_Listener,
  i_TileRect,
  i_TileRectChangeable,
  i_ThreadConfig,
  i_BackgroundTask,
  i_NotifierOperation,
  i_InternalPerformanceCounter,
  i_ListenerNotifierLinksList,
  i_UsedMarksConfig,
  i_MarkSystem,
  u_ChangeableBase;

type
  TVectorItemSubsetChangeableForMarksLayer = class(TChangeableWithSimpleLockBase, IVectorItemSubsetChangeable)
  private
    FMarkDB: IMarkSystem;
    FConfig: IUsedMarksConfig;
    FTileRect: ITileRectChangeable;
    FAppStartedNotifier: INotifierOneOperation;
    FAppClosingNotifier: INotifierOneOperation;
    FItemSelectOversize: TRect;

    FAppStartedListener: IListener;
    FAppClosingListener: IListener;

    FPrepareResultTask: IBackgroundTask;
    FLinksList: IListenerNotifierLinksList;
    FGetMarksCounter: IInternalPerformanceCounter;

    FResult: IVectorItemSubset;

    procedure OnAppStarted;
    procedure OnAppClosing;

    procedure OnPosChange;
    procedure OnMarksDbChange;

    procedure OnPrepareSubset(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation
    );
    function GetMarksSubset(
      const AConfig: IUsedMarksConfigStatic;
      const ATileRect: ITileRect
    ): IVectorItemSubset;
  private
    function GetStatic: IVectorItemSubset;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      const ATileRect: ITileRectChangeable;
      const AMarkSystem: IMarkSystem;
      const AConfig: IUsedMarksConfig;
      const AItemSelectOversize: TRect;
      const AThreadConfig: IThreadConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  i_Projection,
  i_MarkCategoryList,
  u_ListenerNotifierLinksList,
  u_BackgroundTask,
  u_GeoFunc,
  u_ListenerByEvent;

{ TVectorItemSubsetChangeableForMarksLayer }

constructor TVectorItemSubsetChangeableForMarksLayer.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  const ATileRect: ITileRectChangeable;
  const AMarkSystem: IMarkSystem;
  const AConfig: IUsedMarksConfig;
  const AItemSelectOversize: TRect;
  const AThreadConfig: IThreadConfig
);
begin
  Assert(Assigned(ATileRect));
  Assert(Assigned(AMarkSystem));
  Assert(AItemSelectOversize.Left >= 0);
  Assert(AItemSelectOversize.Left < 4096);
  Assert(AItemSelectOversize.Top >= 0);
  Assert(AItemSelectOversize.Top < 4096);
  Assert(AItemSelectOversize.Right >= 0);
  Assert(AItemSelectOversize.Right < 4096);
  Assert(AItemSelectOversize.Bottom >= 0);
  Assert(AItemSelectOversize.Bottom < 4096);
  inherited Create;
  FTileRect := ATileRect;
  FMarkDB := AMarkSystem;
  FConfig := AConfig;
  FItemSelectOversize := AItemSelectOversize;

  FGetMarksCounter := APerfList.CreateAndAddNewCounter('GetMarks');

  FAppStartedNotifier := AAppStartedNotifier;
  FAppClosingNotifier := AAppClosingNotifier;

  FLinksList := TListenerNotifierLinksList.Create;
  FAppStartedListener := TNotifyNoMmgEventListener.Create(Self.OnAppStarted);
  FAppClosingListener := TNotifyNoMmgEventListener.Create(Self.OnAppClosing);

  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnPosChange),
    FTileRect.ChangeNotifier
  );

  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnMarksDbChange),
    FConfig.ChangeNotifier
  );
  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnMarksDbChange),
    FMarkDB.MarkDb.ChangeNotifier
  );
  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnMarksDbChange),
    FMarkDB.CategoryDB.ChangeNotifier
  );
  FPrepareResultTask :=
    TBackgroundTask.Create(
      AAppClosingNotifier,
      OnPrepareSubset,
      AThreadConfig,
      Self.ClassName
    );

end;

destructor TVectorItemSubsetChangeableForMarksLayer.Destroy;
begin
  FLinksList := nil;

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

procedure TVectorItemSubsetChangeableForMarksLayer.AfterConstruction;
begin
  inherited;
  FAppStartedNotifier.Add(FAppStartedListener);
  if FAppStartedNotifier.IsExecuted then begin
    OnAppStarted;
  end;
  FAppClosingNotifier.Add(FAppClosingListener);
  if FAppClosingNotifier.IsExecuted then begin
    OnAppClosing;
  end;
end;

procedure TVectorItemSubsetChangeableForMarksLayer.BeforeDestruction;
begin
  inherited;
  OnAppClosing;
end;

function TVectorItemSubsetChangeableForMarksLayer.GetMarksSubset(
  const AConfig: IUsedMarksConfigStatic;
  const ATileRect: ITileRect
): IVectorItemSubset;
var
  VList: IMarkCategoryList;
  VItemSelectPixelRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
  VLonLatSize: TDoublePoint;
  VProjection: IProjection;
begin
  VList := nil;
  Result := nil;
  if AConfig.IsUseMarks then begin
    VProjection := ATileRect.Projection;
    if not AConfig.IgnoreCategoriesVisible then begin
      VList := FMarkDB.CategoryDB.GetVisibleCategories(VProjection.Zoom);
    end;
    if AConfig.IgnoreCategoriesVisible or (Assigned(VList) and (VList.Count > 0)) then begin
      VItemSelectPixelRect := VProjection.TileRectFloat2PixelRectFloat(DoubleRect(ATileRect.Rect));
      VItemSelectPixelRect.Left := VItemSelectPixelRect.Left - FItemSelectOversize.Left;
      VItemSelectPixelRect.Top := VItemSelectPixelRect.Top - FItemSelectOversize.Top;
      VItemSelectPixelRect.Right := VItemSelectPixelRect.Right + FItemSelectOversize.Right;
      VItemSelectPixelRect.Bottom := VItemSelectPixelRect.Bottom + FItemSelectOversize.Bottom;

      VProjection.ValidatePixelRectFloat(VItemSelectPixelRect);
      VLonLatRect := VProjection.PixelRectFloat2LonLatRect(VItemSelectPixelRect);

      VLonLatSize.X := (VLonLatRect.Right - VLonLatRect.Left) / (VItemSelectPixelRect.Right - VItemSelectPixelRect.Left);
      VLonLatSize.Y := (VLonLatRect.Top - VLonLatRect.Bottom) / (VItemSelectPixelRect.Bottom - VItemSelectPixelRect.Top);

      Result :=
        FMarkDB.MarkDb.GetMarkSubsetByCategoryListInRect(
          VLonLatRect,
          VList,
          AConfig.IgnoreMarksVisible,
          VLonLatSize
        );
    end;
  end;
end;

function TVectorItemSubsetChangeableForMarksLayer.GetStatic: IVectorItemSubset;
begin
  CS.BeginRead;
  try
    Result := FResult;
  finally
    CS.EndRead;
  end;
end;

procedure TVectorItemSubsetChangeableForMarksLayer.OnAppClosing;
begin
  FLinksList.DeactivateLinks;
  FPrepareResultTask.Terminate;
end;

procedure TVectorItemSubsetChangeableForMarksLayer.OnAppStarted;
begin
  FLinksList.ActivateLinks;
  FPrepareResultTask.Start;
  FPrepareResultTask.StartExecute;
end;

procedure TVectorItemSubsetChangeableForMarksLayer.OnMarksDbChange;
begin
  FPrepareResultTask.StopExecute;
  FPrepareResultTask.StartExecute;
end;

procedure TVectorItemSubsetChangeableForMarksLayer.OnPosChange;
begin
  FPrepareResultTask.StopExecute;
  FPrepareResultTask.StartExecute;
end;

procedure TVectorItemSubsetChangeableForMarksLayer.OnPrepareSubset(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation
);
var
  VTileRect: ITileRect;
  VCounterContext: TInternalPerformanceCounterContext;
  VResult: IVectorItemSubset;
  VNeedNotify: Boolean;
begin
  VTileRect := FTileRect.GetStatic;
  VCounterContext := FGetMarksCounter.StartOperation;
  try
    VResult := GetMarksSubset(FConfig.GetStatic, VTileRect);
  finally
    FGetMarksCounter.FinishOperation(VCounterContext);
  end;
  if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
    Exit;
  end;
  CS.BeginWrite;
  try
    if FResult = nil then begin
      VNeedNotify := (VResult <> nil) and (not VResult.IsEmpty);
    end else begin
      VNeedNotify := not FResult.IsEqual(VResult);
    end;
    if VNeedNotify then begin
      FResult := VResult;
    end;
  finally
    CS.EndWrite;
  end;
  if VNeedNotify then begin
    DoChangeNotify;
  end;
end;

end.
