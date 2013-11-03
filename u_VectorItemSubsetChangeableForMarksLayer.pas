unit u_VectorItemSubsetChangeableForMarksLayer;

interface

uses
  SysUtils,
  t_GeoTypes,
  i_VectorItemSubset,
  i_VectorItemSubsetChangeable,
  i_Listener,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_ThreadConfig,
  i_BackgroundTask,
  i_NotifierOperation,
  i_InternalPerformanceCounter,
  i_ListenerNotifierLinksList,
  i_UsedMarksConfig,
  i_MarkSystem,
  u_ChangeableBase;

type
  TVectorItemSubsetChangeableForMarksLayer = class(TChangeableBase, IVectorItemSubsetChangeable)
  private
    FMarkDB: IMarkSystem;
    FConfig: IUsedMarksConfig;
    FPosition: ILocalCoordConverterChangeable;
    FAppStartedNotifier: INotifierOneOperation;
    FAppClosingNotifier: INotifierOneOperation;

    FAppStartedListener: IListener;
    FAppClosingListener: IListener;

    FPrepareResultTask: IBackgroundTask;
    FLinksList: IListenerNotifierLinksList;
    FGetMarksCounter: IInternalPerformanceCounter;

    FResultCS: IReadWriteSync;
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
      const ALocalConverter: ILocalCoordConverter
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
      const APosition: ILocalCoordConverterChangeable;
      const AMarkSystem: IMarkSystem;
      const AConfig: IUsedMarksConfig;
      const AThreadConfig: IThreadConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  i_InterfaceListStatic,
  i_CoordConverter,
  u_ListenerNotifierLinksList,
  u_BackgroundTask,
  u_ListenerByEvent,
  u_Synchronizer;

{ TVectorItemSubsetChangeableForMarksLayer }

constructor TVectorItemSubsetChangeableForMarksLayer.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  const APosition: ILocalCoordConverterChangeable;
  const AMarkSystem: IMarkSystem;
  const AConfig: IUsedMarksConfig;
  const AThreadConfig: IThreadConfig
);
begin
  Assert(Assigned(APosition));
  Assert(Assigned(AMarkSystem));
  inherited Create();
  FPosition := APosition;
  FMarkDB := AMarkSystem;
  FConfig := AConfig;

  FGetMarksCounter := APerfList.CreateAndAddNewCounter('GetMarks');

  FAppStartedNotifier := AAppStartedNotifier;
  FAppClosingNotifier := AAppClosingNotifier;

  FResultCS := MakeSyncRW_Var(Self, False);
  FLinksList := TListenerNotifierLinksList.Create;
  FAppStartedListener := TNotifyNoMmgEventListener.Create(Self.OnAppStarted);
  FAppClosingListener := TNotifyNoMmgEventListener.Create(Self.OnAppClosing);

  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnPosChange),
    FPosition.ChangeNotifier
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
  const ALocalConverter: ILocalCoordConverter
): IVectorItemSubset;
var
  VList: IInterfaceListStatic;
  VZoom: Byte;
  VMapPixelRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
  VGeoConverter: ICoordConverter;
begin
  VList := nil;
  Result := nil;
  if AConfig.IsUseMarks then begin
    VZoom := ALocalConverter.GetZoom;
    if not AConfig.IgnoreCategoriesVisible then begin
      VList := FMarkDB.GetVisibleCategories(VZoom);
    end;
    if AConfig.IgnoreCategoriesVisible or (Assigned(VList) and (VList.Count > 0)) then begin
      VGeoConverter := ALocalConverter.GetGeoConverter;
      VMapPixelRect := ALocalConverter.GetRectInMapPixelFloat;
      VGeoConverter.CheckPixelRectFloat(VMapPixelRect, VZoom);
      VLonLatRect := VGeoConverter.PixelRectFloat2LonLatRect(VMapPixelRect, VZoom);
      Result :=
        FMarkDB.MarkDb.GetMarkSubsetByCategoryListInRect(
          VLonLatRect,
          VList,
          AConfig.IgnoreMarksVisible
        );
    end;
  end;
end;

function TVectorItemSubsetChangeableForMarksLayer.GetStatic: IVectorItemSubset;
begin
  FResultCS.BeginRead;
  try
    Result := FResult;
  finally
    FResultCS.EndRead;
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
  VLocalConverter: ILocalCoordConverter;
  VCounterContext: TInternalPerformanceCounterContext;
  VResult: IVectorItemSubset;
  VNeedNotify: Boolean;
begin
  VLocalConverter := FPosition.GetStatic;
  VCounterContext := FGetMarksCounter.StartOperation;
  try
    VResult := GetMarksSubset(FConfig.GetStatic, VLocalConverter);
  finally
    FGetMarksCounter.FinishOperation(VCounterContext);
  end;
  if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
    Exit;
  end;
  FResultCS.BeginWrite;
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
    FResultCS.EndWrite;
  end;
  if VNeedNotify then begin
    DoChangeNotify;
  end;
end;

end.
