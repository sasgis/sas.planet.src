unit u_VectorItemSubsetChangeableForVectorLayers;

interface

uses
  SysUtils,
  t_GeoTypes,
  i_VectorItemSubset,
  i_Changeable,
  i_VectorItemSubsetChangeable,
  i_Listener,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_BackgroundTask,
  i_MapTypes,
  i_SimpleFlag,
  i_NotifierOperation,
  i_InterfaceListStatic,
  i_InternalPerformanceCounter,
  i_ListenerNotifierLinksList,
  i_TileError,
  u_ChangeableBase;

type
  TVectorItemSubsetChangeableForVectorLayers = class(TChangeableBase, IVectorItemSubsetChangeable)
  private
    FLayersSet: IMapTypeSetChangeable;
    FErrorLogger: ITileErrorLogger;
    FPosition: ILocalCoordConverterChangeable;
    FAppStartedNotifier: INotifierOneOperation;
    FAppClosingNotifier: INotifierOneOperation;

    FAppStartedListener: IListener;
    FAppClosingListener: IListener;

    FPrepareResultTask: IBackgroundTask;
    FLinksList: IListenerNotifierLinksList;
    FSubsetPrepareCounter: IInternalPerformanceCounter;
    FOneTilePrepareCounter: IInternalPerformanceCounter;

    FPrevLayerSet: IMapTypeSet;
    FPrevLocalConverter: ILocalCoordConverter;
    FLayerListeners: IInterfaceListStatic;
    FVersionListener: IListener;

    FDelicateUpdateFlag: ISimpleFlag;

    FResultCS: IReadWriteSync;
    FResult: IVectorItemSubset;


    procedure OnAppStarted;
    procedure OnAppClosing;

    procedure OnPosChange;
    procedure OnLayerSetChange;

    procedure OnMapVersionChange;
    procedure OnTileUpdate(const AMsg: IInterface);
    procedure OnPrepareSubset(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation
    );
    function PrepareSubset(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALocalConverter: ILocalCoordConverter;
      const ALayerSet: IMapTypeSet
    ): IVectorItemSubset;
  private
    function GetStatic: IVectorItemSubset;
  public
    procedure AfterConstruction; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      const APosition: ILocalCoordConverterChangeable;
      const ALayersSet: IMapTypeSetChangeable;
      const AErrorLogger: ITileErrorLogger
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_SimpleFlagWithInterlock,
  u_ListenerNotifierLinksList,
  u_ListenerByEvent;

{ TVectorItemSubsetChangeableForVectorLayers }

constructor TVectorItemSubsetChangeableForVectorLayers.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  const APosition: ILocalCoordConverterChangeable;
  const ALayersSet: IMapTypeSetChangeable;
  const AErrorLogger: ITileErrorLogger
);
begin
  Assert(Assigned(APosition));
  Assert(Assigned(ALayersSet));
  Assert(Assigned(AErrorLogger));
  inherited Create();
  FPosition := APosition;
  FLayersSet := ALayersSet;
  FErrorLogger := AErrorLogger;

  FSubsetPrepareCounter := APerfList.CreateAndAddNewCounter('SubsetPrepare');
  FOneTilePrepareCounter := APerfList.CreateAndAddNewCounter('OneTilePrepare');

  FAppStartedNotifier := AAppStartedNotifier;
  FAppClosingNotifier := AAppClosingNotifier;

  FDelicateUpdateFlag := TSimpleFlagWithInterlock.Create;
  FLinksList := TListenerNotifierLinksList.Create;
  FAppStartedListener := TNotifyNoMmgEventListener.Create(Self.OnAppStarted);
  FAppClosingListener := TNotifyNoMmgEventListener.Create(Self.OnAppClosing);

  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnPosChange),
    FPosition.ChangeNotifier
  );

  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnLayerSetChange),
    FLayersSet.ChangeNotifier
  );
end;

destructor TVectorItemSubsetChangeableForVectorLayers.Destroy;
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

procedure TVectorItemSubsetChangeableForVectorLayers.AfterConstruction;
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

function TVectorItemSubsetChangeableForVectorLayers.GetStatic: IVectorItemSubset;
begin
  FResultCS.BeginRead;
  try
    Result := FResult;
  finally
    FResultCS.EndRead;
  end;
end;

procedure TVectorItemSubsetChangeableForVectorLayers.OnAppClosing;
begin
  FLinksList.DeactivateLinks;
  FPrepareResultTask.Terminate;
end;

procedure TVectorItemSubsetChangeableForVectorLayers.OnAppStarted;
begin
  FLinksList.ActivateLinks;
  FPrepareResultTask.Start;
  FPrepareResultTask.StartExecute;
end;

procedure TVectorItemSubsetChangeableForVectorLayers.OnLayerSetChange;
begin
  FPrepareResultTask.StopExecute;
  FPrepareResultTask.StartExecute;
end;

procedure TVectorItemSubsetChangeableForVectorLayers.OnMapVersionChange;
begin
  FPrepareResultTask.StopExecute;
  FPrepareResultTask.StartExecute;
end;

procedure TVectorItemSubsetChangeableForVectorLayers.OnPosChange;
begin
  FPrepareResultTask.StopExecute;
  FPrepareResultTask.StartExecute;
end;

procedure TVectorItemSubsetChangeableForVectorLayers.OnPrepareSubset(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation
);
var
  VNeedRedraw: Boolean;
  VLocalConverter: ILocalCoordConverter;
  VLayerSet: IMapTypeSet;
  VCounterContext: TInternalPerformanceCounterContext;
  VResult: IVectorItemSubset;
  VNeedNotify: Boolean;
begin
  FDelicateUpdateFlag.CheckFlagAndReset;
  VNeedRedraw := True;
  while VNeedRedraw do begin
    if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
      Exit;
    end;
    VLocalConverter := FPosition.GetStatic;
    VLayerSet := FLayersSet.GetStatic;
    if not VLayerSet.IsEqual(FPrevLayerSet) then begin

    end else if not VLocalConverter.GetIsSameConverter(FPrevLocalConverter) then begin

    end;
    FPrevLayerSet := VLayerSet;
    FPrevLocalConverter := VLocalConverter;

    if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
      Exit;
    end;
    VCounterContext := FSubsetPrepareCounter.StartOperation;
    try
      VResult := PrepareSubset(AOperationID, ACancelNotifier, VLocalConverter, VLayerSet);
    finally
      FSubsetPrepareCounter.FinishOperation(VCounterContext);
    end;
    VNeedNotify := False;
    FResultCS.BeginWrite;
    try
      if FResult = nil then begin
        VNeedNotify := (VResult <> nil) and (not VResult.IsEmpty);
      end else begin
        VNeedNotify := (VResult <> nil) and (not VResult.IsEmpty) and (not FResult.IsEmpty);
      end;
      FResult := VResult;
    finally
      FResultCS.EndWrite;
    end;
    if VNeedNotify then begin
      DoChangeNotify;
    end;
    VNeedRedraw := FDelicateUpdateFlag.CheckFlagAndReset;
  end;
end;

procedure TVectorItemSubsetChangeableForVectorLayers.OnTileUpdate(
  const AMsg: IInterface
);
begin
  FDelicateUpdateFlag.SetFlag;
  FPrepareResultTask.StartExecute;
end;

function TVectorItemSubsetChangeableForVectorLayers.PrepareSubset(
  AOperationID: Integer; const ACancelNotifier: INotifierOperation;
  const ALocalConverter: ILocalCoordConverter;
  const ALayerSet: IMapTypeSet): IVectorItemSubset;
begin
  { TODO -oViktor -c :  08.08.2013 22:19:02 }
end;

end.
