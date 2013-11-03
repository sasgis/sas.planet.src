unit u_BitmapLayerProviderChangeableForGpsTrack;

interface

uses
  i_NotifierTime,
  i_MapLayerGPSTrackConfig,
  i_Bitmap32StaticFactory,
  i_GPSRecorder,
  i_InternalPerformanceCounter,
  i_SimpleFlag,
  i_ListenerNotifierLinksList,
  i_BitmapLayerProvider,
  i_BitmapLayerProviderChangeable,
  u_BitmapLayerProviderChangeableBase;

type
  TBitmapLayerProviderChangeableForGpsTrack = class(TBitmapLayerProviderChangeableBase)
  private
    FConfig: IMapLayerGPSTrackConfig;
    FBitmapFactory: IBitmap32StaticFactory;
    FGPSRecorder: IGpsTrackRecorder;

    FGetTrackCounter: IInternalPerformanceCounter;
    FGpsPosChangeFlag: ISimpleFlag;

    procedure OnConfigChange;
    procedure OnGPSRecorderChange;
    procedure OnTimer;
  protected
    function CreateStatic: IInterface; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const ATimerNoifier: INotifierTime;
      const AConfig: IMapLayerGPSTrackConfig;
      const ABitmapFactory: IBitmap32StaticFactory;
      const AGPSRecorder: IGpsTrackRecorder
    );
  end;

implementation

uses
  u_ListenerByEvent,
  u_ListenerTime,
  u_ListenerNotifierLinksList,
  u_SimpleFlagWithInterlock,
  u_BitmapLayerProviderByTrackPath;

{ TBitmapLayerProviderChangeableForGpsTrack }

constructor TBitmapLayerProviderChangeableForGpsTrack.Create(
  const APerfList: IInternalPerformanceCounterList;
  const ATimerNoifier: INotifierTime;
  const AConfig: IMapLayerGPSTrackConfig;
  const ABitmapFactory: IBitmap32StaticFactory;
  const AGPSRecorder: IGpsTrackRecorder
);
begin
  inherited Create;
  FConfig := AConfig;
  FGPSRecorder := AGPSRecorder;
  FBitmapFactory := ABitmapFactory;

  FGetTrackCounter := APerfList.CreateAndAddNewCounter('GetTrack');
  FGpsPosChangeFlag := TSimpleFlagWithInterlock.Create;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.ChangeNotifier
  );
  LinksList.Add(
    TListenerTimeCheck.Create(Self.OnTimer, 1000),
    ATimerNoifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnGPSRecorderChange),
    FGPSRecorder.ChangeNotifier
  );
end;

function TBitmapLayerProviderChangeableForGpsTrack.CreateStatic: IInterface;
var
  VResult: IBitmapLayerProvider;
  VTrackColorer: ITrackColorerStatic;
  VPointsCount: Integer;
  VLineWidth: Double;
  VEnum: IEnumGPSTrackPoint;
  VVisible: Boolean;
begin
  VResult := nil;
  FConfig.LockRead;
  try
    VVisible := FConfig.Visible;
    VPointsCount := FConfig.LastPointCount;
    VLineWidth := FConfig.LineWidth;
    VTrackColorer := FConfig.TrackColorerConfig.GetStatic;
  finally
    FConfig.UnlockRead
  end;

  if VVisible and (VPointsCount > 1) then begin
    VEnum := FGPSRecorder.LastPoints(VPointsCount);
    VResult :=
      TBitmapLayerProviderByTrackPath.Create(
        VPointsCount,
        VLineWidth,
        VTrackColorer,
        FBitmapFactory,
        VEnum
      );
  end;
  Result := VResult;
end;

procedure TBitmapLayerProviderChangeableForGpsTrack.OnConfigChange;
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TBitmapLayerProviderChangeableForGpsTrack.OnGPSRecorderChange;
begin
  FGpsPosChangeFlag.SetFlag;
end;

procedure TBitmapLayerProviderChangeableForGpsTrack.OnTimer;
begin
  if FGpsPosChangeFlag.CheckFlagAndReset then begin
    LockWrite;
    try
      SetChanged;
    finally
      UnlockWrite;
    end;
  end;
end;

end.
