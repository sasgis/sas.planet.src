unit fr_GpsSatellites;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  GR32_Image,
  i_JclNotify,
  i_JclListenerNotifierLinksList,
  i_LanguageManager,
  i_Sensor,
  i_SatellitesInViewMapDraw,
  u_CommonFormAndFrameParents;

type
  TfrGpsSatellites = class(TFrame)
    SatellitePaintBox: TImage32;
    pnlSatInfoLegend: TPanel;
    pnlSatInfoActive: TPanel;
    lblSatInfoActive: TLabel;
    shpSatInfoActive: TShape;
    pnlSatInfoVisible: TPanel;
    shpSatInfoVisible: TShape;
    lblSatInfoVisible: TLabel;
    pnlSatInfoZeroSignal: TPanel;
    lblSatInfoZeroSignal: TLabel;
    shpSatInfoZeroSignal: TShape;
    procedure SatellitePaintBoxResize(Sender: TObject);
  private
    FGpsSatellitesSensor: ISensorGPSSatellites;
    FMapDraw: ISatellitesInViewMapDraw;

    FLinksList: IJclListenerNotifierLinksList;
    FValueChangeId: Integer;
    FValueShowId: Integer;

    procedure OnSensorDataUpdate;
    procedure OnTimer;
    procedure UpdateDataView;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const ATimerNoifier: IJclNotifier;
      const AGpsSatellitesSensor: ISensorGPSSatellites;
      const AMapDraw: ISatellitesInViewMapDraw;
      AShowLegend: Boolean
    );
  end;

implementation

uses
  i_GPS,
  u_JclListenerNotifierLinksList,
  u_NotifyEventListener;

{$R *.dfm}

constructor TfrGpsSatellites.Create(
  const ALanguageManager: ILanguageManager;
  const ATimerNoifier: IJclNotifier;
  const AGpsSatellitesSensor: ISensorGPSSatellites;
  const AMapDraw: ISatellitesInViewMapDraw;
  AShowLegend: Boolean
);
begin
  inherited Create(ALanguageManager);
  FGpsSatellitesSensor := AGpsSatellitesSensor;
  FMapDraw := AMapDraw;

  pnlSatInfoLegend.Visible := AShowLegend;

  FLinksList := TJclListenerNotifierLinksList.Create;

  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnSensorDataUpdate),
    FGpsSatellitesSensor.ChangeNotifier
  );

  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnTimer),
    ATimerNoifier
  );

  FLinksList.ActivateLinks;
  OnSensorDataUpdate;
end;

procedure TfrGpsSatellites.OnSensorDataUpdate;
begin
  InterlockedIncrement(FValueChangeId);
end;

procedure TfrGpsSatellites.OnTimer;
var
  VId: Integer;
begin
  if Self.Visible then begin
    VId := FValueChangeId;
    if VId <> FValueShowId then begin
      UpdateDataView;
      FValueShowId := VId;
    end;
  end;
end;

procedure TfrGpsSatellites.SatellitePaintBoxResize(Sender: TObject);
begin
  SatellitePaintBox.Bitmap.Lock;
  try
    SatellitePaintBox.Bitmap.SetSizeFrom(SatellitePaintBox);
    UpdateDataView;
  finally
    SatellitePaintBox.Bitmap.Unlock;
  end;
end;

procedure TfrGpsSatellites.UpdateDataView;
var
  VSatellites: IGPSSatellitesInView;
begin
  VSatellites := FGpsSatellitesSensor.Info;
  SatellitePaintBox.Bitmap.Lock;
  try
    FMapDraw.Draw(SatellitePaintBox.Bitmap, VSatellites);
  finally
    SatellitePaintBox.Bitmap.Unlock;
  end;
end;

end.
