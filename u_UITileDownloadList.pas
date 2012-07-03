unit u_UITileDownloadList;

interface

uses
  Classes,
  i_Notifier,
  i_DownloadUIConfig,
  i_ViewPortState,
  i_ActiveMapsConfig,
  i_NotifierTTLCheck,
  i_LocalCoordConverterFactorySimpe,
  i_DownloadInfoSimple,
  i_GlobalInternetState,
  i_TileError;

type
  TUITileDownloadList = class(TInterfacedObject)
  private
    FList: IInterfaceList;
  public
    constructor Create(
      const AGCList: INotifierTTLCheck;
      const AAppClosingNotifier: INotifier;
      const AConfig: IDownloadUIConfig;
      const ACoordConverterFactory: ILocalCoordConverterFactorySimpe;
      const AViewPortState: IViewPortState;
      const AMapsSet: IActiveMapsSet;
      const ADownloadInfo: IDownloadInfoSimple;
      const AGlobalInternetState: IGlobalInternetState;
      const AErrorLogger: ITileErrorLogger
    );
  end;

implementation

uses
  ActiveX,
  u_UiTileDownload;

{ TUITileDownloadList }

constructor TUITileDownloadList.Create(
  const AGCList: INotifierTTLCheck;
  const AAppClosingNotifier: INotifier;
  const AConfig: IDownloadUIConfig;
  const ACoordConverterFactory: ILocalCoordConverterFactorySimpe;
  const AViewPortState: IViewPortState;
  const AMapsSet: IActiveMapsSet;
  const ADownloadInfo: IDownloadInfoSimple;
  const AGlobalInternetState: IGlobalInternetState;
  const AErrorLogger: ITileErrorLogger
);
var
  VEnum: IEnumGUID;
  i: Cardinal;
  VGUID: TGUID;
  VMapTypeActive: IActiveMapSingle;
  VDownload: IInterface;
begin
  inherited Create;
  FList := TInterfaceList.Create;
  VEnum := AMapsSet.GetMapsSet.GetIterator;
  while VEnum.Next(1, VGUID, i) = S_OK do begin
    VMapTypeActive := AMapsSet.GetMapSingle(VGUID);
    if VMapTypeActive.GetMapType.MapType.Zmp.TileDownloaderConfig.Enabled then begin
      VDownload :=
        TUiTileDownload.Create(
          AConfig,
          AGCList,
          AAppClosingNotifier,
          ACoordConverterFactory,
          AViewPortState,
          VMapTypeActive,
          ADownloadInfo,
          AGlobalInternetState,
          AErrorLogger
        );
      FList.Add(VDownload);
    end;
  end;
end;

end.
