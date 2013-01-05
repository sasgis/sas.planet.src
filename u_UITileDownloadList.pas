unit u_UITileDownloadList;

interface

uses
  Classes,
  i_NotifierOperation,
  i_DownloadUIConfig,
  i_LocalCoordConverterChangeable,
  i_ActiveMapsConfig,
  i_NotifierTTLCheck,
  i_MapTypes,
  i_LocalCoordConverterFactorySimpe,
  i_DownloadInfoSimple,
  i_GlobalInternetState,
  i_TileError,
  u_BaseInterfacedObject;

type
  TUITileDownloadList = class(TBaseInterfacedObject)
  private
    FList: IInterfaceList;
  public
    constructor Create(
      const AGCNotifier: INotifierTime;
      const AAppClosingNotifier: INotifierOneOperation;
      const AConfig: IDownloadUIConfig;
      const ACoordConverterFactory: ILocalCoordConverterFactorySimpe;
      const AViewPortState: ILocalCoordConverterChangeable;
      const AMapsSet: IMapTypeSet;
      const AMapsSingleSet: IActiveMapSingleSet;
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
  const AGCNotifier: INotifierTime;
  const AAppClosingNotifier: INotifierOneOperation;
  const AConfig: IDownloadUIConfig;
  const ACoordConverterFactory: ILocalCoordConverterFactorySimpe;
  const AViewPortState: ILocalCoordConverterChangeable;
  const AMapsSet: IMapTypeSet;
  const AMapsSingleSet: IActiveMapSingleSet;
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
  VEnum := AMapsSet.GetIterator;
  while VEnum.Next(1, VGUID, i) = S_OK do begin
    VMapTypeActive := AMapsSingleSet.GetMapSingle(VGUID);
    if VMapTypeActive.GetMapType.MapType.Zmp.TileDownloaderConfig.Enabled then begin
      VDownload :=
        TUiTileDownload.Create(
          AConfig,
          AGCNotifier,
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
