unit u_UITileDownloadList;

interface

uses
  Classes,
  i_NotifierOperation,
  i_DownloadUIConfig,
  i_LocalCoordConverterChangeable,
  i_ActiveMapsConfig,
  i_NotifierTime,
  i_MapTypes,
  i_LocalCoordConverterFactorySimpe,
  i_DownloadInfoSimple,
  i_GlobalInternetState,
  i_InterfaceListStatic,
  i_TileError,
  u_BaseInterfacedObject;

type
  TUITileDownloadList = class(TBaseInterfacedObject)
  private
    FList: IInterfaceListStatic;
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
  i_InterfaceListSimple,
  u_InterfaceListSimple,
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
  VList: IInterfaceListSimple;
begin
  inherited Create;
  VList := TInterfaceListSimple.Create;
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
      VList.Add(VDownload);
    end;
  end;
  FList := VList.MakeStaticAndClear;
end;

end.
