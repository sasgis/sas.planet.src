unit u_UITileDownloadList;

interface

uses
  i_NotifierOperation,
  i_DownloadUIConfig,
  i_LocalCoordConverterChangeable,
  i_NotifierTime,
  i_MapTypeSet,
  i_MapTypeSetChangeable,
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
      const AActiveMaps: IMapTypeSetChangeable;
      const ADownloadInfo: IDownloadInfoSimple;
      const AGlobalInternetState: IGlobalInternetState;
      const AErrorLogger: ITileErrorLogger
    );
  end;

implementation

uses
  ActiveX,
  i_InterfaceListSimple,
  i_MapTypes,
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
  const AActiveMaps: IMapTypeSetChangeable;
  const ADownloadInfo: IDownloadInfoSimple;
  const AGlobalInternetState: IGlobalInternetState;
  const AErrorLogger: ITileErrorLogger
);
var
  VEnum: IEnumUnknown;
  VCnt: Integer;
  VDownload: IInterface;
  VList: IInterfaceListSimple;
  VMapType: IMapType;
begin
  inherited Create;
  VList := TInterfaceListSimple.Create;
  VEnum := AMapsSet.GetMapTypeIterator;
  while VEnum.Next(1, VMapType, @VCnt) = S_OK do begin
    if VMapType.Zmp.TileDownloaderConfig.Enabled then begin
      VDownload :=
        TUiTileDownload.Create(
          AConfig,
          AGCNotifier,
          AAppClosingNotifier,
          ACoordConverterFactory,
          AViewPortState,
          VMapType,
          AActiveMaps,
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
