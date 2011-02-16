unit u_TileDownloaderUI;

interface

uses
  Windows,
  Classes,
  Types,
  i_JclNotify,
  t_CommonTypes,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_ICoordConverter,
  i_ILocalCoordConverter,
  i_IActiveMapsConfig,
  i_IViewPortState,
  i_MapTypes,
  i_ITileDownlodSession,
  u_MapLayerShowError,
  UMapType;

type
  TTileDownloaderUI = class(TThread)
  private
    FMapsSet: IActiveMapsSet;
    FViewPortState: IViewPortState;

    FVisualCoordConverter: ILocalCoordConverter;
    FActiveMapsList: IMapTypeList;
    FMapType: TMapType;

    change_scene: boolean;

    FErrorString: string;
    FTileMaxAgeInInternet: TDateTime;
    FMapTileUpdateEvent: TMapTileUpdateEvent;
    FErrorShowLayer: TTileErrorInfoLayer;
    FUseDownloadChangeNotifier: IJclNotifier;
    FUseDownload: TTileSource;
    FChangePosListener: IJclListener;

    FLoadXY: TPoint;
    FLoadUrl: string;

    class function GetErrStr(Aerr: TDownloadTileResult): string; virtual;
    procedure GetCurrentMapAndPos;
    procedure AfterWriteToFile;
    function GetUseDownload: TTileSource;
    procedure SetUseDownload(const Value: TTileSource);
    procedure ChangePos(Sender: TObject);
  protected
    procedure Execute; override;
  public
    constructor Create(
      AViewPortState: IViewPortState;
      AMapsSet: IActiveMapsSet;
      AMapTileUpdateEvent: TMapTileUpdateEvent;
      AErrorShowLayer: TTileErrorInfoLayer
    ); overload;
    destructor Destroy; override;
    procedure LoadConfig(AConfigProvider: IConfigDataProvider);
    procedure SaveConfig(AConfigProvider: IConfigDataWriteProvider);
    procedure StartThreads;
    procedure SendTerminateToThreads;
    property TileMaxAgeInInternet: TDateTime read FTileMaxAgeInInternet;
    property UseDownload: TTileSource read GetUseDownload write SetUseDownload;
    property UseDownloadChangeNotifier: IJclNotifier read FUseDownloadChangeNotifier;
  end;

implementation

uses
  SysUtils,
  ActiveX,
  u_JclNotify,
  t_GeoTypes,
  u_GlobalState,
  u_NotifyEventListener,
  i_ITileIterator,
  u_TileIteratorSpiralByRect,
  UResStrings;

constructor TTileDownloaderUI.Create(
  AViewPortState: IViewPortState;
  AMapsSet: IActiveMapsSet;
  AMapTileUpdateEvent: TMapTileUpdateEvent;
  AErrorShowLayer: TTileErrorInfoLayer
);
begin
  inherited Create(True);
  FViewPortState := AViewPortState;
  FMapsSet := AMapsSet;
  FMapTileUpdateEvent := AMapTileUpdateEvent;
  FErrorShowLayer := AErrorShowLayer;
  FViewPortState := AViewPortState;
  Priority := tpLower;
  FUseDownload := tsCache;
  randomize;
  FTileMaxAgeInInternet :=  1/24/60;
  FUseDownloadChangeNotifier := TJclBaseNotifier.Create;
  FChangePosListener := TNotifyEventListener.Create(ChangePos);
  FViewPortState.GetChangeNotifier.Add(FChangePosListener);
  FMapsSet.GetChangeNotifier.Add(FChangePosListener);
end;

destructor TTileDownloaderUI.Destroy;
var
  VWaitResult: DWORD;
begin
  FViewPortState.GetChangeNotifier.Remove(FChangePosListener);
  FMapsSet.GetChangeNotifier.Remove(FChangePosListener);

  VWaitResult := WaitForSingleObject(Handle, 10000);
  if VWaitResult = WAIT_TIMEOUT then begin
    TerminateThread(Handle, 0);
  end;
  FUseDownloadChangeNotifier := nil;
  FChangePosListener := nil;
  FMapsSet := nil;
  inherited;
end;


procedure TTileDownloaderUI.ChangePos(Sender: TObject);
begin
  change_scene := True;
end;

procedure TTileDownloaderUI.GetCurrentMapAndPos;
begin
  FVisualCoordConverter := FViewPortState.GetVisualCoordConverter;
  FActiveMapsList := FMapsSet.GetSelectedMapsList;
end;

class function TTileDownloaderUI.GetErrStr(Aerr: TDownloadTileResult): string;
begin
  case Aerr of
    dtrProxyAuthError:
    begin
      result := SAS_ERR_Authorization;
    end;
    dtrBanError:
    begin
      result := SAS_ERR_Ban;
    end;
    dtrTileNotExists:
    begin
      result := SAS_ERR_TileNotExists;
    end;
    dtrDownloadError,
    dtrErrorInternetOpen,
    dtrErrorInternetOpenURL:
    begin
      result := SAS_ERR_Noconnectionstointernet;
    end;
    dtrErrorMIMEType:
    begin
      result := SAS_ERR_TileDownloadContentTypeUnexpcted;
    end;
    dtrUnknownError:
    begin
      Result := SAS_ERR_TileDownloadUnexpectedError;
    end else begin
    result := '';
  end;
  end;
end;

function TTileDownloaderUI.GetUseDownload: TTileSource;
begin
  Result := FUseDownload;
end;

procedure TTileDownloaderUI.LoadConfig(AConfigProvider: IConfigDataProvider);
var
  VConfigProvider: IConfigDataProvider;
begin
  inherited;
  VConfigProvider := AConfigProvider.GetSubItem('VIEW');
  if VConfigProvider <> nil then begin
    case VConfigProvider.ReadInteger('TileSource',1) of
      0: UseDownload := tsInternet;
      2: UseDownload := tsCacheInternet;
    else
      UseDownload := tsCache;
    end;
  end else begin
    UseDownload := tsCache;
  end;
end;

procedure TTileDownloaderUI.SaveConfig(
  AConfigProvider: IConfigDataWriteProvider);
var
  VConfigProvider: IConfigDataWriteProvider;
begin
  inherited;
  VConfigProvider := AConfigProvider.GetOrCreateSubItem('VIEW');
  case UseDownload of
    tsInternet: VConfigProvider.WriteInteger('TileSource', 0);
    tsCache: VConfigProvider.WriteInteger('TileSource', 1);
    tsCacheInternet: VConfigProvider.WriteInteger('TileSource', 2);
  end;
end;

procedure TTileDownloaderUI.SendTerminateToThreads;
begin
  inherited;
  Terminate;
end;

procedure TTileDownloaderUI.SetUseDownload(const Value: TTileSource);
begin
  if FUseDownload <> Value then begin
    FUseDownload := Value;
    change_scene := True;
    FUseDownloadChangeNotifier.Notify(nil);
  end;
end;

procedure TTileDownloaderUI.StartThreads;
begin
  inherited;
  Resume;
end;

procedure TTileDownloaderUI.AfterWriteToFile;
begin
  if FErrorString <> '' then begin
    if FErrorShowLayer <> nil then begin
      FErrorShowLayer.ShowError(FLoadXY, FVisualCoordConverter.GetZoom, FMapType, FErrorString);
    end;
  end else begin
    if FErrorShowLayer <> nil then begin
      FErrorShowLayer.Visible := False;
    end;
    if Addr(FMapTileUpdateEvent) <> nil then begin
      FMapTileUpdateEvent(FMapType, FVisualCoordConverter.GetZoom, FLoadXY);
    end;
  end;
end;

procedure TTileDownloaderUI.Execute;
var
  ii: integer;
  VPixelInTargetMap: TPoint;
  ty: string;
  fileBuf: TMemoryStream;
  res: TDownloadTileResult;
  VNeedDownload: Boolean;
  VIterator: ITileIterator;
  VTile: TPoint;
  VLocalConverter: ILocalCoordConverter;
  VGeoConverter: ICoordConverter;
  VMapPixelRect: TDoubleRect;
  VZoom: Byte;
  VActiveMapsList: IMapTypeList;
  VEnum: IEnumGUID;
  VGUID: TGUID;
  i: Cardinal;
  VMap: IMapType;
begin
  repeat
    if FUseDownload = tsCache then begin
      if Terminated then begin
        break;
      end;
      Sleep(1000);
      if Terminated then begin
        break;
      end;
    end else begin
      if (not change_scene) then begin
        if Terminated then begin
          break;
        end;
        sleep(100);
        if Terminated then begin
          break;
        end;
      end else begin
        if Terminated then begin
          break;
        end;
        change_scene := false;
        Synchronize(GetCurrentMapAndPos);
        if Terminated then begin
          break;
        end;
        VLocalConverter := FVisualCoordConverter;
        if VLocalConverter = nil then begin
          if Terminated then begin
            break;
          end;
          Sleep(1000);
        end else begin
          VMapPixelRect := VLocalConverter.GetRectInMapPixelFloat;
          VZoom := VLocalConverter.GetZoom;
          VActiveMapsList := FActiveMapsList;
          VGeoConverter := VLocalConverter.GetGeoConverter;
          VGeoConverter.CheckPixelRectFloat(VMapPixelRect, VZoom);
          VIterator := TTileIteratorSpiralByRect.Create(VGeoConverter.PixelRectFloat2TileRect(VMapPixelRect, VZoom));
          try
            while VIterator.Next(VTile) do begin
              if Terminated then begin
                break;
              end;
              if change_scene then begin
                Break;
              end;
              VEnum := VActiveMapsList.GetIterator;
              while VEnum.Next(1, VGUID, i) = S_OK do begin
                VMap := VActiveMapsList.GetMapTypeByGUID(VGUID);
                if Terminated then begin
                  break;
                end;
                if change_scene then begin
                  Break;
                end;
                if VMap <> nil then begin
                  FMapType := VMap.MapType;
                  if FMapType.UseDwn then begin
                    VPixelInTargetMap := VGeoConverter.PixelPos2OtherMap(
                      VGeoConverter.TilePos2PixelPos(VTile, VZoom),
                      VZoom,
                      FMapType.GeoConvert
                    );
                    FLoadXY := FMapType.GeoConvert.PixelPos2TilePos(VPixelInTargetMap, VZoom);
                    VNeedDownload := False;
                    if FMapType.TileExists(FLoadXY, VZoom) then begin
                      if FUseDownload = tsInternet then begin
                        if Now - FMapType.TileLoadDate(FLoadXY, VZoom) > FTileMaxAgeInInternet then begin
                          VNeedDownload := True;
                        end;
                      end;
                    end else begin
                      if (FUseDownload = tsInternet) or (FUseDownload = tsCacheInternet) then begin
                        if not(FMapType.TileNotExistsOnServer(FLoadXY, VZoom)) then begin
                          VNeedDownload := True;
                        end;
                      end;
                    end;
                    if VNeedDownload then begin
                      FileBuf := TMemoryStream.Create;
                      try
                        try
                          res := FMapType.DownloadTile(Self, FLoadXY, VZoom, false, 0, FLoadUrl, ty, fileBuf);
                          FErrorString := GetErrStr(res);
                          if (res = dtrOK) or (res = dtrSameTileSize) then begin
                            GState.DownloadInfo.Add(1, fileBuf.Size);
                          end;
                        except
                          on E: Exception do begin
                            FErrorString := E.Message;
                          end;
                        end;
                        if Terminated then begin
                          break;
                        end;
                        Synchronize(AfterWriteToFile);
                      finally
                        FileBuf.Free;
                      end;
                    end;
                  end;
                end;
              end;
            end;
          finally
            VIterator := nil;
          end;
        end;
      end;
    end;
  until Terminated;
end;

end.
