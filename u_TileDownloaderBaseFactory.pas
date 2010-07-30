unit u_TileDownloaderBaseFactory;

interface

uses
  UMapType,
  i_ISimpleFactory;

type
  TTileDownloaderBaseFactory = class(TInterfacedObject, ISimpleFactory)
  private
    FMapType: TMapType;
  public
    constructor Create(AMapType: TMapType);
    function CreateInstance: IUnknown;
  end;

implementation

uses
  u_GlobalState,
  u_TileDownloaderBase;

{ TTileDownloaderBaseFactory }

constructor TTileDownloaderBaseFactory.Create(AMapType: TMapType);
begin
  FMapType := AMapType;
end;

function TTileDownloaderBaseFactory.CreateInstance: IUnknown;
var
  VDownloader: TTileDownloaderBase;
  VTryCount: Integer;
begin
  if GState.TwoDownloadAttempt then begin
    VTryCount := 2;
  end else begin
    VTryCount := 1;
  end;
  VDownloader := TTileDownloaderBase.Create(FMapType.IgnoreContentType,
    FMapType.ContentType, FMapType.DefaultContentType, VTryCount, GState.InetConnect);
  VDownloader.SleepOnResetConnection := FMapType.Sleep;
  VDownloader.WaitInterval := FMapType.Sleep;
  Result := VDownloader;
end;

end.
