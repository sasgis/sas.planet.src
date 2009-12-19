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
begin
  VDownloader := TTileDownloaderBase.Create(FMapType.ContentType, 1, GState.InetConnect);
  VDownloader.SleepOnResetConnection := FMapType.Sleep;
  VDownloader.WaitInterval := FMapType.Sleep;
  Result := VDownloader;
end;

end.
