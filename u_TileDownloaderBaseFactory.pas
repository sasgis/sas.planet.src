unit u_TileDownloaderBaseFactory;

interface

uses
  i_ISimpleFactory;

type
  TTileDownloaderBaseFactory = class(TInterfacedObject, ISimpleFactory)
  private
    FMapType: IUnknown;
    FContent_Type: string;
  public
    constructor Create(AMapType: IUnknown);
    function CreateInstance: IUnknown;
  end;
implementation

uses
  u_TileDownloaderBase;
{ TTileDownloaderBaseFactory }

constructor TTileDownloaderBaseFactory.Create(AMapType: IInterface);
begin
  FMapType := AMapType;
end;

function TTileDownloaderBaseFactory.CreateInstance: IUnknown;
var
  VDownloader: TTileDownloaderBase;
begin
  VDownloader := TTileDownloaderBase.Create(FContent_Type, 1, GState.InetConnect);
  VDownloader.SleepOnResetConnection := FMapType.Sleep;
  Result := VDownloader;
end;

end.
