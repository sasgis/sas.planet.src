unit u_TileDownloaderBaseFactory;

interface

uses
  IniFiles,
  VCLZip,
  UMapType,
  i_ISimpleFactory;

type
  TTileDownloaderBaseFactory = class(TInterfacedObject, ISimpleFactory)
  private
    FMapType: TMapType;
    FIgnoreContent_Type: Boolean;
    FContent_Type: string;
    FDefaultContent_Type: string;
  public
    constructor Create(
      AMapType: TMapType;
      AUnZip: TVCLZip;
      AIniFile: TCustomIniFile
    );
    function CreateInstance: IUnknown;
  end;

implementation

uses
  u_GlobalState,
  u_TileDownloaderBase;

{ TTileDownloaderBaseFactory }

constructor TTileDownloaderBaseFactory.Create(
  AMapType: TMapType;
  AUnZip: TVCLZip;
  AIniFile: TCustomIniFile
);
begin
  FMapType := AMapType;
  FIgnoreContent_Type:=AIniFile.ReadBool('PARAMS','IgnoreContentType', False);
  FDefaultContent_Type:=AIniFile.ReadString('PARAMS','DefaultContentType','image/jpg');
  FContent_Type:=AIniFile.ReadString('PARAMS','ContentType','image/jpg');

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
  VDownloader := TTileDownloaderBase.Create(FIgnoreContent_Type,
    FContent_Type, FDefaultContent_Type, VTryCount, GState.InetConnect);
  VDownloader.SleepOnResetConnection := FMapType.Sleep;
  VDownloader.WaitInterval := FMapType.Sleep;
  Result := VDownloader;
end;

end.
