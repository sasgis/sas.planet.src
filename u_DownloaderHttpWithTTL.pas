unit u_DownloaderHttpWithTTL;

interface

uses
  SysUtils,
  i_NotifierOperation,
  i_ListenerTime,
  i_NotifierTime,
  i_DownloadRequest,
  i_DownloadResult,
  i_DownloadResultFactory,
  i_Downloader,
  u_DownloaderHttp,
  u_BaseInterfacedObject;

type
  TDownloaderHttpWithTTL = class(TBaseInterfacedObject, IDownloader)
  private
    FResultFactory: IDownloadResultFactory;
    FGCNotifier: INotifierTime;
    FAllowUseCookie: Boolean;
    FTTLListener: IListenerTimeWithUsedFlag;
    FCS: IReadWriteSync;
    FDownloader: IDownloader;
    procedure OnTTLTrim;
  private
    function DoRequest(
      const ARequest: IDownloadRequest;
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer
    ): IDownloadResult;
  public
    constructor Create(
      const AGCNotifier: INotifierTime;
      const AResultFactory: IDownloadResultFactory;
      const AAllowUseCookie: Boolean = False
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_Synchronizer,
  u_ListenerTime;

{ TDownloaderHttpWithTTL }

constructor TDownloaderHttpWithTTL.Create(
  const AGCNotifier: INotifierTime;
  const AResultFactory: IDownloadResultFactory;
  const AAllowUseCookie: Boolean
);
const
  CHttpClientTTL = 300000; // 5 min
begin
  inherited Create;
  FAllowUseCookie := AAllowUseCookie;
  FResultFactory := AResultFactory;
  FGCNotifier := AGCNotifier;
  FCS := MakeSyncRW_Std(Self, FALSE);
  FTTLListener := TListenerTTLCheck.Create(Self.OnTTLTrim, CHttpClientTTL);
  FGCNotifier.Add(FTTLListener);
end;

destructor TDownloaderHttpWithTTL.Destroy;
begin
  if Assigned(FGCNotifier) and Assigned(FTTLListener) then begin
    FGCNotifier.Remove(FTTLListener);
    FTTLListener := nil;
    FGCNotifier := nil;
  end;
  FCS := nil;
  inherited;
end;

function TDownloaderHttpWithTTL.DoRequest(
  const ARequest: IDownloadRequest;
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer
): IDownloadResult;
var
  VDownloader: IDownloader;
begin
  FCS.BeginWrite;
  try
    FTTLListener.UpdateUseTime;
    VDownloader := FDownloader;
    if VDownloader = nil then begin
      VDownloader := TDownloaderHttp.Create(FResultFactory, FAllowUseCookie);
      FDownloader := VDownloader;
    end;
    Result := VDownloader.DoRequest(ARequest, ACancelNotifier, AOperationID);
    FTTLListener.UpdateUseTime;
  finally
    FCS.EndWrite;
  end;
end;

procedure TDownloaderHttpWithTTL.OnTTLTrim;
begin
  FCS.BeginWrite;
  try
    FDownloader := nil;
  finally
    FCS.EndWrite;
  end;
end;

end.
