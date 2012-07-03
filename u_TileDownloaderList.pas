unit u_TileDownloaderList;

interface

uses
  Windows,
  SysUtils,
  i_Notify,
  i_TileDownloaderConfig,
  i_TileDownloader,
  i_DownloadResultFactory,
  i_TileDownloadResultSaver,
  i_TileDownloaderState,
  i_TTLCheckNotifier,
  i_TileDownloadRequestBuilderFactory,
  i_TileDownloaderList;

type
  TTileDownloaderList = class(TInterfacedObject, ITileDownloaderList)
  private
    FGCList: ITTLCheckNotifier;
    FAppClosingNotifier: INotifier;
    FResultFactory: IDownloadResultFactory;
    FDownloadSystemState: ITileDownloaderStateChangeble;
    FTileDownloaderConfig: ITileDownloaderConfig;
    FResultSaver: ITileDownloadResultSaver;
    FRequestBuilderFactory: ITileDownloadRequestBuilderFactory;

    FChangeCounter: Integer;
    FChangeNotifier: INotifier;
    FConfigListener: IListener;
    FCS: IReadWriteSync;

    FStatic: ITileDownloaderListStatic;
    procedure OnConfigChange;
    function CreateDownloader: ITileDownloader;
  protected
    function GetStatic: ITileDownloaderListStatic;
    function GetChangeNotifier: INotifier;
  public
    constructor Create(
      const AGCList: ITTLCheckNotifier;
      const AAppClosingNotifier: INotifier;
      const AResultFactory: IDownloadResultFactory;
      const ADownloadSystemState: ITileDownloaderStateChangeble;
      const ATileDownloaderConfig: ITileDownloaderConfig;
      const AResultSaver: ITileDownloadResultSaver;
      const ARequestBuilderFactory: ITileDownloadRequestBuilderFactory
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_Synchronizer,
  i_TileDownloadRequestBuilder,
  i_Downloader,
  u_Notifier,
  u_NotifyEventListener,
  u_LastResponseInfo,
  u_TileDownloadRequestBuilderLazy,
  u_DownloaderHttpWithTTL,
  u_TileDownloaderSimple,
  u_TileDownloaderListStatic;

{ TTileDownloaderList }

constructor TTileDownloaderList.Create(
  const AGCList: ITTLCheckNotifier;
  const AAppClosingNotifier: INotifier;
  const AResultFactory: IDownloadResultFactory;
  const ADownloadSystemState: ITileDownloaderStateChangeble;
  const ATileDownloaderConfig: ITileDownloaderConfig;
  const AResultSaver: ITileDownloadResultSaver;
  const ARequestBuilderFactory: ITileDownloadRequestBuilderFactory
);
begin
  inherited Create;
  FGCList := AGCList;
  FAppClosingNotifier := AAppClosingNotifier;
  FResultFactory := AResultFactory;
  FDownloadSystemState := ADownloadSystemState;
  FTileDownloaderConfig := ATileDownloaderConfig;
  FResultSaver := AResultSaver;
  FRequestBuilderFactory := ARequestBuilderFactory;

  FChangeNotifier := TBaseNotifier.Create;
  FCS := MakeSyncRW_Var(Self);

  FConfigListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);

  FTileDownloaderConfig.ChangeNotifier.Add(FConfigListener);
  FDownloadSystemState.ChangeNotifier.Add(FConfigListener);

  OnConfigChange;
end;

destructor TTileDownloaderList.Destroy;
begin
  FTileDownloaderConfig.ChangeNotifier.Remove(FConfigListener);
  FDownloadSystemState.ChangeNotifier.Remove(FConfigListener);

  FConfigListener := nil;
  FTileDownloaderConfig := nil;
  FRequestBuilderFactory := nil;
  FDownloadSystemState := nil;

  FCS := nil;
  inherited;
end;

function TTileDownloaderList.CreateDownloader: ITileDownloader;
var
  VDownloader: IDownloader;
  VTileDownloadRequestBuilder: ITileDownloadRequestBuilder;
begin
  VDownloader :=
    TDownloaderHttpWithTTL.Create(
      FGCList,
      FResultFactory
    );
  VTileDownloadRequestBuilder :=
    TTileDownloadRequestBuilderLazy.Create(
      VDownloader,
      FRequestBuilderFactory
    );
  Result :=
    TTileDownloaderSimple.Create(
      FAppClosingNotifier,
      VTileDownloadRequestBuilder,
      FTileDownloaderConfig,
      VDownloader,
      FResultSaver,
      TLastResponseInfo.Create
    );
end;

function TTileDownloaderList.GetChangeNotifier: INotifier;
begin
  Result := FChangeNotifier;
end;

function TTileDownloaderList.GetStatic: ITileDownloaderListStatic;
begin
  FCS.BeginRead;
  try
    Result := FStatic;
  finally
    FCS.EndRead;
  end;
end;

procedure TTileDownloaderList.OnConfigChange;
var
  VStatic: ITileDownloaderListStatic;
  VList: array of ITileDownloader;
  VCount: Integer;
  VOldCount: Integer;
  VCountForCopy: Integer;
  i: Integer;
  VState: ITileDownloaderStateStatic;
  VCounter: Integer;
begin
  VCounter := InterlockedIncrement(FChangeCounter);
  VStatic := GetStatic;
  VCount := FTileDownloaderConfig.MaxConnectToServerCount;
  VState := FDownloadSystemState.GetStatic;
  if not VState.Enabled then begin
    VCount := 0;
  end;

  VOldCount := 0;
  if VStatic <> nil then begin
    VOldCount := VStatic.Count;
  end;

  if VOldCount <> VCount then begin
    SetLength(VList, VCount);
    if InterlockedCompareExchange(FChangeCounter, VCounter, VCounter) <> VCounter then begin
      Exit;
    end;
    VCountForCopy := VOldCount;
    if VCount < VCountForCopy then begin
      VCountForCopy := VCount;
    end;
    for i := 0 to VCountForCopy - 1 do begin
      VList[i] := VStatic.Item[i];
    end;
    for i := VCountForCopy to VCount - 1 do begin
      VList[i] := CreateDownloader;
      if InterlockedCompareExchange(FChangeCounter, VCounter, VCounter) <> VCounter then begin
        Exit;
      end;
    end;
    VStatic := TTileDownloaderListStatic.Create(VList);
    if InterlockedCompareExchange(FChangeCounter, VCounter, VCounter) <> VCounter then begin
      Exit;
    end;
    FCS.BeginWrite;
    try
      FStatic := VStatic;
    finally
      FCS.EndWrite;
    end;
    FChangeNotifier.Notify(nil);
  end else if VStatic = nil then begin
    SetLength(VList, 0);
    VStatic := TTileDownloaderListStatic.Create(VList);
    if InterlockedCompareExchange(FChangeCounter, VCounter, VCounter) <> VCounter then begin
      Exit;
    end;
    FCS.BeginWrite;
    try
      FStatic := VStatic;
    finally
      FCS.EndWrite;
    end;
  end;
end;

end.



