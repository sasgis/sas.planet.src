unit u_TileRequestProcessorPool;

interface

uses
  SysUtils,
  i_Notifier,
  i_NotifierOperation,
  i_Listener,
  i_Thread,
  i_ThreadConfig,
  i_ListenerTime,
  i_NotifierTime,
  i_InterfaceQueue,
  i_TileDownloaderList,
  i_TileRequestProcessorPool,
  u_BaseInterfacedObject;

type
  TTileRequestProcessorPool = class(TBaseInterfacedObject, ITileRequestProcessorPool)
  private
    type
    TArrayOfThread = array of IThread;
  private
    FThreadConfig: IThreadConfig;
    FDownloaderList: ITileDownloaderList;
    FGCNotifier: INotifierTime;
    FAppClosingNotifier: INotifierOneOperation;
    FTileRequestQueue: IInterfaceQueue;

    FTTLListener: IListenerTimeWithUsedFlag;
    FDownloadersListListener: IListener;

    FThreadArray: TArrayOfThread;
    FThreadArrayCS: IReadWriteSync;

    procedure OnTTLTrim;
    procedure OnDownloadersListChange;
  private
    procedure InitThreadsIfNeed;
  public
    constructor Create(
      const AGCNotifier: INotifierTime;
      const AThreadConfig: IThreadConfig;
      const AAppClosingNotifier: INotifierOneOperation;
      const ATileRequestQueue: IInterfaceQueue;
      const ADownloaderList: ITileDownloaderList
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_Synchronizer,
  i_TileDownloader,
  u_ListenerByEvent,
  u_ListenerTime,
  u_TileRequestQueueProcessorThread;

{ TTileRequestProcessorPool }

constructor TTileRequestProcessorPool.Create(
  const AGCNotifier: INotifierTime;
  const AThreadConfig: IThreadConfig;
  const AAppClosingNotifier: INotifierOneOperation;
  const ATileRequestQueue: IInterfaceQueue;
  const ADownloaderList: ITileDownloaderList
);
begin
  inherited Create;
  FGCNotifier := AGCNotifier;
  FThreadConfig := AThreadConfig;
  FAppClosingNotifier := AAppClosingNotifier;
  FTileRequestQueue := ATileRequestQueue;

  FThreadArrayCS := MakeSyncRW_Big(Self);

  FDownloaderList := ADownloaderList;

  FDownloadersListListener := TNotifyNoMmgEventListener.Create(Self.OnDownloadersListChange);
  FDownloaderList.ChangeNotifier.Add(FDownloadersListListener);

  FTTLListener := TListenerTTLCheck.Create(Self.OnTTLTrim, 60000);
  FGCNotifier.Add(FTTLListener);

  OnDownloadersListChange;
end;

destructor TTileRequestProcessorPool.Destroy;
begin
  OnTTLTrim;
  FDownloaderList.ChangeNotifier.Remove(FDownloadersListListener);
  FGCNotifier.Remove(FTTLListener);
  FTTLListener := nil;
  FGCNotifier := nil;
  FDownloadersListListener := nil;
  FDownloaderList := nil;

  FThreadArrayCS := nil;
  inherited;
end;

procedure TTileRequestProcessorPool.InitThreadsIfNeed;
var
  VThreadArray: TArrayOfThread;
  VDownloaderList: ITileDownloaderListStatic;
  i: Integer;
  VTileDownloaderSync: ITileDownloader;
begin
  FTTLListener.UpdateUseTime;

  FThreadArrayCS.BeginRead;
  try
    VThreadArray := FThreadArray;
  finally
    FThreadArrayCS.EndRead;
  end;

  if VThreadArray = nil then begin
    FThreadArrayCS.BeginWrite;
    try
      VThreadArray := FThreadArray;

      if VThreadArray = nil then begin
        VDownloaderList := FDownloaderList.GetStatic;
        if VDownloaderList <> nil then begin
          SetLength(VThreadArray, VDownloaderList.Count);
          for i := 0 to VDownloaderList.Count - 1 do begin
            VTileDownloaderSync := VDownloaderList.Item[i];
            if VTileDownloaderSync <> nil then begin
              VThreadArray[i] :=
                TTileRequestQueueProcessorThread.Create(
                  FThreadConfig,
                  FAppClosingNotifier,
                  FTileRequestQueue,
                  VTileDownloaderSync
                );
              VThreadArray[i].Start;
            end else begin
              VThreadArray[i] := nil;
            end;
          end;

          FThreadArray := VThreadArray;
        end;
      end;
    finally
      FThreadArrayCS.EndWrite;
    end;
  end;
end;

procedure TTileRequestProcessorPool.OnDownloadersListChange;
begin
  OnTTLTrim;
end;

procedure TTileRequestProcessorPool.OnTTLTrim;
var
  VThreadArray: TArrayOfThread;
  i: Integer;
  VItem: IThread;
begin
  FThreadArrayCS.BeginWrite;
  try
    VThreadArray := FThreadArray;
    FThreadArray := nil;
  finally
    FThreadArrayCS.EndWrite;
  end;

  if VThreadArray <> nil then begin
    for i := 0 to Length(VThreadArray) - 1 do begin
      VItem := VThreadArray[i];
      VThreadArray[i] := nil;
      if VItem <> nil then begin
        VItem.Terminate;
        VItem := nil;
      end;
    end;
    VThreadArray := nil;
  end;
end;

end.
