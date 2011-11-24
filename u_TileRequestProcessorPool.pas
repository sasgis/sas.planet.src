unit u_TileRequestProcessorPool;

interface

uses
  SyncObjs,
  i_JclNotify,
  i_Thread,
  i_TTLCheckListener,
  i_TTLCheckNotifier,
  i_TileRequestQueue,
  i_TileDownloaderList,
  i_TileDownloaderAsync,
  i_ITileRequestProcessorPool;

type
  TTileRequestProcessorPool = class(TInterfacedObject, ITileRequestProcessorPool)
  private type
    TArrayOfThread = array of IThread;
  private
    FDownloaderList: ITileDownloaderList;
    FGCList: ITTLCheckNotifier;
    FTileRequestQueue: ITileRequestQueue;

    FDownloaderListStatic: ITileDownloaderListStatic;
    FThreadArray: TArrayOfThread;
    FTTLListener: ITTLCheckListener;
    FDownloadersListListener: IJclListener;
    FCS: TCriticalSection;

    procedure OnTTLTrim(Sender: TObject);
    procedure OnDownloadersListChange;
  protected
    procedure InitThreadsIfNeed;
  public
    constructor Create(
      AGCList: ITTLCheckNotifier;
      ATileRequestQueue: ITileRequestQueue;
      ADownloaderList: ITileDownloaderList
    );
    destructor Destroy; override;
  end;
implementation

uses
  SysUtils,
  u_NotifyEventListener,
  u_TTLCheckListener,
  u_TileRequestQueueProcessorThread;

{ TTileRequestProcessorPool }

constructor TTileRequestProcessorPool.Create(
  AGCList: ITTLCheckNotifier;
  ATileRequestQueue: ITileRequestQueue;
  ADownloaderList: ITileDownloaderList
);
begin
  FGCList := AGCList;
  FTileRequestQueue := ATileRequestQueue;

  FCS := TCriticalSection.Create;

  FDownloaderList := ADownloaderList;

  FDownloadersListListener := TNotifyNoMmgEventListener.Create(Self.OnDownloadersListChange);
  FDownloaderList.ChangeNotifier.Add(FDownloadersListListener);

  FTTLListener := TTTLCheckListener.Create(Self.OnTTLTrim, 60000, 1000);
  FGCList.Add(FTTLListener);

  OnDownloadersListChange;
end;

destructor TTileRequestProcessorPool.Destroy;
begin
  FDownloaderList.ChangeNotifier.Remove(FDownloadersListListener);
  FGCList.Remove(FTTLListener);
  FTTLListener := nil;
  FGCList := nil;
  FDownloadersListListener := nil;
  FDownloaderList := nil;
  FDownloaderListStatic := nil;

  FreeAndNil(FCS);
  inherited;
end;

procedure TTileRequestProcessorPool.InitThreadsIfNeed;
var
  VThreadArray: TArrayOfThread;
  VDownloaderList: ITileDownloaderListStatic;
  i: Integer;
begin
  FTTLListener.UpdateUseTime;
  if FThreadArray = nil then begin
    FCS.Acquire;
    try
      if FThreadArray = nil then begin
        VDownloaderList := FDownloaderListStatic;
        if VDownloaderList <> nil then begin
          SetLength(VThreadArray, VDownloaderList.Count);
          for i := 0 to VDownloaderList.Count - 1 do begin
            VThreadArray[i] := TTileRequestQueueProcessorThread.Create(FTileRequestQueue, VDownloaderList.Item[i]);
            VThreadArray[i].Start;
          end;
        end;
      end;
    finally
      FCS.Release;
    end;
  end;
end;

procedure TTileRequestProcessorPool.OnDownloadersListChange;
begin
  FDownloaderListStatic := FDownloaderList.GetStatic;
  OnTTLTrim(nil);
end;

procedure TTileRequestProcessorPool.OnTTLTrim(Sender: TObject);
var
  VThreadArray: TArrayOfThread;
  i: Integer;
begin
  if FThreadArray <> nil then begin
    FCS.Acquire;
    try
      VThreadArray := FThreadArray;
      FThreadArray := nil;
    finally
      FCS.Release;
    end;
  end;
  if VThreadArray <> nil then begin
    for i := 0 to Length(VThreadArray) - 1 do begin
      VThreadArray[i].Terminate;
      VThreadArray[i] := nil;
    end;
    VThreadArray := nil;
  end;
end;

end.
