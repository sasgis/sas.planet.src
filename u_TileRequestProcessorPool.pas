unit u_TileRequestProcessorPool;

interface

uses
  SyncObjs,
  Classes,
  i_JclNotify,
  i_Thread,
  i_TTLCheckListener,
  i_TTLCheckNotifier,
  i_TileRequestQueue,
  i_TileDownloaderList,
  i_ITileRequestProcessorPool;

type
  TTileRequestProcessorPool = class(TInterfacedObject, ITileRequestProcessorPool)
  private type
    TArrayOfThread = array of IThread;
  private
    FDownloaderList: ITileDownloaderList;
    FGCList: ITTLCheckNotifier;
    FPriority: TThreadPriority;
    FAppClosingNotifier: IJclNotifier;
    FTileRequestQueue: ITileRequestQueue;

    FTTLListener: ITTLCheckListener;
    FDownloadersListListener: IJclListener;
    FCS: TCriticalSection;

    FThreadArray: TArrayOfThread;
    FThreadArrayCS: TCriticalSection;

    procedure OnTTLTrim(Sender: TObject);
    procedure OnDownloadersListChange;
  protected
    procedure InitThreadsIfNeed;
  public
    constructor Create(
      AGCList: ITTLCheckNotifier;
      APriority: TThreadPriority;
      AAppClosingNotifier: IJclNotifier;
      ATileRequestQueue: ITileRequestQueue;
      ADownloaderList: ITileDownloaderList
    );
    destructor Destroy; override;
  end;
implementation

uses
  SysUtils,
  i_TileDownloader,
  u_NotifyEventListener,
  u_TTLCheckListener,
  u_TileRequestQueueProcessorThread;

{ TTileRequestProcessorPool }

constructor TTileRequestProcessorPool.Create(
  AGCList: ITTLCheckNotifier;
  APriority: TThreadPriority;
  AAppClosingNotifier: IJclNotifier;
  ATileRequestQueue: ITileRequestQueue;
  ADownloaderList: ITileDownloaderList
);
begin
  FGCList := AGCList;
  FPriority := APriority;
  FAppClosingNotifier := AAppClosingNotifier;
  FTileRequestQueue := ATileRequestQueue;

  FCS := TCriticalSection.Create;
  FThreadArrayCS := TCriticalSection.Create;

  FDownloaderList := ADownloaderList;

  FDownloadersListListener := TNotifyNoMmgEventListener.Create(Self.OnDownloadersListChange);
  FDownloaderList.ChangeNotifier.Add(FDownloadersListListener);

  FTTLListener := TTTLCheckListener.Create(Self.OnTTLTrim, 60000, 1000);
  FGCList.Add(FTTLListener);

  OnDownloadersListChange;
end;

destructor TTileRequestProcessorPool.Destroy;
begin
  OnTTLTrim(nil);
  FDownloaderList.ChangeNotifier.Remove(FDownloadersListListener);
  FGCList.Remove(FTTLListener);
  FTTLListener := nil;
  FGCList := nil;
  FDownloadersListListener := nil;
  FDownloaderList := nil;

  FreeAndNil(FCS);
  FreeAndNil(FThreadArrayCS);
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
  FThreadArrayCS.Acquire;
  try
    VThreadArray := FThreadArray;
  finally
    FThreadArrayCS.Release;
  end;
  if VThreadArray = nil then begin
    FCS.Acquire;
    try
      FThreadArrayCS.Acquire;
      try
        VThreadArray := FThreadArray;
      finally
        FThreadArrayCS.Release;
      end;
      if VThreadArray = nil then begin
        VDownloaderList := FDownloaderList.GetStatic;
        if VDownloaderList <> nil then begin
          SetLength(VThreadArray, VDownloaderList.Count);
          for i := 0 to VDownloaderList.Count - 1 do begin
            VTileDownloaderSync := VDownloaderList.Item[i];
            if VTileDownloaderSync <> nil then begin
              VThreadArray[i] :=
                TTileRequestQueueProcessorThread.Create(
                  FPriority,
                  FAppClosingNotifier,
                  FTileRequestQueue,
                  VTileDownloaderSync
                );
              VThreadArray[i].Start;
            end else begin
              VThreadArray[i] := nil;
            end;
          end;
          FThreadArrayCS.Acquire;
          try
            FThreadArray := VThreadArray;
          finally
            FThreadArrayCS.Release;
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
  OnTTLTrim(nil);
end;

procedure TTileRequestProcessorPool.OnTTLTrim(Sender: TObject);
var
  VThreadArray: TArrayOfThread;
  i: Integer;
  VItem: IThread;
begin
  FThreadArrayCS.Acquire;
  try
    VThreadArray := FThreadArray;
    FThreadArray := nil;
  finally
    FThreadArrayCS.Release
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
