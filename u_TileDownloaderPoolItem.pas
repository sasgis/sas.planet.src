unit u_TileDownloaderPoolItem;

interface

uses
  Windows,
  Classes,
  i_OperationNotifier,
  i_TTLCheckListener,
  i_TTLCheckNotifier,
  i_TileRequest,
  i_TileDownloaderAsync;

type
  ITileDownloaderPoolItem = interface
    ['{2A65C6A0-A7B6-4C6D-BB6A-ADEE1922BD04}']
    function TryExecRequest(
      ATileRequest: ITileRequest;
      ACancelNotifier: IOperationNotifier;
      AOperationID: Integer
    ): Boolean;
  end;

  TTileDownloaderPoolItem = class(TInterfacedObject, ITileDownloaderPoolItem)
  private
    FTileDownloaderSimple: ITileDownloader;
    FOnFinishEvent: TNotifyEvent;
    FGCList: ITTLCheckNotifier;

    FTileDownloaderAsync: ITileDownloader;
    FTTLListener: ITTLCheckListener;

    FUseCounter: Integer;
    procedure OnFinishRequest(Sender: TObject);
    procedure OnTTLTrim(Sender: TObject);
  protected
    function TryExecRequest(
      ATileRequest: ITileRequest;
      ACancelNotifier: IOperationNotifier;
      AOperationID: Integer
    ): Boolean;
  public
    constructor Create(
      AGCList: ITTLCheckNotifier;
      AOnFinishEvent: TNotifyEvent;
      ATileDownloaderSync: ITileDownloader
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_TTLCheckListener,
  u_TileDownloaderAsyncByThread;

{ TTileDownloaderPoolItem }

constructor TTileDownloaderPoolItem.Create(
  AGCList: ITTLCheckNotifier;
  AOnFinishEvent: TNotifyEvent;
  ATileDownloaderSync: ITileDownloader
);
begin
  FTileDownloaderSimple := ATileDownloaderSync;
  FGCList := AGCList;
  FOnFinishEvent := AOnFinishEvent;
  FUseCounter := 0;

  FTTLListener := TTTLCheckListener.Create(Self.OnTTLTrim, 60000, 1000);
  FGCList.Add(FTTLListener);
end;

destructor TTileDownloaderPoolItem.Destroy;
begin
  FGCList.Remove(FTTLListener);
  FTTLListener := nil;
  FGCList := nil;
  inherited;
end;

procedure TTileDownloaderPoolItem.OnFinishRequest(Sender: TObject);
begin
  InterlockedDecrement(FUseCounter);
  FOnFinishEvent(Sender);
end;

procedure TTileDownloaderPoolItem.OnTTLTrim(Sender: TObject);
begin
  FTileDownloaderAsync := nil;
end;

function TTileDownloaderPoolItem.TryExecRequest(ATileRequest: ITileRequest;
  ACancelNotifier: IOperationNotifier; AOperationID: Integer): Boolean;
var
  VUseCounter: Integer;
  VTileDownloaderAsync: ITileDownloader;
begin
  VUseCounter := InterlockedIncrement(FUseCounter);
  if VUseCounter = 1 then begin
    Result := True;
    FTTLListener.UpdateUseTime;
    VTileDownloaderAsync := FTileDownloaderAsync;
    if VTileDownloaderAsync = nil then begin
      VTileDownloaderAsync := TTileDownloaderAsyncByThread.Create(Self.OnFinishRequest, FTileDownloaderSimple);
      FTileDownloaderAsync := VTileDownloaderAsync;
    end;
    VTileDownloaderAsync.Download(ATileRequest, ACancelNotifier, AOperationID);
    FTTLListener.UpdateUseTime;
  end else begin
    InterlockedDecrement(FUseCounter);
    Result := False;
  end;
end;

end.
