unit u_TileDownloaderEventProcessor;

interface

uses
  Windows,
  Classes,
  SysUtils,
  SyncObjs,
  i_TileDownlodSession,
  u_TileDownloaderEventElement,
  u_MapLayerShowError,
  u_MapType;

type
  TTileDownloaderEventProcessor = class (TTHread)
  private
    FMapTileUpdateEvent: TMapTileUpdateEvent;
    FErrorShowLayer: TTileErrorInfoLayer;
    FEventList: TList;
    FThreadSafe: TCriticalSection;
    FSemaphore: THandle;
    procedure Lock;
    procedure Unlock;
    procedure ProcessEventList;
  protected
    procedure Execute; override;
  public
    constructor Create(AMapTileUpdateEvent: TMapTileUpdateEvent; AErrorShowLayer: TTileErrorInfoLayer);
    destructor Destroy; override;
    procedure AddEvent(AMapType: TMapType; ATile: TPoint; AZoom: Byte; ATileSize: Int64; AResult: TDownloadTileResult);
  end;

implementation

{ TTileDownloaderEventProcessor }

constructor TTileDownloaderEventProcessor.Create(AMapTileUpdateEvent: TMapTileUpdateEvent; AErrorShowLayer: TTileErrorInfoLayer);
begin
  inherited Create(True);
  FSemaphore := CreateSemaphore(nil, 0, 1, nil);
  FMapTileUpdateEvent := AMapTileUpdateEvent;
  FErrorShowLayer := AErrorShowLayer;
  FThreadSafe := TCriticalSection.Create;
  FEventList := TList.Create;
  Self.FreeOnTerminate := True;
  Self.Resume;
end;

destructor TTileDownloaderEventProcessor.Destroy;
var
  I: Integer;
  VEvent: TTileDownloaderEventElement;
begin
    Lock;
  try
    for I := 0 to FEventList.Count - 1 do
    try
      VEvent := TTileDownloaderEventElement(FEventList.Items[I]);
      if Assigned(VEvent) then
        FreeAndNil(VEvent);
    except
    end;
    FEventList.Clear;
    FreeAndNil(FEventList);
  finally
    Unlock;
  end;
  CloseHandle(FSemaphore);
  FThreadSafe.Free;
  inherited Destroy;
end;

procedure TTileDownloaderEventProcessor.Lock;
begin
  FThreadSafe.Acquire;
end;

procedure TTileDownloaderEventProcessor.Unlock;
begin
  FThreadSafe.Release;
end;

procedure TTileDownloaderEventProcessor.AddEvent(AMapType: TMapType; ATile: TPoint; AZoom: Byte; ATileSize: Int64; AResult: TDownloadTileResult);
var
  VEvent: TTileDownloaderEventElement;
begin
  VEvent := TTileDownloaderEventElement.Create(FMapTileUpdateEvent, FErrorShowLayer);
  VEvent.MapType := AMapType;
  VEvent.TileXY := ATile;
  VEvent.TileZoom := AZoom;
  VEvent.TileSize := ATileSize;
  VEvent.DownloadResult := AResult;
  Lock;
  try
    FEventList.Add(VEvent);
  finally
    Unlock;
  end;
  ReleaseSemaphore(FSemaphore, 1, nil);
end;

procedure TTileDownloaderEventProcessor.ProcessEventList;
var
  I: Integer;
  VEvent: TTileDownloaderEventElement;
begin
  try
    for I := 0 to FEventList.Count - 1 do
    try
      VEvent := TTileDownloaderEventElement(FEventList.Items[I]);
      try
        if Assigned(VEvent) then
          VEvent.Process;
      except
      end;
    finally
      FreeAndNil(VEvent);
    end;
  finally
    FEventList.Clear;
  end;
end;

procedure TTileDownloaderEventProcessor.Execute;
begin
  repeat
    if Terminated then
      Break;
    if WaitForSingleObject(FSemaphore, 300) = WAIT_OBJECT_0 then
    begin
        Lock;
      try
        Synchronize(ProcessEventList);
      finally
        Unlock;
      end;
    end;
  until False;
end;

end.
