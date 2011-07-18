unit u_PoolOfObjectsSimple;

interface

uses
  Windows,
  Classes,
  SyncObjs,
  i_JclNotify,
  i_OperationCancelNotifier,
  i_SimpleFactory,
  i_ObjectWithTTL,
  i_PoolElement,
  i_IPoolOfObjectsSimple;

type
  TPoolOfObjectsSimple = class(TInterfacedObject, IPoolOfObjectsSimple, IObjectWithTTL)
  private
    FList: TList;
    FObjectFactory: ISimpleFactory;
    FObjectTimeToLive: Cardinal;
    FCheckInterval: Cardinal;
    FOldestObjectTime: Cardinal;
    FLastCheckTime: Cardinal;
    FSemaphore: THandle;

    FCancelEvent: TEvent;
    FCancelListener: IJclListener;
    procedure OnDownloadCanceled(Sender: TObject);
  protected { IPoolOfObjectsSimple }
    function TryGetPoolElement(
      ACancelNotifier: IOperationCancelNotifier
    ): IPoolElement;
    function GetPoolSize: Cardinal;
  protected { IObjectWithTTL }
    function GetNextCheckTime: Cardinal;
    procedure TrimByTTL;
  public
    constructor Create(
      APoolSize: Cardinal;
      AObjectFactory: ISimpleFactory;
      AObjectTimeToLive: Cardinal;
      ACheckInterval: Cardinal
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_NotifyEventListener,
  u_PoolElement;

{ TPoolOfObjectsSimple }

constructor TPoolOfObjectsSimple.Create(APoolSize: Cardinal;
  AObjectFactory: ISimpleFactory; AObjectTimeToLive: Cardinal;
  ACheckInterval: Cardinal);
var
  i: integer;
begin
  FCancelEvent := TEvent.Create;
  FCancelListener := TNotifyEventListener.Create(Self.OnDownloadCanceled);

  FObjectFactory := AObjectFactory;
  FObjectTimeToLive := AObjectTimeToLive;
  FCheckInterval := ACheckInterval;
  FList := TList.Create;
  FList.Count := APoolSize;
  FSemaphore := CreateSemaphore(nil, FList.Count, FList.Count, '');
  for i := 0 to FList.Count - 1 do begin
    FList.Items[i] := TPoolElement.Create(FObjectFactory, FSemaphore);
  end;
  FOldestObjectTime := 0;
  FLastCheckTime := GetTickCount;
end;

destructor TPoolOfObjectsSimple.Destroy;
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do begin
    TPoolElement(FList.Items[i]).Free;
  end;
  FreeAndNil(FList);
  if FSemaphore <> 0 then begin
    CloseHandle(FSemaphore);
    FSemaphore := 0;
  end;
  FCancelEvent.SetEvent;
  FreeAndNil(FCancelEvent);
  inherited;
end;

function TPoolOfObjectsSimple.GetNextCheckTime: Cardinal;
begin
  if FOldestObjectTime <= 0 then begin
    Result := FLastCheckTime + FCheckInterval;
  end else begin
    Result := FOldestObjectTime + FObjectTimeToLive;
  end;
end;

function TPoolOfObjectsSimple.GetPoolSize: Cardinal;
begin
  Result := FList.Count;
end;

procedure TPoolOfObjectsSimple.OnDownloadCanceled(Sender: TObject);
begin
  FCancelEvent.SetEvent;
end;

procedure TPoolOfObjectsSimple.TrimByTTL;
var
  i: integer;
  VMinTime: Cardinal;
  VLastUse: Cardinal;
  VOldestUse: Cardinal;
  VElement: TPoolElement;
begin
  FLastCheckTime := GetTickCount;
  VMinTime := FLastCheckTime - FObjectTimeToLive;
  VOldestUse := 0;
  for i := 0 to FList.Count - 1 do begin
    VElement := TPoolElement(FList.Items[i]);
    VElement.FreeObjectByTTL(VMinTime);
    VLastUse := VElement.GetLastUseTime;
    if (VLastUse > 0) then begin
      if (VOldestUse <= 0) or ((VOldestUse > 0) and (VLastUse < VOldestUse)) then begin
        VOldestUse := VLastUse;
      end;
    end;
  end;
  FOldestObjectTime := VOldestUse;
end;

function TPoolOfObjectsSimple.TryGetPoolElement(
  ACancelNotifier: IOperationCancelNotifier
): IPoolElement;
var
  i: integer;
  VHandles: array [0..1] of THandle;
  VWaitResult: DWORD;
begin
  Result := nil;
  if ACancelNotifier <> nil then begin
    ACancelNotifier.AddListener(FCancelListener);
  end;
  try
    VHandles[0] := FSemaphore;
    VHandles[1] := FCancelEvent.Handle;
    while Result = nil do begin
      if (ACancelNotifier <> nil) and ACancelNotifier.Canceled then begin
        Break;
      end;
      VWaitResult := WaitForMultipleObjects(Length(VHandles), @VHandles[0], False, INFINITE);
      if VWaitResult = WAIT_OBJECT_0 then begin
        while Result = nil do begin
          if (ACancelNotifier <> nil) and ACancelNotifier.Canceled then begin
            Break;
          end;
          for i := 0 to FList.Count - 1 do begin
            Result := TPoolElement(FList.Items[i]).TryLock;
            if Result <> nil then begin
              Break;
            end;
          end;
        end;
      end;
    end;
  finally
    if ACancelNotifier <> nil then begin
      ACancelNotifier.RemoveListener(FCancelListener);
    end;
  end;
end;

end.
