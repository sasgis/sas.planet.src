unit u_PoolOfObjectsSimple;

interface

uses
  Windows,
  Classes,
  i_ISimpleFactory,
  i_IObjectWithTTL,
  i_IPoolElement,
  i_IPoolOfObjectsSimple;

type
  TPoolOfObjectsSimple = class(TInterfacedObject, IPoolOfObjectsSimple, IObjectWithTTL)
  private
    FList: TList;
    FObjectFactory: ISimpleFactory;
    FObjectTimeToLive: TDateTime;
    FCheckInterval: TDateTime;
    FOldestObjectTime: TDateTime;
    FLastCheckTime: TDateTime;
    FWaitSleep: Cardinal;
  public
    constructor Create(APoolSize: Cardinal; AObjectFactory: ISimpleFactory;
    AObjectTimeToLive: TDateTime; ACheckInterval: TDateTime);
    destructor Destroy; override;
    function TryGetPoolElement(ATimeOut: Cardinal): IPoolElement;
    function GetPoolSize: Cardinal;
    function GetNextCheckTime: TDateTime;
    procedure TrimByTTL;
  end;

implementation

uses
  SysUtils,
  u_PoolElement;

{ TPoolOfObjectsSimple }

constructor TPoolOfObjectsSimple.Create(APoolSize: Cardinal;
  AObjectFactory: ISimpleFactory; AObjectTimeToLive: TDateTime;
  ACheckInterval: TDateTime);
var
  i: integer;
begin
  FObjectFactory := AObjectFactory;
  FObjectTimeToLive := AObjectTimeToLive;
  FCheckInterval := ACheckInterval;
  FList := TList.Create;
  FList.Count := APoolSize;
  for i := 0 to FList.Count - 1 do begin
    FList.Items[i] := TPoolElement.Create(FObjectFactory);
  end;
  FOldestObjectTime := -1;
  FLastCheckTime := Now;
end;

destructor TPoolOfObjectsSimple.Destroy;
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do  begin
    TPoolElement(FList.Items[i]).Free;
  end;
  FreeAndNil(FList);
  inherited;
end;

function TPoolOfObjectsSimple.GetNextCheckTime: TDateTime;
begin
  if FOldestObjectTime < 0 then begin
    Result := FLastCheckTime + FCheckInterval;
  end else begin
    Result := FOldestObjectTime + FObjectTimeToLive;
  end;
end;

function TPoolOfObjectsSimple.GetPoolSize: Cardinal;
begin
  Result := FList.Count;
end;

procedure TPoolOfObjectsSimple.TrimByTTL;
var
  i: integer;
  VMinTime: TDateTime;
  VLastUse: TDateTime;
  VOldestUse: TDateTime;
  VElement: TPoolElement;
begin
  VMinTime := Now - FObjectTimeToLive;
  VOldestUse := -1;
  for i := 0 to FList.Count - 1 do  begin
    VElement := TPoolElement(FList.Items[i]);
    VElement.FreeObjectByTTL(VMinTime);
    VLastUse := VElement.GetLastUseTime;
    if (VLastUse > 0) then begin
      if (VOldestUse < 0) or ((VOldestUse > 0) and (VLastUse < VOldestUse)) then begin
        VOldestUse := VLastUse;
      end;
    end;
  end;
  FOldestObjectTime := VOldestUse;
end;

function TPoolOfObjectsSimple.TryGetPoolElement(
  ATimeOut: Cardinal): IPoolElement;
var
  i: integer;
  VStepsCount: Cardinal;
  VStep: Cardinal;
begin
  Result := nil;
  for i := 0 to FList.Count - 1 do begin
    Result := TPoolElement(FList.Items[i]).TryLock;
    if Result <> nil then begin
      Break;
    end;
  end;
  if Result <> nil then begin
    VStepsCount := ATimeOut div FWaitSleep;
    VStep := 0;
    while (Result = nil) and (VStep < VStepsCount) do begin
      Sleep(FWaitSleep);
      for i := 0 to FList.Count - 1 do begin
        Result := TPoolElement(FList.Items[i]).TryLock;
        if Result <> nil then begin
          Break;
        end;
      end;
      Inc(VStep);
    end;
  end;
end;

end.
