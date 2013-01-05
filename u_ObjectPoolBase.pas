unit u_ObjectPoolBase;

interface

uses
  SysUtils,
  i_NotifierTime,
  i_ListenerTime,
  u_BaseInterfacedObject,
  u_ObjectFromPoolAbstract;

type
  IFreeObjectProcedure = interface
    function IsNeedDestroyObject(AObject: TObjectFromPoolAbstract): Boolean;
  end;

  IFreeObjectProcedureInternal = interface(IFreeObjectProcedure)
    procedure Disable;
  end;

  TObjectPoolBase = class(TBaseInterfacedObject)
  private
    FSync: IReadWriteSync;
    FTTLNotifier: INotifierTime;
    FMinStackSize: Integer;
    FMaxStackSize: Integer;

    FMeanStackSize: Integer;
    FFreeProcedure: IFreeObjectProcedureInternal;
    FTTLListener: IListenerTime;

    FStackHead: TObjectFromPoolAbstract;
    FStackSize: Integer;
    procedure _PushObject(AObject: TObjectFromPoolAbstract);
    function _PullObject: TObjectFromPoolAbstract;
    procedure OnTime;
  protected
    function PullOrCreateObject: TObjectFromPoolAbstract;
    function BuildNewObject(const AFreeProcedure: IFreeObjectProcedure): TObjectFromPoolAbstract; virtual; abstract;
  public
    constructor Create(
      const ATTLNotifier: INotifierTime;
      const ASync: IReadWriteSync;
      const AMinStackSize: Integer;
      const AMaxStackSize: Integer
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_ListenerTTLCheck;

type
  TFreeObjectProcedureInternal = class(TBaseInterfacedObject, IFreeObjectProcedure, IFreeObjectProcedureInternal)
  private
    FPool: TObjectPoolBase;
    FSync: IReadWriteSync;
  private
    function IsNeedDestroyObject(AObject: TObjectFromPoolAbstract): Boolean;
    procedure Disable;
  public
    constructor Create(
      APool: TObjectPoolBase;
      const ASync: IReadWriteSync
    );
  end;
{ TFreeObjectProcedureInternal }

constructor TFreeObjectProcedureInternal.Create(
  APool: TObjectPoolBase;
  const ASync: IReadWriteSync
);
begin
  Assert(APool <> nil);
  Assert(ASync <> nil);
  inherited Create;
  FPool := APool;
  FSync := ASync;
end;

procedure TFreeObjectProcedureInternal.Disable;
begin
  FSync.BeginWrite;
  try
    FPool := nil;
  finally
    FSync.EndWrite;
  end;
end;

function TFreeObjectProcedureInternal.IsNeedDestroyObject(
  AObject: TObjectFromPoolAbstract): Boolean;
begin
  FSync.BeginWrite;
  try
    if FPool = nil then begin
      Result := True;
    end else begin
      FPool._PushObject(AObject);
      Result := False;
    end;
  finally
    FSync.EndWrite;
  end;
end;

{ TObjectPoolBase }

constructor TObjectPoolBase.Create(
  const ATTLNotifier: INotifierTime;
  const ASync: IReadWriteSync;
  const AMinStackSize: Integer;
  const AMaxStackSize: Integer
);
begin
  Assert(ATTLNotifier <> nil);
  Assert(ASync <> nil);
  Assert(AMinStackSize >= 0);
  Assert(AMaxStackSize >= 0);
  Assert(AMinStackSize < AMaxStackSize);
  inherited Create;
  FTTLNotifier := ATTLNotifier;
  FSync := ASync;
  FMinStackSize := AMinStackSize;
  FMaxStackSize := AMaxStackSize;
  FMeanStackSize := (FMinStackSize + FMaxStackSize) div 2;
  FStackSize := 0;
  FStackHead := nil;
  FFreeProcedure := TFreeObjectProcedureInternal.Create(Self, FSync);
  FTTLListener := TListenerTimeCheck.Create(Self.OnTime, 30000);
  FTTLNotifier.Add(FTTLListener);
end;

destructor TObjectPoolBase.Destroy;
var
  VObject: TObjectFromPoolAbstract;
begin
  FFreeProcedure.Disable;

  if FTTLNotifier <> nil then begin
    FTTLNotifier.Remove(FTTLListener);
  end;

  FSync.BeginWrite;
  try
    while True do begin
      VObject := _PullObject;
      if VObject = nil then begin
        Break;
      end else begin
        VObject.Free;
      end;
    end;
  finally
    FSync.EndWrite;
  end;
  inherited;
end;

procedure TObjectPoolBase.OnTime;
var
  VDirection: Integer;
  VObject: TObjectFromPoolAbstract;
begin
  FSync.BeginWrite;
  try
    if FStackSize < FMinStackSize then begin
      VDirection := +1;
    end else if FStackSize > FMaxStackSize then begin
      VDirection := -1;
    end else begin
      VDirection := 0;
    end;
  finally
    FSync.EndWrite;
  end;
  if VDirection = 0 then begin
    Exit;
  end else if VDirection > 0 then begin
    while VDirection > 0 do begin
      VObject := BuildNewObject(FFreeProcedure);
      FSync.BeginWrite;
      try
        _PushObject(VObject);
        if FStackSize > FMeanStackSize then begin
          VDirection := 0;
        end;
      finally
        FSync.EndWrite;
      end;
    end;
  end else begin
    while VDirection < 0 do begin
      FSync.BeginWrite;
      try
        VObject := _PullObject;
        if FStackSize < FMeanStackSize then begin
          VDirection := 0;
        end;
        VObject.Free;
      finally
        FSync.EndWrite;
      end;
    end;
  end;
end;

function TObjectPoolBase.PullOrCreateObject: TObjectFromPoolAbstract;
begin
  FSync.BeginWrite;
  try
    Result := _PullObject;
  finally
    FSync.EndWrite;
  end;
  if Result = nil then begin
    Result := BuildNewObject(FFreeProcedure);
  end;
end;

function TObjectPoolBase._PullObject: TObjectFromPoolAbstract;
begin
  Result := FStackHead;
  if Result <> nil then begin
    Dec(FStackSize);
    FStackHead := Result.NextFree;
    Result.NextFree := nil;
  end;
end;

procedure TObjectPoolBase._PushObject(AObject: TObjectFromPoolAbstract);
begin
  AObject.NextFree := FStackHead;
  FStackHead := AObject;
  Inc(FStackSize);
end;

end.
