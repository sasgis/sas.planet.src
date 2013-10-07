unit u_DebugInfoSubSystem;

interface

uses
  SysUtils,
  i_InternalPerformanceCounter,
  i_DebugInfoSubSystem,
  i_InterfaceListStatic,
  i_InterfaceListSimple,
  i_InternalDebugConfig;

type
  TDebugInfoSubSystem = class(TInterfacedObject, IDebugInfoSubSystem)
  private
    FRootCounterList: IInternalPerformanceCounterList;
    FListCS: IReadWriteSync;
    FList: IInterfaceListSimple;
  private
    function GetRootCounterList: IInternalPerformanceCounterList;
    function GetStaticDataList: IInterfaceListStatic;
  public
    constructor Create(const AConfig: IInternalDebugConfig);
  end;

implementation

uses
  u_BaseInterfacedObject,
  u_BaseInterfacedObjectDebug,
  u_InternalPerformanceCounter,
  u_InternalPerformanceCounterFake,
  u_InternalPerformanceCounterList,
  u_InterfaceListSimple,
  u_Synchronizer;

{ TDebugInfoSubSystem }

constructor TDebugInfoSubSystem.Create(const AConfig: IInternalDebugConfig);
begin
  inherited Create;
  FListCS := MakeSyncRW_Var(Self, False);
  if AConfig.IsShowDebugInfo then begin
    FList := TInterfaceListSimple.Create;
    FList.Capacity := 1000;
    FRootCounterList := TInternalPerformanceCounterList.Create('', FListCS, FList, TInternalPerformanceCounterFactory.Create);
  end else begin
    FRootCounterList := TInternalPerformanceCounterFake.Create;
  end;
end;

function TDebugInfoSubSystem.GetRootCounterList: IInternalPerformanceCounterList;
begin
  Result := FRootCounterList;
end;

function TDebugInfoSubSystem.GetStaticDataList: IInterfaceListStatic;
var
  VList: IInterfaceListSimple;
  i: Integer;
  VItem: IInternalPerformanceCounter;
begin
  Result := nil;
  if Assigned(FList) then begin
    VList := TInterfaceListSimple.Create;
    VList.Capacity := FList.Count;
    FListCS.BeginRead;
    try
      for i := 0 to FList.Count - 1 do begin
        VItem := IInternalPerformanceCounter(FList[i]);
        VList.Add(VItem.GetStaticData);
      end;
    finally
      FListCS.EndRead;
    end;
    if TBaseInterfacedObject = TBaseInterfacedObjectDebug then begin
      TBaseInterfacedObjectDebug.AddStaticDataToList(VList);
    end;
    Result := VList.MakeStaticAndClear;
  end;
end;

end.
