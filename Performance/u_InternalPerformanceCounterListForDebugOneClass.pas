unit u_InternalPerformanceCounterListForDebugOneClass;

interface

uses
  ActiveX,
  i_IDList,
  i_InternalPerformanceCounter,
  i_InternalPerformanceCounterListForDebug;

type
  TInternalPerformanceCounterListForDebugOneClass = class(TInterfacedObject, IInternalPerformanceCounterList, IInternalPerformanceCounterListForDebugOneClass)
  private
    FClass: TClass;
    FCounterCreate: IInternalPerformanceCounter;
    FCounterDestroy: IInternalPerformanceCounter;
  private
    function GetName: string;

    function GetStaticDataList: IIDInterfaceList;
    procedure AppendStaticDataToList(const ADataList: IIDInterfaceList);
    function GetEunm: IEnumUnknown;
    function CreateAndAddNewCounter(const AName: string): IInternalPerformanceCounter;
    function CreateAndAddNewSubList(const AName: string): IInternalPerformanceCounterList;
    procedure AddSubList(const ASubList: IInternalPerformanceCounterList);
  private
    function GetCounterCreate: IInternalPerformanceCounter;
    function GetCounterDestroy: IInternalPerformanceCounter;
  public
    constructor Create(
      AClass: TClass;
      const AFactory: IInternalPerformanceCounterFactory
    );
  end;

implementation

uses
  u_EnumUnknownTwoItems,
  u_IDInterfaceList;

{ TInternalPerformanceCounterListForDebugOneClass }

constructor TInternalPerformanceCounterListForDebugOneClass.Create(
  AClass: TClass;
  const AFactory: IInternalPerformanceCounterFactory
);
begin
  inherited Create;
  FClass := AClass;
  FCounterCreate := AFactory.Build('Create');
  FCounterDestroy := AFactory.Build('Destroy');
end;

procedure TInternalPerformanceCounterListForDebugOneClass.AddSubList(
  const ASubList: IInternalPerformanceCounterList);
begin
  Assert(False);
end;

function TInternalPerformanceCounterListForDebugOneClass.CreateAndAddNewCounter(
  const AName: string): IInternalPerformanceCounter;
begin
  Assert(False);
end;

function TInternalPerformanceCounterListForDebugOneClass.CreateAndAddNewSubList(
  const AName: string): IInternalPerformanceCounterList;
begin
  Assert(False);
end;

function TInternalPerformanceCounterListForDebugOneClass.GetCounterCreate: IInternalPerformanceCounter;
begin
  Result := FCounterCreate;
end;

function TInternalPerformanceCounterListForDebugOneClass.GetCounterDestroy: IInternalPerformanceCounter;
begin
  Result := FCounterDestroy;
end;

function TInternalPerformanceCounterListForDebugOneClass.GetEunm: IEnumUnknown;
begin
  Result := TEnumUnknownTwoItems.Create(FCounterCreate, FCounterDestroy);
end;

function TInternalPerformanceCounterListForDebugOneClass.GetName: string;
begin
  Result := FClass.ClassName;
end;

procedure TInternalPerformanceCounterListForDebugOneClass.AppendStaticDataToList(
  const ADataList: IIDInterfaceList);
begin
  ADataList.Add(FCounterCreate.Id, FCounterCreate.GetStaticData);
  ADataList.Add(FCounterDestroy.Id, FCounterDestroy.GetStaticData);
end;

function TInternalPerformanceCounterListForDebugOneClass.GetStaticDataList: IIDInterfaceList;
begin
  Result := TIDInterfaceList.Create;
  AppendStaticDataToList(Result);
end;

end.
