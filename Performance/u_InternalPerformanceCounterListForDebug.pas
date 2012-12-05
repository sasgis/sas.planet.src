unit u_InternalPerformanceCounterListForDebug;

interface

uses
  ActiveX,
  i_IDList,
  i_InternalPerformanceCounter,
  i_InternalPerformanceCounterListForDebug;

type
  TInternalPerformanceCounterListForDebug = class(TInterfacedObject, IInternalPerformanceCounterList, IInternalPerformanceCounterListForDebug)
  private
    FName: string;
    FFactory: IInternalPerformanceCounterFactory;
    FList: IIDInterfaceList;
  private
    function GetName: string;

    function GetStaticDataList: IIDInterfaceList;
    procedure AppendStaticDataToList(const ADataList: IIDInterfaceList);
    function GetEunm: IEnumUnknown;
    function CreateAndAddNewCounter(const AName: string): IInternalPerformanceCounter;
    function CreateAndAddNewSubList(const AName: string): IInternalPerformanceCounterList;
    procedure AddSubList(const ASubList: IInternalPerformanceCounterList);
  private
    function GetCounterByClass(AClass: TClass): IInternalPerformanceCounterListForDebugOneClass;
  public
    constructor Create(
      const AName: string;
      const AFactory: IInternalPerformanceCounterFactory
    );
  end;

implementation

uses
  u_IDInterfaceList,
  u_InternalPerformanceCounterListForDebugOneClass;

{ TInternalPerformanceCounterListForDebug }

constructor TInternalPerformanceCounterListForDebug.Create(const AName: string;
  const AFactory: IInternalPerformanceCounterFactory);
begin
  inherited Create;
  FName := AName;
  FFactory := AFactory;
  FList := TIDInterfaceList.Create(False);
end;

procedure TInternalPerformanceCounterListForDebug.AddSubList(
  const ASubList: IInternalPerformanceCounterList);
begin
  Assert(False);
end;

function TInternalPerformanceCounterListForDebug.CreateAndAddNewCounter(
  const AName: string): IInternalPerformanceCounter;
begin
  Assert(False);
  Result := nil;
end;

function TInternalPerformanceCounterListForDebug.CreateAndAddNewSubList(
  const AName: string): IInternalPerformanceCounterList;
begin
  Assert(False);
  Result := nil;
end;

function TInternalPerformanceCounterListForDebug.GetCounterByClass(
  AClass: TClass): IInternalPerformanceCounterListForDebugOneClass;
var
  VId: Integer;
begin
  VId := Integer(AClass);
  Result := IInternalPerformanceCounterListForDebugOneClass(FList.GetByID(VId));
  if Result = nil then begin
    Result := TInternalPerformanceCounterListForDebugOneClass.Create(AClass, FFactory);
    FList.Add(VId, Result);
  end;
end;

function TInternalPerformanceCounterListForDebug.GetEunm: IEnumUnknown;
begin
  Result := FList.GetEnumUnknown;
end;

function TInternalPerformanceCounterListForDebug.GetName: string;
begin
  Result := FName;
end;

procedure TInternalPerformanceCounterListForDebug.AppendStaticDataToList(
  const ADataList: IIDInterfaceList);
var
  VEnum: IEnumUnknown;
  VItem: IInternalPerformanceCounterListForDebugOneClass;
  VCnt: Integer;
begin
  VEnum := FList.GetEnumUnknown;
  while VEnum.Next(1, VItem, @VCnt) = S_OK do begin
    ADataList.Add(VItem.CounterCreate.Id, VItem.CounterCreate.GetStaticData);
    ADataList.Add(VItem.CounterDestroy.Id, VItem.CounterDestroy.GetStaticData);
  end;
end;

function TInternalPerformanceCounterListForDebug.GetStaticDataList: IIDInterfaceList;
begin
  Result := TIDInterfaceList.Create;
  AppendStaticDataToList(Result);
end;

end.
