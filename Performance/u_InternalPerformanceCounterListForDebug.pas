unit u_InternalPerformanceCounterListForDebug;

interface

uses
  ActiveX,
  i_IDList,
  i_InterfaceListSimple,
  i_InternalPerformanceCounter,
  i_InternalPerformanceCounterListForDebug;

type
  TInternalPerformanceCounterListForDebug = class(TInterfacedObject, IInternalPerformanceCounterListForDebug)
  private
    FName: string;
    FFactory: IInternalPerformanceCounterFactory;
    FList: IIDInterfaceList;
  private
    function GetCounterByClass(AClass: TClass): IInternalPerformanceCounterListForDebugOneClass;
    procedure AddStaticDataToList(const AList: IInterfaceListSimple);
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

constructor TInternalPerformanceCounterListForDebug.Create(
  const AName: string;
  const AFactory: IInternalPerformanceCounterFactory
);
begin
  inherited Create;
  FName := AName;
  FFactory := AFactory;
  FList := TIDInterfaceList.Create(False, 4000);
end;

function TInternalPerformanceCounterListForDebug.GetCounterByClass(
  AClass: TClass): IInternalPerformanceCounterListForDebugOneClass;
var
  VId: Integer;
begin
  VId := Integer(AClass);
  Result := IInternalPerformanceCounterListForDebugOneClass(FList.GetByID(VId));
  if Result = nil then begin
    Result := TInternalPerformanceCounterListForDebugOneClass.Create(FName + '/' + AClass.ClassName, FFactory);
    FList.Add(VId, Result);
  end;
end;

procedure TInternalPerformanceCounterListForDebug.AddStaticDataToList(
  const AList: IInterfaceListSimple);
var
  VEnum: IEnumUnknown;
  VItem: IInternalPerformanceCounterListForDebugOneClass;
  VCnt: Integer;
begin
  VEnum := FList.GetEnumUnknown;
  while VEnum.Next(1, VItem, @VCnt) = S_OK do begin
    AList.Add(VItem.CounterCreate.GetStaticData);
    AList.Add(VItem.CounterDestroy.GetStaticData);
  end;
end;

end.
