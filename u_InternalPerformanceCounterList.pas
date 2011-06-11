unit u_InternalPerformanceCounterList;

interface

uses
  Windows,
  Classes,
  ActiveX,
  i_IDList,
  i_InternalPerformanceCounter;

type
  TInternalPerformanceCounterList = class(TInterfacedObject, IInternalPerformanceCounterList)
  private
    FList: IInterfaceList;
    FName: string;
    procedure AppendStaticListByCounterList(AResultList: IIDInterfaceList; ACounterList: IInternalPerformanceCounterList);
  protected
    function GetName: string;

    function GetStaticDataList: IIDInterfaceList;
    function GetEunm: IEnumUnknown;
    function CreateAndAddNewCounter(AName: string): IInternalPerformanceCounter;
    function CreateAndAddNewSubList(AName: string): IInternalPerformanceCounterList;
  public
    constructor Create(AName: string);
  end;

implementation

uses
  SysUtils,
  u_EnumUnknown,
  u_IDInterfaceList,
  u_InternalPerformanceCounter;

{ TInternalPerformanceCounterList }

procedure TInternalPerformanceCounterList.AppendStaticListByCounterList(
  AResultList: IIDInterfaceList; ACounterList: IInternalPerformanceCounterList);
var
  VEnum: IEnumUnknown;
  Vcnt: Integer;
  VUnknown: IInterface;
  VCounter: IInternalPerformanceCounter;
  VList: IInternalPerformanceCounterList;
begin
  VEnum := ACounterList.GetEunm;
  while VEnum.Next(1, VUnknown, @Vcnt)=S_OK do begin
    if Supports(VUnknown, IInternalPerformanceCounter, VCounter) then begin
      AResultList.Add(VCounter.Id, VCounter.GetStaticData);
    end else if Supports(VUnknown, IInternalPerformanceCounterList, VList) then begin
      AppendStaticListByCounterList(AResultList, VList);
    end;
  end;
end;

constructor TInternalPerformanceCounterList.Create(AName: string);
begin
  FList := TInterfaceList.Create;
  FName := AName;
end;

function TInternalPerformanceCounterList.CreateAndAddNewCounter(
  AName: string): IInternalPerformanceCounter;
begin
  Result := TInternalPerformanceCounter.Create(AName);
  FList.Add(Result);
end;

function TInternalPerformanceCounterList.CreateAndAddNewSubList(
  AName: string): IInternalPerformanceCounterList;
begin
  Result := TInternalPerformanceCounterList.Create(AName);
  FList.Add(Result);
end;

function TInternalPerformanceCounterList.GetEunm: IEnumUnknown;
begin
  Result := TEnumUnknown.Create(FList);
end;

function TInternalPerformanceCounterList.GetName: string;
begin
  Result := FName;
end;

function TInternalPerformanceCounterList.GetStaticDataList: IIDInterfaceList;
begin
  Result := TIDInterfaceList.Create;
  AppendStaticListByCounterList(Result, Self);
end;

end.
