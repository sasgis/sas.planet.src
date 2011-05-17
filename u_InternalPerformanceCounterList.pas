unit u_InternalPerformanceCounterList;

interface

uses
  Windows,
  Classes,
  SyncObjs,
  ActiveX,
  i_InternalPerformanceCounter;

type
  TInternalPerformanceCounterList = class(TInterfacedObject, IInternalPerformanceCounterList)
  private
    FList: IInterfaceList;
    FName: string;
  protected
    function GetName: string;

    function GetEunm: IEnumUnknown;
    function CreateAndAddNewCounter(AName: string): IInternalPerformanceCounter;
    function CreateAndAddNewSubList(AName: string): IInternalPerformanceCounterList;
  public
    constructor Create(AName: string);
  end;

implementation

uses
  u_EnumUnknown,
  u_InternalPerformanceCounter;

{ TInternalPerformanceCounterList }

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

end.
