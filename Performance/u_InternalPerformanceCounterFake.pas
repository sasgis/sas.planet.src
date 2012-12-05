unit u_InternalPerformanceCounterFake;

interface

uses
  ActiveX,
  i_IDList,
  i_InternalPerformanceCounter;

type
  TInternalPerformanceCounterFake = class(TInterfacedObject, IInternalPerformanceCounterList, IInternalPerformanceCounter)
  private
    { IInternalPerformanceCounterList }
    function GetName: string;
    function GetStaticDataList: IIDInterfaceList;
    procedure AppendStaticDataToList(const ADataList: IIDInterfaceList);
    function GetEunm: IEnumUnknown;
    function CreateAndAddNewCounter(const AName: string): IInternalPerformanceCounter;
    function CreateAndAddNewSubList(const AName: string): IInternalPerformanceCounterList;
    procedure AddSubList(const ASubList: IInternalPerformanceCounterList);
  private
    { IInternalPerformanceCounter }
    function GetId: Integer;
    function StartOperation: TInternalPerformanceCounterContext;
    procedure FinishOperation(const AContext: TInternalPerformanceCounterContext);
    function GetCounter: Cardinal;
    function GetTotalTime: TDateTime;
    function GetMaxTime: TDateTime;
    function GetMinTime: TDateTime;
    function GetStaticData: IInternalPerformanceCounterStaticData;
  end;

implementation

{ TInternalPerformanceCounterFake }

procedure TInternalPerformanceCounterFake.AppendStaticDataToList(
  const ADataList: IIDInterfaceList);
begin
  // empty
end;

function TInternalPerformanceCounterFake.CreateAndAddNewCounter(const AName: string): IInternalPerformanceCounter;
begin
  Result := Self;
end;

function TInternalPerformanceCounterFake.CreateAndAddNewSubList(const AName: string): IInternalPerformanceCounterList;
begin
  Result := Self;
end;

procedure TInternalPerformanceCounterFake.AddSubList(
  const ASubList: IInternalPerformanceCounterList
);
begin
  // empty
end;

procedure TInternalPerformanceCounterFake.FinishOperation(const AContext: TInternalPerformanceCounterContext);
begin
  // empty
end;

function TInternalPerformanceCounterFake.GetCounter: Cardinal;
begin
  Result := 0;
end;

function TInternalPerformanceCounterFake.GetEunm: IEnumUnknown;
begin
  Result := nil;
end;

function TInternalPerformanceCounterFake.GetId: Integer;
begin
  Result := Integer(Self);
end;

function TInternalPerformanceCounterFake.GetName: string;
begin
  Result := '';
end;

function TInternalPerformanceCounterFake.GetStaticData: IInternalPerformanceCounterStaticData;
begin
  Result := nil;
end;

function TInternalPerformanceCounterFake.GetStaticDataList: IIDInterfaceList;
begin
  Result := nil;
end;

function TInternalPerformanceCounterFake.GetTotalTime: TDateTime;
begin
  Result := 0;
end;

function TInternalPerformanceCounterFake.GetMaxTime: TDateTime;
begin
  Result := 0;
end;

function TInternalPerformanceCounterFake.GetMinTime: TDateTime;
begin
  Result := 0;
end;

function TInternalPerformanceCounterFake.StartOperation: TInternalPerformanceCounterContext;
begin
  Result := 0;
end;

end.
