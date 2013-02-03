unit u_RegionProcessProgressInfo;

interface

uses
  i_NotifierOperation,
  i_RegionProcessProgressInfo,
  u_BaseInterfacedObject;

type
  TRegionProcessProgressInfo = class(TBaseInterfacedObject, IProgressInfoBase, IRegionProcessProgressInfo, IRegionProcessProgressInfoInternal)
  private
    FProcessedRatio: Double;
    FFinished: Boolean;
    FCaption: string;
    FFirstLine: string;
    FSecondLine: string;
    FCancelNotifier: INotifierOperation;
    FOperationID: Integer;
  private
    function GetProcessedRatio: Double;
    procedure SetProcessedRatio(const AValue: Double);
  private
    function GetFinished: Boolean;

    function GetCaption: string;
    procedure SetCaption(const AValue: string);

    function GetFirstLine: string;
    procedure SetFirstLine(const AValue: string);

    function GetSecondLine: string;
    procedure SetSecondLine(const AValue: string);

    procedure Finish;
    function GetCancelNotifier: INotifierOperation;
    function GetOperationID: Integer;
  public
    constructor Create(
      const ACancelNotifier: INotifierOperation;
      const AOperationID: Integer
    );
  end;

implementation

{ TRegionProcessProgressInfo }

constructor TRegionProcessProgressInfo.Create(
  const ACancelNotifier: INotifierOperation;
  const AOperationID: Integer
);
begin
  inherited Create;
  FCancelNotifier := ACancelNotifier;
  FOperationID := AOperationID;
  FFinished := False;
  FProcessedRatio := 0;
end;

procedure TRegionProcessProgressInfo.Finish;
begin
  FFinished := True;
end;

function TRegionProcessProgressInfo.GetCancelNotifier: INotifierOperation;
begin
  Result := FCancelNotifier;
end;

function TRegionProcessProgressInfo.GetCaption: string;
begin
  Result := FCaption;
end;

function TRegionProcessProgressInfo.GetFinished: Boolean;
begin
  Result := FFinished;
end;

function TRegionProcessProgressInfo.GetFirstLine: string;
begin
  Result := FFirstLine;
end;

function TRegionProcessProgressInfo.GetOperationID: Integer;
begin
  Result := FOperationID;
end;

function TRegionProcessProgressInfo.GetProcessedRatio: Double;
begin
  Result := FProcessedRatio;
end;

function TRegionProcessProgressInfo.GetSecondLine: string;
begin
  Result := FSecondLine;
end;

procedure TRegionProcessProgressInfo.SetCaption(const AValue: string);
begin
  FCaption := AValue;
end;

procedure TRegionProcessProgressInfo.SetFirstLine(const AValue: string);
begin
  FFirstLine := AValue;
end;

procedure TRegionProcessProgressInfo.SetProcessedRatio(const AValue: Double);
begin
  if AValue < 0 then begin
    FProcessedRatio := 0;
  end else if AValue > 1 then begin
    FProcessedRatio := 1;
  end else begin
    FProcessedRatio := AValue;
  end;
end;

procedure TRegionProcessProgressInfo.SetSecondLine(const AValue: string);
begin
  FSecondLine := AValue;
end;

end.
