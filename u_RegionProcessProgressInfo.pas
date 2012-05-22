unit u_RegionProcessProgressInfo;

interface

uses
  i_RegionProcessProgressInfo;

type
  TRegionProcessProgressInfo = class(TInterfacedObject, IProgressInfoBase, IRegionProcessProgressInfo, IRegionProcessProgressInfoInternal)
  private
    FProcessedRatio: Double;
    FFinished: Boolean;
    FCaption: string;
    FFirstLine: string;
    FSecondLine: string;
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
  public
    constructor Create;
  end;

implementation

{ TRegionProcessProgressInfo }

constructor TRegionProcessProgressInfo.Create;
begin
  inherited Create;
  FFinished := False;
  FProcessedRatio := 0;
end;

procedure TRegionProcessProgressInfo.Finish;
begin
  FFinished := True;
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
