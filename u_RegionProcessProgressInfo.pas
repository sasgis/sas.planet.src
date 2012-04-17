unit u_RegionProcessProgressInfo;

interface

uses
  i_RegionProcessProgressInfo;

type
  TRegionProcessProgressInfo = class(TInterfacedObject, IProgressInfo, IRegionProcessProgressInfo)
  private
    FProcessed: Double;
    FFinished: Boolean;
    FCaption: string;
    FFirstLine: string;
    FSecondLine: string;
  private
    function GetProcessed: Double;
    procedure SetProcessed(const AValue: Double);
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
    constructor Create();
  end;

implementation

{ TRegionProcessProgressInfo }

constructor TRegionProcessProgressInfo.Create;
begin
  inherited Create;
  FFinished := False;
  FProcessed := 0;
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

function TRegionProcessProgressInfo.GetProcessed: Double;
begin
  Result := FProcessed;
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

procedure TRegionProcessProgressInfo.SetProcessed(const AValue: Double);
begin
  if AValue < 0 then begin
    FProcessed := 0;
  end else if AValue > 1 then begin
    FProcessed := 1;
  end else begin
    FProcessed := AValue;
  end;
end;

procedure TRegionProcessProgressInfo.SetSecondLine(const AValue: string);
begin
  FSecondLine := AValue;
end;

end.
