unit u_MouseState;

interface

uses
  Types,
  SyncObjs,
  Classes,
  Controls,
  i_MouseState,
  i_MouseHandler;

type
  TMouseState = class(TInterfacedObject, IMouseState, IMouseHandler)
  private
    FCS: TCriticalSection;
    FCurentPos: TPoint;
    FCurrentShift: TShiftState;
    FLastDownPos: array [TMouseButton] of TPoint;
    FLastDownShift: array [TMouseButton] of TShiftState;
    FLastUpPos: array [TMouseButton] of TPoint;
    FLastUpShift: array [TMouseButton] of TShiftState;
  protected
    function GetCurentPos: TPoint;
    function GetCurrentShift: TShiftState;

    function GetLastDownShift(AButton: TMouseButton): TShiftState;
    function GetLastDownPos(AButton: TMouseButton): TPoint;
    function GetLastUpShift(AButton: TMouseButton): TShiftState;
    function GetLastUpPos(AButton: TMouseButton): TPoint;
  protected
    procedure OnMouseMove(
      AShift: TShiftState;
      APos: TPoint
    );
    procedure OnMouseDown(
      AButton: TMouseButton;
      AShift: TShiftState;
      APos: TPoint
    );
    procedure OnMouseUp(
      AButton: TMouseButton;
      AShift: TShiftState;
      APos: TPoint
    );
  public
    constructor Create();
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TMouseState }

constructor TMouseState.Create;
begin
  FCS := TCriticalSection.Create;
end;

destructor TMouseState.Destroy;
begin
  FreeAndNil(FCS);
  inherited;
end;

function TMouseState.GetCurentPos: TPoint;
begin
  FCS.Acquire;
  try
    Result := FCurentPos;
  finally
    FCS.Release;
  end;
end;

function TMouseState.GetCurrentShift: TShiftState;
begin
  FCS.Acquire;
  try
    Result := FCurrentShift;
  finally
    FCS.Release;
  end;
end;

function TMouseState.GetLastDownPos(AButton: TMouseButton): TPoint;
begin
  FCS.Acquire;
  try
    Result := FLastDownPos[AButton];
  finally
    FCS.Release;
  end;
end;

function TMouseState.GetLastDownShift(AButton: TMouseButton): TShiftState;
begin
  FCS.Acquire;
  try
    Result := FLastDownShift[AButton];
  finally
    FCS.Release;
  end;
end;

function TMouseState.GetLastUpPos(AButton: TMouseButton): TPoint;
begin
  FCS.Acquire;
  try
    Result := FLastUpPos[AButton];
  finally
    FCS.Release;
  end;
end;

function TMouseState.GetLastUpShift(AButton: TMouseButton): TShiftState;
begin
  FCS.Acquire;
  try
    Result := FLastUpShift[AButton];
  finally
    FCS.Release;
  end;
end;

procedure TMouseState.OnMouseDown(AButton: TMouseButton; AShift: TShiftState;
  APos: TPoint);
begin
  FCS.Acquire;
  try
    FCurentPos := APos;
    FCurrentShift := AShift;
    FLastDownPos[AButton] := APos;
    FLastDownShift[AButton] := AShift;
    FLastUpPos[AButton] := APos;
    FLastUpShift[AButton] := AShift;
  finally
    FCS.Release;
  end;
end;

procedure TMouseState.OnMouseMove(AShift: TShiftState; APos: TPoint);
begin
  FCS.Acquire;
  try
    FCurentPos := APos;
    FCurrentShift := AShift;
  finally
    FCS.Release;
  end;
end;

procedure TMouseState.OnMouseUp(AButton: TMouseButton; AShift: TShiftState;
  APos: TPoint);
begin
  FCS.Acquire;
  try
    FCurentPos := APos;
    FCurrentShift := AShift;
    FLastUpPos[AButton] := APos;
    FLastUpShift[AButton] := AShift;
  finally
    FCS.Release;
  end;
end;

end.
