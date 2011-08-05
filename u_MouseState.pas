unit u_MouseState;

interface

uses
  Windows,
  Types,
  SyncObjs,
  Classes,
  Controls,
  t_GeoTypes,
  i_MouseState,
  i_MouseHandler;

type
  TMouseState = class(TInterfacedObject, IMouseState, IMouseHandler)
  private
    FCS: TCriticalSection;
    FMaxTime: Double;

    FCurentPos: TPoint;
    FCurentTime: TLargeInteger;
    FCurrentSpeed: TDoublePoint;

    FCurrentShift: TShiftState;
    FLastDownPos: array [TMouseButton] of TPoint;
    FLastDownShift: array [TMouseButton] of TShiftState;
    FLastUpPos: array [TMouseButton] of TPoint;
    FLastUpShift: array [TMouseButton] of TShiftState;

    procedure SetCurrentPos(
      APosition: TPoint
    );
  protected
    function GetCurentPos: TPoint;
    function GetCurentSpeed: TDoublePoint;
    function GetCurrentShift: TShiftState;

    function GetLastDownShift(AButton: TMouseButton): TShiftState;
    function GetLastDownPos(AButton: TMouseButton): TPoint;
    function GetLastUpShift(AButton: TMouseButton): TShiftState;
    function GetLastUpPos(AButton: TMouseButton): TPoint;
  protected
    procedure OnMouseMove(
      AShift: TShiftState;
      APosition: TPoint
    );
    procedure OnMouseDown(
      AButton: TMouseButton;
      AShift: TShiftState;
      APosition: TPoint
    );
    procedure OnMouseUp(
      AButton: TMouseButton;
      AShift: TShiftState;
      APosition: TPoint
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
  FCurentTime := 0;
  FMaxTime := 3;
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

function TMouseState.GetCurentSpeed: TDoublePoint;
begin
  FCS.Acquire;
  try
    Result := FCurrentSpeed;
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
  APosition: TPoint);
begin
  FCS.Acquire;
  try
    SetCurrentPos(APosition);
    FCurrentShift := AShift;
    FLastDownPos[AButton] := APosition;
    FLastDownShift[AButton] := AShift;
    FLastUpPos[AButton] := APosition;
    FLastUpShift[AButton] := AShift;
  finally
    FCS.Release;
  end;
end;

procedure TMouseState.OnMouseMove(AShift: TShiftState; APosition: TPoint);
begin
  FCS.Acquire;
  try
    SetCurrentPos(APosition);
    FCurrentShift := AShift;
  finally
    FCS.Release;
  end;
end;

procedure TMouseState.OnMouseUp(AButton: TMouseButton; AShift: TShiftState;
  APosition: TPoint);
begin
  FCS.Acquire;
  try
    SetCurrentPos(APosition);
    FCurrentShift := AShift;
    FLastUpPos[AButton] := APosition;
    FLastUpShift[AButton] := AShift;
  finally
    FCS.Release;
  end;
end;

procedure TMouseState.SetCurrentPos(APosition: TPoint);
var
  VCurrTime: TLargeInteger;
  VFrequency: TLargeInteger;
  VTimeFromLastMove: Double;
  VCurrentSpeed: TDoublePoint;
  VAlfa: Double;
  VBeta: Double;
begin
  QueryPerformanceCounter(VCurrTime);
  if FCurentTime <> 0 then begin
    QueryPerformanceFrequency(VFrequency);
    VTimeFromLastMove := (VCurrTime - FCurentTime) / VFrequency;
    if VTimeFromLastMove < FMaxTime then begin
      if VTimeFromLastMove > 0.001 then begin
        VCurrentSpeed.X := (FCurentPos.X - APosition.X) / VTimeFromLastMove;
        VCurrentSpeed.Y := (FCurentPos.Y - APosition.Y) / VTimeFromLastMove;
//        VAlfa := VTimeFromLastMove / FMaxTime;
//        if VAlfa > 0.2 then begin
//          VAlfa := 0.2;
//        end;
        VAlfa := 1;
        VBeta := 1 - VAlfa;
        FCurrentSpeed.X := VAlfa * VCurrentSpeed.X + VBeta * FCurrentSpeed.X;
        FCurrentSpeed.Y := VAlfa * VCurrentSpeed.Y + VBeta * FCurrentSpeed.Y;
      end;
    end else begin
      FCurrentSpeed.X := 0;
      FCurrentSpeed.Y := 0;
    end;
  end else begin
    FCurrentSpeed.X := 0;
    FCurrentSpeed.Y := 0;
  end;

  FCurentPos := APosition;
  FCurentTime := VCurrTime;
end;

end.
