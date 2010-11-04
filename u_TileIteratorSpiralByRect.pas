unit u_TileIteratorSpiralByRect;

interface

uses
  Types,
  u_TileIteratorAbstract;

type
  TTileIteratorSpiralByRect = class(TTileIteratorAbstract)
  protected
    FCenterPoint: TPoint;
    FMaxRadius: Integer;

    FEOI: Boolean;
    FCurrentRing: Integer;
    FIndexInRing: Integer;

    function CheckPoint(APoint: TPoint): Boolean;
    class function GetMaxRing(ACenterPoint: TPoint; ARect: TRect): Integer;
    class function GetTilesInRingCount(ARad: Integer): Integer;
    class function GetDeltaByRingAndIndex(ARad: Integer; AIndex: Integer): TPoint;
  public
    constructor Create(ARect: TRect; APoint: TPoint); overload;
    constructor Create(ARect: TRect); overload;
    function Next(out ATile: TPoint): Boolean; override;
    procedure Reset;
  end;

implementation

{ TTileIteratorSpiralByRect }

constructor TTileIteratorSpiralByRect.Create(ARect: TRect; APoint: TPoint);
begin
  FTilesRect:=ARect;
  FCenterPoint:=APoint;

  FMaxRadius:=GetMaxRing(FCenterPoint, FTilesRect);

  Reset;
end;

function TTileIteratorSpiralByRect.CheckPoint(APoint: TPoint): Boolean;
begin
  Result:=
    (APoint.X >= FTilesRect.Left) and
    (APoint.Y >= FTilesRect.Top) and
    (APoint.X < FTilesRect.Right) and
    (APoint.Y < FTilesRect.Bottom);
end;

constructor TTileIteratorSpiralByRect.Create(ARect: TRect);
begin
  Create(ARect, Point((ARect.Left + ARect.Right) div 2, (ARect.Top + ARect.Bottom) div 2));
end;

class function TTileIteratorSpiralByRect.GetDeltaByRingAndIndex(ARad,
  AIndex: Integer): TPoint;
var
  VTilesInLine: Integer;
  VLineIndex: Integer;
  VIndexInLine: Integer;
begin
  Result := Point(0, 0);
  if ARad > 0 then begin
    VTilesInLine := ARad;
    VLineIndex := AIndex div (VTilesInLine);
    VIndexInLine := AIndex - VLineIndex * VTilesInLine;
    case VLineIndex of
      0: begin
        Result.X := 1 + VIndexInLine;
        Result.Y := - ARad;
      end;
      1: begin
        Result.X := ARad;
        Result.Y := 1 - ARad + VIndexInLine;
      end;
      2: begin
        Result.X := ARad;
        Result.Y := 1 + VIndexInLine;
      end;
      3: begin
        Result.X := ARad - 1 - VIndexInLine;
        Result.Y := ARad;
      end;
      4: begin
        Result.X := - 1 - VIndexInLine;
        Result.Y := ARad;
      end;
      5: begin
        Result.X := -ARad;
        Result.Y := ARad - 1  - VIndexInLine;
      end;
      6: begin
        Result.X := -ARad;
        Result.Y := -1 - VIndexInLine;
      end;
      7: begin
        Result.X := 1 - ARad + VIndexInLine;
        Result.Y := - ARad;
      end;
    end;
  end;
end;

class function TTileIteratorSpiralByRect.GetMaxRing(ACenterPoint: TPoint; ARect: TRect): Integer;
var
  VRad: Integer;
begin
  Result := ACenterPoint.x-ARect.Left;
  VRad := ARect.Right-ACenterPoint.x;
  if(VRad > Result)then begin
    Result:=VRad;
  end;
  VRad := ACenterPoint.y - ARect.Top;
  if(VRad > Result)then begin
    Result:=VRad;
  end;
  VRad := ARect.Bottom - ACenterPoint.y;
  if(VRad > Result)then begin
    Result:=VRad;
  end;
end;

class function TTileIteratorSpiralByRect.GetTilesInRingCount(ARad: Integer): Integer;
begin
  Result := 0;
  if ARad = 0 then begin
    Result := 1;
  end else if ARad > 0 then begin
    Result := (ARad * 2) * 4;
  end;
end;

function TTileIteratorSpiralByRect.Next(out ATile: TPoint): Boolean;
var
  VDelta: TPoint;
  VFound: Boolean;
begin
  Result := False;
  while (not FEOI) and (not Result) do begin
    VDelta := GetDeltaByRingAndIndex(FCurrentRing, FIndexInRing);
    ATile.X := FCenterPoint.X + VDelta.X;
    ATile.Y := FCenterPoint.Y + VDelta.Y;
    Inc(FIndexInRing);
    if FIndexInRing >= GetTilesInRingCount(FCurrentRing) then begin
      Inc(FCurrentRing);
      FIndexInRing := 0;
      if FCurrentRing > FMaxRadius then begin
        FEOI := True;
      end;
    end;
    Result := CheckPoint(ATile);
  end;
end;

procedure TTileIteratorSpiralByRect.Reset;
begin
  FEOI:=IsRectEmpty(FTilesRect);
  FCurrentRing := 0;
  FIndexInRing := 0;
end;

end.
