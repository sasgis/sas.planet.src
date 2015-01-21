unit u_HashMatrix;

interface

uses
  Types,
  t_Hash,
  i_HashMatrix,
  u_BaseInterfacedObject;

type
  THashMatrix = class(TBaseInterfacedObject, IHashMatrix)
  private type TMatrixType = array of THashValue;
  private
    FMatrix: TMatrixType;
    FRect: TRect;
  public
    procedure Reset(const ARect: TRect);
    procedure ChangeRect(const ARect: TRect);
    procedure SetHash(
      const APos: TPoint;
      const AHash: THashValue
    );
    function GetHash(const APos: TPoint): THashValue;
  public
    constructor Create;
  end;

implementation

function IndexByPos(
  const ARect: TRect;
  const APos: TPoint
): Integer; inline;
begin
  Result := APos.X - ARect.Left + (APos.Y - ARect.Top) * (ARect.Right - ARect.Left);
end;

{ TTileHashMatrix }

constructor THashMatrix.Create;
begin
  inherited Create;
  FMatrix := nil;
  FRect := Rect(0, 0, 0, 0);
end;

function THashMatrix.GetHash(const APos: TPoint): THashValue;
begin
  if PtInRect(FRect, APos) then begin
    Result := FMatrix[IndexByPos(FRect, APos)];
  end else begin
    Result := 0;
  end;
end;

procedure THashMatrix.ChangeRect(const ARect: TRect);
var
  VIntersectRect: TRect;
  VTargetLineWidth: Integer;
  VTargetSize: Integer;
  VIntersectWidth: Integer;
  i: Integer;
  VTargetMatrix: TMatrixType;
begin
  if EqualRect(ARect, FRect) then begin
    Exit;
  end else if not IntersectRect(VIntersectRect, ARect, FRect) then begin
    Reset(ARect);
  end else begin
    VTargetLineWidth := ARect.Right - ARect.Left;
    VIntersectWidth := VIntersectRect.Right - VIntersectRect.Left;
    VTargetSize := VTargetLineWidth * (ARect.Bottom - ARect.Top);
    SetLength(VTargetMatrix, VTargetSize);

    for i := VIntersectRect.Top to VIntersectRect.Bottom - 1 do begin
      Move(
        FMatrix[IndexByPos(FRect, Point(VIntersectRect.Left, i))],
        VTargetMatrix[IndexByPos(ARect, Point(VIntersectRect.Left, i))],
        VIntersectWidth * SizeOf(FMatrix[0])
      );
    end;
    FRect := ARect;
    FMatrix := VTargetMatrix;
  end;
end;

procedure THashMatrix.Reset(const ARect: TRect);
var
  VSize: Integer;
begin
  FRect := ARect;
  if not IsRectEmpty(FRect) then begin
    VSize := (FRect.Right - FRect.Left) * (FRect.Bottom - FRect.Top);
    SetLength(FMatrix, VSize);
    FillChar(FMatrix[0], VSize * SizeOf(FMatrix[0]), 0);
  end;
end;

procedure THashMatrix.SetHash(
  const APos: TPoint;
  const AHash: THashValue
);
begin
  if PtInRect(FRect, APos) then begin
    FMatrix[IndexByPos(FRect, APos)] := AHash;
  end;
end;

end.
