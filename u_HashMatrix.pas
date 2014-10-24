unit u_HashMatrix;

interface

uses
  Types,
  t_Hash,
  i_HashMatrix,
  u_BaseInterfacedObject;

type
  THashMatrix = class(TBaseInterfacedObject, IHashMatrix)
  private
    FMatrix: array of THashValue;
    FRect: TRect;
  public
    procedure Reset(const ARect: TRect);
    procedure ChangeRect(const ARect: TRect);
    procedure SetHash(const APos: TPoint; const AHash: THashValue);
    function GetHash(const APos: TPoint): THashValue;
  public
    constructor Create;
  end;

implementation

{ TTileHashMatrix }

constructor THashMatrix.Create;
begin
  inherited Create;
  FMatrix := nil;
end;

function THashMatrix.GetHash(const APos: TPoint): THashValue;
var
  VIndex: Integer;
begin
  if PtInRect(FRect, APos) then begin
    VIndex := APos.X - FRect.Left + (APos.Y - FRect.Top) * (FRect.Right - FRect.Left);
    Result := FMatrix[VIndex];
  end else begin
    Result := 0;
  end;
end;

procedure THashMatrix.ChangeRect(const ARect: TRect);
begin

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
var
  VIndex: Integer;
begin
  if PtInRect(FRect, APos) then begin
    VIndex := APos.X - FRect.Left + (APos.Y - FRect.Top) * (FRect.Right - FRect.Left);
    FMatrix[VIndex] := AHash;
  end;
end;

end.
