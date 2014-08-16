unit u_TileHashMatrix;

interface

uses
  Types,
  t_Hash,
  u_BaseInterfacedObject;

type
  ITileHashMatrix = interface
    ['{30ADFD38-9B4C-49EC-B6D4-5902B5696E7B}']
    procedure Reset(const ATileRect: TRect);
    procedure SetTileHash(const ATile: TPoint; const AHash: THashValue);
    function GetTileHash(const ATile: TPoint): THashValue;
  end;

  TTileHashMatrix = class(TBaseInterfacedObject, ITileHashMatrix)
  private
    FMatrix: array of THashValue;
    FTileRect: TRect;
  public
    procedure Reset(const ATileRect: TRect);
    procedure SetTileHash(const ATile: TPoint; const AHash: THashValue);
    function GetTileHash(const ATile: TPoint): THashValue;
  public
    constructor Create;
  end;

implementation

{ TTileHashMatrix }

constructor TTileHashMatrix.Create;
begin
  inherited Create;
  FMatrix := nil;
end;

function TTileHashMatrix.GetTileHash(const ATile: TPoint): THashValue;
var
  VIndex: Integer;
begin
  if PtInRect(FTileRect, ATile) then begin
    VIndex := ATile.X - FTileRect.Left + (ATile.Y - FTileRect.Top) * (FTileRect.Right - FTileRect.Left);
    Result := FMatrix[VIndex];
  end else begin
    Result := 0;
  end;
end;

procedure TTileHashMatrix.Reset(const ATileRect: TRect);
var
  VSize: Integer;
begin
  FTileRect := ATileRect;
  if not IsRectEmpty(FTileRect) then begin
    VSize := (FTileRect.Right - FTileRect.Left) * (FTileRect.Bottom - FTileRect.Top);
    SetLength(FMatrix, VSize);
    FillChar(FMatrix[0], VSize * SizeOf(FMatrix[0]), 0);
  end;
end;

procedure TTileHashMatrix.SetTileHash(
  const ATile: TPoint;
  const AHash: THashValue
);
var
  VIndex: Integer;
begin
  if PtInRect(FTileRect, ATile) then begin
    VIndex := ATile.X - FTileRect.Left + (ATile.Y - FTileRect.Top) * (FTileRect.Right - FTileRect.Left);
    FMatrix[VIndex] := AHash;
  end;
end;

end.
