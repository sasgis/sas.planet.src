unit u_HashMatrix_Test;

interface

uses
  Types,
  TestFramework,
  i_HashTileMatrixBuilder;

type
  TestTHashMatrix = class(TTestCase)
  protected
    FCheckRect: TRect;
    FMatrix: IHashTileMatrixBuilder;
    procedure SetUp; override;
    procedure SetMatrixRect(
      const ARect: TRect
    );
    procedure CheckMatrixRect(
      const ARect: TRect
    );
  published
    procedure TestSetToEmpty;
    procedure TestSetInner;
    procedure TestSetOuter;
    procedure TestReset;
    procedure TestMoveFull;
    procedure TestMoveInner;
    procedure TestMoveOuter;
    procedure TestMoveLeft;
    procedure TestMoveRight;
    procedure TestMoveTop;
    procedure TestMoveBottom;
    procedure TestMoveTopLeft;
    procedure TestMoveTopRight;
    procedure TestMoveBottomLeft;
    procedure TestMoveBottomRight;
    procedure TestMoveWidth;
    procedure TestMoveHeight;
  end;

implementation

uses
  i_TileIterator,
  u_TileIteratorByRect,
  u_HashTileMatrixBuilder;

{ TestTHashMatrix }

procedure TestTHashMatrix.SetUp;
begin
  inherited;
  FMatrix := THashTileMatrixBuilder.Create;
  FCheckRect := Rect(0, 0, 10, 10);
end;

procedure TestTHashMatrix.SetMatrixRect(
  const ARect: TRect
);
var
  VPos: TPoint;
  VIterator: ITileIterator;
begin
  VIterator := TTileIteratorByRect.Create(ARect);
  while VIterator.Next(VPos) do begin
    FMatrix.Tiles[VPos] := VPos.X * 10 + VPos.Y;
  end;
end;

procedure TestTHashMatrix.CheckMatrixRect(
  const ARect: TRect
);
var
  VPos: TPoint;
  VIterator: ITileIterator;
begin
  VIterator := TTileIteratorByRect.Create(FCheckRect);
  while VIterator.Next(VPos) do begin
    if PtInRect(ARect, VPos) then begin
      CheckEquals(VPos.X * 10 + VPos.Y, FMatrix.Tiles[VPos]);
    end else begin
      CheckEquals(0, FMatrix.Tiles[VPos]);
    end;
  end;
end;

procedure TestTHashMatrix.TestMoveBottom;
var
  VSourceRect: TRect;
  VTargetRect: TRect;
  VValidRect: TRect;
begin
  VSourceRect := Rect(2, 3, 4, 6);
  FMatrix.Reset(VSourceRect);

  SetMatrixRect(VSourceRect);
  CheckMatrixRect(VSourceRect);
  VTargetRect := Rect(3, 4, 4, 8);
  FMatrix.SetRect(VTargetRect);
  IntersectRect(VValidRect, VSourceRect, VTargetRect);
  CheckMatrixRect(VValidRect);
end;

procedure TestTHashMatrix.TestMoveBottomLeft;
var
  VSourceRect: TRect;
  VTargetRect: TRect;
  VValidRect: TRect;
begin
  VSourceRect := Rect(2, 3, 4, 6);
  FMatrix.Reset(VSourceRect);

  SetMatrixRect(VSourceRect);
  CheckMatrixRect(VSourceRect);
  VTargetRect := Rect(1, 4, 3, 7);
  FMatrix.SetRect(VTargetRect);
  IntersectRect(VValidRect, VSourceRect, VTargetRect);
  CheckMatrixRect(VValidRect);
end;

procedure TestTHashMatrix.TestMoveBottomRight;
var
  VSourceRect: TRect;
  VTargetRect: TRect;
  VValidRect: TRect;
begin
  VSourceRect := Rect(2, 3, 4, 6);
  FMatrix.Reset(VSourceRect);

  SetMatrixRect(VSourceRect);
  CheckMatrixRect(VSourceRect);
  VTargetRect := Rect(3, 4, 5, 8);
  FMatrix.SetRect(VTargetRect);
  IntersectRect(VValidRect, VSourceRect, VTargetRect);
  CheckMatrixRect(VValidRect);
end;

procedure TestTHashMatrix.TestMoveFull;
var
  VSourceRect: TRect;
  VTargetRect: TRect;
  VValidRect: TRect;
begin
  VSourceRect := Rect(2, 3, 4, 6);
  FMatrix.Reset(VSourceRect);

  SetMatrixRect(VSourceRect);
  CheckMatrixRect(VSourceRect);
  VTargetRect := Rect(4, 4, 5, 8);
  FMatrix.SetRect(VTargetRect);
  IntersectRect(VValidRect, VSourceRect, VTargetRect);
  CheckMatrixRect(VValidRect);
end;

procedure TestTHashMatrix.TestMoveHeight;
var
  VSourceRect: TRect;
  VTargetRect: TRect;
  VValidRect: TRect;
begin
  VSourceRect := Rect(2, 3, 4, 6);
  FMatrix.Reset(VSourceRect);

  SetMatrixRect(VSourceRect);
  CheckMatrixRect(VSourceRect);
  VTargetRect := Rect(3, 1, 5, 8);
  FMatrix.SetRect(VTargetRect);
  IntersectRect(VValidRect, VSourceRect, VTargetRect);
  CheckMatrixRect(VValidRect);
end;

procedure TestTHashMatrix.TestMoveInner;
var
  VSourceRect: TRect;
  VTargetRect: TRect;
  VValidRect: TRect;
begin
  VSourceRect := Rect(2, 3, 4, 6);
  FMatrix.Reset(VSourceRect);

  SetMatrixRect(VSourceRect);
  CheckMatrixRect(VSourceRect);
  VTargetRect := Rect(3, 4, 4, 5);
  FMatrix.ChangeRect(VTargetRect);
  IntersectRect(VValidRect, VSourceRect, VTargetRect);
  CheckMatrixRect(VValidRect);
end;

procedure TestTHashMatrix.TestMoveLeft;
var
  VSourceRect: TRect;
  VTargetRect: TRect;
  VValidRect: TRect;
begin
  VSourceRect := Rect(2, 3, 4, 6);
  FMatrix.Reset(VSourceRect);

  SetMatrixRect(VSourceRect);
  CheckMatrixRect(VSourceRect);
  VTargetRect := Rect(1, 3, 3, 5);
  FMatrix.SetRect(VTargetRect);
  IntersectRect(VValidRect, VSourceRect, VTargetRect);
  CheckMatrixRect(VValidRect);
end;

procedure TestTHashMatrix.TestMoveOuter;
var
  VSourceRect: TRect;
  VTargetRect: TRect;
  VValidRect: TRect;
begin
  VSourceRect := Rect(2, 3, 4, 6);
  FMatrix.Reset(VSourceRect);

  SetMatrixRect(VSourceRect);
  CheckMatrixRect(VSourceRect);
  VTargetRect := Rect(1, 1, 5, 8);
  FMatrix.ChangeRect(VTargetRect);
  IntersectRect(VValidRect, VSourceRect, VTargetRect);
  CheckMatrixRect(VValidRect);
end;

procedure TestTHashMatrix.TestMoveRight;
var
  VSourceRect: TRect;
  VTargetRect: TRect;
  VValidRect: TRect;
begin
  VSourceRect := Rect(2, 3, 4, 6);
  FMatrix.Reset(VSourceRect);

  SetMatrixRect(VSourceRect);
  CheckMatrixRect(VSourceRect);
  VTargetRect := Rect(3, 4, 5, 5);
  FMatrix.ChangeRect(VTargetRect);
  IntersectRect(VValidRect, VSourceRect, VTargetRect);
  CheckMatrixRect(VValidRect);
end;

procedure TestTHashMatrix.TestMoveTop;
var
  VSourceRect: TRect;
  VTargetRect: TRect;
  VValidRect: TRect;
begin
  VSourceRect := Rect(2, 3, 4, 6);
  FMatrix.Reset(VSourceRect);

  SetMatrixRect(VSourceRect);
  CheckMatrixRect(VSourceRect);
  VTargetRect := Rect(3, 2, 4, 5);
  FMatrix.ChangeRect(VTargetRect);
  IntersectRect(VValidRect, VSourceRect, VTargetRect);
  CheckMatrixRect(VValidRect);
end;

procedure TestTHashMatrix.TestMoveTopLeft;
var
  VSourceRect: TRect;
  VTargetRect: TRect;
  VValidRect: TRect;
begin
  VSourceRect := Rect(2, 3, 4, 6);
  FMatrix.Reset(VSourceRect);

  SetMatrixRect(VSourceRect);
  CheckMatrixRect(VSourceRect);
  VTargetRect := Rect(1, 1, 4, 5);
  FMatrix.ChangeRect(VTargetRect);
  IntersectRect(VValidRect, VSourceRect, VTargetRect);
  CheckMatrixRect(VValidRect);
end;

procedure TestTHashMatrix.TestMoveTopRight;
var
  VSourceRect: TRect;
  VTargetRect: TRect;
  VValidRect: TRect;
begin
  VSourceRect := Rect(2, 3, 4, 6);
  FMatrix.Reset(VSourceRect);

  SetMatrixRect(VSourceRect);
  CheckMatrixRect(VSourceRect);
  VTargetRect := Rect(3, 2, 7, 5);
  FMatrix.ChangeRect(VTargetRect);
  IntersectRect(VValidRect, VSourceRect, VTargetRect);
  CheckMatrixRect(VValidRect);
end;

procedure TestTHashMatrix.TestMoveWidth;
var
  VSourceRect: TRect;
  VTargetRect: TRect;
  VValidRect: TRect;
begin
  VSourceRect := Rect(2, 3, 4, 6);
  FMatrix.Reset(VSourceRect);

  SetMatrixRect(VSourceRect);
  CheckMatrixRect(VSourceRect);
  VTargetRect := Rect(1, 4, 5, 6);
  FMatrix.ChangeRect(VTargetRect);
  IntersectRect(VValidRect, VSourceRect, VTargetRect);
  CheckMatrixRect(VValidRect);
end;

procedure TestTHashMatrix.TestReset;
begin
  FMatrix.Reset(Rect(-1, -1, 1, 1));
  SetMatrixRect(Rect(1, 1, 5, 7));
  FMatrix.Reset(Rect(-1, -1, 1, 1));
  CheckMatrixRect(Rect(0, 0, 0, 0));
end;

procedure TestTHashMatrix.TestSetInner;
var
  VPos: TPoint;
begin
  FMatrix.Reset(Rect(-1, -1, 1, 1));
  VPos := Point(0, 0);
  FMatrix.SetHash(VPos, 111);
  CheckEquals(111, FMatrix.GetHash(VPos));
  VPos := Point(-1, 0);
  FMatrix.SetHash(VPos, 222);
  CheckEquals(222, FMatrix.GetHash(VPos));
  VPos := Point(-1, -1);
  FMatrix.SetHash(VPos, 333);
  CheckEquals(333, FMatrix.GetHash(VPos));
end;

procedure TestTHashMatrix.TestSetOuter;
var
  VPos: TPoint;
begin
  FMatrix.Reset(Rect(-1, -1, 1, 1));
  VPos := Point(10, 10);
  FMatrix.SetHash(VPos, 111);
  CheckEquals(0, FMatrix.GetHash(VPos));
  VPos := Point(1, 1);
  FMatrix.SetHash(VPos, 222);
  CheckEquals(0, FMatrix.GetHash(VPos));
  VPos := Point(-2, -1);
  FMatrix.SetHash(VPos, 333);
  CheckEquals(0, FMatrix.GetHash(VPos));
end;

procedure TestTHashMatrix.TestSetToEmpty;
var
  VPos: TPoint;
begin
  VPos := Point(0, 0);
  FMatrix.SetHash(VPos, 555);
  CheckEquals(0, FMatrix.GetHash(VPos));
  VPos := Point(1, 1);
  FMatrix.SetHash(VPos, 555);
  CheckEquals(0, FMatrix.GetHash(VPos));
  VPos := Point(-1, -1);
  FMatrix.SetHash(VPos, 555);
  CheckEquals(0, FMatrix.GetHash(VPos));
end;

initialization
  RegisterTest(TestTHashMatrix.Suite);

end.
