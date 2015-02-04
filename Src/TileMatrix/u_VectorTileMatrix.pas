unit u_VectorTileMatrix;

interface

uses
  Types,
  t_Hash,
  i_TileRect,
  i_VectorItemSubset,
  i_VectorTileMatrix,
  i_InterfaceListStatic,
  u_BaseInterfacedObject;

type
  TVectorTileMatrix = class(TBaseInterfacedObject, IVectorTileMatrix)
  private
    FHash: THashValue;
    FTileRect: ITileRect;
    FTileCount: TPoint;
    FItems: IInterfaceListStatic;
  private
    function GetHash: THashValue;
    function GetTileRect: ITileRect;
    function GetElementByTile(const ATile: TPoint): IVectorItemSubset;
    function GetItem(AX, AY: Integer): IVectorItemSubset;
  public
    constructor Create(
      const AHash: THashValue;
      const ATileRect: ITileRect;
      const AItems: IInterfaceListStatic
    );
  end;

implementation

{ TVectorTileMatrix }

constructor TVectorTileMatrix.Create(
  const AHash: THashValue;
  const ATileRect: ITileRect;
  const AItems: IInterfaceListStatic
);
var
  VItemsCount: Integer;
begin
  Assert(Assigned(AItems));
  Assert(Assigned(ATileRect));
  inherited Create;
  FHash := AHash;
  FTileRect := ATileRect;
  FItems := AItems;
  FTileCount := Point(FTileRect.Right - FTileRect.Left, FTileRect.Bottom - FTileRect.Top);
  Assert(FTileCount.X > 0);
  Assert(FTileCount.Y > 0);
  if FTileCount.X < 0 then begin
    FTileCount.X := 0;
  end;
  if FTileCount.Y < 0 then begin
    FTileCount.Y := 0;
  end;
  VItemsCount := FTileCount.X * FTileCount.Y;
  Assert(VItemsCount = AItems.Count);
end;

function TVectorTileMatrix.GetElementByTile(
  const ATile: TPoint
): IVectorItemSubset;
begin
  Result := GetItem(ATile.X - FTileRect.Left, ATile.Y - FTileRect.Top);
end;

function TVectorTileMatrix.GetHash: THashValue;
begin
  Result := FHash;
end;

function TVectorTileMatrix.GetItem(AX, AY: Integer): IVectorItemSubset;
var
  VIndex: Integer;
  VX, VY: Integer;
begin
  Result := nil;
  VX := AX;
  if VX >= FTileCount.X then begin
    VX := -1;
  end;

  VY := AY;
  if VY >= FTileCount.Y then begin
    VY := -1;
  end;

  if (VX >= 0) and (VY >= 0) then begin
    VIndex := VY * FTileCount.X + VX;
    Result := IVectorItemSubset(FItems[VIndex]);
  end;
end;

function TVectorTileMatrix.GetTileRect: ITileRect;
begin
  Result := FTileRect;
end;

end.
