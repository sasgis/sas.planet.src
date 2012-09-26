unit u_TileRectInfo;

interface

uses
  Types,
  i_BinaryData,
  i_TileIterator,
  i_MapVersionInfo,
  i_ContentTypeInfo,
  i_TileInfoBasic;

type
  TTileInfoInternal = record
    FLoadDate: TDateTime;
    FVersionInfo: IMapVersionInfo;
    FContentType: IContentTypeInfoBasic;
    FData: IBinaryData;
    FSize: Cardinal;
    FInfoType: TTileInfoType;
  end;
  TArrayOfTileInfoInternal = array of TTileInfoInternal;
  PTileInfoInternalArray = ^TTileInfoInternalArray;
  TTileInfoInternalArray = array [0..0] of TTileInfoInternal;

  TTileRectInfo = class(TInterfacedObject, ITileRectInfo)
  private
    FTileRect: TRect;
    FTileCount: TPoint;
    FZoom: Byte;
    FItems: TArrayOfTileInfoInternal;
  private
    function GetTileRect: TRect;
    function GetZoom: Byte;
    function GetEnum(const ATileIterator: ITileIterator): IEnumTileInfo;
  public
    constructor CreateWithOwn(
      const ATileRect: TRect;
      AZoom: Byte;
      const AItems: TArrayOfTileInfoInternal
    );
    destructor Destroy; override;
  end;

implementation

{ TEnumTileInfo }

type
  TEnumTileInfo = class(TInterfacedObject, IEnumTileInfo)
  private
    FRef: IInterface;
    FZoom: Byte;
    FTileRect: TRect;
    FItems: PTileInfoInternalArray;
    FTileIterator: ITileIterator;
    function TileToIndex(const ATile: TPoint): Integer;
  private
    function Next(var ATileInfo: TTileInfo): Boolean;
  public
    constructor Create(
      const ARef: IInterface;
      const AZoom: Byte;
      const ATileRect: TRect;
      AItems: PTileInfoInternalArray;
      const ATileIterator: ITileIterator
    );
  end;

constructor TEnumTileInfo.Create(
  const ARef: IInterface;
  const AZoom: Byte;
  const ATileRect: TRect;
  AItems: PTileInfoInternalArray;
  const ATileIterator: ITileIterator
);
begin
  inherited Create;
  FRef := ARef;
  FZoom := AZoom;
  FTileRect := ATileRect;
  FItems := AItems;
  FTileIterator := ATileIterator;
end;

function TEnumTileInfo.Next(var ATileInfo: TTileInfo): Boolean;
var
  VTile: TPoint;
  VIndex: Integer;
begin
  Result := FTileIterator.Next(VTile);
  if Result then begin
    ATileInfo.FTile := VTile;
    ATileInfo.FZoom := FZoom;
    VIndex := TileToIndex(VTile);
    if VIndex < 0 then begin
      ATileInfo.FInfoType := titUnknown;
      ATileInfo.FContentType := nil;
      ATileInfo.FVersionInfo := nil;
      ATileInfo.FData := nil;
      ATileInfo.FSize := 0;
    end else begin
      ATileInfo.FInfoType := FItems[VIndex].FInfoType;
      ATileInfo.FContentType := FItems[VIndex].FContentType;
      ATileInfo.FVersionInfo := FItems[VIndex].FVersionInfo;
      ATileInfo.FData := FItems[VIndex].FData;
      ATileInfo.FSize := FItems[VIndex].FSize;
      ATileInfo.FLoadDate := FItems[VIndex].FLoadDate;
    end;
  end;
end;

function TEnumTileInfo.TileToIndex(const ATile: TPoint): Integer;
begin
  if (ATile.X < FTileRect.Left) or (ATile.X >= FTileRect.Right) then begin
    Result := -1;
  end else if (ATile.Y < FTileRect.Top) or (ATile.Y >= FTileRect.Bottom) then begin
    Result := -1;
  end else begin
    Result :=
      (ATile.X - FTileRect.Left) +
      (ATile.Y - FTileRect.Top) * (FTileRect.Right - FTileRect.Left);
  end;
end;

{ TTileRectInfo }

constructor TTileRectInfo.CreateWithOwn(
  const ATileRect: TRect;
  AZoom: Byte;
  const AItems: TArrayOfTileInfoInternal
);
begin
  inherited Create;
  FTileRect := ATileRect;
  FZoom := AZoom;
  FTileCount.X := FTileRect.Right - FTileRect.Left;
  FTileCount.Y := FTileRect.Bottom - FTileRect.Top;
  FItems := AItems;
end;

destructor TTileRectInfo.Destroy;
var
  VCount: Integer;
  i: Integer;
begin
  if FItems <> nil then begin
    VCount := FTileCount.X * FTileCount.Y;
    for i := 0 to VCount - 1 do begin
      FItems[i].FVersionInfo := nil;
      FItems[i].FContentType := nil;
      FItems[i].FData := nil;
    end;
    FItems := nil;
  end;
  inherited;
end;

function TTileRectInfo.GetEnum(const ATileIterator: ITileIterator): IEnumTileInfo;
begin
  Result := TEnumTileInfo.Create(Self, FZoom, FTileRect, Addr(FItems[0]), ATileIterator);
end;

function TTileRectInfo.GetTileRect: TRect;
begin
  Result := FTileRect;
end;

function TTileRectInfo.GetZoom: Byte;
begin
  Result := FZoom;
end;

end.
