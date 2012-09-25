unit u_TileRectInfoShort;

interface

uses
  Types,
  i_TileIterator,
  i_MapVersionInfo,
  i_ContentTypeInfo,
  i_TileInfoBasic;

type
  TTileInfoShortInternal = record
    FLoadDate: TDateTime;
    FSize: Cardinal;
    FInfoType: TTileInfoType;
  end;
  TArrayOfTileInfoShortInternal = array of TTileInfoShortInternal;
  PTileInfoShortInternalArray = ^TTileInfoShortInternalArray;
  TTileInfoShortInternalArray = array [0..0] of TTileInfoShortInternal;

  TTileRectInfoShort = class(TInterfacedObject, ITileRectInfo)
  private
    FTileRect: TRect;
    FTileCount: TPoint;
    FZoom: Byte;
    FVersionInfo: IMapVersionInfo;
    FContentType: IContentTypeInfoBasic;
    FItems: TArrayOfTileInfoShortInternal;
  private
    function GetTileRect: TRect;
    function GetZoom: Byte;
    function GetEnum(const ATileIterator: ITileIterator): IEnumTileInfo;
  public
    constructor CreateWithOwn(
      const ATileRect: TRect;
      AZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      const AContentType: IContentTypeInfoBasic;
      const AItems: TArrayOfTileInfoShortInternal
    );
    destructor Destroy; override;
  end;

implementation

{ TEnumTileInfoShort }

type
  TEnumTileInfoShort = class(TInterfacedObject, IEnumTileInfo)
  private
    FRef: IInterface;
    FVersionInfo: IMapVersionInfo;
    FContentType: IContentTypeInfoBasic;
    FZoom: Byte;
    FTileRect: TRect;
    FItems: PTileInfoShortInternalArray;
    FTileIterator: ITileIterator;
    function TileToIndex(const ATile: TPoint): Integer;
  private
    function Next(var ATileInfo: TTileInfo): Boolean;
  public
    constructor Create(
      const ARef: IInterface;
      const AVersionInfo: IMapVersionInfo;
      const AContentType: IContentTypeInfoBasic;
      const AZoom: Byte;
      const ATileRect: TRect;
      AItems: PTileInfoShortInternalArray;
      const ATileIterator: ITileIterator
    );
  end;

constructor TEnumTileInfoShort.Create(
  const ARef: IInterface;
  const AVersionInfo: IMapVersionInfo;
  const AContentType: IContentTypeInfoBasic;
  const AZoom: Byte;
  const ATileRect: TRect;
  AItems: PTileInfoShortInternalArray;
  const ATileIterator: ITileIterator
);
begin
  inherited Create;
  FRef := ARef;
  FVersionInfo := AVersionInfo;
  FContentType := AContentType;
  FZoom := AZoom;
  FTileRect := ATileRect;
  FItems := AItems;
  FTileIterator := ATileIterator;
end;

function TEnumTileInfoShort.Next(var ATileInfo: TTileInfo): Boolean;
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
      ATileInfo.FContentType := FContentType;
      ATileInfo.FVersionInfo := FVersionInfo;
      ATileInfo.FData := nil;
      ATileInfo.FSize := FItems[VIndex].FSize;
      ATileInfo.FLoadDate := FItems[VIndex].FLoadDate;
    end;
  end;
end;

function TEnumTileInfoShort.TileToIndex(const ATile: TPoint): Integer;
begin
  Result := TileInRectToIndex(@FTileRect, ATile);
end;

{ TTileRectInfoShort }

constructor TTileRectInfoShort.CreateWithOwn(
  const ATileRect: TRect;
  AZoom: Byte;
  const AVersionInfo: IMapVersionInfo;
  const AContentType: IContentTypeInfoBasic;
  const AItems: TArrayOfTileInfoShortInternal
);
begin
  inherited Create;
  FTileRect := ATileRect;
  FContentType := AContentType;
  FVersionInfo := AVersionInfo;
  FZoom := AZoom;
  FTileCount.X := FTileRect.Right - FTileRect.Left;
  FTileCount.Y := FTileRect.Bottom - FTileRect.Top;
  FItems := AItems;
end;

destructor TTileRectInfoShort.Destroy;
begin
  FItems := nil;
  inherited;
end;

function TTileRectInfoShort.GetEnum(
  const ATileIterator: ITileIterator
): IEnumTileInfo;
begin
  Result :=
    TEnumTileInfoShort.Create(
      Self,
      FVersionInfo,
      FContentType,
      FZoom,
      FTileRect,
      Addr(FItems[0]),
      ATileIterator
    );
end;

function TTileRectInfoShort.GetTileRect: TRect;
begin
  Result := FTileRect;
end;

function TTileRectInfoShort.GetZoom: Byte;
begin
  Result := FZoom;
end;

end.
