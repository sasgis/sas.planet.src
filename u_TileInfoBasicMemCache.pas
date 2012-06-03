unit u_TileInfoBasicMemCache;

interface

uses
  Types,
  Windows,
  Classes,
  SyncObjs,
  i_MapVersionInfo,
  i_TileInfoBasic;

type
  TTileInfoBasicMemCache = class
  private
    FList: TList;
    FMaxTileInfoCounts: Integer;
    FTileInfoTTL: Cardinal;
    FCS: TCriticalSection;
  public
    constructor Create(
      const AMaxTileInfoCounts: Integer;
      const ATileInfoTTL: Cardinal
    );
    destructor Destroy; override;

    procedure Add(
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      const ATileInfoBasic: ITileInfoBasic
    );

    procedure Remove(
      const AXY: TPoint;
      const AZoom: Byte
    );

    function Get(
      const AXY: TPoint;
      const AZoom: Byte
    ): ITileInfoBasic;

    procedure Clear;
  end;

implementation

type
  TTileInfoCacheRec = record
    TileTTL: Cardinal;
    TileXY: TPoint;
    TileZoom: Byte;
    TileVersionInfo: IMapVersionInfo;
    TileInfoBasic: ITileInfoBasic;
  end;
  PTileInfoCacheRec = ^TTileInfoCacheRec;

{ TTileInfoBasicMemCache }

constructor TTileInfoBasicMemCache.Create(
  const AMaxTileInfoCounts: Integer;
  const ATileInfoTTL: Cardinal
);
begin
  inherited Create;
  FCS := TCriticalSection.Create;
  FList := TList.Create;
  FMaxTileInfoCounts := AMaxTileInfoCounts;
  FTileInfoTTL := ATileInfoTTL;
end;

destructor TTileInfoBasicMemCache.Destroy;
begin
  Self.Clear;
  FList.Free;
  FCS.Free;
  inherited Destroy;
end;

procedure TTileInfoBasicMemCache.Add(
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo;
  const ATileInfoBasic: ITileInfoBasic
);
var
  I: Integer;
  VTile: PTileInfoCacheRec;
  VReplaceOld: Boolean;
  VOldestItem: Integer;
  VMinTTL: Cardinal;
begin
  FCS.Acquire;
  try
    VOldestItem := -1;
    VMinTTL := $FFFFFFFF;
    VReplaceOld := (FMaxTileInfoCounts >= FList.Count);

    for I := 0 to FList.Count - 1 do begin
      VTile := PTileInfoCacheRec(FList.Items[I]);
      if (VTile.TileXY.X = AXY.X) and
         (VTile.TileXY.Y = AXY.Y) and
         (VTile.TileZoom = AZoom)
      then begin
        VTile.TileTTL := GetTickCount + FTileInfoTTL;
        VTile.TileVersionInfo := AVersionInfo;
        VTile.TileInfoBasic := ATileInfoBasic;
        Exit;
      end else begin
        if VTile.TileTTL < VMinTTL then begin
          VOldestItem := I;
          VMinTTL := VTile.TileTTL;
        end;
      end;
    end;

    if VReplaceOld then begin
      if (FList.Count > 0) and (FList.Count < VOldestItem) then begin
        Dispose(PTileInfoCacheRec(FList.Items[VOldestItem]));
        FList.Delete(VOldestItem);
      end;
    end;

    New(VTile);

    VTile.TileTTL := GetTickCount + FTileInfoTTL;
    VTile.TileXY := AXY;
    VTile.TileZoom := AZoom;
    VTile.TileVersionInfo := AVersionInfo;
    VTile.TileInfoBasic := ATileInfoBasic;

    FList.Add(VTile);
  finally
    FCS.Release;
  end;
end;

procedure TTileInfoBasicMemCache.Remove(
  const AXY: TPoint;
  const AZoom: Byte
);
var
  I: Integer;
  VTile: PTileInfoCacheRec;
begin
  FCS.Acquire;
  try
    for I := 0 to FList.Count - 1 do begin
      VTile := PTileInfoCacheRec(FList.Items[I]);
      if (VTile.TileXY.X = AXY.X) and
         (VTile.TileXY.Y = AXY.Y) and
         (VTile.TileZoom = AZoom)
      then begin
        Dispose(PTileInfoCacheRec(FList.Items[I]));
        FList.Delete(I);
        Break;
      end;
    end;
  finally
    FCS.Release;
  end;
end;

function TTileInfoBasicMemCache.Get(
  const AXY: TPoint;
  const AZoom: Byte
): ITileInfoBasic;
var
  I: Integer;
  VTile: PTileInfoCacheRec;
begin
  FCS.Acquire;
  try
    Result := nil;
    for I := 0 to FList.Count - 1 do begin
      VTile := PTileInfoCacheRec(FList.Items[I]);
      if (VTile.TileXY.X = AXY.X) and
         (VTile.TileXY.Y = AXY.Y) and
         (VTile.TileZoom = AZoom)
      then begin
        if (VTile.TileTTL > GetTickCount) then begin
          Dispose(PTileInfoCacheRec(FList.Items[I]));
          FList.Delete(I);
        end else begin
          VTile.TileTTL := GetTickCount + FTileInfoTTL;
          Result := VTile.TileInfoBasic;
        end;
        Break;
      end;
    end;
  finally
    FCS.Release;
  end;
end;

procedure TTileInfoBasicMemCache.Clear;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do begin
    Dispose(PTileInfoCacheRec(FList.Items[I]));
  end;
  FList.Clear;
end;

end.
