unit u_NotifierTileRectUpdate;

interface

uses
  Types,
  SysUtils,
  i_Listener,
  i_TileKey,
  i_TileRect,
  i_CoordConverter,
  i_NotifierTileRectUpdate;

type
  TListenerRecord = record
    Listener: IListener;
    Rect: TRect;
  end;

  TNotifierTileRectUpdate = class(TInterfacedObject, INotifierTileRectUpdate, INotifierTileRectUpdateInternal)
  private
    FGeoCoder: ICoordConverter;
    FZoom: Byte;
    FSynchronizer: IReadWriteSync;
    FCount: Integer;
    FList: array of TListenerRecord;
    function CalcGrowSize(AOldSize: Integer): Integer;
  private
    function GetGeoCoder: ICoordConverter;
    function GetZoom: Byte;

    procedure Add(
      const AListener: IListener;
      const ATileRect: TRect
    );
    procedure Remove(const AListener: IListener);

  private
    procedure TileUpdateNotify(const ATileKey: ITileKey);
    procedure TileRectUpdateNotify(const ATileRect: ITileRect);
  public
    constructor Create(
      AZoom: Byte;
      const AGeoCoder: ICoordConverter
    );
    destructor Destroy; override;
  end;

implementation

uses
  Classes,
  u_Synchronizer;

{ TNotifierTileRectUpdate }

constructor TNotifierTileRectUpdate.Create(
  AZoom: Byte;
  const AGeoCoder: ICoordConverter
);
begin
  inherited Create;
  FZoom := AZoom;
  FGeoCoder := AGeoCoder;
  FSynchronizer := MakeSyncRW_Big(Self, False);
end;

destructor TNotifierTileRectUpdate.Destroy;
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do begin
    FList[i].Listener := nil;
  end;
  FList := nil;
  FGeoCoder := nil;
  inherited;
end;

function TNotifierTileRectUpdate.CalcGrowSize(AOldSize: Integer): Integer;
begin
  if AOldSize < 8 then begin
    Result := 8;
  end else begin
    Result := AOldSize * 2;
  end;
end;

procedure TNotifierTileRectUpdate.Add(
  const AListener: IListener;
  const ATileRect: TRect
);
var
  i: Integer;
  VIndex: Integer;
begin
  Assert(AListener <> nil);
  FSynchronizer.BeginWrite;
  try
    VIndex := -1;
    for i := 0 to FCount - 1 do begin
      if FList[i].Listener = AListener then begin
        VIndex := i;
        Break;
      end;
    end;
    if VIndex < 0 then begin
      if FCount >= Length(FList) then begin
        SetLength(FList, CalcGrowSize(Length(FList)));
      end;
      VIndex := FCount;
      Inc(FCount);
    end;
    FList[VIndex].Listener := AListener;
    FList[VIndex].Rect := ATileRect;
  finally
    FSynchronizer.EndWrite;
  end;
end;

function TNotifierTileRectUpdate.GetGeoCoder: ICoordConverter;
begin
  Result := FGeoCoder;
end;

function TNotifierTileRectUpdate.GetZoom: Byte;
begin
  Result := FZoom;
end;

procedure TNotifierTileRectUpdate.Remove(
  const AListener: IListener
);
var
  i: Integer;
  VIndex: Integer;
begin
  Assert(AListener <> nil);
  FSynchronizer.BeginWrite;
  try
    VIndex := -1;
    for i := 0 to FCount - 1 do begin
      if FList[i].Listener = AListener then begin
        VIndex := i;
        Break;
      end;
    end;
    if VIndex >= 0 then begin
      FList[VIndex].Listener := nil;
      Dec(FCount);
      if VIndex < FCount then begin
        Pointer(FList[VIndex].Listener) := Pointer(FList[FCount].Listener);
        FList[VIndex].Rect := FList[FCount].Rect;
        Pointer(FList[FCount].Listener) := nil;
      end;
    end;
  finally
    FSynchronizer.EndWrite;
  end;
end;

procedure TNotifierTileRectUpdate.TileRectUpdateNotify(
  const ATileRect: ITileRect
);
var
  i: Integer;
  VList: TList;
  VListener: IListener;
begin
  VList := TList.Create;
  try
    VList.Capacity := 8;
    FSynchronizer.BeginRead;
    try
      for i := 0 to FCount - 1 do begin
        if (ATileRect = nil) or ATileRect.IsIntersecWithRect(FList[i].Rect) then begin
          FList[i].Listener._AddRef;
          VList.Add(Pointer(FList[i].Listener));
        end;
      end;
    finally
      FSynchronizer.EndRead;
    end;
    if VList.Count > 0 then begin
      for i := 0 to VList.Count - 1 do begin
        VListener := IListener(VList[i]);
        VListener.Notification(ATileRect);
        VListener._Release;
        VListener := nil;
      end;
    end;
  finally
    VList.Free;
  end;
end;

procedure TNotifierTileRectUpdate.TileUpdateNotify(const ATileKey: ITileKey);
var
  i: Integer;
  VTile: TPoint;
  VList: TList;
  VListener: IListener;
begin
  VList := TList.Create;
  try
    VList.Capacity := 8;
    VTile := ATileKey.Tile;
    FSynchronizer.BeginRead;
    try
      for i := 0 to FCount - 1 do begin
        if PtInRect(FList[i].Rect, VTile) then begin
          FList[i].Listener._AddRef;
          VList.Add(Pointer(FList[i].Listener));
        end;
      end;
    finally
      FSynchronizer.EndRead;
    end;
    if VList.Count > 0 then begin
      for i := 0 to VList.Count - 1 do begin
        VListener := IListener(VList[i]);
        VListener.Notification(ATileKey);
        VListener._Release;
        VListener := nil;
      end;
    end;
  finally
    VList.Free;
  end;
end;

end.
