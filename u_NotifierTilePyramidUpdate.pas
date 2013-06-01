unit u_NotifierTilePyramidUpdate;

interface

uses
  Types,
  Classes,
  SysUtils,
  i_Listener,
  i_TileKey,
  i_TileRect,
  i_CoordConverter,
  i_NotifierTilePyramidUpdate,
  u_BaseInterfacedObject;

type
  TListenerRecord = record
    Listener: IListener;
    Rect: TRect;
  end;

  TNotifierTileRectUpdateOneZoomSimple = class
  private
    FListenerByAllZoom: TList;
    FCount: Integer;
    FListenerByRect: array of TListenerRecord;
    function CalcGrowSize(AOldSize: Integer): Integer;

  public
    procedure AddListenerByRect(
      const AListener: IListener;
      const ATileRect: TRect
    );
    procedure AddListener(
      const AListener: IListener
    );
    procedure Remove(const AListener: IListener);

    procedure PrepareListenersListFull(AListeners: TList);
    procedure PrepareListenersListByTile(const ATile: TPoint; AListeners: TList);
    procedure PrepareListenersListByRect(const ATileRect: TRect; AListeners: TList);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TNotifierTilePyramidUpdate = class(TBaseInterfacedObject, INotifierTilePyramidUpdate, INotifierTilePyramidUpdateInternal)
  private
    FGeoCoder: ICoordConverter;
    FMinValidZoom: Byte;
    FMaxValidZoom: Byte;
    FSynchronizer: IReadWriteSync;
    FListeners: TList;
    FListenersByZoom: array of TNotifierTileRectUpdateOneZoomSimple;
  private
    function GetGeoCoder: ICoordConverter;

    procedure AddListenerByRect(
      const AListener: IListener;
      const AZoom: Byte;
      const ATileRect: TRect
    );
    procedure AddListenerByZoom(
      const AListener: IListener;
      const AZoom: Byte
    );
    procedure AddListener(
      const AListener: IListener
    );
    procedure Remove(const AListener: IListener);
  private
    procedure TileFullUpdateNotify;
    procedure TileUpdateNotify(const ATileKey: ITileKey); overload;
    procedure TileUpdateNotify(const ATile: TPoint; const AZoom: Byte); overload;
    procedure TileRectUpdateNotify(const ATileRect: ITileRect); overload;
    procedure TileRectUpdateNotify(const ATileRect: TRect; const AZoom: Byte); overload;
  public
    constructor Create(
      const AGeoCoder: ICoordConverter
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_TileKey,
  u_Synchronizer;

{ TNotifierTileRectUpdateOneZoomSimple }

constructor TNotifierTileRectUpdateOneZoomSimple.Create;
begin
  inherited Create;
  FListenerByAllZoom := TList.Create;
end;

destructor TNotifierTileRectUpdateOneZoomSimple.Destroy;
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do begin
    FListenerByRect[i].Listener := nil;
  end;
  FListenerByRect := nil;
  if Assigned(FListenerByAllZoom) then begin
    for i := 0 to FListenerByAllZoom.Count - 1 do begin
      IInterface(FListenerByAllZoom[i])._Release;
    end;
    FreeAndNil(FListenerByAllZoom);
  end;
  inherited;
end;

procedure TNotifierTileRectUpdateOneZoomSimple.AddListener(
  const AListener: IListener);
var
  i: Integer;
  VIndex: Integer;
begin
  Assert(AListener <> nil);
  VIndex := -1;
  for i := 0 to FCount - 1 do begin
    if FListenerByRect[i].Listener = AListener then begin
      VIndex := i;
      Break;
    end;
  end;
  if VIndex >= 0 then begin
    FListenerByRect[VIndex].Listener := nil;
    Dec(FCount);
    if VIndex < FCount then begin
      Pointer(FListenerByRect[VIndex].Listener) := Pointer(FListenerByRect[FCount].Listener);
      FListenerByRect[VIndex].Rect := FListenerByRect[FCount].Rect;
      Pointer(FListenerByRect[FCount].Listener) := nil;
    end;
  end;
  if FListenerByAllZoom.IndexOf(Pointer(AListener)) < 0 then begin
    FListenerByAllZoom.Add(Pointer(AListener));
    AListener._AddRef;
  end;
end;

procedure TNotifierTileRectUpdateOneZoomSimple.AddListenerByRect(
  const AListener: IListener; const ATileRect: TRect);
var
  i: Integer;
  VIndex: Integer;
begin
  Assert(AListener <> nil);
  VIndex := -1;
  for i := 0 to FCount - 1 do begin
    if FListenerByRect[i].Listener = AListener then begin
      VIndex := i;
      Break;
    end;
  end;
  if VIndex < 0 then begin
    if FCount >= Length(FListenerByRect) then begin
      SetLength(FListenerByRect, CalcGrowSize(Length(FListenerByRect)));
    end;
    VIndex := FCount;
    Inc(FCount);
  end;
  FListenerByRect[VIndex].Listener := AListener;
  FListenerByRect[VIndex].Rect := ATileRect;
  VIndex := FListenerByAllZoom.IndexOf(Pointer(AListener));
  if VIndex >= 0 then begin
    AListener._Release;
    FListenerByAllZoom.Delete(VIndex);
  end;
end;

function TNotifierTileRectUpdateOneZoomSimple.CalcGrowSize(
  AOldSize: Integer): Integer;
begin
  if AOldSize < 8 then begin
    Result := 8;
  end else begin
    Result := AOldSize * 2;
  end;
end;

procedure TNotifierTileRectUpdateOneZoomSimple.Remove(
  const AListener: IListener);
var
  i: Integer;
  VIndex: Integer;
begin
  Assert(AListener <> nil);
  VIndex := -1;
  for i := 0 to FCount - 1 do begin
    if FListenerByRect[i].Listener = AListener then begin
      VIndex := i;
      Break;
    end;
  end;
  if VIndex >= 0 then begin
    FListenerByRect[VIndex].Listener := nil;
    Dec(FCount);
    if VIndex < FCount then begin
      Pointer(FListenerByRect[VIndex].Listener) := Pointer(FListenerByRect[FCount].Listener);
      FListenerByRect[VIndex].Rect := FListenerByRect[FCount].Rect;
      Pointer(FListenerByRect[FCount].Listener) := nil;
    end;
  end;
  VIndex := FListenerByAllZoom.IndexOf(Pointer(AListener));
  if VIndex >= 0 then begin
    AListener._Release;
    FListenerByAllZoom.Delete(VIndex);
  end;
end;

procedure TNotifierTileRectUpdateOneZoomSimple.PrepareListenersListByRect(
  const ATileRect: TRect; AListeners: TList);
var
  i: Integer;
  VListener: Pointer;
  VIntersectRect: TRect;
begin
  for i := 0 to FCount - 1 do begin
    if IntersectRect(VIntersectRect, FListenerByRect[i].Rect, ATileRect) then begin
      VListener := Pointer(FListenerByRect[i].Listener);
      AListeners.Add(VListener);
      IInterface(VListener)._AddRef;
    end;
  end;
  for i := 0 to FListenerByAllZoom.Count - 1 do begin
    VListener := FListenerByAllZoom.Items[i];
    AListeners.Add(VListener);
    IInterface(VListener)._AddRef;
  end;
end;

procedure TNotifierTileRectUpdateOneZoomSimple.PrepareListenersListByTile(
  const ATile: TPoint; AListeners: TList);
var
  i: Integer;
  VListener: Pointer;
begin
  for i := 0 to FCount - 1 do begin
    if PtInRect(FListenerByRect[i].Rect, ATile) then begin
      VListener := Pointer(FListenerByRect[i].Listener);
      AListeners.Add(VListener);
      IInterface(VListener)._AddRef;
    end;
  end;
  for i := 0 to FListenerByAllZoom.Count - 1 do begin
    VListener := FListenerByAllZoom.Items[i];
    AListeners.Add(VListener);
    IInterface(VListener)._AddRef;
  end;
end;

procedure TNotifierTileRectUpdateOneZoomSimple.PrepareListenersListFull(
  AListeners: TList);
var
  i: Integer;
  VListener: Pointer;
begin
  for i := 0 to FCount - 1 do begin
    VListener := Pointer(FListenerByRect[i].Listener);
    AListeners.Add(VListener);
    IInterface(VListener)._AddRef;
  end;
  for i := 0 to FListenerByAllZoom.Count - 1 do begin
    VListener := FListenerByAllZoom.Items[i];
    AListeners.Add(VListener);
    IInterface(VListener)._AddRef;
  end;
end;

{ TNotifierTileRectUpdate }

constructor TNotifierTilePyramidUpdate.Create(const AGeoCoder: ICoordConverter);
var
  VCount: Integer;
  i: Integer;
begin
  Assert(AGeoCoder <> nil);
  inherited Create;
  FGeoCoder := AGeoCoder;
  FMinValidZoom := FGeoCoder.MinZoom;
  FMaxValidZoom := FGeoCoder.MaxZoom;
  FSynchronizer := MakeSyncRW_Big(Self, False);
  FListeners := TList.Create;
  VCount := FMaxValidZoom - FMinValidZoom + 1;
  SetLength(FListenersByZoom, VCount);
  for i := 0 to VCount - 1 do begin
    FListenersByZoom[i] := TNotifierTileRectUpdateOneZoomSimple.Create;
  end;
end;

destructor TNotifierTilePyramidUpdate.Destroy;
var
  i: Integer;
begin
  for i := 0 to Length(FListenersByZoom) - 1 do begin
    FreeAndNil(FListenersByZoom[i]);
  end;
  FListenersByZoom := nil;
  if Assigned(FListeners) then begin
    for i := 0 to FListeners.Count - 1 do begin
      IInterface(FListeners[i])._Release;
    end;
    FreeAndNil(FListeners);
  end;
  inherited;
end;

procedure TNotifierTilePyramidUpdate.AddListener(const AListener: IListener);
var
  i: Integer;
  VIndex: Integer;
begin
  Assert(AListener <> nil);
  FSynchronizer.BeginWrite;
  try
    for i := 0 to Length(FListenersByZoom) - 1 do begin
      FListenersByZoom[i].Remove(AListener);
    end;
    VIndex := FListeners.IndexOf(Pointer(AListener));
    if VIndex < 0 then begin
      FListeners.Add(Pointer(AListener));
      AListener._AddRef;
    end;
  finally
    FSynchronizer.EndWrite;
  end;
end;

procedure TNotifierTilePyramidUpdate.AddListenerByRect(
  const AListener: IListener;
  const AZoom: Byte;
  const ATileRect: TRect
);
var
  i: Integer;
  VIndex: Integer;
  VZoomIndex: Integer;
begin
  Assert(AListener <> nil);
  VZoomIndex := AZoom - FMinValidZoom;
  FSynchronizer.BeginWrite;
  try
    for i := 0 to Length(FListenersByZoom) - 1 do begin
      if i = VZoomIndex then begin
        FListenersByZoom[i].AddListenerByRect(AListener, ATileRect);
      end else begin
        FListenersByZoom[i].Remove(AListener);
      end;
    end;
    VIndex := FListeners.IndexOf(Pointer(AListener));
    if VIndex >= 0 then begin
      AListener._Release;
      FListeners.Delete(VIndex);
    end;
  finally
    FSynchronizer.EndWrite;
  end;
end;

procedure TNotifierTilePyramidUpdate.AddListenerByZoom(
  const AListener: IListener;
  const AZoom: Byte
);
var
  i: Integer;
  VIndex: Integer;
  VZoomIndex: Integer;
begin
  Assert(AListener <> nil);
  VZoomIndex := AZoom - FMinValidZoom;
  FSynchronizer.BeginWrite;
  try
    for i := 0 to Length(FListenersByZoom) - 1 do begin
      if i = VZoomIndex then begin
        FListenersByZoom[i].AddListener(AListener);
      end else begin
        FListenersByZoom[i].Remove(AListener);
      end;
    end;
    VIndex := FListeners.IndexOf(Pointer(AListener));
    if VIndex >= 0 then begin
      AListener._Release;
      FListeners.Delete(VIndex);
    end;
  finally
    FSynchronizer.EndWrite;
  end;
end;

function TNotifierTilePyramidUpdate.GetGeoCoder: ICoordConverter;
begin
  Result := FGeoCoder;
end;

procedure TNotifierTilePyramidUpdate.Remove(const AListener: IListener);
var
  i: Integer;
  VIndex: Integer;
begin
  Assert(AListener <> nil);
  FSynchronizer.BeginWrite;
  try
    for i := 0 to Length(FListenersByZoom) - 1 do begin
      FListenersByZoom[i].Remove(AListener);
    end;
    VIndex := FListeners.IndexOf(Pointer(AListener));
    if VIndex >= 0 then begin
      AListener._Release;
      FListeners.Delete(VIndex);
    end;
  finally
    FSynchronizer.EndWrite;
  end;
end;

procedure TNotifierTilePyramidUpdate.TileRectUpdateNotify(
  const ATileRect: ITileRect);
var
  i: Integer;
  VList: TList;
  VListener: Pointer;
  VZoomIndex: Integer;
begin
  VList := TList.Create;
  try
    VList.Capacity := 8;
    FSynchronizer.BeginRead;
    try
      for i := 0 to FListeners.Count - 1 do begin
        VListener := FListeners.Items[i];
        VList.Add(VListener);
        IInterface(VListener)._AddRef;;
      end;
      if ATileRect <> nil then begin
        VZoomIndex := ATileRect.Zoom - FMinValidZoom;
        if (VZoomIndex >= 0) and (VZoomIndex < Length(FListenersByZoom)) then begin
          FListenersByZoom[VZoomIndex].PrepareListenersListByRect(ATileRect.Rect, VList);
        end;
      end else begin
        for i := 0 to Length(FListenersByZoom) - 1 do begin
          FListenersByZoom[i].PrepareListenersListFull(VList);
        end;
      end;
    finally
      FSynchronizer.EndRead;
    end;
    if VList.Count > 0 then begin
      for i := 0 to VList.Count - 1 do begin
        VListener := VList[i];
        IListener(VListener).Notification(ATileRect);
        IInterface(VListener)._Release;
        VListener := nil;
      end;
    end;
  finally
    VList.Free;
  end;
end;

procedure TNotifierTilePyramidUpdate.TileUpdateNotify(const ATileKey: ITileKey);
var
  i: Integer;
  VList: TList;
  VListener: Pointer;
  VZoomIndex: Integer;
begin
  VList := TList.Create;
  try
    VList.Capacity := 8;
    FSynchronizer.BeginRead;
    try
      for i := 0 to FListeners.Count - 1 do begin
        VListener := FListeners.Items[i];
        VList.Add(VListener);
        IInterface(VListener)._AddRef;;
      end;
      if ATileKey <> nil then begin
        VZoomIndex := ATileKey.Zoom - FMinValidZoom;
        if (VZoomIndex >= 0) and (VZoomIndex < Length(FListenersByZoom)) then begin
          FListenersByZoom[VZoomIndex].PrepareListenersListByTile(ATileKey.Tile, VList);
        end;
      end else begin
        for i := 0 to Length(FListenersByZoom) - 1 do begin
          FListenersByZoom[i].PrepareListenersListFull(VList);
        end;
      end;
    finally
      FSynchronizer.EndRead;
    end;
    if VList.Count > 0 then begin
      for i := 0 to VList.Count - 1 do begin
        VListener := VList[i];
        IListener(VListener).Notification(ATileKey);
        IInterface(VListener)._Release;
        VListener := nil;
      end;
    end;
  finally
    VList.Free;
  end;
end;

procedure TNotifierTilePyramidUpdate.TileFullUpdateNotify;
var
  i: Integer;
  VList: TList;
  VListener: Pointer;
begin
  VList := TList.Create;
  try
    VList.Capacity := 8;
    FSynchronizer.BeginRead;
    try
      for i := 0 to FListeners.Count - 1 do begin
        VListener := FListeners.Items[i];
        VList.Add(VListener);
        IInterface(VListener)._AddRef;;
      end;
      for i := 0 to Length(FListenersByZoom) - 1 do begin
        FListenersByZoom[i].PrepareListenersListFull(VList);
      end;
    finally
      FSynchronizer.EndRead;
    end;
    if VList.Count > 0 then begin
      for i := 0 to VList.Count - 1 do begin
        VListener := VList[i];
        IListener(VListener).Notification(nil);
        IInterface(VListener)._Release;
        VListener := nil;
      end;
    end;
  finally
    VList.Free;
  end;
end;

procedure TNotifierTilePyramidUpdate.TileRectUpdateNotify(const ATileRect: TRect;
  const AZoom: Byte);
var
  i: Integer;
  VList: TList;
  VListener: Pointer;
  VZoomIndex: Integer;
  VTileRect: ITileRect;
begin
  VList := TList.Create;
  try
    VList.Capacity := 8;
    FSynchronizer.BeginRead;
    try
      for i := 0 to FListeners.Count - 1 do begin
        VListener := FListeners.Items[i];
        VList.Add(VListener);
        IInterface(VListener)._AddRef;;
      end;
      VZoomIndex := AZoom - FMinValidZoom;
      if (VZoomIndex >= 0) and (VZoomIndex < Length(FListenersByZoom)) then begin
        FListenersByZoom[VZoomIndex].PrepareListenersListByRect(ATileRect, VList);
      end;
    finally
      FSynchronizer.EndRead;
    end;
    if VList.Count > 0 then begin
      VTileRect := nil; //TODO: Доделать создание ITileRect
      for i := 0 to VList.Count - 1 do begin
        VListener := VList[i];
        IListener(VListener).Notification(VTileRect);
        IInterface(VListener)._Release;
        VListener := nil;
      end;
    end;
  finally
    VList.Free;
  end;
end;

procedure TNotifierTilePyramidUpdate.TileUpdateNotify(const ATile: TPoint;
  const AZoom: Byte);
var
  i: Integer;
  VList: TList;
  VListener: Pointer;
  VZoomIndex: Integer;
  VKey: ITileKey;
begin
  VList := TList.Create;
  try
    VList.Capacity := 8;
    FSynchronizer.BeginRead;
    try
      for i := 0 to FListeners.Count - 1 do begin
        VListener := FListeners.Items[i];
        VList.Add(VListener);
        IInterface(VListener)._AddRef;;
      end;
      VZoomIndex := AZoom - FMinValidZoom;
      if (VZoomIndex >= 0) and (VZoomIndex < Length(FListenersByZoom)) then begin
        FListenersByZoom[VZoomIndex].PrepareListenersListByTile(ATile, VList);
      end;
    finally
      FSynchronizer.EndRead;
    end;
    if VList.Count > 0 then begin
      VKey := TTileKey.Create(ATile, AZoom);
      for i := 0 to VList.Count - 1 do begin
        VListener := VList[i];
        IListener(VListener).Notification(VKey);
        IInterface(VListener)._Release;
        VListener := nil;
      end;
    end;
  finally
    VList.Free;
  end;
end;

end.
