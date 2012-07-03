unit u_LonLatRectUpdateNotifier;

interface

uses
  SysUtils,
  t_GeoTypes,
  i_Notify,
  i_LonLatRect,
  i_LonLatRectUpdateNotifier;

type
  TListenerRecord = record
    Listener: IListener;
    Rect: ILonLatRect;
  end;

  TLonLatRectUpdateNotifier = class(TInterfacedObject, INotifierLonLatRectUpdate, INotifierLonLatRectUpdateInternal)
  private
    FSynchronizer: IReadWriteSync;
    FCount: Integer;
    FList: array of TListenerRecord;
    function CalcGrowSize(AOldSize: Integer): Integer;
  private
    procedure Add(
      const AListener: IListener;
      const ARect: ILonLatRect
    ); stdcall;
    procedure Remove(const AListener: IListener); stdcall;
  private
    procedure RectUpdateNotify(const ARect: ILonLatRect); overload; stdcall;
    procedure RectUpdateNotify(const ARect: TDoubleRect); overload; stdcall;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  u_LonLatRect,
  u_Synchronizer;

{ TLonLatRectUpdateNotifier }

constructor TLonLatRectUpdateNotifier.Create;
begin
  inherited Create;
  FSynchronizer := MakeSyncRW_Big(Self, False);
end;

destructor TLonLatRectUpdateNotifier.Destroy;
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do begin
    FList[i].Listener := nil;
  end;
  FList := nil;
  inherited;
end;

procedure TLonLatRectUpdateNotifier.Add(
  const AListener: IListener;
  const ARect: ILonLatRect
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
    FList[VIndex].Rect := ARect;
  finally
    FSynchronizer.EndWrite;
  end;
end;

function TLonLatRectUpdateNotifier.CalcGrowSize(AOldSize: Integer): Integer;
begin
  if AOldSize < 8 then begin
    Result := 8;
  end else begin
    Result := AOldSize * 2;
  end;
end;

procedure TLonLatRectUpdateNotifier.RectUpdateNotify(const ARect: TDoubleRect);
var
  i: Integer;
  VRect: ILonLatRect;
begin
  FSynchronizer.BeginRead;
  try
    for i := 0 to FCount - 1 do begin
      if (FList[i].Rect = nil) or FList[i].Rect.IsIntersecWithRect(ARect) then begin
        if VRect = nil then begin
          VRect := TLonLatRect.Create(ARect);
        end;
        FList[i].Listener.Notification(VRect);
      end;
    end;
  finally
    FSynchronizer.EndRead;
  end;
end;

procedure TLonLatRectUpdateNotifier.RectUpdateNotify(const ARect: ILonLatRect);
var
  i: Integer;
begin
  FSynchronizer.BeginRead;
  try
    for i := 0 to FCount - 1 do begin
      if (FList[i].Rect = nil) or FList[i].Rect.IsIntersecWithRect(ARect) then begin
        FList[i].Listener.Notification(ARect);
      end;
    end;
  finally
    FSynchronizer.EndRead;
  end;
end;

procedure TLonLatRectUpdateNotifier.Remove(const AListener: IListener);
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
      FList[VIndex].Rect := nil;
      Dec(FCount);
      if VIndex < FCount then begin
        System.Move(
          FList[VIndex + 1],
          FList[VIndex],
          (FCount - VIndex) * SizeOf(TListenerRecord)
        );
        Pointer(FList[FCount].Listener) := nil;
        Pointer(FList[FCount].Rect) := nil;
      end;
    end;
  finally
    FSynchronizer.EndWrite;
  end;
end;

end.



