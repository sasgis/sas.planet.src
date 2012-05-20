unit u_LonLatRectUpdateNotifier;

interface

uses
  SysUtils,
  t_GeoTypes,
  i_JclNotify,
  i_LonLatRect,
  i_LonLatRectUpdateNotifier;

type
  TListenerRecord = record
    Listener: IJclListener;
    Rect: TDoubleRect;
  end;

  TLonLatRectUpdateNotifier = class(TInterfacedObject, ILonLatRectUpdateNotifier, ILonLatRectUpdateNotifierInternal)
  private
    FSynchronizer: IReadWriteSync;
    FCount: Integer;
    FList: array of TListenerRecord;
    function CalcGrowSize(AOldSize: Integer): Integer;
  private
    procedure Add(
      const AListener: IJclListener;
      const ARect: TDoubleRect
    ); stdcall;
    procedure Remove(const AListener: IJclListener); stdcall;
  private
    procedure RectUpdateNotify(const ARect: ILonLatRect); overload; stdcall;
    procedure RectUpdateNotify(const ARect: TDoubleRect); overload; stdcall;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  u_GeoFun,
  u_LonLatRect;

{ TLonLatRectUpdateNotifier }

constructor TLonLatRectUpdateNotifier.Create;
begin
  inherited Create;
  FSynchronizer := TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TLonLatRectUpdateNotifier.Destroy;
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do begin
    FList[i].Listener := nil;
  end;
  FList := nil;
  FSynchronizer := nil;
  inherited;
end;

procedure TLonLatRectUpdateNotifier.Add(
  const AListener: IJclListener;
  const ARect: TDoubleRect
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
      if IsIntersecLonLatRect(FList[i].Rect, ARect) then begin
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
      if ARect.IsIntersecWithRect(FList[i].Rect) then begin
        FList[i].Listener.Notification(ARect);
      end;
    end;
  finally
    FSynchronizer.EndRead;
  end;
end;

procedure TLonLatRectUpdateNotifier.Remove(const AListener: IJclListener);
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
        System.Move(
          FList[VIndex + 1],
          FList[VIndex],
          (FCount - VIndex) * SizeOf(TListenerRecord)
        );
        Pointer(FList[FCount].Listener) := nil;
      end;
    end;
  finally
    FSynchronizer.EndWrite;
  end;
end;

end.
