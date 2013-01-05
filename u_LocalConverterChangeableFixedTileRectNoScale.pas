unit u_LocalConverterChangeableFixedTileRectNoScale;

interface

uses
  i_Listener,
  i_Notifier,
  i_InternalPerformanceCounter,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe,
  i_LocalCoordConverterChangeable,
  u_BaseInterfacedObject;

type
  TLocalConverterChangeableFixedTileRectNoScale = class(TBaseInterfacedObject, ILocalCoordConverterChangeable)
  private
    FInternal: ILocalCoordConverterChangeableInternal;
    FSoruce: ILocalCoordConverterChangeable;
    FConverterFactory: ILocalCoordConverterFactorySimpe;
    FSourceListener: IListener;
    procedure OnSourceChange;
    function GetConverterForSource(
      const ACurrentCoordConverter: ILocalCoordConverter;
      const AVisualCoordConverter: ILocalCoordConverter
    ): ILocalCoordConverter;
  private
    function GetBeforeChangeNotifier: INotifier;
    function GetChangeNotifier: INotifier;
    function GetAfterChangeNotifier: INotifier;
    function GetStatic: ILocalCoordConverter;
  public
    constructor Create(
      const AChangeCounter: IInternalPerformanceCounter;
      const AConverterFactory: ILocalCoordConverterFactorySimpe;
      const ASoruce: ILocalCoordConverterChangeable
    );
    destructor Destroy; override;
  end;

implementation

uses
  Types,
  i_CoordConverter,
  u_SimpleFlagWithInterlock,
  u_ListenerByEvent,
  u_LocalCoordConverterChangeable;

{ TLocalConverterChangeableFixedTileRectNoScale }

constructor TLocalConverterChangeableFixedTileRectNoScale.Create(
  const AChangeCounter: IInternalPerformanceCounter;
  const AConverterFactory: ILocalCoordConverterFactorySimpe;
  const ASoruce: ILocalCoordConverterChangeable);
begin
  Assert(AChangeCounter <> nil);
  Assert(AConverterFactory <> nil);
  Assert(ASoruce <> nil);
  inherited Create;
  FSoruce := ASoruce;
  FConverterFactory := AConverterFactory;

  FInternal :=
    TLocalCoordConverterChangeable.Create(
      TSimpleFlagWithInterlock.Create,
      FSoruce.GetStatic,
      AChangeCounter
    );
  FSourceListener := TNotifyNoMmgEventListener.Create(Self.OnSourceChange);
  FSoruce.ChangeNotifier.Add(FSourceListener);
end;

destructor TLocalConverterChangeableFixedTileRectNoScale.Destroy;
begin
  if FSoruce <> nil then begin
    FSoruce.ChangeNotifier.Remove(FSourceListener);
    FSoruce := nil;
    FSourceListener := nil;
  end;
  inherited;
end;

function TLocalConverterChangeableFixedTileRectNoScale.GetAfterChangeNotifier: INotifier;
begin
  Result := FInternal.AfterChangeNotifier;
end;

function TLocalConverterChangeableFixedTileRectNoScale.GetBeforeChangeNotifier: INotifier;
begin
  Result := FInternal.BeforeChangeNotifier;
end;

function TLocalConverterChangeableFixedTileRectNoScale.GetChangeNotifier: INotifier;
begin
  Result := FInternal.ChangeNotifier;
end;

function TLocalConverterChangeableFixedTileRectNoScale.GetConverterForSource(
  const ACurrentCoordConverter: ILocalCoordConverter;
  const AVisualCoordConverter: ILocalCoordConverter
): ILocalCoordConverter;
var
  VZoom: Byte;
  VConverter: ICoordConverter;
  VViewSize: TPoint;
  VSourcePixelRect: TRect;
  VTileRect: TRect;
  VResultMapPixelRect: TRect;
  VResultLocalPixelRect: TRect;
begin
  VConverter := AVisualCoordConverter.GetGeoConverter;
  VZoom := AVisualCoordConverter.GetZoom;
  VSourcePixelRect := AVisualCoordConverter.GetRectInMapPixel;
  VViewSize.X := VSourcePixelRect.Right - VSourcePixelRect.Left;
  VViewSize.Y := VSourcePixelRect.Bottom - VSourcePixelRect.Top;
  VConverter.CheckPixelRect(VSourcePixelRect, VZoom);
  VTileRect := VConverter.PixelRect2TileRect(VSourcePixelRect, VZoom);
  VResultMapPixelRect := VConverter.TileRect2PixelRect(VTileRect, VZoom);

  if EqualRect(ACurrentCoordConverter.GetRectInMapPixel, VResultMapPixelRect) then begin
    Result := ACurrentCoordConverter;
  end else if EqualRect(VSourcePixelRect, VResultMapPixelRect) and (Abs(AVisualCoordConverter.GetScale - 1) < 0.001) then begin
    Result := AVisualCoordConverter;
  end else begin
    VResultLocalPixelRect :=
      Rect(
        0, 0,
        VResultMapPixelRect.Right - VResultMapPixelRect.Left,
        VResultMapPixelRect.Bottom - VResultMapPixelRect.Top
      );
    Result :=
      FConverterFactory.CreateConverterNoScale(
        VResultLocalPixelRect,
        VZoom,
        VConverter,
        VResultMapPixelRect.TopLeft
      )
  end;
end;

function TLocalConverterChangeableFixedTileRectNoScale.GetStatic: ILocalCoordConverter;
begin
  Result := FInternal.GetStatic;
end;

procedure TLocalConverterChangeableFixedTileRectNoScale.OnSourceChange;
begin
  FInternal.SetConverter(GetConverterForSource(FInternal.GetStatic, FSoruce.GetStatic));
end;

end.
