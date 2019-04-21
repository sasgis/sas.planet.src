unit u_MarkerProviderByAppearancePointIcon;

interface

uses
  GR32,
  t_Hash,
  i_HashFunction,
  i_InternalPerformanceCounter,
  i_HashInterfaceCache,


  i_Bitmap32Static,
  i_Bitmap32BufferFactory,
  i_BitmapMarker,
  i_AppearanceOfVectorItem,
  i_MarkerProviderByAppearancePointIcon,
  u_BaseInterfacedObject;

type
  TMarkerProviderByAppearancePointIcon = class(TBaseInterfacedObject, IMarkerProviderByAppearancePointIcon)
  private
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FMarkerDefault: IBitmapMarkerChangeable;
    FHashFunction: IHashFunction;
    FCache: IHashInterfaceCache;

    function ModifyMarkerWithResize(
      const ASourceMarker: IBitmapMarker;
      ASize: Integer
    ): IBitmapMarker;
  private
    function CreateByKey(
      const AKey: THashValue;
      const AData: Pointer
    ): IInterface;
  private
    function GetMarker(
      const AAppearance: IAppearancePointIcon
    ): IBitmapMarker;
  public
    constructor Create(
      const APerfCounterList: IInternalPerformanceCounterList;
      const AHashFunction: IHashFunction;
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const AMarkerDefault: IBitmapMarkerChangeable
    );
  end;


implementation

uses
  Types,
  SysUtils,
  GR32_Resamplers,
  t_GeoTypes,
  u_HashInterfaceCache2Q,
  u_Synchronizer,
  u_Bitmap32ByStaticBitmap,
  u_BitmapMarker,
  u_BitmapFunc,



  u_GeoFunc;

type
  PDataRecord = ^TDataRecord;

  TDataRecord = record
    Size: Integer;
    Marker: IBitmapMarker;
  end;

{ TMarkerProviderByAppearancePointIcon }

constructor TMarkerProviderByAppearancePointIcon.Create(
  const APerfCounterList: IInternalPerformanceCounterList;
  const AHashFunction: IHashFunction;
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const AMarkerDefault: IBitmapMarkerChangeable
);
begin
  Assert(Assigned(AHashFunction));
  Assert(Assigned(ABitmap32StaticFactory));
  inherited Create;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FMarkerDefault := AMarkerDefault;
  FHashFunction := AHashFunction;

  FCache :=
    THashInterfaceCache2Q.Create(
      GSync.SyncVariable.Make(Self.ClassName),
      APerfCounterList.CreateAndAddNewSubList('Cache'),
      Self.CreateByKey,
      14,  // 2^14 elements in hash-table
      1000,
      4000,
      1000
    );

end;

function TMarkerProviderByAppearancePointIcon.ModifyMarkerWithResize(
  const ASourceMarker: IBitmapMarker;
  ASize: Integer
): IBitmapMarker;
var
  VSizeSource: TPoint;
  VSizeTarget: TPoint;
  VBitmap: TBitmap32ByStaticBitmap;
  VFixedOnBitmap: TDoublePoint;
  VScale: Double;
  VSampler: TCustomResampler;
  VBitmapStatic: IBitmap32Static;
begin
  Result := nil;
  VSizeSource := ASourceMarker.Size;
  if VSizeSource.X > 0 then begin
    VScale := ASize / VSizeSource.X;
    VSizeTarget.X := Trunc(VSizeSource.X * VScale + 0.5);
    VSizeTarget.Y := Trunc(VSizeSource.Y * VScale + 0.5);
    VBitmap := TBitmap32ByStaticBitmap.Create(FBitmap32StaticFactory);
    try
      VBitmap.SetSize(VSizeTarget.X, VSizeTarget.Y);
      VSampler := TLinearResampler.Create;
      try
        StretchTransferFull(
          VBitmap,
          VBitmap.BoundsRect,
          ASourceMarker,
          VSampler,
          dmOpaque
        );
      finally
        VSampler.Free;
      end;
      VBitmapStatic := VBitmap.MakeAndClear;
    finally
      VBitmap.Free;
    end;
    VFixedOnBitmap := DoublePoint(ASourceMarker.AnchorPoint.X * VScale, ASourceMarker.AnchorPoint.Y * VScale);
    Result := TBitmapMarker.Create(VBitmapStatic, VFixedOnBitmap);
  end;
end;

function TMarkerProviderByAppearancePointIcon.CreateByKey(
  const AKey: THashValue;
  const AData: Pointer
): IInterface;
var
  VData: PDataRecord;
  VMarker: IBitmapMarker;
begin
  VData := PDataRecord(AData);
  VMarker := VData^.Marker;
  if VData^.Size <> VData^.Marker.Size.X then begin
    VMarker := ModifyMarkerWithResize(VMarker, VData.Size);
  end;
  Result := VMarker;
end;

function TMarkerProviderByAppearancePointIcon.GetMarker(
  const AAppearance: IAppearancePointIcon
): IBitmapMarker;
var
  VData: TDataRecord;
begin
  Result := nil;
  if Assigned(AAppearance) then begin
    if Assigned(AAppearance.Pic) then begin
      VData.Marker := AAppearance.Pic.GetMarker;
    end;
    if Assigned(VData.Marker) then begin
      VData.Size := AAppearance.MarkerSize;
      if VData.Size <= 0 then begin
        Result := nil;
      end else begin
        Result := IBitmapMarker(FCache.GetOrCreateItem(AAppearance.Hash, @VData));
      end;
      Exit;
    end;
  end;

  if not Assigned(Result) then begin
    if Assigned(FMarkerDefault) then begin
      Result := FMarkerDefault.GetStatic;
    end;
  end;
end;

end.
