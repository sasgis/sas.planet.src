unit u_LocalCoordConverterFactory;

interface

uses
  Types,
  t_Hash,
  t_GeoTypes,
  i_HashFunction,
  i_ProjectionInfo,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactory,
  u_HashCacheWithQueuesAbstract;

type
  TLocalCoordConverterFactory = class(THashCacheWithQueuesAbstract, ILocalCoordConverterFactory)
  private
    FHashFunction: IHashFunction;
  protected
    function CreateByKey(
      const AKey: THashValue;
      AData: Pointer
    ): IInterface; override;
  private
    function CreateNoScaleIntDelta(
      const ALocalRect: TRect;
      const AProjection: IProjectionInfo;
      const AMapPixelAtLocalZero: TPoint
    ): ILocalCoordConverter;
    function CreateNoScale(
      const ALocalRect: TRect;
      const AProjection: IProjectionInfo;
      const AMapPixelAtLocalZero: TDoublePoint
    ): ILocalCoordConverter;
    function CreateScaled(
      const ALocalRect: TRect;
      const AProjection: IProjectionInfo;
      const AMapScale: Double;
      const AMapPixelAtLocalZero: TDoublePoint
    ): ILocalCoordConverter;
  public
    constructor Create(
      const AHashFunction: IHashFunction
    );
  end;

implementation

uses
  u_LocalCoordConverter;

type
  PDataRecord = ^TDataRecord;
  TDataRecord = record
    Hash: THashValue;
    LocalRect: TRect;
    Projection: IProjectionInfo;
    MapPixelAtLocalZeroDouble: TDoublePoint;
    MapPixelAtLocalZeroInteger: TPoint;
    MapScale: Double;
    ConverterType: (ctNoScale, ctNoScaleIntDelta, ctScaled);
  end;

{ TLocalCoordConverterFactory }

constructor TLocalCoordConverterFactory.Create(
  const AHashFunction: IHashFunction
);
begin
  inherited Create(13, 0, 1024, 0); // 2^13 elements in hash-table, LRU 1024 elements
  FHashFunction := AHashFunction;
end;

function TLocalCoordConverterFactory.CreateByKey(
  const AKey: THashValue;
  AData: Pointer
): IInterface;
var
  VData: PDataRecord;
  VResult: ILocalCoordConverter;
begin
  inherited;
  VResult := nil;
  VData := PDataRecord(AData);
  case VData.ConverterType of
    ctNoScale: begin
      VResult :=
      TLocalCoordConverterNoScale.Create(
        VData.Hash,
        VData.LocalRect,
        VData.Projection,
        VData.MapPixelAtLocalZeroDouble
      );
    end;
    ctNoScaleIntDelta: begin
      VResult :=
        TLocalCoordConverterNoScaleIntDelta.Create(
          VData.Hash,
          VData.LocalRect,
          VData.Projection,
          VData.MapPixelAtLocalZeroInteger
        );
    end;
    ctScaled: begin
      VResult :=
        TLocalCoordConverter.Create(
          VData.Hash,
          VData.LocalRect,
          VData.Projection,
          VData.MapScale,
          VData.MapPixelAtLocalZeroDouble
        );
    end;
  end;

  Result := VResult;
end;

function TLocalCoordConverterFactory.CreateNoScale(
  const ALocalRect: TRect;
  const AProjection: IProjectionInfo;
  const AMapPixelAtLocalZero: TDoublePoint
): ILocalCoordConverter;
var
  VHash: THashValue;
  VData: TDataRecord;
begin
  VHash := $2eb7867c2318cc59;
  FHashFunction.UpdateHashByRect(VHash, ALocalRect);
  FHashFunction.UpdateHashByHash(VHash, AProjection.Hash);
  FHashFunction.UpdateHashByDoublePoint(VHash, AMapPixelAtLocalZero);
  VData.ConverterType := ctNoScale;
  VData.LocalRect := ALocalRect;
  VData.Projection := AProjection;
  VData.MapPixelAtLocalZeroDouble := AMapPixelAtLocalZero;

  Result := ILocalCoordConverter(GetOrCreateItem(VHash, @VData));
end;

function TLocalCoordConverterFactory.CreateNoScaleIntDelta(
  const ALocalRect: TRect;
  const AProjection: IProjectionInfo;
  const AMapPixelAtLocalZero: TPoint
): ILocalCoordConverter;
var
  VHash: THashValue;
  VData: TDataRecord;
begin
  VHash := $801bc862120f6bf5;
  FHashFunction.UpdateHashByRect(VHash, ALocalRect);
  FHashFunction.UpdateHashByHash(VHash, AProjection.Hash);
  FHashFunction.UpdateHashByPoint(VHash, AMapPixelAtLocalZero);
  VData.ConverterType := ctNoScaleIntDelta;
  VData.LocalRect := ALocalRect;
  VData.Projection := AProjection;
  VData.MapPixelAtLocalZeroInteger := AMapPixelAtLocalZero;

  Result := ILocalCoordConverter(GetOrCreateItem(VHash, @VData));
end;

function TLocalCoordConverterFactory.CreateScaled(
  const ALocalRect: TRect;
  const AProjection: IProjectionInfo;
  const AMapScale: Double;
  const AMapPixelAtLocalZero: TDoublePoint
): ILocalCoordConverter;
var
  VHash: THashValue;
  VData: TDataRecord;
begin
  VHash := $de6a45ffc3ed1159;
  FHashFunction.UpdateHashByRect(VHash, ALocalRect);
  FHashFunction.UpdateHashByHash(VHash, AProjection.Hash);
  FHashFunction.UpdateHashByDouble(VHash, AMapScale);
  FHashFunction.UpdateHashByDoublePoint(VHash, AMapPixelAtLocalZero);
  VData.ConverterType := ctScaled;
  VData.LocalRect := ALocalRect;
  VData.Projection := AProjection;
  VData.MapScale := AMapScale;
  VData.MapPixelAtLocalZeroDouble := AMapPixelAtLocalZero;

  Result := ILocalCoordConverter(GetOrCreateItem(VHash, @VData));
end;

end.
