unit u_GeometryProjectedProvider;

interface

uses
  t_Hash,
  i_Projection,
  i_GeometryLonLat,
  i_GeometryProjected,
  i_GeometryProjectedProvider,
  i_HashFunction,
  i_GeometryProjectedFactory,
  i_HashInterfaceCache,
  u_BaseInterfacedObject;

type
  TGeometryProjectedProvider = class(TBaseInterfacedObject, IGeometryProjectedProvider)
  private
    FHashFunction: IHashFunction;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FCache: IHashInterfaceCache;
  private
    function CreateByKey(
      const AKey: THashValue;
      const AData: Pointer
    ): IInterface;
  private
    function GetProjectedPath(
      const AProjection: IProjection;
      const ALine: IGeometryLonLatLine
    ): IGeometryProjectedLine;
    function GetProjectedPolygon(
      const AProjection: IProjection;
      const ALine: IGeometryLonLatPolygon
    ): IGeometryProjectedPolygon;
  public
    constructor Create(
      const AHashFunction: IHashFunction;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory
    );
  end;

implementation

uses
  t_GeoTypes,
  u_HashInterfaceCache2Q,
  u_Synchronizer;

type
  PDataRecord = ^TDataRecord;

  TDataRecord = record
    Path: IGeometryLonLatLine;
    Polygon: IGeometryLonLatPolygon;
    Projection: IProjection;
  end;


{ TProjectedGeometryProvider }

const
  CMinProjectedLineSize = 1;
  CMinProjectedPolygonSize = 10;

constructor TGeometryProjectedProvider.Create(
  const AHashFunction: IHashFunction;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory
);
begin
  inherited Create;
  FHashFunction := AHashFunction;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FCache :=
    THashInterfaceCache2Q.Create(
      GSync.SyncVariable.Make(Self.ClassName),
      Self.CreateByKey,
      14,  // 2^14 elements in hash-table
      1000,
      4000,
      1000
    );
end;

function TGeometryProjectedProvider.CreateByKey(
  const AKey: THashValue;
  const AData: Pointer
): IInterface;
var
  VData: PDataRecord;
  VResultPath: IGeometryProjectedLine;
  VResultPolygon: IGeometryProjectedPolygon;
  VProjection: IProjection;
  VTestArrLenLonLatRect: TDoubleRect;
  VTestArrLenPixelRect: TDoubleRect;
begin
  Result := nil;
  VData := PDataRecord(AData);
  VProjection := VData^.Projection;
  if Assigned(VData^.Path) then begin
    VTestArrLenLonLatRect := VData^.Path.Bounds.Rect;
    VProjection.ProjectionType.ValidateLonLatRect(VTestArrLenLonLatRect);
    VTestArrLenPixelRect :=
      VProjection.LonLatRect2PixelRectFloat(
        VTestArrLenLonLatRect
      );
    if (abs(VTestArrLenPixelRect.Left - VTestArrLenPixelRect.Right) > CMinProjectedLineSize) or
      (abs(VTestArrLenPixelRect.Top - VTestArrLenPixelRect.Bottom) > CMinProjectedLineSize) then begin
      VResultPath :=
        FVectorGeometryProjectedFactory.CreateProjectedLineByLonLatPath(
          VData^.Projection,
          VData^.Path
        );
    end else begin
      VResultPath := nil;
    end;
    Result := VResultPath;
  end else if Assigned(VData^.Polygon) then begin
    VTestArrLenLonLatRect := VData^.Polygon.Bounds.Rect;
    VProjection.ProjectionType.ValidateLonLatRect(VTestArrLenLonLatRect);
    VTestArrLenPixelRect :=
      VProjection.LonLatRect2PixelRectFloat(
        VTestArrLenLonLatRect
      );
    if (abs(VTestArrLenPixelRect.Left - VTestArrLenPixelRect.Right) > CMinProjectedPolygonSize) or
      (abs(VTestArrLenPixelRect.Top - VTestArrLenPixelRect.Bottom) > CMinProjectedPolygonSize) then begin
      VResultPolygon :=
        FVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
          VData^.Projection,
          VData^.Polygon
        );
    end else begin
      VResultPolygon := nil;
    end;
    Result := VResultPolygon;
  end;
end;

function TGeometryProjectedProvider.GetProjectedPath(
  const AProjection: IProjection;
  const ALine: IGeometryLonLatLine
): IGeometryProjectedLine;
var
  VHash: THashValue;
  VData: TDataRecord;
begin
  VHash := $fbcb5f3e1bef5742;
  FHashFunction.UpdateHashByHash(VHash, ALine.Hash);
  FHashFunction.UpdateHashByHash(VHash, AProjection.Hash);
  VData.Path := ALine;
  VData.Polygon := nil;
  VData.Projection := AProjection;

  Result := IGeometryProjectedMultiLine(FCache.GetOrCreateItem(VHash, @VData));
end;

function TGeometryProjectedProvider.GetProjectedPolygon(
  const AProjection: IProjection;
  const ALine: IGeometryLonLatPolygon
): IGeometryProjectedPolygon;
var
  VHash: THashValue;
  VData: TDataRecord;
begin
  VHash := $5af2a0463bf6e921;
  FHashFunction.UpdateHashByHash(VHash, ALine.Hash);
  FHashFunction.UpdateHashByHash(VHash, AProjection.Hash);
  VData.Path := nil;
  VData.Polygon := ALine;
  VData.Projection := AProjection;

  Result := IGeometryProjectedMultiPolygon(FCache.GetOrCreateItem(VHash, @VData));
end;

end.
