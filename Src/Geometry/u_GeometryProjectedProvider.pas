unit u_GeometryProjectedProvider;

interface

uses
  t_Hash,
  i_ProjectionInfo,
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
      const AProjectionInfo: IProjectionInfo;
      const ALine: IGeometryLonLatLine
    ): IGeometryProjectedLine;
    function GetProjectedPolygon(
      const AProjectionInfo: IProjectionInfo;
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
  i_CoordConverter,
  u_HashInterfaceCache2Q,
  u_Synchronizer;

type
  PDataRecord = ^TDataRecord;

  TDataRecord = record
    Path: IGeometryLonLatLine;
    Polygon: IGeometryLonLatPolygon;
    ProjectionInfo: IProjectionInfo;
  end;


{ TProjectedGeometryProvider }

const
  CMinProjectedSize = 10;

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
  VResultPath: IGeometryProjectedMultiLine;
  VResultPolygon: IGeometryProjectedMultiPolygon;
  VGeoConverter: ICoordConverter;
  VTestArrLenLonLatRect: TDoubleRect;
  VTestArrLenPixelRect: TDoubleRect;
begin
  Result := nil;
  VData := PDataRecord(AData);
  VGeoConverter := VData^.ProjectionInfo.GeoConverter;
  if Assigned(VData^.Path) then begin
    VTestArrLenLonLatRect := VData^.Path.Bounds.Rect;
    VGeoConverter.CheckLonLatRect(VTestArrLenLonLatRect);
    VTestArrLenPixelRect :=
      VGeoConverter.LonLatRect2PixelRectFloat(
        VTestArrLenLonLatRect,
        VData^.ProjectionInfo.Zoom
      );
    if (abs(VTestArrLenPixelRect.Left - VTestArrLenPixelRect.Right) > CMinProjectedSize) or
      (abs(VTestArrLenPixelRect.Top - VTestArrLenPixelRect.Bottom) > CMinProjectedSize) then begin
      VResultPath :=
        FVectorGeometryProjectedFactory.CreateProjectedPathByLonLatPath(
          VData^.ProjectionInfo,
          VData^.Path
        );
    end else begin
      VResultPath :=
        FVectorGeometryProjectedFactory.CreateProjectedPath(
          nil,
          0
        );
    end;
    Result := VResultPath;
  end else if Assigned(VData^.Polygon) then begin
    VTestArrLenLonLatRect := VData^.Polygon.Bounds.Rect;
    VGeoConverter.CheckLonLatRect(VTestArrLenLonLatRect);
    VTestArrLenPixelRect :=
      VGeoConverter.LonLatRect2PixelRectFloat(
        VTestArrLenLonLatRect,
        VData^.ProjectionInfo.Zoom
      );
    if (abs(VTestArrLenPixelRect.Left - VTestArrLenPixelRect.Right) > CMinProjectedSize) or
      (abs(VTestArrLenPixelRect.Top - VTestArrLenPixelRect.Bottom) > CMinProjectedSize) then begin
      VResultPolygon :=
        FVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
          VData^.ProjectionInfo,
          VData^.Polygon
        );
    end else begin
      VResultPolygon :=
        FVectorGeometryProjectedFactory.CreateProjectedPolygon(
          nil,
          0
        );
    end;
    Result := VResultPolygon;
  end;
end;

function TGeometryProjectedProvider.GetProjectedPath(
  const AProjectionInfo: IProjectionInfo;
  const ALine: IGeometryLonLatLine
): IGeometryProjectedLine;
var
  VHash: THashValue;
  VData: TDataRecord;
begin
  VHash := $fbcb5f3e1bef5742;
  FHashFunction.UpdateHashByHash(VHash, ALine.Hash);
  FHashFunction.UpdateHashByHash(VHash, AProjectionInfo.Hash);
  VData.Path := ALine;
  VData.Polygon := nil;
  VData.ProjectionInfo := AProjectionInfo;

  Result := IGeometryProjectedMultiLine(FCache.GetOrCreateItem(VHash, @VData));
end;

function TGeometryProjectedProvider.GetProjectedPolygon(
  const AProjectionInfo: IProjectionInfo;
  const ALine: IGeometryLonLatPolygon
): IGeometryProjectedPolygon;
var
  VHash: THashValue;
  VData: TDataRecord;
begin
  VHash := $5af2a0463bf6e921;
  FHashFunction.UpdateHashByHash(VHash, ALine.Hash);
  FHashFunction.UpdateHashByHash(VHash, AProjectionInfo.Hash);
  VData.Path := nil;
  VData.Polygon := ALine;
  VData.ProjectionInfo := AProjectionInfo;

  Result := IGeometryProjectedMultiPolygon(FCache.GetOrCreateItem(VHash, @VData));
end;

end.
