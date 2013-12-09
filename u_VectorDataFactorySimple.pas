unit u_VectorDataFactorySimple;

interface

uses
  t_Hash,
  t_GeoTypes,
  i_HashFunction,
  i_Appearance,
  i_HtmlToHintTextConverter,
  i_VectorItemLonLat,
  i_VectorDataItemSimple,
  i_VectorDataFactory,
  u_BaseInterfacedObject;

type
  TVectorDataFactorySimple = class(TBaseInterfacedObject, IVectorDataFactory)
  private
    FHashFunction: IHashFunction;
    FHintConverter: IHtmlToHintTextConverter;
  private
    function BuildPoint(
      const AIdData: Pointer;
      const AAppearance: IAppearance;
      const AName: string;
      const ADesc: string;
      const APoint: TDoublePoint
    ): IVectorDataItemPoint;
    function BuildPath(
      const AIdData: Pointer;
      const AAppearance: IAppearance;
      const AName: string;
      const ADesc: string;
      const ALine: IGeometryLonLatMultiLine
    ): IVectorDataItemLine;
    function BuildPoly(
      const AIdData: Pointer;
      const AAppearance: IAppearance;
      const AName: string;
      const ADesc: string;
      const APoly: IGeometryLonLatMultiPolygon
    ): IVectorDataItemPoly;
  public
    constructor Create(
      const AHashFunction: IHashFunction;
      const AHintConverter: IHtmlToHintTextConverter
    );
  end;

implementation

uses
  u_GeoFun,
  u_VectorDataItemPoint,
  u_VectorDataItemPolygon;

{ TVectorDataFactorySimple }

constructor TVectorDataFactorySimple.Create(
  const AHashFunction: IHashFunction;
  const AHintConverter: IHtmlToHintTextConverter
);
begin
  Assert(Assigned(AHashFunction));
  Assert(Assigned(AHintConverter));
  inherited Create;
  FHashFunction := AHashFunction;
  FHintConverter := AHintConverter;
end;

function TVectorDataFactorySimple.BuildPath(
  const AIdData: Pointer;
  const AAppearance: IAppearance;
  const AName, ADesc: string;
  const ALine: IGeometryLonLatMultiLine
): IVectorDataItemLine;
var
  VHash: THashValue;
begin
  Assert(Assigned(ALine));
  VHash := ALine.Hash;
  FHashFunction.UpdateHashByString(VHash, AName);
  FHashFunction.UpdateHashByString(VHash, ADesc);
  if Assigned(AAppearance) then begin
    FHashFunction.UpdateHashByHash(VHash, AAppearance.Hash);
  end;
  Result :=
    TVectorDataItemPath.Create(
      VHash,
      AAppearance,
      FHintConverter,
      AName,
      ADesc,
      ALine
    );
end;

function TVectorDataFactorySimple.BuildPoint(
  const AIdData: Pointer;
  const AAppearance: IAppearance;
  const AName, ADesc: string;
  const APoint: TDoublePoint
): IVectorDataItemPoint;
var
  VHash: THashValue;
begin
  Assert(not PointIsEmpty(APoint));
  VHash := FHashFunction.CalcHashByDoublePoint(APoint);
  FHashFunction.UpdateHashByString(VHash, AName);
  FHashFunction.UpdateHashByString(VHash, ADesc);
  if Assigned(AAppearance) then begin
    FHashFunction.UpdateHashByHash(VHash, AAppearance.Hash);
  end;
  Result :=
    TVectorDataItemPoint.Create(
      VHash,
      AAppearance,
      FHintConverter,
      AName,
      ADesc,
      APoint
    );
end;

function TVectorDataFactorySimple.BuildPoly(
  const AIdData: Pointer;
  const AAppearance: IAppearance;
  const AName, ADesc: string;
  const APoly: IGeometryLonLatMultiPolygon
): IVectorDataItemPoly;
var
  VHash: THashValue;
begin
  Assert(Assigned(APoly));
  VHash := APoly.Hash;
  FHashFunction.UpdateHashByString(VHash, AName);
  FHashFunction.UpdateHashByString(VHash, ADesc);
  if Assigned(AAppearance) then begin
    FHashFunction.UpdateHashByHash(VHash, AAppearance.Hash);
  end;
  Result :=
    TVectorDataItemPoly.Create(
      VHash,
      AAppearance,
      FHintConverter,
      AName,
      ADesc,
      APoly
    );
end;

end.
