unit u_VectorDataFactorySimple;

interface

uses
  t_Hash,
  t_GeoTypes,
  i_HashFunction,
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
      const AName: string;
      const ADesc: string;
      const APoint: TDoublePoint
    ): IVectorDataItemPoint;
    function BuildPath(
      const AIdData: Pointer;
      const AName: string;
      const ADesc: string;
      const ALine: ILonLatPath
    ): IVectorDataItemLine;
    function BuildPoly(
      const AIdData: Pointer;
      const AName: string;
      const ADesc: string;
      const APoly: ILonLatPolygon
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
  const AName, ADesc: string;
  const ALine: ILonLatPath
): IVectorDataItemLine;
var
  VHash: THashValue;
begin
  Assert(Assigned(ALine));
  VHash := ALine.Hash;
  FHashFunction.UpdateHashByString(VHash, AName);
  FHashFunction.UpdateHashByString(VHash, ADesc);
  Result :=
    TVectorDataItemPath.Create(
      VHash,
      FHintConverter,
      AName,
      ADesc,
      ALine
    );
end;

function TVectorDataFactorySimple.BuildPoint(
  const AIdData: Pointer;
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
  Result :=
    TVectorDataItemPoint.Create(
      VHash,
      FHintConverter,
      AName,
      ADesc,
      APoint
    );
end;

function TVectorDataFactorySimple.BuildPoly(
  const AIdData: Pointer;
  const AName, ADesc: string;
  const APoly: ILonLatPolygon
): IVectorDataItemPoly;
var
  VHash: THashValue;
begin
  Assert(Assigned(APoly));
  VHash := APoly.Hash;
  FHashFunction.UpdateHashByString(VHash, AName);
  FHashFunction.UpdateHashByString(VHash, ADesc);
  Result :=
    TVectorDataItemPoly.Create(
      VHash,
      FHintConverter,
      AName,
      ADesc,
      APoly
    );
end;

end.
