unit u_VectorDataFactorySimple;

interface

uses
  t_Hash,
  i_HashFunction,
  i_Appearance,
  i_HtmlToHintTextConverter,
  i_GeometryLonLat,
  i_VectorDataItemSimple,
  i_VectorDataFactory,
  u_BaseInterfacedObject;

type
  TVectorDataItemMainInfoFactory = class(TBaseInterfacedObject, IVectorDataItemMainInfoFactory)
  private
    FHashFunction: IHashFunction;
    FHintConverter: IHtmlToHintTextConverter;
  private
    function BuildMainInfo(
      const AIdData: Pointer;
      const AName: string;
      const ADesc: string
    ): IVectorDataItemMainInfo;
  public
    constructor Create(
      const AHashFunction: IHashFunction;
      const AHintConverter: IHtmlToHintTextConverter
    );
  end;

  TVectorDataFactorySimple = class(TBaseInterfacedObject, IVectorDataFactory)
  private
    FHashFunction: IHashFunction;
  private
    function BuildPoint(
      const AMainInfo: IVectorDataItemMainInfo;
      const AAppearance: IAppearance;
      const APoint: IGeometryLonLatPoint
    ): IVectorDataItemPoint;
    function BuildPath(
      const AMainInfo: IVectorDataItemMainInfo;
      const AAppearance: IAppearance;
      const ALine: IGeometryLonLatMultiLine
    ): IVectorDataItemLine;
    function BuildPoly(
      const AMainInfo: IVectorDataItemMainInfo;
      const AAppearance: IAppearance;
      const APoly: IGeometryLonLatMultiPolygon
    ): IVectorDataItemPoly;
  public
    constructor Create(
      const AHashFunction: IHashFunction
    );
  end;

implementation

uses
  u_VectorDataItemBase,
  u_VectorDataItemPoint,
  u_VectorDataItemPolygon;

{ TVectorDataFactorySimple }

constructor TVectorDataFactorySimple.Create(
  const AHashFunction: IHashFunction
);
begin
  Assert(Assigned(AHashFunction));
  inherited Create;
  FHashFunction := AHashFunction;
end;

function TVectorDataFactorySimple.BuildPath(
  const AMainInfo: IVectorDataItemMainInfo;
  const AAppearance: IAppearance;
  const ALine: IGeometryLonLatMultiLine
): IVectorDataItemLine;
var
  VHash: THashValue;
begin
  Assert(Assigned(ALine));
  Assert(Assigned(AMainInfo));
  VHash := ALine.Hash;
  FHashFunction.UpdateHashByHash(VHash, AMainInfo.Hash);
  if Assigned(AAppearance) then begin
    FHashFunction.UpdateHashByHash(VHash, AAppearance.Hash);
  end;
  Result :=
    TVectorDataItemPath.Create(
      VHash,
      AAppearance,
      AMainInfo,
      ALine
    );
end;

function TVectorDataFactorySimple.BuildPoint(
  const AMainInfo: IVectorDataItemMainInfo;
  const AAppearance: IAppearance;
  const APoint: IGeometryLonLatPoint
): IVectorDataItemPoint;
var
  VHash: THashValue;
begin
  Assert(Assigned(APoint));
  VHash := APoint.Hash;
  FHashFunction.UpdateHashByHash(VHash, AMainInfo.Hash);
  if Assigned(AAppearance) then begin
    FHashFunction.UpdateHashByHash(VHash, AAppearance.Hash);
  end;
  Result :=
    TVectorDataItemPoint.Create(
      VHash,
      AAppearance,
      AMainInfo,
      APoint
    );
end;

function TVectorDataFactorySimple.BuildPoly(
  const AMainInfo: IVectorDataItemMainInfo;
  const AAppearance: IAppearance;
  const APoly: IGeometryLonLatMultiPolygon
): IVectorDataItemPoly;
var
  VHash: THashValue;
begin
  Assert(Assigned(APoly));
  VHash := APoly.Hash;
  FHashFunction.UpdateHashByHash(VHash, AMainInfo.Hash);
  if Assigned(AAppearance) then begin
    FHashFunction.UpdateHashByHash(VHash, AAppearance.Hash);
  end;
  Result :=
    TVectorDataItemPoly.Create(
      VHash,
      AAppearance,
      AMainInfo,
      APoly
    );
end;

{ TVectorDataItemMainInfoFactory }

constructor TVectorDataItemMainInfoFactory.Create(
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

function TVectorDataItemMainInfoFactory.BuildMainInfo(
  const AIdData: Pointer;
  const AName, ADesc: string
): IVectorDataItemMainInfo;
var
  VHash: THashValue;
begin
  VHash := FHashFunction.CalcHashByString(AName);
  FHashFunction.UpdateHashByString(VHash, ADesc);
  Result :=
    TVectorDataItemMainInfo.Create(
      VHash,
      FHintConverter,
      AName,
      ADesc
    );
end;

end.
