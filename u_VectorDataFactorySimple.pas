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
  VHash := ALine.Hash;
  if AName <> '' then begin
    VHash := FHashFunction.CalcHashWithSeed(@AName[1], Length(AName) * SizeOf(Char), VHash);
  end;
  if ADesc <> '' then begin
    VHash := FHashFunction.CalcHashWithSeed(@ADesc[1], Length(ADesc) * SizeOf(Char), VHash);
  end;
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
  VHash := FHashFunction.CalcHash(@APoint, SizeOf(APoint));
  if AName <> '' then begin
    VHash := FHashFunction.CalcHashWithSeed(@AName[1], Length(AName) * SizeOf(Char), VHash);
  end;
  if ADesc <> '' then begin
    VHash := FHashFunction.CalcHashWithSeed(@ADesc[1], Length(ADesc) * SizeOf(Char), VHash);
  end;
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
  VHash := APoly.Hash;
  if AName <> '' then begin
    VHash := FHashFunction.CalcHashWithSeed(@AName[1], Length(AName) * SizeOf(Char), VHash);
  end;
  if ADesc <> '' then begin
    VHash := FHashFunction.CalcHashWithSeed(@ADesc[1], Length(ADesc) * SizeOf(Char), VHash);
  end;
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
