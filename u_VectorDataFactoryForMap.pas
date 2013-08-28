unit u_VectorDataFactoryForMap;

interface

uses
  Windows,
  t_Hash,
  t_GeoTypes,
  i_HashFunction,
  i_StringProvider,
  i_HtmlToHintTextConverter,
  i_VectorItemLonLat,
  i_VectorDataItemSimple,
  i_VectorDataFactory,
  u_BaseInterfacedObject;

type
  PIdData = ^TIdData;
  TIdData = record
    UrlPrefix: IStringProvider;
    NextIndex: Integer;
  end;

  TVectorDataFactoryForMap = class(TBaseInterfacedObject, IVectorDataFactory)
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
  u_VectorDataItemOfMapPoint,
  u_VectorDataItemOfMapPolygon;

{ TVectorDataFactoryForMap }

constructor TVectorDataFactoryForMap.Create(
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

function TVectorDataFactoryForMap.BuildPath(
  const AIdData: Pointer;
  const AName, ADesc: string;
  const ALine: ILonLatPath
): IVectorDataItemLine;
var
  VIndex: Integer;
  VHash: THashValue;
begin
  Assert(AIdData <> nil);
  Assert(Assigned(ALine));
  Result := nil;
  if AIdData <> nil then begin
    VIndex := InterlockedIncrement(PIdData(AIdData).NextIndex) - 1;
    VHash := ALine.Hash;
    FHashFunction.UpdateHashByString(VHash, AName);
    FHashFunction.UpdateHashByString(VHash, ADesc);
    Result :=
      TVectorDataItemOfMapPath.Create(
        VHash,
        FHintConverter,
        PIdData(AIdData).UrlPrefix,
        VIndex,
        AName,
        ADesc,
        ALine
      );
  end;
end;

function TVectorDataFactoryForMap.BuildPoint(
  const AIdData: Pointer;
  const AName, ADesc: string;
  const APoint: TDoublePoint
): IVectorDataItemPoint;
var
  VIndex: Integer;
  VHash: THashValue;
begin
  Assert(not PointIsEmpty(APoint));
  Assert(AIdData <> nil);
  Result := nil;
  if AIdData <> nil then begin
    VIndex := InterlockedIncrement(PIdData(AIdData).NextIndex) - 1;
    VHash := FHashFunction.CalcHashByDoublePoint(APoint);
    FHashFunction.UpdateHashByString(VHash, AName);
    FHashFunction.UpdateHashByString(VHash, ADesc);
    Result :=
      TVectorDataItemOfMapPoint.Create(
        VHash,
        FHintConverter,
        PIdData(AIdData).UrlPrefix,
        VIndex,
        AName,
        ADesc,
        APoint
      );
  end;
end;

function TVectorDataFactoryForMap.BuildPoly(
  const AIdData: Pointer;
  const AName, ADesc: string;
  const APoly: ILonLatPolygon
): IVectorDataItemPoly;
var
  VIndex: Integer;
  VHash: THashValue;
begin
  Assert(AIdData <> nil);
  Assert(Assigned(APoly));
  Result := nil;
  if AIdData <> nil then begin
    VIndex := InterlockedIncrement(PIdData(AIdData).NextIndex) - 1;
    VHash := APoly.Hash;
    FHashFunction.UpdateHashByString(VHash, AName);
    FHashFunction.UpdateHashByString(VHash, ADesc);
    Result :=
      TVectorDataItemOfMapPoly.Create(
        VHash,
        FHintConverter,
        PIdData(AIdData).UrlPrefix,
        VIndex,
        AName,
        ADesc,
        APoly
      );
  end;
end;

end.
