unit u_VectorDataFactoryForMap;

interface

uses
  Windows,
  t_GeoTypes,
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
      const AHintConverter: IHtmlToHintTextConverter
    );
  end;

implementation

uses
  u_VectorDataItemOfMapPoint,
  u_VectorDataItemOfMapPolygon;

{ TVectorDataFactoryForMap }

constructor TVectorDataFactoryForMap.Create(
  const AHintConverter: IHtmlToHintTextConverter
);
begin
  inherited Create;
  FHintConverter := AHintConverter;
end;

function TVectorDataFactoryForMap.BuildPath(
  const AIdData: Pointer;
  const AName, ADesc: string;
  const ALine: ILonLatPath
): IVectorDataItemLine;
var
  VIndex: Integer;
begin
  Assert(AIdData <> nil);
  Result := nil;
  if AIdData <> nil then begin
    VIndex := InterlockedIncrement(PIdData(AIdData).NextIndex) - 1;
    Result :=
      TVectorDataItemOfMapPath.Create(
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
begin
  Assert(AIdData <> nil);
  Result := nil;
  if AIdData <> nil then begin
    VIndex := InterlockedIncrement(PIdData(AIdData).NextIndex) - 1;
    Result :=
      TVectorDataItemOfMapPoint.Create(
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
begin
  Assert(AIdData <> nil);
  Result := nil;
  if AIdData <> nil then begin
    VIndex := InterlockedIncrement(PIdData(AIdData).NextIndex) - 1;
    Result :=
      TVectorDataItemOfMapPoly.Create(
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
