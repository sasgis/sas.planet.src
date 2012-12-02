unit u_VectorDataFactorySimple;

interface

uses
  t_GeoTypes,
  i_HtmlToHintTextConverter,
  i_VectorItemLonLat,
  i_VectorDataItemSimple,
  i_VectorDataFactory,
  u_BaseInterfacedObject;

type
  TVectorDataFactorySimple = class(TBaseInterfacedObject, IVectorDataFactory)
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
  u_VectorDataItemPoint,
  u_VectorDataItemPolygon;

{ TVectorDataFactorySimple }

constructor TVectorDataFactorySimple.Create(
  const AHintConverter: IHtmlToHintTextConverter
);
begin
  inherited Create;
  FHintConverter := AHintConverter;
end;

function TVectorDataFactorySimple.BuildPath(
  const AIdData: Pointer;
  const AName, ADesc: string;
  const ALine: ILonLatPath
): IVectorDataItemLine;
begin
  Result :=
    TVectorDataItemPath.Create(
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
begin
  Result :=
    TVectorDataItemPoint.Create(
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
begin
  Result :=
    TVectorDataItemPoly.Create(
      FHintConverter,
      AName,
      ADesc,
      APoly
    );
end;

end.
