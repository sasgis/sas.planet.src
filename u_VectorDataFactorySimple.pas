unit u_VectorDataFactorySimple;

interface

uses
  t_GeoTypes,
  i_HtmlToHintTextConverter,
  i_VectorItemLonLat,
  i_VectorDataItemSimple,
  i_VectorDataFactory;

type
  TVectorDataFactorySimple = class(TInterfacedObject, IVectorDataFactory)
  private
    FHintConverter: IHtmlToHintTextConverter;
  private
    function BuildPoint(
      AId: string;
      AName: string;
      ADesc: string;
      APoint: TDoublePoint
    ): IVectorDataItemPoint;
    function BuildPath(
      AId: string;
      AName: string;
      ADesc: string;
      ALine: ILonLatPath
    ): IVectorDataItemLine;
    function BuildPoly(
      AId: string;
      AName: string;
      ADesc: string;
      APoly: ILonLatPolygon
    ): IVectorDataItemPoly;
  public
    constructor Create(
      AHintConverter: IHtmlToHintTextConverter
    );
  end;

implementation

uses
  u_VectorDataItemPoint,
  u_VectorDataItemPolygon;

{ TVectorDataFactorySimple }

constructor TVectorDataFactorySimple.Create(
  AHintConverter: IHtmlToHintTextConverter);
begin
  FHintConverter := AHintConverter;
end;

function TVectorDataFactorySimple.BuildPath(
  AId, AName, ADesc: string;
  ALine: ILonLatPath
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
  AId, AName, ADesc: string;
  APoint: TDoublePoint
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
  AId, AName, ADesc: string;
  APoly: ILonLatPolygon
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
