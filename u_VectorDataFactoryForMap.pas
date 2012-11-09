unit u_VectorDataFactoryForMap;

interface

uses
  Types,
  t_GeoTypes,
  i_HtmlToHintTextConverter,
  i_VectorItemLonLat,
  i_VectorDataItemSimple,
  i_VectorDataFactory;

type
  PIdData = ^TIdData;
  TIdData = record
    Tile: TPoint;
    NextIndex: Integer;
    Zoom: Byte;
  end;

  TVectorDataFactoryForMap = class(TInterfacedObject, IVectorDataFactory)
  private
    FHintConverter: IHtmlToHintTextConverter;
    FURLPrefix: string;
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
      const AURLPrefix: string;
      const AHintConverter: IHtmlToHintTextConverter
    );
  end;

implementation

uses
  u_VectorDataItemPoint,
  u_VectorDataItemPolygon;

{ TVectorDataFactoryForMap }

constructor TVectorDataFactoryForMap.Create(const AURLPrefix: string;
  const AHintConverter: IHtmlToHintTextConverter);
begin
  inherited Create;
  FURLPrefix := AURLPrefix;
  FHintConverter := AHintConverter;
end;

function TVectorDataFactoryForMap.BuildPath(
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

function TVectorDataFactoryForMap.BuildPoint(
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

function TVectorDataFactoryForMap.BuildPoly(
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
