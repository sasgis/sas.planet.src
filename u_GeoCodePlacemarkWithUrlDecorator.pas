unit u_GeoCodePlacemarkWithUrlDecorator;

interface

uses
  t_Hash,
  t_GeoTypes,
  i_Appearance,
  i_LonLatRect,
  i_GeometryLonLat,
  i_VectorDataItemSimple,
  i_GeoCoder,
  u_BaseInterfacedObject;

type
  TGeoCodePlacemarkWithUrlDecorator = class(TBaseInterfacedObject, IGeoCodePlacemark, IVectorDataItemPoint, IVectorDataItemSimple)
  private
    FSource: IGeoCodePlacemark;
    FUrl: string;
  private
    function GetHash: THashValue;
    function GetAppearance: IAppearance;
    function GetPoint: IGeometryLonLatPoint;
    function GetName: string;
    function GetDesc: string;
    function GetGeometry: IGeometryLonLat;
    function GetLLRect: ILonLatRect;
    function IsEqual(const AItem: IVectorDataItemSimple): Boolean;
    function GetGoToLonLat: TDoublePoint;
    function GetHintText: string;
    function GetInfoHTML: string;
    function GetInfoUrl: string;
    function GetInfoCaption: string;
  private
    function GetAccuracy: Integer;
  public
    constructor Create(
      const ASource: IGeoCodePlacemark;
      const AUrl: string
    );
  end;

implementation

{ TGeoCodePlacemarkWithUrlDecorator }

constructor TGeoCodePlacemarkWithUrlDecorator.Create(
  const ASource: IGeoCodePlacemark; const AUrl: string);
begin
  inherited Create;
  FSource := ASource;
  FUrl := AUrl;
end;

function TGeoCodePlacemarkWithUrlDecorator.GetAccuracy: Integer;
begin
  Result := FSource.GetAccuracy;
end;

function TGeoCodePlacemarkWithUrlDecorator.GetAppearance: IAppearance;
begin
  Result := FSource.Appearance;
end;

function TGeoCodePlacemarkWithUrlDecorator.GetDesc: string;
begin
  Result := FSource.Desc;
end;

function TGeoCodePlacemarkWithUrlDecorator.GetGeometry: IGeometryLonLat;
begin
  Result := FSource.Geometry;
end;

function TGeoCodePlacemarkWithUrlDecorator.GetGoToLonLat: TDoublePoint;
begin
  Result := FSource.GetGoToLonLat;
end;

function TGeoCodePlacemarkWithUrlDecorator.GetHash: THashValue;
begin
  Result := FSource.Hash;
end;

function TGeoCodePlacemarkWithUrlDecorator.GetHintText: string;
begin
  Result := FSource.GetHintText;
end;

function TGeoCodePlacemarkWithUrlDecorator.GetInfoCaption: string;
begin
  Result := FSource.GetInfoCaption;
end;

function TGeoCodePlacemarkWithUrlDecorator.GetInfoHTML: string;
begin
  Result := FSource.GetInfoHTML;
end;

function TGeoCodePlacemarkWithUrlDecorator.GetInfoUrl: string;
begin
  Result := FUrl;
end;

function TGeoCodePlacemarkWithUrlDecorator.GetLLRect: ILonLatRect;
begin
  Result := FSource.LLRect;
end;

function TGeoCodePlacemarkWithUrlDecorator.GetName: string;
begin
  Result := FSource.Name;
end;

function TGeoCodePlacemarkWithUrlDecorator.GetPoint: IGeometryLonLatPoint;
begin
  Result := FSource.Point;
end;

function TGeoCodePlacemarkWithUrlDecorator.IsEqual(
  const AItem: IVectorDataItemSimple): Boolean;
begin
  Result := FSource.IsEqual(AItem);
end;

end.
