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
  TGeoCodePlacemarkInfoWithUrlDecorator = class(TBaseInterfacedObject, IGeoCodePlacemarkInfo, IVectorDataItemMainInfo)
  private
    FSource: IGeoCodePlacemarkInfo;
    FUrl: string;
  private
    function GetHash: THashValue;
    function GetName: string;
    function GetDesc: string;
    function IsEqual(const AItem: IVectorDataItemMainInfo): Boolean;
    function GetHintText: string;
    function GetInfoHTML: string;
    function GetInfoUrl: string;
    function GetInfoCaption: string;
    function GetAccuracy: Integer;
  public
    constructor Create(
      const ASource: IGeoCodePlacemarkInfo;
      const AUrl: string
    );
  end;

implementation

{ TGeoCodePlacemarkWithUrlDecorator }

constructor TGeoCodePlacemarkInfoWithUrlDecorator.Create(
  const ASource: IGeoCodePlacemarkInfo;
  const AUrl: string
);
begin
  inherited Create;
  FSource := ASource;
  FUrl := AUrl;
end;

function TGeoCodePlacemarkInfoWithUrlDecorator.GetAccuracy: Integer;
begin
  Result := FSource.GetAccuracy;
end;

function TGeoCodePlacemarkInfoWithUrlDecorator.GetDesc: string;
begin
  Result := FSource.Desc;
end;

function TGeoCodePlacemarkInfoWithUrlDecorator.GetHash: THashValue;
begin
  Result := FSource.Hash;
end;

function TGeoCodePlacemarkInfoWithUrlDecorator.GetHintText: string;
begin
  Result := FSource.GetHintText;
end;

function TGeoCodePlacemarkInfoWithUrlDecorator.GetInfoCaption: string;
begin
  Result := FSource.GetInfoCaption;
end;

function TGeoCodePlacemarkInfoWithUrlDecorator.GetInfoHTML: string;
begin
  Result := FSource.GetInfoHTML;
end;

function TGeoCodePlacemarkInfoWithUrlDecorator.GetInfoUrl: string;
begin
  Result := FUrl;
end;

function TGeoCodePlacemarkInfoWithUrlDecorator.GetName: string;
begin
  Result := FSource.Name;
end;

function TGeoCodePlacemarkInfoWithUrlDecorator.IsEqual(
  const AItem: IVectorDataItemMainInfo
): Boolean;
begin
  Result := FSource.IsEqual(AItem);
end;

end.
