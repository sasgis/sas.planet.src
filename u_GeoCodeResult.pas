unit u_GeoCodeResult;

interface

uses
  ActiveX,
  Classes,
  t_GeoTypes,
  i_GeoCoder;

type
  TGeoCodeResult = class(TInterfacedObject, IGeoCodeResult)
  private
    FSearchText: WideString;
    FList: IInterfaceList;
    function GetSearchText: WideString; safecall;
    function GetPlacemarks: IEnumUnknown; safecall;
    function GetPlacemarksCount: integer; safecall;
  public
    constructor Create(ASearchText: WideString; AList: IInterfaceList);
    destructor Destroy; override;
  end;

implementation

uses
  u_EnumUnknown;

{ TGeoCodeResult }

constructor TGeoCodeResult.Create(ASearchText: WideString;
  AList: IInterfaceList);
begin
  FSearchText := ASearchText;
  FList := AList;
end;

destructor TGeoCodeResult.Destroy;
begin
  FList := nil;
  FSearchText := '';
  inherited;
end;

function TGeoCodeResult.GetPlacemarks: IEnumUnknown;
begin
  Result := TEnumUnknown.Create(FList);
end;

function TGeoCodeResult.GetPlacemarksCount: integer;
begin
  Result := FList.Count;
end;

function TGeoCodeResult.GetSearchText: WideString;
begin
  Result := FSearchText;
end;

end.
 