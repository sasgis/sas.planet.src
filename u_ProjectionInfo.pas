unit u_ProjectionInfo;

interface

uses
  i_CoordConverter,
  i_ProjectionInfo;

type
  TProjectionInfo = class(TInterfacedObject, IProjectionInfo)
  private
    FGeoConverter: ICoordConverter;
    FZoom: Byte;
  private
    function GetIsSameProjectionInfo(AProjection: IProjectionInfo): Boolean;
    function GetZoom: Byte;
    function GetGeoConverter: ICoordConverter;
  public
    constructor Create(
      AGeoConverter: ICoordConverter;
      AZoom: Byte
    );
  end;

implementation

{ TProjectionInfo }

constructor TProjectionInfo.Create(AGeoConverter: ICoordConverter; AZoom: Byte);
begin
  FGeoConverter := AGeoConverter;
  FZoom := AZoom;
end;

function TProjectionInfo.GetGeoConverter: ICoordConverter;
begin
  Result := FGeoConverter;
end;

function TProjectionInfo.GetIsSameProjectionInfo(
  AProjection: IProjectionInfo): Boolean;
var
  VSelf: IProjectionInfo;
begin
  VSelf := Self;
  if VSelf = AProjection then begin
    Result := True;
  end else if AProjection = nil then begin
    Result := False;
  end else begin
    Result := False;
    if FZoom = AProjection.Zoom then begin
      if FGeoConverter.IsSameConverter(AProjection.GeoConverter) then begin
        Result := True;
      end;
    end;
  end;
end;

function TProjectionInfo.GetZoom: Byte;
begin
  Result := FZoom;
end;

end.
