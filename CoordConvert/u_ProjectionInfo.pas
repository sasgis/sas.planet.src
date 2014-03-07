unit u_ProjectionInfo;

interface

uses
  t_Hash,
  i_CoordConverter,
  i_ProjectionInfo,
  u_BaseInterfacedObject;

type
  TProjectionInfo = class(TBaseInterfacedObject, IProjectionInfo)
  private
    FHash: THashValue;
    FGeoConverter: ICoordConverter;
    FZoom: Byte;
  private
    function GetHash: THashValue;
    function GetZoom: Byte;
    function GetGeoConverter: ICoordConverter;
    function GetIsSameProjectionInfo(const AProjection: IProjectionInfo): Boolean;
  public
    constructor Create(
      const AHash: THashValue;
      const AGeoConverter: ICoordConverter;
      AZoom: Byte
    );
  end;

implementation

{ TProjectionInfo }

constructor TProjectionInfo.Create(
  const AHash: THashValue;
  const AGeoConverter: ICoordConverter;
  AZoom: Byte
);
begin
  inherited Create;
  FHash := AHash;
  FGeoConverter := AGeoConverter;
  FZoom := AZoom;
end;

function TProjectionInfo.GetGeoConverter: ICoordConverter;
begin
  Result := FGeoConverter;
end;

function TProjectionInfo.GetHash: THashValue;
begin
  Result := FHash;
end;

function TProjectionInfo.GetIsSameProjectionInfo(
  const AProjection: IProjectionInfo): Boolean;
var
  VSelf: IProjectionInfo;
begin
  VSelf := Self;
  if VSelf = AProjection then begin
    Result := True;
  end else if AProjection = nil then begin
    Result := False;
  end else begin
    if (FHash <> 0) and (AProjection.Hash <> 0) and (FHash <> AProjection.Hash) then begin
      Result := False;
      Exit;
    end;
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
