unit u_ProjectionSetSimple;

interface

uses
  t_Hash,
  i_HashFunction,
  i_ProjectionInfo,
  i_ProjectionSet,
  i_CoordConverter,
  u_BaseInterfacedObject;

type
  TProjectionSetSimple = class(TBaseInterfacedObject, IProjectionSet)
  private
    FHash: THashValue;
    FGeoConverter: ICoordConverter;
    FZoomCount: Byte;
    FZooms: array of IProjectionInfo;
  private
    function GetHash: THashValue;
    function IsSame(const AProjectionSet: IProjectionSet): Boolean;

    function GetZoomCount: Byte;

    function GetZoom(const AIndex: Byte): IProjectionInfo;

    procedure ValidateZoom(var AZoom: Byte);
    function CheckZoom(const AZoom: Byte): Boolean;

    function GetSuitableProjection(const AProjection: IProjectionInfo): IProjectionInfo;
    function GetSuitableZoom(const AProjection: IProjectionInfo): Byte;
    function IsProjectionFromThisSet(const AProjection: IProjectionInfo): Boolean;

    function GetGeoConvert: ICoordConverter; // TODO: Deleate later
  public
    constructor Create(
      const AHashFunction: IHashFunction;
      const AGeoConverter: ICoordConverter
    );
  end;

implementation

uses
  u_ProjectionInfo;

{ TProjectionSetSimple }

constructor TProjectionSetSimple.Create(
  const AHashFunction: IHashFunction;
  const AGeoConverter: ICoordConverter
);
var
  i: Integer;
  VHash: THashValue;
begin
  Assert(Assigned(AHashFunction));
  Assert(Assigned(AGeoConverter));
  inherited Create;
  FHash := AGeoConverter.Hash;
  FGeoConverter := AGeoConverter;
  FZoomCount := 24;
  SetLength(FZooms, FZoomCount);
  for i := 0 to FZoomCount - 1 do begin
    VHash := AGeoConverter.Hash;
    AHashFunction.UpdateHashByInteger(VHash, i);
    FZooms[i] := TProjectionInfo.Create(VHash, FGeoConverter, i);
  end;
end;

function TProjectionSetSimple.GetGeoConvert: ICoordConverter;
begin
  Result := FGeoConverter;
end;

function TProjectionSetSimple.GetHash: THashValue;
begin
  Result := FHash;
end;

function TProjectionSetSimple.GetSuitableProjection(
  const AProjection: IProjectionInfo
): IProjectionInfo;
begin
  Result := FZooms[AProjection.Zoom]; // TODO: fix later
end;

function TProjectionSetSimple.GetSuitableZoom(
  const AProjection: IProjectionInfo
): Byte;
begin
  Result := AProjection.Zoom; // TODO: fix later
end;

function TProjectionSetSimple.GetZoom(const AIndex: Byte): IProjectionInfo;
begin
  Result := FZooms[AIndex];
end;

function TProjectionSetSimple.GetZoomCount: Byte;
begin
  Result := FZoomCount;
end;

function TProjectionSetSimple.IsProjectionFromThisSet(
  const AProjection: IProjectionInfo
): Boolean;
begin
  Assert(Assigned(AProjection));
  Result := False;
  if FGeoConverter.ProjectionType.IsSame(AProjection.ProjectionType) then begin
    if AProjection.Zoom < FZoomCount then begin
      // TODO: fix search zooms later
      Result := FZooms[AProjection.Zoom].GetIsSameProjectionInfo(AProjection);
    end;
  end;
end;

function TProjectionSetSimple.IsSame(
  const AProjectionSet: IProjectionSet
): Boolean;
var
  VSelf: IProjectionSet;
  i: Integer;
begin
  VSelf := Self;
  if VSelf = AProjectionSet then begin
    Result := True;
  end else if AProjectionSet = nil then begin
    Result := False;
  end else begin
    if (FHash <> 0) and (AProjectionSet.Hash <> 0) and (FHash <> AProjectionSet.Hash) then begin
      Result := False;
      Exit;
    end;
    Result := False;
    if FZoomCount = AProjectionSet.ZoomCount then begin
      Result := True;
      for i := 0 to FZoomCount - 1 do begin
        if not FZooms[i].GetIsSameProjectionInfo(AProjectionSet.Zooms[i]) then begin
          Result := False;
          Break;
        end;
      end;
    end;
  end;
end;

function TProjectionSetSimple.CheckZoom(const AZoom: Byte): Boolean;
begin
  Result := AZoom < FZoomCount;
end;

procedure TProjectionSetSimple.ValidateZoom(var AZoom: Byte);
begin
  if AZoom >= FZoomCount then begin
    AZoom := FZoomCount - 1;
  end;
end;

end.
