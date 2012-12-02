unit u_TileKey;

interface

uses
  Types,
  i_TileKey,
  u_BaseInterfacedObject;

type
  TTileKey = class(TBaseInterfacedObject, ITileKey)
  private
    FTile: TPoint;
    FZoom: Byte;
  private
    function GetTile: TPoint;
    function GetZoom: Byte;

    function IsSame(const AValue: ITileKey): Boolean;
  public
    constructor Create(
      const ATile: TPoint;
      const AZoom: Byte
    );
  end;

implementation

{ TTileKey }

constructor TTileKey.Create(
  const ATile: TPoint;
  const AZoom: Byte
);
begin
  inherited Create;
  FTile := ATile;
  FZoom := AZoom;
end;

function TTileKey.GetTile: TPoint;
begin
  Result := FTile;
end;

function TTileKey.GetZoom: Byte;
begin
  Result := FZoom;
end;

function TTileKey.IsSame(const AValue: ITileKey): Boolean;
var
  VTile: TPoint;
begin
  if AValue = nil then begin
    Result := False;
  end else begin
    VTile := AValue.Tile;
    if (FTile.X <> VTile.X) or (FTile.Y <> VTile.Y) or (FZoom <> AValue.Zoom) then begin
      Result := False;
    end else begin
      Result := True;
    end;
  end;
end;

end.
