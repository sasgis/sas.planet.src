unit u_TileKey;

interface

uses
  Types,
  i_MapVersionInfo,
  i_TileKey;

type
  TTileKey = class(TInterfacedObject, ITileKey)
  private
    FTile: TPoint;
    FZoom: Byte;
    FVersionInfo: IMapVersionInfo;
  private
    function GetTile: TPoint;
    function GetZoom: Byte;
    function GetVersionInfo: IMapVersionInfo;

    function IsSame(const AValue: ITileKey): Boolean;
  public
    constructor Create(
      const ATile: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo
    );
  end;

implementation

{ TTileKey }

constructor TTileKey.Create(
  const ATile: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo
);
begin
  FTile := ATile;
  FZoom := AZoom;
  FVersionInfo := AVersionInfo;
end;

function TTileKey.GetTile: TPoint;
begin
  Result := FTile;
end;

function TTileKey.GetVersionInfo: IMapVersionInfo;
begin
  Result := FVersionInfo;
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
      if FVersionInfo = AValue.VersionInfo then begin
        Result := True;
      end else if FVersionInfo <> nil then begin
        Result := FVersionInfo.IsSame(AValue.VersionInfo);
      end else begin
        Result := False;
      end;
    end;
  end;
end;

end.
