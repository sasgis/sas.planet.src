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

    function IsSame(AValue: ITileKey): Boolean;
  public
    constructor Create(
      ATile: TPoint;
      AZoom: Byte;
      AVersionInfo: IMapVersionInfo
    );
  end;

implementation

{ TTileKey }

constructor TTileKey.Create(
  ATile: TPoint;
  AZoom: Byte;
  AVersionInfo: IMapVersionInfo
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

function TTileKey.IsSame(AValue: ITileKey): Boolean;
begin
  if AValue = nil then begin
    Result := False;
  end else begin
    if (FTile <> AValue.Tile) or (FZoom <> AValue.Zoom) then begin
      Result := False;
    end else begin
      if FVersionInfo = AValue.VersionInfo then begin
        Result := True;
      end else if FVersionInfo <> nil then begin
        Result := FVersionInfo.IsSame(AValue.VersionInfo);
      end;
    end;
  end;
end;

end.
