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

end.
