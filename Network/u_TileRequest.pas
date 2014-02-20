unit u_TileRequest;

interface

uses
  Types,
  i_TileRequest,
  i_MapVersionInfo,
  u_BaseInterfacedObject;

type
  TTileRequest = class(TBaseInterfacedObject, ITileRequest)
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
      const ATile: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo
    );
  end;

  TTileRequestWithSizeCheck = class(TTileRequest, ITileRequestWithSizeCheck);

implementation

{ TTileRequest }

constructor TTileRequest.Create(
  const ATile: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo
);
begin
  inherited Create;
  FTile := ATile;
  FZoom := AZoom;
  FVersionInfo := AVersionInfo;
end;

function TTileRequest.GetTile: TPoint;
begin
  Result := FTile;
end;

function TTileRequest.GetVersionInfo: IMapVersionInfo;
begin
  Result := FVersionInfo;
end;

function TTileRequest.GetZoom: Byte;
begin
  Result := FZoom;
end;

end.
