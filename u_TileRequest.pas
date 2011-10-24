unit u_TileRequest;

interface

uses
  Types,
  i_ZmpInfo,
  i_TileRequest,
  i_MapVersionInfo;

type
  TTileRequest = class(TInterfacedObject, ITileRequest)
  private
    FZmp: IZmpInfo;
    FTile: TPoint;
    FZoom: Byte;
    FVersionInfo: IMapVersionInfo;
  protected
    function GetZmp: IZmpInfo;
    function GetTile: TPoint;
    function GetZoom: Byte;
    function GetVersionInfo: IMapVersionInfo;
  public
    constructor Create(
      AZmp: IZmpInfo;
      ATile: TPoint;
      AZoom: Byte;
      AVersionInfo: IMapVersionInfo
    );
  end;

implementation

{ TTileRequest }

constructor TTileRequest.Create(
  AZmp: IZmpInfo;
  ATile: TPoint;
  AZoom: Byte;
  AVersionInfo: IMapVersionInfo
);
begin
  FZmp := AZmp;
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

function TTileRequest.GetZmp: IZmpInfo;
begin
  Result := FZmp;
end;

function TTileRequest.GetZoom: Byte;
begin
  Result := FZoom;
end;

end.
