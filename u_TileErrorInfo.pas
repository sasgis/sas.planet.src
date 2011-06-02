unit u_TileErrorInfo;

interface

uses
  Types,
  u_MapType,
  i_TileError;

type
  TTileErrorInfo = class(TInterfacedObject, ITileErrorInfo)
  private
    FMapType: TMapType;
    FZoom: Byte;
    FTile: TPoint;
    FErrorText: string;
  protected
    function GetMapType: TMapType;
    function GetZoom: Byte;
    function GetTile: TPoint;
    function GetErrorText: string;
  public
    constructor Create(
      AMapType: TMapType;
      AZoom: Byte;
      ATile: TPoint;
      AErrorText: string
    );
  end;

implementation

{ TTileErrorInfo }

constructor TTileErrorInfo.Create(AMapType: TMapType; AZoom: Byte;
  ATile: TPoint; AErrorText: string);
begin
  FMapType := AMapType;
  FZoom := AZoom;
  FTile := ATile;
  FErrorText := AErrorText;
end;

function TTileErrorInfo.GetErrorText: string;
begin
  Result := FErrorText;
end;

function TTileErrorInfo.GetMapType: TMapType;
begin
  Result := FMapType;
end;

function TTileErrorInfo.GetTile: TPoint;
begin
  Result := FTile;
end;

function TTileErrorInfo.GetZoom: Byte;
begin
  Result := FZoom;
end;

end.
