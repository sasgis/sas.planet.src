unit u_ErrorInfoSimple;

interface

uses
  Types,
  i_ErrorInfo,
  i_TileRequestResult,
  u_BaseInterfacedObject;

type
  TErrorInfoSimple = class(TBaseInterfacedObject, IErrorInfoSimple)
  private
    FText: string;
  private
    function GetErrorText: string;
  public
    constructor Create(
      const AText: string
    );
  end;

  TErrorInfoByTileRequestResult = class(TBaseInterfacedObject, IErrorInfoSimple, IErrorInfoMapType, IErrorInfoTile)
  private
    FResult: ITileRequestResultError;
    FMapTypeGUID: TGUID;
  private
    function GetErrorText: string;
    function GetZoom: Byte;
    function GetTile: TPoint;
    function GetMapTypeGUID: TGUID;
  public
    constructor Create(
      const AResult: ITileRequestResultError;
      const AMapTypeGUID: TGUID
    );
  end;

implementation

{ TErrorInfoSimple }

constructor TErrorInfoSimple.Create(const AText: string);
begin
  inherited Create;
  FText := AText;
end;

function TErrorInfoSimple.GetErrorText: string;
begin
  Result := FText;
end;

{ TErrorInfoByTileRequestResult }

constructor TErrorInfoByTileRequestResult.Create(
  const AResult: ITileRequestResultError;
  const AMapTypeGUID: TGUID
);
begin
  inherited Create;
  FResult := AResult;
  FMapTypeGUID := AMapTypeGUID;
end;

function TErrorInfoByTileRequestResult.GetErrorText: string;
begin
  Result := FResult.ErrorText;
end;

function TErrorInfoByTileRequestResult.GetMapTypeGUID: TGUID;
begin
  Result := FMapTypeGUID;
end;

function TErrorInfoByTileRequestResult.GetTile: TPoint;
begin
  Result := FResult.Request.Tile;
end;

function TErrorInfoByTileRequestResult.GetZoom: Byte;
begin
  Result := FResult.Request.Zoom;
end;

end.
