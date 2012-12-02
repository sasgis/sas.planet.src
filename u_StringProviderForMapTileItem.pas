unit u_StringProviderForMapTileItem;

interface

uses
  Types,
  i_StringProvider,
  u_BaseInterfacedObject;

type
  TStringProviderForMapTileItem = class(TBaseInterfacedObject, IStringProvider)
  private
    FURLPrefix: string;
    FTile: TPoint;
    FZoom: Byte;
  private
    function GetValue: string;
  public
    constructor Create(
      const AURLPrefix: string;
      const ATile: TPoint;
      const AZoom: Byte
    );
  end;

implementation

uses
  SysUtils;

{ TUrlForMapTileItem }

constructor TStringProviderForMapTileItem.Create(
  const AURLPrefix: string;
  const ATile: TPoint;
  const AZoom: Byte
);
begin
  inherited Create;
  FURLPrefix := AURLPrefix;
  FTile := ATile;
  FZoom := AZoom;
end;

function TStringProviderForMapTileItem.GetValue: string;
begin
  Result :=
    FURLPrefix +
    IntToStr(FZoom) + '/' +
    IntToStr(FTile.X) + '/' +
    IntToStr(FTile.Y) + '/';
end;

end.
