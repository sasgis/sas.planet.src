unit u_FillingMapLayerConfigStatic;

interface

uses
  GR32,
  i_MapTypes,
  i_IFillingMapLayerConfig;

type
  TFillingMapLayerConfigStatic = class(TInterfacedObject, IFillingMapLayerConfigStatic)
  private
    FVisible: Boolean;
    FSourceMap: IMapType;
    FSourceZoom: Byte;
    FNoTileColor: TColor32;
    FShowTNE: Boolean;
    FTNEColor: TColor32;
  protected
    function GetVisible: Boolean;
    function GetSourceMap: IMapType;
    function GetSourceZoom: Byte;
    function GetNoTileColor: TColor32;
    function GetShowTNE: Boolean;
    function GetTNEColor: TColor32;
  public
    constructor Create(
      AVisible: Boolean;
      ASourceMap: IMapType;
      ASourceZoom: Byte;
      ANoTileColor: TColor32;
      AShowTNE: Boolean;
      ATNEColor: TColor32
    );
  end;

implementation

{ TFillingMapLayerConfigStatic }

constructor TFillingMapLayerConfigStatic.Create(AVisible: Boolean;
  ASourceMap: IMapType; ASourceZoom: Byte; ANoTileColor: TColor32;
  AShowTNE: Boolean; ATNEColor: TColor32);
begin
  FVisible := AVisible;
  FSourceMap := ASourceMap;
  FSourceZoom := ASourceZoom;
  FNoTileColor := ANoTileColor;
  FShowTNE := AShowTNE;
  FTNEColor := ATNEColor;
end;

function TFillingMapLayerConfigStatic.GetNoTileColor: TColor32;
begin
  Result := FNoTileColor;
end;

function TFillingMapLayerConfigStatic.GetShowTNE: Boolean;
begin
  Result := FShowTNE;
end;

function TFillingMapLayerConfigStatic.GetSourceMap: IMapType;
begin
  Result := FSourceMap;
end;

function TFillingMapLayerConfigStatic.GetSourceZoom: Byte;
begin
  Result := FSourceZoom
end;

function TFillingMapLayerConfigStatic.GetTNEColor: TColor32;
begin
  Result := FTNEColor;
end;

function TFillingMapLayerConfigStatic.GetVisible: Boolean;
begin
  Result := FVisible;
end;

end.
