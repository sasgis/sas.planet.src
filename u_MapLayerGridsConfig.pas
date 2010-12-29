unit u_MapLayerGridsConfig;

interface

uses
  i_IConfigDataElement,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_MapLayerGridsConfig,
  u_ConfigDataElementComplexBase;

type
  TMapLayerGridsConfig = class(TConfigDataElementComplexBase, IMapLayerGridsConfig)
  private
    FTileGrid: ITileGridConfig;
    FGenShtabGrid: IGenShtabGridConfig;
  protected
    function GetTileGrid: ITileGridConfig;
    function GetGenShtabGrid: IGenShtabGridConfig;
  public
    constructor Create;
  end;
implementation

end.
