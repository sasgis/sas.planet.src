{******************************************************************************}
{* SAS.Планета (SAS.Planet)                                                   *}
{* Copyright (C) 2007-2011, авторы программы SAS.Планета (SAS.Planet).        *}
{* Это программа является свободным программным обеспечением. Вы можете       *}
{* распространять и/или модифицировать её согласно условиям Стандартной       *}
{* Общественной Лицензии GNU, опубликованной Фондом Свободного Программного   *}
{* Обеспечения, версии 3. Эта программа распространяется в надежде, что она   *}
{* будет полезной, но БЕЗ ВСЯКИХ ГАРАНТИЙ, в том числе подразумеваемых        *}
{* гарантий ТОВАРНОГО СОСТОЯНИЯ ПРИ ПРОДАЖЕ и ГОДНОСТИ ДЛЯ ОПРЕДЕЛЁННОГО      *}
{* ПРИМЕНЕНИЯ. Смотрите Стандартную Общественную Лицензию GNU версии 3, для   *}
{* получения дополнительной информации. Вы должны были получить копию         *}
{* Стандартной Общественной Лицензии GNU вместе с программой. В случае её     *}
{* отсутствия, посмотрите http://www.gnu.org/licenses/.                       *}
{*                                                                            *}
{* http://sasgis.ru/sasplanet                                                 *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_MapLayerGridsConfig;

interface

uses
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

uses
  u_ConfigSaveLoadStrategyBasicProviderSubItem,
  u_TileGridConfig,
  u_GenShtabGridConfig;

{ TMapLayerGridsConfig }

constructor TMapLayerGridsConfig.Create;
begin
  inherited;
  FTileGrid := TTileGridConfig.Create;
  Add(FTileGrid, TConfigSaveLoadStrategyBasicProviderSubItem.Create('TileGrid'));
  FGenShtabGrid := TGenShtabGridConfig.Create;
  Add(FGenShtabGrid, TConfigSaveLoadStrategyBasicProviderSubItem.Create('GenShtabGrid'));
end;

function TMapLayerGridsConfig.GetGenShtabGrid: IGenShtabGridConfig;
begin
  Result := FGenShtabGrid;
end;

function TMapLayerGridsConfig.GetTileGrid: ITileGridConfig;
begin
  Result := FTileGrid;
end;

end.
