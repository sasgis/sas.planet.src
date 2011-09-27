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

unit u_MarksLayerConfig;

interface

uses
  i_UsedMarksConfig,
  i_MarksDrawConfig,
  i_MarksLayerConfig,
  u_ConfigDataElementComplexBase;

type
  TMarksLayerConfig = class(TConfigDataElementComplexBase, IMarksLayerConfig)
  private
    FMarksShowConfig: IUsedMarksConfig;
    FMarksDrawConfig: IMarksDrawConfig;
  protected
    function GetMarksShowConfig: IUsedMarksConfig;
    function GetMarksDrawConfig: IMarksDrawConfig;
  public
    constructor Create();
  end;

implementation

uses
  u_ConfigSaveLoadStrategyBasicUseProvider,
  u_UsedMarksConfig,
  u_MarksDrawConfig;

{ TMainFormLayersConfig }

constructor TMarksLayerConfig.Create;
begin
  inherited Create;

  FMarksShowConfig := TUsedMarksConfig.Create;
  Add(FMarksShowConfig, TConfigSaveLoadStrategyBasicUseProvider.Create);

  FMarksDrawConfig := TMarksDrawConfig.Create;
  Add(FMarksDrawConfig, TConfigSaveLoadStrategyBasicUseProvider.Create);
end;

function TMarksLayerConfig.GetMarksDrawConfig: IMarksDrawConfig;
begin
  Result := FMarksDrawConfig;
end;

function TMarksLayerConfig.GetMarksShowConfig: IUsedMarksConfig;
begin
  Result := FMarksShowConfig;
end;

end.
