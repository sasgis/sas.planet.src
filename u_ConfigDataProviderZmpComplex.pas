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

unit u_ConfigDataProviderZmpComplex;

interface

uses
  i_ConfigDataProvider,
  u_ConfigDataProviderWithLocal;

type
  TConfigDataProviderZmpComplex = class(TConfigDataProviderWithLocal)
  public
    constructor Create(
      AZmpMapConfig: IConfigDataProvider;
      ALocalMapConfig: IConfigDataProvider
    );
  end;

implementation

uses
  Classes,
  u_ConfigDataProviderWithUseDepreciated,
  u_ConfigDataProviderVirtualWithSubItem,
  u_ConfigDataProviderWithReplacedSubItem;

{ TConfigDataProviderZmpComplex }

constructor TConfigDataProviderZmpComplex.Create(AZmpMapConfig,
  ALocalMapConfig: IConfigDataProvider);
var
  VConfig: IConfigDataProvider;
  VParamsTXT: IConfigDataProvider;
  VParams: IConfigDataProvider;
  VRenamesList: TStringList;
  VLocalMapConfig: IConfigDataProvider;
begin
  VConfig := AZmpMapConfig;
  VParamsTXT := VConfig.GetSubItem('params.txt');
  VParams := VParamsTXT.GetSubItem('PARAMS');

  VRenamesList := TStringList.Create;
  try
    VRenamesList.Values['URLBase'] := 'DefURLBase';
    VRenamesList.Values['HotKey'] := 'DefHotKey';
    VParams := TConfigDataProviderWithUseDepreciated.Create(VParams, VRenamesList);
  finally
    VRenamesList.Free;
  end;
  VParamsTXT := TConfigDataProviderWithReplacedSubItem.Create(VParamsTXT, 'PARAMS', VParams);
  VConfig := TConfigDataProviderWithReplacedSubItem.Create(VConfig, 'params.txt', VParamsTXT);

  VLocalMapConfig :=
    TConfigDataProviderVirtualWithSubItem.Create(
      'params.txt',
      TConfigDataProviderVirtualWithSubItem.Create(
        'PARAMS',
        ALocalMapConfig
      )
    );

  inherited Create(VConfig, VLocalMapConfig);
end;

end.
