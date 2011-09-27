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

unit u_MapAbilitiesConfigStatic;

interface

uses
  i_MapAbilitiesConfig;

type
  TMapAbilitiesConfigStatic = class(TInterfacedObject, IMapAbilitiesConfigStatic)
  private
    FIsLayer: Boolean;
    FIsShowOnSmMap: Boolean;
    FIsUseStick: Boolean;
    FIsUseGenPrevious: Boolean;
    FUseDownload: Boolean;
  protected
    function GetIsLayer: Boolean;
    function GetIsShowOnSmMap: Boolean;
    function GetIsUseStick: Boolean;
    function GetIsUseGenPrevious: Boolean;
    function GetUseDownload: Boolean;
  public
    constructor Create(
      AIsLayer: Boolean;
      AIsShowOnSmMap: Boolean;
      AIsUseStick: Boolean;
      AIsUseGenPrevious: Boolean;
      AUseDownload: Boolean
    );
  end;

implementation

{ TMapAbilitiesConfigStatic }

constructor TMapAbilitiesConfigStatic.Create(
  AIsLayer,
  AIsShowOnSmMap,
  AIsUseStick,
  AIsUseGenPrevious,
  AUseDownload: boolean
);
begin
  FIsLayer := AIsLayer;
  FIsShowOnSmMap := AIsShowOnSmMap;
  FIsUseStick := AIsUseStick;
  FIsUseGenPrevious := AIsUseGenPrevious;
  FUseDownload := AUseDownload;
end;

function TMapAbilitiesConfigStatic.GetIsLayer: Boolean;
begin
  Result := FIsLayer;
end;

function TMapAbilitiesConfigStatic.GetIsShowOnSmMap: Boolean;
begin
  Result := FIsShowOnSmMap;
end;

function TMapAbilitiesConfigStatic.GetIsUseGenPrevious: Boolean;
begin
  Result := FIsUseGenPrevious;
end;

function TMapAbilitiesConfigStatic.GetIsUseStick: Boolean;
begin
  Result := FIsUseStick;
end;

function TMapAbilitiesConfigStatic.GetUseDownload: Boolean;
begin
  Result := FUseDownload;
end;

end.
