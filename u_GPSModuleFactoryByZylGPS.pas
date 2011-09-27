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

unit u_GPSModuleFactoryByZylGPS;

interface

uses
  i_GPSPositionFactory,
  i_GPSModuleByCOM,
  i_GPSModuleByCOMFactory;

type
  TGPSModuleFactoryByZylGPS = class(TInterfacedObject, IGPSModuleByCOMFactory)
  private
    FGPSPositionFactory: IGPSPositionFactory;
  protected
    function CreateGPSModule: IGPSModuleByCOM;
  public
    constructor Create(AGPSPositionFactory: IGPSPositionFactory);
  end;

implementation

uses
  u_GPSModuleByZylGPS;

{ TGPSModuleFactoryByZylGPS }

constructor TGPSModuleFactoryByZylGPS.Create(
  AGPSPositionFactory: IGPSPositionFactory);
begin
  FGPSPositionFactory := AGPSPositionFactory;
end;

function TGPSModuleFactoryByZylGPS.CreateGPSModule: IGPSModuleByCOM;
begin
  Result := TGPSModuleByZylGPS.Create(FGPSPositionFactory);
end;

end.
