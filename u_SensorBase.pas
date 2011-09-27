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

unit u_SensorBase;

interface

uses
  i_JclNotify,
  i_LanguageManager,
  i_Sensor,
  i_SensorList,
  u_UserInterfaceItemBase,
  u_ConfigDataElementBase;

type
  TSensorBase = class(TUserInterfaceItemBase, ISensor, ISensorListEntity)
  private
    FCanReset: Boolean;
    FSensorTypeIID: TGUID;

    FDataUpdateNotifier: IJclNotifier;
  protected
    procedure NotifyDataUpdate;
  protected
    function CanReset: Boolean;
    procedure Reset; virtual;
    function GetSensorTypeIID: TGUID;
    function GetDataUpdateNotifier: IJclNotifier;
    function GetSensor: ISensor;
  public
    constructor Create(
      AGUID: TGUID;
      ACanReset: Boolean;
      ASensorTypeIID: TGUID;
      ALanguageManager: ILanguageManager
    );
  end;

implementation

uses
  u_JclNotify;

{ TSensorBase }

constructor TSensorBase.Create(
  AGUID: TGUID;
  ACanReset: Boolean;
  ASensorTypeIID: TGUID;
  ALanguageManager: ILanguageManager
);
begin
  inherited Create(AGUID, ALanguageManager);
  FCanReset := ACanReset;
  FSensorTypeIID := ASensorTypeIID;

  FDataUpdateNotifier := TJclBaseNotifier.Create;
end;

function TSensorBase.CanReset: Boolean;
begin
  Result := FCanReset;
end;

function TSensorBase.GetDataUpdateNotifier: IJclNotifier;
begin
  Result := FDataUpdateNotifier;
end;

function TSensorBase.GetSensor: ISensor;
begin
  Result := Self;
end;

function TSensorBase.GetSensorTypeIID: TGUID;
begin
  Result := FSensorTypeIID;
end;

procedure TSensorBase.NotifyDataUpdate;
begin
  FDataUpdateNotifier.Notify(nil);
end;

procedure TSensorBase.Reset;
begin
end;

end.
