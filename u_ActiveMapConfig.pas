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

unit u_ActiveMapConfig;

interface

uses
  Windows,
  SysUtils,
  i_JclNotify,
  i_GUIDSet,
  i_MapTypes,
  i_ActiveMapsConfig,
  u_ConfigDataElementBase;

type
  TActiveMapConfig = class(TConfigDataElementBaseEmptySaveLoad, IActiveMap)
  private
    FSelectedGUID: TGUID;
    FMapsSet: IMapTypeSet;
    FSingeMapsList: IGUIDInterfaceSet;
  protected
    FMainMapChangeNotyfier: IJclNotifier;
    FMainMapListener: IJclListener;
    procedure OnMainMapChange(const AGUID: TGUID);
  protected
    function GetSelectedGUID: TGUID;
    function GetMapSingle(const AMapGUID: TGUID): IActiveMapSingle;
    function GetMapsSet: IMapTypeSet;
  public
    constructor Create(AMainMapChangeNotyfier: IJclNotifier; ASingeMapsList: IGUIDInterfaceSet; AMapsSet: IMapTypeSet);
    destructor Destroy; override;
  end;

implementation

uses
  ActiveX,
  u_NotifyWithGUIDEvent;

{ TActiveMapConfigNew }

constructor TActiveMapConfig.Create(AMainMapChangeNotyfier: IJclNotifier;
  ASingeMapsList: IGUIDInterfaceSet; AMapsSet: IMapTypeSet);
var
  i: Cardinal;
begin
  inherited Create;
  FMapsSet := AMapsSet;
  FSingeMapsList := ASingeMapsList;
  FMainMapChangeNotyfier := AMainMapChangeNotyfier;
  FMainMapListener := TNotifyWithGUIDEventListener.Create(Self.OnMainMapChange);
  FMainMapChangeNotyfier.Add(FMainMapListener);
  if FMapsSet.GetIterator.Next(1, FSelectedGUID, i) <> S_OK then begin
    raise Exception.Create('Empty maps list');
  end;
end;

destructor TActiveMapConfig.Destroy;
begin
  FMainMapChangeNotyfier.Remove(FMainMapListener);
  FMainMapListener := nil;
  FMainMapChangeNotyfier := nil;
  FMapsSet := nil;
  FSingeMapsList := nil;
  inherited;
end;

function TActiveMapConfig.GetMapSingle(const AMapGUID: TGUID): IActiveMapSingle;
begin
  if FMapsSet.GetMapTypeByGUID(AMapGUID) <> nil then begin
    Result := IActiveMapSingle(FSingeMapsList.GetByGUID(AMapGUID));
  end;
end;

function TActiveMapConfig.GetMapsSet: IMapTypeSet;
begin
  Result := FMapsSet;
end;

function TActiveMapConfig.GetSelectedGUID: TGUID;
begin
  LockRead;
  try
    Result := FSelectedGUID;
  finally
    UnlockRead;
  end;
end;

procedure TActiveMapConfig.OnMainMapChange(const AGUID: TGUID);
begin
  LockWrite;
  try
    if not IsEqualGUID(FSelectedGUID, AGUID) then begin
      FSelectedGUID := AGUID;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
