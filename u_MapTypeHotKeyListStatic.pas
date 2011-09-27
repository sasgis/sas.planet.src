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

unit u_MapTypeHotKeyListStatic;

interface

uses
  Classes,
  i_MapTypes,
  i_MapTypeHotKeyListStatic,
  i_IDList;

type
  TMapTypeHotKeyListStatic = class(TInterfacedObject, IMapTypeHotKeyListStatic)
  private
    FList: IIDInterfaceList;
  protected
    function GetMapTypeGUIDByHotKey(AHotKey: TShortCut): IMapType;
  public
    constructor Create(
      AMapsSet: IMapTypeSet
    );
  end;

implementation

uses
  ActiveX,
  u_IDInterfaceList;

{ TMapTypeHotKeyListStatic }

constructor TMapTypeHotKeyListStatic.Create(AMapsSet: IMapTypeSet);
var
  VEnum: IEnumGUID;
  VGUID: TGUID;
  VGetCount: Cardinal;
  VMap: IMapType;
  VHotKey: TShortCut;
begin
  FList := TIDInterfaceList.Create(False);
  VEnum := AMapsSet.GetIterator;
  while VEnum.Next(1, VGUID, VGetCount) = S_OK do begin
    VMap := AMapsSet.GetMapTypeByGUID(VGUID);
    VHotKey := VMap.MapType.GUIConfig.HotKey;
    if VHotKey <> 0 then begin
      FList.Add(VHotKey, VMap);
    end;
  end;
end;

function TMapTypeHotKeyListStatic.GetMapTypeGUIDByHotKey(
  AHotKey: TShortCut): IMapType;
begin
  Result := IMapType(FList.GetByID(AHotKey));
end;

end.
