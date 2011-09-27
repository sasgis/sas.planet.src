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

unit u_PathDetalizeProviderListBase;

interface

uses
  ActiveX,
  u_ConfigDataElementBase,
  i_GUIDSet,
  i_PathDetalizeProviderList;

type
  TPathDetalizeProviderListBase = class(TConfigDataElementBaseEmptySaveLoad, IPathDetalizeProviderList)
  private
    FList: IGUIDInterfaceSet;
  protected { IPathDetalizeProviderList }
    function GetGUIDEnum: IEnumGUID;
    function Get(AGUID: TGUID): IPathDetalizeProviderListEntity;
  protected
    procedure Add(AItem: IPathDetalizeProviderListEntity);
  public
    constructor Create;
  end;

implementation

uses
  u_GUIDInterfaceSet;

{ TPathDetalizeProviderListBase }

constructor TPathDetalizeProviderListBase.Create;
begin
  inherited;
  FList := TGUIDInterfaceSet.Create(False);
end;

procedure TPathDetalizeProviderListBase.Add(
  AItem: IPathDetalizeProviderListEntity);
begin
  LockWrite;
  try
    if not FList.IsExists(AItem.GUID) then begin
      FList.Add(AItem.GUID, AItem);
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

function TPathDetalizeProviderListBase.Get(
  AGUID: TGUID): IPathDetalizeProviderListEntity;
begin
  LockRead;
  try
    Result := IPathDetalizeProviderListEntity(FList.GetByGUID(AGUID));
  finally
    UnlockRead;
  end;
end;

function TPathDetalizeProviderListBase.GetGUIDEnum: IEnumGUID;
begin
  Result := FList.GetGUIDEnum;
end;

end.
