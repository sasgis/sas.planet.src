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

unit u_IeEmbeddedProtocolFactory;

interface

uses
  Windows,
  ActiveX,
  i_InternalDomainInfoProvider;

type
  TIeEmbeddedProtocolFactory = class(TInterfacedObject, IClassFactory)
  private
    FDomainList: IInternalDomainInfoProviderList;
  protected
    { IClassFactory }
    function CreateInstance(const unkOuter: IUnknown; const iid: TIID;
      out obj): HResult; stdcall;
    function LockServer(fLock: BOOL): HResult; stdcall;
  public
    constructor Create(ADomainList: IInternalDomainInfoProviderList);
  end;

implementation

uses
  SysUtils,
  u_IeEmbeddedProtocol;

{ TIeEmbeddedProtocolFactory }

constructor TIeEmbeddedProtocolFactory.Create(
  ADomainList: IInternalDomainInfoProviderList);
begin
  FDomainList := ADomainList;
end;

function TIeEmbeddedProtocolFactory.CreateInstance(
  const unkOuter: IInterface; const iid: TIID; out obj): HResult;
var
  VProtocol: TIeEmbeddedProtocol;
  VGUIDSTring: string;
begin
  if (unkOuter <> nil) then begin
    Result := CLASS_E_NOAGGREGATION;
    Exit;
  end;
  VGUIDSTring := GUIDToString(iid);
  VProtocol := TIeEmbeddedProtocol.Create(FDomainList);
  if VProtocol.GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
  if VProtocol.RefCount = 0 then VProtocol.Free;
end;

function TIeEmbeddedProtocolFactory.LockServer(fLock: BOOL): HResult;
begin
  Result := E_NOTIMPL;
end;

end.
