{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_IeEmbeddedProtocolFactory;

interface

uses
  Windows,
  ActiveX,
  i_InternalDomainInfoProvider,
  u_BaseInterfacedObject;

type
  TIeEmbeddedProtocolFactory = class(TBaseInterfacedObject, IClassFactory)
  private
    FDomainList: IInternalDomainInfoProviderList;
  private
    { IClassFactory }
    function CreateInstance(
      const unkOuter: IUnknown;
      const iid: TIID;
      out obj
    ): HResult; stdcall;
    function LockServer(fLock: BOOL): HResult; stdcall;
  public
    constructor Create(const ADomainList: IInternalDomainInfoProviderList);
  end;

implementation

uses
  SysUtils,
  u_IeEmbeddedProtocol;

{ TIeEmbeddedProtocolFactory }

constructor TIeEmbeddedProtocolFactory.Create(
  const ADomainList: IInternalDomainInfoProviderList
);
begin
  inherited Create;
  FDomainList := ADomainList;
end;

function TIeEmbeddedProtocolFactory.CreateInstance(
  const unkOuter: IInterface;
  const iid: TIID;
  out obj
): HResult;
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
  if VProtocol.GetInterface(IID, obj) then begin
    Result := 0;
  end else begin
    Result := E_NOINTERFACE;
  end;
  if VProtocol.RefCount = 0 then begin
    VProtocol.Free;
  end;
end;

function TIeEmbeddedProtocolFactory.LockServer(fLock: BOOL): HResult;
begin
  Result := E_NOTIMPL;
end;

end.
