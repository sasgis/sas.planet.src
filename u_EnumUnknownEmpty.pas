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

unit u_EnumUnknownEmpty;

interface

uses
  ActiveX,
  u_BaseInterfacedObject;

type
  TEmptyEnum = class(TBaseInterfacedObject, IEnumUnknown)
    function Next(
      celt: Longint;
      out elt;
      pceltFetched: PLongint
    ): HResult; stdcall;
    function Skip(celt: Longint): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out enm: IEnumUnknown): HResult; stdcall;
  end;

implementation

{ TEmptyEnum }

function TEmptyEnum.Clone(out enm: IEnumUnknown): HResult;
begin
  enm := Self;
  Result := S_OK;
end;

function TEmptyEnum.Next(
  celt: Integer;
  out elt;
  pceltFetched: PLongint
): HResult;
begin
  pceltFetched^ := 0;
  Result := S_FALSE;
end;

function TEmptyEnum.Reset: HResult;
begin
  Result := S_OK;
end;

function TEmptyEnum.Skip(celt: Integer): HResult;
begin
  Result := S_FALSE;
end;

end.
