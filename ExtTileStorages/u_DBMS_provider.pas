{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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

unit u_DBMS_provider;

interface

uses
  t_ETS_Provider,
  t_ETS_Result;

function r_DBMS_Provider_Query_Info(const AProvHandle: TETS_Provider_Handle;
                                    const AProvQueryInfoClass: LongWord;
                                    const AProvQueryInfoSize: LongWord;
                                    const AProvQueryInfoData: Pointer): LongInt; stdcall;

implementation

function r_DBMS_Provider_Query_Info(const AProvHandle: TETS_Provider_Handle;
                                    const AProvQueryInfoClass: LongWord;
                                    const AProvQueryInfoSize: LongWord;
                                    const AProvQueryInfoData: Pointer): LongInt; stdcall;
begin
  Result:=ETSR_NOT_SUPPORTED;
end;

end.