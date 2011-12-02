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

unit u_TileStorageTypeAbilities;

interface

uses
  i_StorageTypeAbilities;

type
  TTileStorageTypeAbilitiesFieFolder = class(TInterfacedObject, IStorageTypeAbilities)
  protected
    function GetIsReadOnly: boolean;
    function GetAllowAdd: Boolean;
    function GetAllowDelete: boolean;
    function GetAllowReplace: boolean;
  end;

  TTileStorageTypeAbilitiesGE = class(TInterfacedObject, IStorageTypeAbilities)
  protected
    function GetIsReadOnly: boolean;
    function GetAllowAdd: Boolean;
    function GetAllowDelete: boolean;
    function GetAllowReplace: boolean;
  end;

implementation


{ TTileStorageTypeAbilitiesFieFolder }

function TTileStorageTypeAbilitiesFieFolder.GetAllowAdd: Boolean;
begin
  Result := True;
end;

function TTileStorageTypeAbilitiesFieFolder.GetAllowDelete: boolean;
begin
  Result := True;
end;

function TTileStorageTypeAbilitiesFieFolder.GetAllowReplace: boolean;
begin
  Result := True;
end;

function TTileStorageTypeAbilitiesFieFolder.GetIsReadOnly: boolean;
begin
  Result := False;
end;

{ TTileStorageTypeAbilitiesGE }

function TTileStorageTypeAbilitiesGE.GetAllowAdd: Boolean;
begin
  Result := False;
end;

function TTileStorageTypeAbilitiesGE.GetAllowDelete: boolean;
begin
  Result := False;
end;

function TTileStorageTypeAbilitiesGE.GetAllowReplace: boolean;
begin
  Result := False;
end;

function TTileStorageTypeAbilitiesGE.GetIsReadOnly: boolean;
begin
  Result := True;
end;

end.
