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

unit u_TileStorageTypeAbilities;

interface

uses
  i_StorageTypeAbilities;

type
  TTileStorageTypeAbilitiesFileFolder = class(TInterfacedObject, IStorageTypeAbilities)
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

  TTileStorageTypeAbilitiesBerkeleyDB = class(TInterfacedObject, IStorageTypeAbilities)
  protected
    function GetIsReadOnly: boolean;
    function GetAllowAdd: Boolean;
    function GetAllowDelete: boolean;
    function GetAllowReplace: boolean;
  end;

  TTileStorageTypeAbilitiesDBMS = class(TInterfacedObject, IStorageTypeAbilities)
  protected
    function GetIsReadOnly: boolean;
    function GetAllowAdd: Boolean;
    function GetAllowDelete: boolean;
    function GetAllowReplace: boolean;
  end;

implementation


{ TTileStorageTypeAbilitiesFieFolder }

function TTileStorageTypeAbilitiesFileFolder.GetAllowAdd: Boolean;
begin
  Result := True;
end;

function TTileStorageTypeAbilitiesFileFolder.GetAllowDelete: boolean;
begin
  Result := True;
end;

function TTileStorageTypeAbilitiesFileFolder.GetAllowReplace: boolean;
begin
  Result := True;
end;

function TTileStorageTypeAbilitiesFileFolder.GetIsReadOnly: boolean;
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

{ TTileStorageTypeAbilitiesBerkeleyDB }

function TTileStorageTypeAbilitiesBerkeleyDB.GetAllowAdd: Boolean;
begin
  Result := True;
end;

function TTileStorageTypeAbilitiesBerkeleyDB.GetAllowDelete: boolean;
begin
  Result := True;
end;

function TTileStorageTypeAbilitiesBerkeleyDB.GetAllowReplace: boolean;
begin
  Result := True;
end;

function TTileStorageTypeAbilitiesBerkeleyDB.GetIsReadOnly: boolean;
begin
  Result := False;
end;

{ TTileStorageTypeAbilitiesDBMS }

function TTileStorageTypeAbilitiesDBMS.GetAllowAdd: Boolean;
begin
  Result := True;
end;

function TTileStorageTypeAbilitiesDBMS.GetAllowDelete: boolean;
begin
  Result := True;
end;

function TTileStorageTypeAbilitiesDBMS.GetAllowReplace: boolean;
begin
  Result := True;
end;

function TTileStorageTypeAbilitiesDBMS.GetIsReadOnly: boolean;
begin
  Result := False;
end;

end.
