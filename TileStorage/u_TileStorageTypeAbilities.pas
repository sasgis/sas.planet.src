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
  i_TileStorageAbilities,
  u_BaseInterfacedObject;

type
  TTileStorageTypeAbilitiesFileFolder = class(TBaseInterfacedObject, ITileStorageAbilities)
  private
    function GetIsReadOnly: Boolean;
    function GetAllowAdd: Boolean;
    function GetAllowDelete: Boolean;
    function GetAllowReplace: Boolean;
  end;

  TTileStorageTypeAbilitiesGE = class(TBaseInterfacedObject, ITileStorageAbilities)
  private
    function GetIsReadOnly: Boolean;
    function GetAllowAdd: Boolean;
    function GetAllowDelete: Boolean;
    function GetAllowReplace: Boolean;
  end;

  TTileStorageTypeAbilitiesBerkeleyDB = class(TBaseInterfacedObject, ITileStorageAbilities)
  private
    FIsReadOnly: Boolean;
  private
    function GetIsReadOnly: Boolean;
    function GetAllowAdd: Boolean;
    function GetAllowDelete: Boolean;
    function GetAllowReplace: Boolean;
  public
    constructor Create(const AIsReadOnly: Boolean);
  end;

  TTileStorageTypeAbilitiesDBMS = class(TBaseInterfacedObject, ITileStorageAbilities)
  private
    function GetIsReadOnly: Boolean;
    function GetAllowAdd: Boolean;
    function GetAllowDelete: Boolean;
    function GetAllowReplace: Boolean;
  end;

  TTileStorageTypeAbilitiesRAM = class(TBaseInterfacedObject, ITileStorageAbilities)
  private
    function GetIsReadOnly: Boolean;
    function GetAllowAdd: Boolean;
    function GetAllowDelete: Boolean;
    function GetAllowReplace: Boolean;
  end;

implementation


{ TTileStorageTypeAbilitiesFieFolder }

function TTileStorageTypeAbilitiesFileFolder.GetAllowAdd: Boolean;
begin
  Result := True;
end;

function TTileStorageTypeAbilitiesFileFolder.GetAllowDelete: Boolean;
begin
  Result := True;
end;

function TTileStorageTypeAbilitiesFileFolder.GetAllowReplace: Boolean;
begin
  Result := True;
end;

function TTileStorageTypeAbilitiesFileFolder.GetIsReadOnly: Boolean;
begin
  Result := False;
end;

{ TTileStorageTypeAbilitiesGE }

function TTileStorageTypeAbilitiesGE.GetAllowAdd: Boolean;
begin
  Result := False;
end;

function TTileStorageTypeAbilitiesGE.GetAllowDelete: Boolean;
begin
  Result := False;
end;

function TTileStorageTypeAbilitiesGE.GetAllowReplace: Boolean;
begin
  Result := False;
end;

function TTileStorageTypeAbilitiesGE.GetIsReadOnly: Boolean;
begin
  Result := True;
end;

{ TTileStorageTypeAbilitiesBerkeleyDB }

constructor TTileStorageTypeAbilitiesBerkeleyDB.Create(const AIsReadOnly: Boolean);
begin
  inherited Create;
  FIsReadOnly := AIsReadOnly;
end;

function TTileStorageTypeAbilitiesBerkeleyDB.GetAllowAdd: Boolean;
begin
  Result := not FIsReadOnly;
end;

function TTileStorageTypeAbilitiesBerkeleyDB.GetAllowDelete: Boolean;
begin
  Result := not FIsReadOnly;
end;

function TTileStorageTypeAbilitiesBerkeleyDB.GetAllowReplace: Boolean;
begin
  Result := not FIsReadOnly;
end;

function TTileStorageTypeAbilitiesBerkeleyDB.GetIsReadOnly: Boolean;
begin
  Result := FIsReadOnly;
end;

{ TTileStorageTypeAbilitiesDBMS }

function TTileStorageTypeAbilitiesDBMS.GetAllowAdd: Boolean;
begin
  Result := True;
end;

function TTileStorageTypeAbilitiesDBMS.GetAllowDelete: Boolean;
begin
  Result := True;
end;

function TTileStorageTypeAbilitiesDBMS.GetAllowReplace: Boolean;
begin
  Result := True;
end;

function TTileStorageTypeAbilitiesDBMS.GetIsReadOnly: Boolean;
begin
  Result := False;
end;

{ TTileStorageTypeAbilitiesRAM }

function TTileStorageTypeAbilitiesRAM.GetAllowAdd: Boolean;
begin
  Result := True;
end;

function TTileStorageTypeAbilitiesRAM.GetAllowDelete: Boolean;
begin
  Result := True;
end;

function TTileStorageTypeAbilitiesRAM.GetAllowReplace: Boolean;
begin
  Result := True;
end;

function TTileStorageTypeAbilitiesRAM.GetIsReadOnly: Boolean;
begin
  Result := False;
end;

end.
