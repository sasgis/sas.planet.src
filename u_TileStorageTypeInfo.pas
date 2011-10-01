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

unit u_TileStorageTypeInfo;

interface

uses
  i_TileStorageTypeInfo;

type
  TTileStorageTypeInfoFieFolder = class(TInterfacedObject, ITileStorageTypeInfo)
  protected
    function GetIsFileCache: Boolean;
    function GetIsReadOnly: boolean;
    function GetAllowDelete: boolean;
    function GetAllowSave: boolean;
    function GetAllowMultiWrite: Boolean;
    function GetAllowMultiRead: Boolean;
  end;

  TTileStorageTypeGE = class(TInterfacedObject, ITileStorageTypeInfo)
  protected
    function GetIsFileCache: Boolean;
    function GetIsReadOnly: boolean;
    function GetAllowDelete: boolean;
    function GetAllowSave: boolean;
    function GetAllowMultiWrite: Boolean;
    function GetAllowMultiRead: Boolean;
  end;

implementation

{ TTileStorageTypeInfoFieFolder }

function TTileStorageTypeInfoFieFolder.GetAllowDelete: boolean;
begin
  Result := True;
end;

function TTileStorageTypeInfoFieFolder.GetAllowMultiRead: Boolean;
begin
  Result := True;
end;

function TTileStorageTypeInfoFieFolder.GetAllowMultiWrite: Boolean;
begin
  Result := True;
end;

function TTileStorageTypeInfoFieFolder.GetAllowSave: boolean;
begin
  Result := True;
end;

function TTileStorageTypeInfoFieFolder.GetIsFileCache: Boolean;
begin
  Result := True;
end;

function TTileStorageTypeInfoFieFolder.GetIsReadOnly: boolean;
begin
  Result := False;
end;

{ TTileStorageTypeGE }

function TTileStorageTypeGE.GetAllowDelete: boolean;
begin
  Result := False;
end;

function TTileStorageTypeGE.GetAllowMultiRead: Boolean;
begin
  Result := False;
end;

function TTileStorageTypeGE.GetAllowMultiWrite: Boolean;
begin
  Result := False;
end;

function TTileStorageTypeGE.GetAllowSave: boolean;
begin
  Result := False;
end;

function TTileStorageTypeGE.GetIsFileCache: Boolean;
begin
  Result := False;
end;

function TTileStorageTypeGE.GetIsReadOnly: boolean;
begin
  Result := True;
end;

end.
