{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2019, SAS.Planet development team.                      *}
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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit i_StorageState;

interface

uses
  i_Changeable;

type
  IStorageStateStatic = interface
    ['{C3CDBB82-28B7-4470-9DC6-C17A5B69F07A}']
    function GetReadAccess: Boolean;
    property ReadAccess: Boolean read GetReadAccess;

    function GetScanAccess: Boolean;
    property ScanAccess: Boolean read GetScanAccess;

    function GetAddAccess: Boolean;
    property AddAccess: Boolean read GetAddAccess;

    function GetDeleteAccess: Boolean;
    property DeleteAccess: Boolean read GetDeleteAccess;

    function GetReplaceAccess: Boolean;
    property ReplaceAccess: Boolean read GetReplaceAccess;

    function IsSame(const AValue: IStorageStateStatic): Boolean;
  end;

  IStorageStateChangeble = interface(IChangeable)
    ['{6202AB73-00A2-4711-87F4-D195CEFD9C3F}']
    function GetStatic: IStorageStateStatic;
  end;

implementation

end.
