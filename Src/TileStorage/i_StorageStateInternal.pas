{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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

unit i_StorageStateInternal;

interface

uses
  t_CommonTypes,
  i_StorageState,
  i_Changeable;

type
  IStorageStateInternal = interface(IChangeable)
    ['{1ABE492B-E945-4921-AA99-709090CF62F5}']
    function GetReadAccess: TAccesState;
    procedure SetReadAccess(AValue: TAccesState);
    property ReadAccess: TAccesState read GetReadAccess write SetReadAccess;

    function GetWriteAccess: TAccesState;
    procedure SetWriteAccess(AValue: TAccesState);
    property WriteAccess: TAccesState read GetWriteAccess write SetWriteAccess;

    function GetDeleteAccess: TAccesState;
    procedure SetDeleteAccess(AValue: TAccesState);
    property DeleteAccess: TAccesState read GetDeleteAccess write SetDeleteAccess;

    function GetAddAccess: TAccesState;
    procedure SetAddAccess(AValue: TAccesState);
    property AddAccess: TAccesState read GetAddAccess write SetAddAccess;

    function GetReplaceAccess: TAccesState;
    procedure SetReplaceAccess(AValue: TAccesState);
    property ReplaceAccess: TAccesState read GetReplaceAccess write SetReplaceAccess;

    function GetStatic: IStorageStateStatic;
  end;

implementation

end.
