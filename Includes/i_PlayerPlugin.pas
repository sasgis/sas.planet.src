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

unit i_PlayerPlugin;

interface

type
  IPlayerTask = interface
    ['{9E61F3B6-284F-4932-B741-6A3839F45EB3}']
  end;

  IPlayerPlugin = interface
    ['{B89ACAA7-0B87-40EB-991A-73D9939454CA}']
    function Available: Boolean;
    function PlayByDefault(const AFilename: String): IPlayerTask;
    function PlayByDescription(const ADescription: String): IPlayerTask;
    function PlayWithOptions(const AFilename: String; const APlayOptions: LongWord): IPlayerTask;
  end;

  IPlayerDLL = interface
    ['{9E61F3B6-284F-4932-B741-6A3839F45EB3}']
    function DLLAvailable: Boolean;
    function DLLFunc: Pointer;
    function DLLHandle: THandle;
    function DLLInitPlugin: Byte;
    function DLLPluginHandlePtr: Pointer;
  end;

implementation

end.