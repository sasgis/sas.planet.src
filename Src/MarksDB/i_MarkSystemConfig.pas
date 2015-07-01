{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit i_MarkSystemConfig;

interface

uses
  i_ConfigDataElement,
  i_MarkSystemImplConfig;

type
  IMarkSystemConfigStatic = interface
    ['{6AE87F7B-FB72-44B5-81C4-17E1B4024528}']
    function GetDatabaseUID: TGUID;
    property DatabaseUID: TGUID read GetDatabaseUID;

    function GetDisplayName: string;
    property DisplayName: string read GetDisplayName;

    function GetImplConfig: IMarkSystemImplConfigStatic;
    property ImplConfig: IMarkSystemImplConfigStatic read GetImplConfig;
  end;

  IMarkSystemConfigListChangeable = interface(IConfigDataElement)
    ['{EBC8A96C-EBFF-4BD0-B4E9-591785D85599}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function Get(const AIndex: Integer): IMarkSystemConfigStatic;

    function GetActiveConfigIndex: Integer;
    procedure SetActiveConfigIndex(const AValue: Integer);
    property ActiveConfigIndex: Integer read GetActiveConfigIndex write SetActiveConfigIndex;

    function GetActiveConfig: IMarkSystemConfigStatic;

    procedure Delete(const AIndex: Integer);

    function Add(
      const ADatabaseUID: TGUID;
      const ADisplayName: string;
      const AImplConfig: IMarkSystemImplConfigStatic;
      const ASetAsActive: Boolean
    ): Integer; overload;

    function Add(
      const AConfig: IMarkSystemConfigStatic;
      const ASetAsActive: Boolean
    ): Integer; overload;
  end;

implementation

end.
