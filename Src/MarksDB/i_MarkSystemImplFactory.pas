{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2021, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit i_MarkSystemImplFactory;

interface

uses
  ActiveX,
  i_MarkSystemImpl,
  i_MarkSystemImplConfig,
  i_NotifierOperation;

type
  IMarkSystemImplFactory = interface
    ['{6ADF8D8C-670C-4282-9BC7-A3F9250181C6}']
    function GetIsInitializationRequired: Boolean;
    property IsInitializationRequired: Boolean read GetIsInitializationRequired;

    function Build(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ABasePath: string;
      const AImplConfig: IMarkSystemImplConfigStatic
    ): IMarkSystemImpl;
  end;

  IMarkSystemImplFactoryListElement = interface
    ['{7227214C-BBDB-4136-A0FF-E2FD95A41EF7}']
    function GetGUID: TGUID;
    property GUID: TGUID read GetGUID;

    function GetCaption: string;
    property Caption: string read GetCaption;

    function GetFactory: IMarkSystemImplFactory;
    property Factory: IMarkSystemImplFactory read GetFactory;
  end;

  IMarkSystemImplFactoryListStatic = interface
    ['{F3DEF1AA-B4CE-4453-ABF0-C4EE81DAB17A}']
    function GetGUIDEnum: IEnumGUID;
    function Get(const AGUID: TGUID): IMarkSystemImplFactoryListElement;
  end;

implementation

end.
