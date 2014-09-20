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

unit i_MarkSystemImplFactory;

interface

uses
  i_MarkSystemImpl,
  i_NotifierOperation,
  i_Changeable;

type
  IMarkSystemImplFactory = interface
    ['{6ADF8D8C-670C-4282-9BC7-A3F9250181C6}']
    function GetIsInitializationRequired: Boolean;
    property IsInitializationRequired: Boolean read GetIsInitializationRequired;

    function Build(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ABasePath: string;
      const AReadOnly: Boolean = False
    ): IMarkSystemImpl;
  end;

  IMarkSystemImplFactoryChangeable = interface(IChangeable)
    ['{F3DEF1AA-B4CE-4453-ABF0-C4EE81DAB17A}']
    function GetStatic: IMarkSystemImplFactory;
  end;

implementation

end.
