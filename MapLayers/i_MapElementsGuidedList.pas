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

unit i_MapElementsGuidedList;

interface

uses
  Classes,
  ActiveX;

type
  IMapElementsGuidedList = interface
    ['{F5EBA5E1-8894-4F31-85F9-A4AD02950E8A}']
    function GetMapElementsWithGUID(const AGUID: TGUID): IInterfaceList;
    function GetMapElementsWithoutGUID: IInterfaceList;
    procedure CopyMapElementsToList(
      const AWithoutGUID, AWithGUID: Boolean;
      ADstList: IInterfaceList
    );
    procedure ClearMapElements;
    // enum lists - override for special order of layers
    function GetGUIDEnum: IEnumGUID;
  end;

implementation

end.