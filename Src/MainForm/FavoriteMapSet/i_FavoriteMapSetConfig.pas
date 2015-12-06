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

unit i_FavoriteMapSetConfig;

interface

uses
  Classes,
  i_GUIDListStatic,
  i_ConfigDataElement,
  i_InterfaceListStatic,
  i_FavoriteMapSetItemStatic;

type
  IFavoriteMapSetConfig = interface(IConfigDataElement)
  ['{C98ACE9E-513B-47F1-A908-5931F538ED0A}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetByID(const AID: TGUID): IFavoriteMapSetItemStatic;

    procedure Delete(const AID: TGUID);

    procedure Add(
      const AID: TGUID;
      const ABaseMap: TGUID;
      const ALayers: IGUIDSetStatic;
      const AZoom: Integer;
      const AName: string;
      const AHotKey: TShortCut;
      const ASortIndex: Integer
    );

    procedure Update(
      const AID: TGUID;
      const ABaseMap: TGUID;
      const ALayers: IGUIDSetStatic;
      const AZoom: Integer;
      const AName: string;
      const AHotKey: TShortCut;
      const ASortIndex: Integer
    );

    function GetStatic: IInterfaceListStatic;
  end;

implementation

end.
