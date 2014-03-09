{******************************************************************************}
{* SAS.Planet (SAS.ѕланета)                                                   *}
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

unit i_GUIDListStatic;

interface

type
  // Ќеизмен€емый массив GUID-ов, пор€док играет роль, нет быстрой проверки наличи€
  IGUIDListStatic = interface
    ['{0197E18E-381E-426B-8575-4C50C6B7E11F}']
    function GetItem(AIndex: Integer): TGUID;
    property Items[AIndex: Integer]: TGUID read GetItem;

    function GetCount: Integer;
    property Count: Integer read GetCount;
  end;

  // Ќабор GUID-ов пор€док несущественный, дублей быть не может, быстра€ проверка наличи€
  IGUIDSetStatic = interface
    ['{C4A0FDB6-8517-4CBE-8225-3ABE94127193}']
    function GetItem(AIndex: Integer): TGUID;
    property Items[AIndex: Integer]: TGUID read GetItem;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    // ѕроверка наличи€ GUID в списке
    function IsExists(const AGUID: TGUID): boolean;
  end;

implementation

end.
