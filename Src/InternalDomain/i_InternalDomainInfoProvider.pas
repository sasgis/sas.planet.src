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

unit i_InternalDomainInfoProvider;

interface

uses
  i_BinaryData;

type
  IInternalDomainInfoProvider = interface
    ['{CD84B08E-E84B-4688-9D9A-A9A34F29139D}']
    function LoadBinaryByFilePath(
      const AFilePath: string;
      out AContentType: AnsiString
    ): IBinaryData;
  end;

  IInternalDomainInfoProviderList = interface
    ['{C2FE2C8D-C9F3-48F7-AB3B-37119722D118}']
    function GetByName(const AName: string): IInternalDomainInfoProvider;
  end;

implementation

end.
