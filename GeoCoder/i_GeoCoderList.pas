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

unit i_GeoCoderList;

interface

uses
  ActiveX,
  i_GeoCoder;

type
  IGeoCoderListEntity = interface
    ['{FB6DA76B-1706-4F85-A2A0-53E61F4AED2F}']
    function GetGUID: TGUID;
    function GetCaption: WideString;
    function GetGeoCoder: IGeoCoder;
  end;

  IGeoCoderList = interface
    ['{34A0BB9F-8C6B-4664-B299-4F78710E0996}']
    function GetGUIDEnum: IEnumGUID;
    function Get(const AGUID: TGUID): IGeoCoderListEntity;
  end;

implementation

end.
