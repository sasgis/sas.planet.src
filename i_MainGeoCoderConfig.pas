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

unit i_MainGeoCoderConfig;

interface

uses
  i_GeoCoderList,
  i_StringHistory,
  i_ConfigDataElement;

type
  IMainGeoCoderConfig = interface(IConfigDataElement)
    ['{9C11F955-42C4-4AED-A398-796DC5ABC936}']
    function GetList: IGeoCoderList;

    function GetSearchHistory: IStringHistory;
    property SearchHistory: IStringHistory read GetSearchHistory;

    function GetActiveGeoCoderGUID: TGUID;
    procedure SetActiveGeoCoderGUID(const AValue: TGUID);
    property ActiveGeoCoderGUID: TGUID read GetActiveGeoCoderGUID write SetActiveGeoCoderGUID;

    function GetActiveGeoCoder: IGeoCoderListEntity;
  end;

implementation

end.
