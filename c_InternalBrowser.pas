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

unit c_InternalBrowser;

interface

const
  CSASProtocolName = 'sas';
  CSASInternalURLPrefix = CSASProtocolName + '://';
  CZmpInfoInternalDomain = 'ZmpInfo';
  CMapDataInternalDomain = 'MapData';
  CMediaDataInternalDomain = 'MediaData';
  CShowMessageDomain = 'ShowMessage';
  CLastSearchResultsInternalDomain = 'SearchResults';
  CMarksSystemInternalDomain = 'Placemarks';
  CTileStorageOptionsInternalDomain = 'TileStorageOptions';
  CZmpInfoInternalURL = CSASInternalURLPrefix + CZmpInfoInternalDomain + '/';
  CMapDataInternalURL = CSASInternalURLPrefix + CMapDataInternalDomain + '/';
  CMediaDataInternalURL = CSASInternalURLPrefix + CMediaDataInternalDomain + '/';
  CLastSearchResultsInternalURL = CSASInternalURLPrefix + CLastSearchResultsInternalDomain + '/';
  CMarksSystemInternalURL = CSASInternalURLPrefix + CMarksSystemInternalDomain + '/';
  CTileStorageOptionsInternalURL = CSASInternalURLPrefix + CTileStorageOptionsInternalDomain + '/';
  CShowMessageInternalURL = CSASInternalURLPrefix + CShowMessageDomain + '/';
  CVectorItemDescriptionSuffix = 'Description';
  CVectorItemInfoSuffix = 'Info';

implementation

end.
