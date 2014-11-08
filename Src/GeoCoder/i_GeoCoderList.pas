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

unit i_GeoCoderList;

interface

uses
  i_GeoCoder,
  i_Changeable;

type
  IGeoCoderListEntity = interface
    ['{FB6DA76B-1706-4F85-A2A0-53E61F4AED2F}']
    function GetGUID: TGUID;
    property GUID: TGUID read GetGUID;

    function GetCaption: WideString;
    property Caption: WideString read GetCaption;

    function GetGeoCoder: IGeoCoder;
    property GeoCoder: IGeoCoder read GetGeoCoder;
  end;

  IGeoCoderChangeable = interface(IChangeable)
    ['{342F0234-F74E-4CDD-A6A6-ACF58AD42968}']
    function GetStatic: IGeoCoderListEntity;
  end;

  IGeoCoderListStatic = interface
    ['{FA67ADC7-97A5-4653-BA15-D3BB9C2C9D10}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItem(const AIndex: Integer): IGeoCoderListEntity;
    property Items[const AIndex: Integer]: IGeoCoderListEntity read GetItem;

    function GetIndexByGUID(const AGUID: TGUID): Integer;
  end;

implementation

end.
