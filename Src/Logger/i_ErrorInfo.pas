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

unit i_ErrorInfo;

interface

uses
  Types,
  i_LonLatRect;

type
  IErrorInfoSimple = interface
    ['{53FB6DC2-73AB-4A32-8CA2-BDC129298810}']
    function GetErrorText: string;
    property ErrorText: string read GetErrorText;
  end;

  IErrorInfoWithCaption = interface
    ['{744830D6-D864-40CA-AAA0-F608D0F9CD4B}']
    function GetCaption: string;
    property Caption: string read GetCaption;
  end;

  IErrorInfoWithGeoRect = interface
    ['{4DBF2561-0D5E-42FA-8A0A-DFB78FD548F5}']
    function GetRect: ILonLatRect;
    property Rect: ILonLatRect read GetRect;
  end;

  IErrorInfoTile = interface
    ['{B10B733B-BA3F-416D-B9B1-9BDDE576BB30}']
    function GetZoom: Byte;
    property Zoom: Byte read GetZoom;

    function GetTile: TPoint;
    property Tile: TPoint read GetTile;
  end;

  IErrorInfoMapType = interface
    ['{2A88F07B-6C27-45E9-B37C-A1F58EE6008B}']
    function GetMapTypeGUID: TGUID;
    property MapTypeGUID: TGUID read GetMapTypeGUID;
  end;

implementation

end.
