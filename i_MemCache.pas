{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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

unit i_MemCache;

interface

uses
  Types;
type
  ICacheElement = interface
  ['{3E56D8A9-51CB-4CB0-AABF-CD09145CEFB1}']
    function GetIsEmpty: Boolean;
    function GetPutTime: TDateTime;
    function GetLastAccessTime: TDateTime;
    procedure GetTileCoord(var AXY: TPoint; var AZoom: Byte);
    function GetObject: TObject;
  end;

  IMemCache = interface
  ['{75B5851E-3BC3-41B4-9E2E-8804AB073BCB}']
    function GetByCoord(AXY: TPoint; AZoom: Byte): ICacheElement;
    procedure PutObject(AXY: TPoint; AZoom: Byte; AObj: TObject);
    procedure TrimByTimeToLive;
    procedure TrimByCount;
  end;
implementation

end.
