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

unit i_Bitmap32Static;

interface

uses
  Types,
  t_Hash,
  t_Bitmap32,
  i_Changeable;

type
  IBitmap32Buffer = interface
    ['{CE710076-F0B6-43BF-A70F-15B40555DBFA}']
    function GetSize: TPoint;
    property Size: TPoint read GetSize;

    function GetData: PColor32Array;
    property Data: PColor32Array read GetData;
  end;

  IBitmap32Static = interface
    ['{DD4607C4-EBAE-4F5F-8400-68DE3C644F97}']
    function GetHash: THashValue;
    property Hash: THashValue read GetHash;

    function GetSize: TPoint;
    property Size: TPoint read GetSize;

    function GetData: PColor32Array;
    property Data: PColor32Array read GetData;
  end;

  IBitmapChangeable = interface(IChangeable)
    ['{D0735E64-ED1C-42B0-8892-ADFDF9C56BE4}']
    function GetStatic: IBitmap32Static;
  end;

implementation

end.
