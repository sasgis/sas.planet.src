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

unit i_MarkCategoryFactoryConfig;

interface

uses
  i_StringConfigDataElement,
  i_ConfigDataElement;

type
  IMarkCategoryFactoryConfig = interface(IConfigDataElement)
    ['{5F3F4E37-FF16-4DC6-8A37-ADB30868C2DA}']
    function GetDefaultName: IStringConfigDataElement;
    property DefaultName: IStringConfigDataElement read GetDefaultName;

    function GetAfterScale: Integer;
    procedure SetAfterScale(AValue: Integer);
    property AfterScale: Integer read GetAfterScale write SetAfterScale;

    function GetBeforeScale: Integer;
    procedure SetBeforeScale(AValue: Integer);
    property BeforeScale: Integer read GetBeforeScale write SetBeforeScale;

    function GetDBFileName: String;
    procedure SetDBFileName(AValue: String);
    property DBFileName: String read GetDBFileName write SetDBFileName;
  end;

implementation

end.
