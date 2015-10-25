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

unit i_ContentTypeInfo;

interface

uses
  i_BitmapTileSaveLoad,
  i_VectorDataLoader;

type
  IContentTypeInfoBasic = interface
    ['{A2FC7C16-1B96-4AA2-BC70-1A353E4E1923}']
    function GetContentType: AnsiString;
    function GetDefaultExt: AnsiString;
    function CheckOtherForSaveCompatible(const AContentType: IContentTypeInfoBasic): Boolean;
  end;

  IContentTypeInfoBitmap = interface(IContentTypeInfoBasic)
    ['{DB6FAD7E-CACD-47C7-BA5E-9D1A0959FE88}']
    function GetLoader: IBitmapTileLoader;
    function GetSaver: IBitmapTileSaver;
  end;

  IContentTypeInfoVectorData = interface(IContentTypeInfoBasic)
    ['{62A84A15-D775-4F23-A12D-C25948182757}']
    function GetLoader: IVectorDataLoader;
  end;


implementation

end.
