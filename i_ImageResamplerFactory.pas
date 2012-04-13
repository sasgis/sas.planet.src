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

unit i_ImageResamplerFactory;

interface

uses
  GR32;

type
  IImageResamplerFactory = interface
    ['{4829EE36-667A-4A25-8CE0-1DAFDDC9B3D9}']
    function CreateResampler: TCustomResampler;
  end;

  IImageResamplerFactoryList = interface
    ['{CC888F5D-5DDA-427F-8127-93B0F1BD8CA5}']
    function Count: Integer;

    function Get(AIndex: Integer): IImageResamplerFactory;
    property Items[Index: Integer]: IImageResamplerFactory read Get; default;

    function GetCaption(AIndex: Integer): string;
    property Captions[Index: Integer]: string read GetCaption;
  end;

implementation

end.
