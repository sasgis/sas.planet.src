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

unit i_CoordConverterList;

interface

uses
  i_ProjectionSet,
  i_CoordConverter;

type
  ICoordConverterList = interface
    ['{CC888F5D-5DDA-427F-8127-93B0F1BD8CA5}']
    function Count: Integer;

    function Get(AIndex: Integer): ICoordConverter;
    property Items[Index: Integer]: ICoordConverter read Get; default;

    function GetCaption(AIndex: Integer): string;
    property Captions[Index: Integer]: string read GetCaption;
  end;

  IProjectionSetList = interface
    ['{0E60087D-7B38-4612-A21E-DC64C73FF4E6}']
    function Count: Integer;

    function Get(AIndex: Integer): IProjectionSet;
    property Items[Index: Integer]: IProjectionSet read Get; default;

    function GetCaption(AIndex: Integer): string;
    property Captions[Index: Integer]: string read GetCaption;
  end;

implementation

end.
