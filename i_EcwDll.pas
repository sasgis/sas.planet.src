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

unit i_EcwDll;

interface

uses
  ECWWriter;

type
  IEcwDll = interface
  ['{B5E36492-CA31-4114-A65F-4EC6E0B76DCC}']
    function GetCompressAllocClient: NCSEcwCompressAllocClient;
    property CompressAllocClient: NCSEcwCompressAllocClient read GetCompressAllocClient;

    function GetCompressOpen: NCSEcwCompressOpen;
    property CompressOpen: NCSEcwCompressOpen read GetCompressOpen;

    function GetCompress: NCSEcwCompress;
    property Compress: NCSEcwCompress read GetCompress;

    function GetCompressClose: NCSEcwCompressClose;
    property CompressClose: NCSEcwCompressClose read GetCompressClose;

    function GetCompressFreeClient: NCSEcwCompressFreeClient;
    property CompressFreeClient: NCSEcwCompressFreeClient read GetCompressFreeClient;
  end;

implementation

end.
