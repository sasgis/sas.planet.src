{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2019, SAS.Planet development team.                      *}
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

unit i_ArchiveReadWrite;

interface

uses
  i_BinaryData;

type
  // reader
  IArchiveReaderBase = interface
    ['{D162E59B-3F5C-41D0-BD6E-BEB302E77E2E}']
  end;

  IArchiveReader = interface(IArchiveReaderBase)
    ['{1C1C0E2A-929E-457D-880F-47EAD7FBD0CA}']
    function GetItemsCount: Integer;
    function GetItemByName(const AItemName: string): IBinaryData;
    function GetItemNameByIndex(const AItemIndex: Integer): string;
    function GetItemByIndex(
      const AItemIndex: Integer;
      out AItemName: string
    ): IBinaryData;
  end;

  IArchiveReaderSequential = interface(IArchiveReaderBase)
    ['{29AE3F26-EA70-4DBB-94A8-ADD02E176B97}']
    procedure Reset;
    function Next(
      out AFileData: IBinaryData;
      out AFileNameInArchive: string;
      out AFileDate: TDateTime
    ): Boolean;
  end;

  // writer
  IArchiveWriterBase = interface
    ['{4D3441CF-FC43-4683-88F6-7B677F931B1A}']
  end;

  IArchiveWriter = interface(IArchiveWriterBase)
    ['{339859BF-E9E7-4E25-9BF8-5C7281734C52}']
    function AddFile(
      const AFileData: IBinaryData;
      const AFileNameInArchive: string;
      const AFileDate: TDateTime
    ): Integer;
  end;

  IArchiveWriterSequential = interface(IArchiveWriterBase)
    ['{27F15477-5D26-4E70-9A37-796A1DEC8634}']
    procedure Add(
      const AFileData: IBinaryData;
      const AFileNameInArchive: string;
      const AFileDate: TDateTime
    );
  end;

implementation

end.
