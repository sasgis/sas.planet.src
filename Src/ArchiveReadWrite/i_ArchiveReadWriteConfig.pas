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

unit i_ArchiveReadWriteConfig;

interface

type
  IArchiveReadConfig = interface
    ['{F4F91A40-6034-4525-BB25-A1808BE2EEDB}']
  end;

  IArchiveWriteConfig = interface
    ['{6E06150B-432A-4E0C-BE82-F291AA58CE34}']
  end;

  IArchiveWriteConfigFrame = interface
    ['{E4D76A42-EE4B-4DA2-AB1E-7154E2D425E4}']
    function GetWriteConfig: IArchiveWriteConfig;
    procedure Reset(const AWriteConfig: IArchiveWriteConfig);
  end;

  TZipCompressionLevel = (zclFast, zclNormal, zclBest);
  TZipCompressionMethod = (zcmStore, zcmDeflate, zcmBZip2, zcmLZMA);

  IArchiveWriteZipConfig = interface(IArchiveWriteConfig)
    ['{D6C01DE6-EE54-41BB-8137-9483C531B757}']
    function GetCompressionLevel: TZipCompressionLevel;
    property CompressionLevel: TZipCompressionLevel read GetCompressionLevel;

    function GetCompressionMethod: TZipCompressionMethod;
    property CompressionMethod: TZipCompressionMethod read GetCompressionMethod;

    function GetVolumeSize: Int64;
    property VolumeSize: Int64 read GetVolumeSize;
  end;

implementation

end.
