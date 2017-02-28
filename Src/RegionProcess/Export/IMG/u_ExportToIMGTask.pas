{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2017, SAS.Planet development team.                      *}
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

unit u_ExportToIMGTask;

interface

uses
  i_TileStorage,
  i_MapVersionRequest,
  i_BitmapTileSaveLoad;

type
  TExportToIMGTaskItem = record
    FSourceTileStorage: ITileStorage;
    FSourceMapVersion: IMapVersionRequest;
    FSourceScale: Byte;
    FDeviceZoomStart, FDeviceZoomEnd: Byte;
  end;

  TIMGMapFormat = (mfOld, mfOldInGMP, mfNew);  

  TExportToIMGTask = record
    FMapName: String;
    FCodePageIndex: Integer;
    FIMGMapFormat: TIMGMapFormat;
    FDrawOrder: Integer;
    FMapSeries: Integer;
    FMapID: LongWord;
    FItems: array of TExportToIMGTaskItem;
    FUseRecolor: Boolean;
    FBitmapTileSaver: IBitmapTileSaver;
    FVolumeSize: LongWord;
    FKeepTempFiles: Boolean;

    FMapCompilerPath: String;
    FMapCompilerLicensePath: String;
    FGMTPath: String;
  end;

implementation

end.
