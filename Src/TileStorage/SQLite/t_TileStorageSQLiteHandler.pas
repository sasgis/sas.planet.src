{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2021, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit t_TileStorageSQLiteHandler;

interface

uses
  t_TileStorageSQLite,
  t_NotifierOperationRec,
  i_MapVersionInfo,
  i_InterfaceListSimple;

type
  TVersionColMode = (
    vcm_None = 0,
    vcm_Int,
    vcm_Text
  );

  TTBColInfo = record
    // 5..8: x, y, [v], [c], s, [h], d, b
    ColCount: Integer;

    // 0 - no field 'v' for Version
    // 1 - has field 'v' of type INT (actually Int64)
    // 2 - has field 'v' of type TEXT
    ModeV: TVersionColMode;

    // if true - has field 'c' for content-type as TEXT
    HasC: Boolean;

    // if true - has field 'h' for crc32 as INT
    HasH: Boolean;
  end;
  PTBColInfo = ^TTBColInfo;

  TSelectTileInfoComplex = record
    TileResult: PGetTileResult;
    RequestedVersionInfo: IMapVersionInfo; // запрошенна€ верси€ (как заготовка дл€ читаемого тайла)
    RequestedVersionAsInt: Int64;          // результат конвертации  требуемой версии в Int64
    RequestedVersionIsInt: Boolean;        // признак, что требуема€ верси€ целочисленна€
    RequestedVersionIsSet: Boolean;        // признак, что требуема€ верси€ установлена (не пуста€)
    RequestedVersionToDB: AnsiString;      // верси€ дл€ запроса к Ѕƒ
    SelectMode: TGetTileInfoModeSQLite;
  end;
  PSelectTileInfoComplex = ^TSelectTileInfoComplex;

implementation


end.
