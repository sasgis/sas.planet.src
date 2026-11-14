{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
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

unit c_ShpMarksCommon;

interface

const
  // the DBF schema of the marks export/import
  CFieldNameName     = 'NAME';
  CFieldNameDesc     = 'DESC';
  CFieldNameCategory = 'CATEGORY';

  // an XBase string field holds at most 254 bytes
  CFieldWidthName     = 254;
  CFieldWidthDesc     = 254;
  CFieldWidthCategory = 254;

  // a value longer than the field width is written into the continuation
  // fields DESC1..DESCn / CATEGORY1..CATEGORYn (also read back by the import);
  // the cap keeps every field name within the 10-character dBase limit and
  // bounds the DBF header growth - a longer tail is lost
  CContFieldMaxCount = 99;

implementation


end.
