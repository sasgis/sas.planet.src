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

unit i_HashFunctionImpl;

interface

uses
  t_Hash;

type
  IHashFunctionImpl = interface
    ['{79D1F243-3578-4AD9-9E2C-8F28A22D7B1A}']
    function CalcHash(
      const ABuffer: Pointer;
      const ASize: Integer
    ): THashValue;

    function CalcHashWithSeed(
      const ABuffer: Pointer;
      const ASize: Integer;
      const ASeed: THashValue
    ): THashValue;
  end;



implementation

end.
