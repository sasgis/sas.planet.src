{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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

unit u_HashFunctionCityHash;

interface

uses
  t_Hash,
  i_HashFunctionImpl,
  u_BaseInterfacedObject;

type
  THashFunctionCityHash = class(TBaseInterfacedObject, IHashFunctionImpl)
  private
    { IHashFunctionImpl }
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

uses
  CityHash;

{ THashFunctionCityHash }

function THashFunctionCityHash.CalcHash(
  const ABuffer: Pointer;
  const ASize: Integer
): THashValue;
begin
  if ABuffer <> nil then begin
    Result := CityHash64(ABuffer, ASize);
  end else begin
    Result := 0;
  end;
end;

function THashFunctionCityHash.CalcHashWithSeed(
  const ABuffer: Pointer;
  const ASize: Integer;
  const ASeed: THashValue
): THashValue;
begin
  if ABuffer <> nil then begin
    Result := CityHash64WithSeed(ABuffer, ASize, ASeed);
  end else begin
    Result := ASeed;
  end;
end;

end.
