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

unit u_HashFunctionCityHash;

interface

uses
  t_Hash,
  i_HashFunctionImpl,
  u_BaseInterfacedObject;

type
  THashFunctionCityHash = class(TBaseInterfacedObject, IHashFunctionImpl)
  private
    function CalcHash(
      ABuffer: Pointer;
      ASize: Integer
    ): THashValue;
    function CalcHashWithSeed(
      ABuffer: Pointer;
      ASize: Integer;
      const ASeed: THashValue
    ): THashValue;
  end;

implementation

uses
  CityHash;

{ THashFunctionCityHash }

function THashFunctionCityHash.CalcHash(
  ABuffer: Pointer;
  ASize: Integer
): THashValue;
begin
  Result := CityHash64(ABuffer, ASize);
end;

function THashFunctionCityHash.CalcHashWithSeed(
  ABuffer: Pointer;
  ASize: Integer;
  const ASeed: THashValue
): THashValue;
begin
  Result := CityHash64WithSeed(ABuffer, ASize, ASeed);
end;

end.
