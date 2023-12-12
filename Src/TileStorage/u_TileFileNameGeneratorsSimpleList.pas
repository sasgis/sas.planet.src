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

unit u_TileFileNameGeneratorsSimpleList;

interface

uses
  i_TileFileNameGenerator,
  i_TileFileNameGeneratorsList,
  u_BaseInterfacedObject;

type
  TTileFileNameGeneratorsSimpleList = class(TBaseInterfacedObject, ITileFileNameGeneratorsList)
  private
    FItems: array of ITileFileNameGenerator;
  private
    { ITileFileNameGeneratorsList }
    function GetGenerator(const ACacheType: Byte): ITileFileNameGenerator;
  public
    constructor Create;
  end;

implementation

uses
  c_CacheTypeCodes,
  u_TileFileNameSAS,
  u_TileFileNameGMV,
  u_TileFileNameES,
  u_TileFileNameGM1,
  u_TileFileNameGM2,
  u_TileFileNameGM3,
  u_TileFileNameMOBAC,
  u_TileFileNameTMS,
  u_TileFileNameOsmAnd;

{ TTileFileNameGeneratorsSimpleList }

constructor TTileFileNameGeneratorsSimpleList.Create;
begin
  inherited Create;
  SetLength(FItems, 9);
  FItems[0] := TTileFileNameGMV.Create;
  FItems[1] := TTileFileNameSAS.Create;
  FItems[2] := TTileFileNameES.Create;
  FItems[3] := TTileFileNameGM1.Create;
  FItems[4] := TTileFileNameGM2.Create;
  FItems[5] := TTileFileNameGM3.Create;
  FItems[6] := TTileFileNameMOBAC.Create;
  FItems[7] := TTileFileNameOsmAnd.Create;
  FItems[8] := TTileFileNameTMS.Create;
end;

function TTileFileNameGeneratorsSimpleList.GetGenerator(
  const ACacheType: Byte
): ITileFileNameGenerator;
begin
  Assert(ACacheType <> c_File_Cache_Id_DEFAULT);
  case ACacheType of
    c_File_Cache_Id_GMV:
    begin
      Result := FItems[0];
    end;
    c_File_Cache_Id_SAS:
    begin
      Result := FItems[1];
    end;
    c_File_Cache_Id_ES:
    begin
      Result := FItems[2];
    end;
    c_File_Cache_Id_GM:
    begin
      Result := FItems[3];
    end;
    c_File_Cache_Id_GM_Aux:
    begin
      Result := FItems[4];
    end;
    c_File_Cache_Id_GM_Bing:
    begin
      Result := FItems[5];
    end;
    c_File_Cache_Id_MOBAC:
    begin
      Result := FItems[6];
    end;
    c_File_Cache_Id_OsmAnd:
    begin
      Result := FItems[7];
    end;
    c_File_Cache_Id_TMS:
    begin
      Result := FItems[8];
    end;
  else
    Assert(False);
    Result := nil;
  end;
end;

end.
