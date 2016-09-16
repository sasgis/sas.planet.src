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

unit u_ProjConverterFactory;

interface

uses
  Windows,
  SysUtils,
  i_ProjConverter,
  u_BaseInterfacedObject;

type
  TProjConverterFactory = class(TBaseInterfacedObject, IProjConverterFactory)
  private
    FProj4Status: Integer; // 0 - not loaded; 1 - ok; 2 - error
  private
    function _GetByInitString(const AArgs: AnsiString): IProjConverter;
  private
    { IProjConverterFactory }
    function GetByEPSG(const AEPSG: Integer): IProjConverter;
    function GetByInitString(const AArgs: AnsiString): IProjConverter;
  public
    constructor Create;
  end;

implementation

uses
  Proj4,
  Proj4Defs,
  ALString,
  u_ProjConverterByDll;

const
  cProj4NotLoaded = 0;
  cProj4LoadedOK = 1;
  cProj4LoadedError = 2;

{ TProjConverterFactory }

constructor TProjConverterFactory.Create;
begin
  inherited Create;
  FProj4Status := cProj4NotLoaded;
end;

function TProjConverterFactory._GetByInitString(
  const AArgs: AnsiString
): IProjConverter;
var
  VProj4Status: Integer;
begin
  Result := nil;
  if AArgs <> '' then begin

    VProj4Status := InterlockedCompareExchange(FProj4Status, 0, 0);

    if VProj4Status = cProj4NotLoaded then begin
      try
        if init_proj4_dll(proj4_dll, True) then begin
          VProj4Status := cProj4LoadedOK;
        end else begin
          VProj4Status := cProj4LoadedError;
        end;
        InterlockedExchange(FProj4Status, VProj4Status);
      except
        InterlockedExchange(FProj4Status, cProj4LoadedError);
        raise;
      end;
    end;

    if VProj4Status = cProj4LoadedOK then begin
      Result := TProjConverterByDll.Create(AArgs);
    end;
  end;
end;

function TProjConverterFactory.GetByEPSG(const AEPSG: Integer): IProjConverter;
var
  VArgs: AnsiString;
begin
  VArgs := Proj4ArgsByEpsg(AEPSG);
  Result := _GetByInitString(VArgs);
end;

function TProjConverterFactory.GetByInitString(
  const AArgs: AnsiString
): IProjConverter;
const
  cEPSG: AnsiString = 'EPSG:';
var
  VEPSG: Integer;
  VArgs: AnsiString;
begin
  VArgs := AArgs;
  if ALSameText(ALCopyStr(AArgs, 1, Length(cEPSG)), cEPSG) then begin
    if ALTryStrToInt(ALCopyStr(AArgs, Length(cEPSG) + 1, Length(AArgs)), VEPSG) then begin
      VArgs := Proj4ArgsByEpsg(VEPSG);
    end;
  end;
  Result := _GetByInitString(VArgs);
end;

end.
