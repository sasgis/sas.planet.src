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
    FSearchPath: string;
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
  Proj4.API,
  Proj4.Defines,
  u_AnsiStr,
  u_ProjConverterByProj4;

const
  CProj4NotLoaded   = 0;
  CProj4LoadedOK    = 1;
  CProj4LoadedError = 2;

{ TProjConverterFactory }

constructor TProjConverterFactory.Create;
begin
  inherited Create;

  FSearchPath := ExtractFilePath(Paramstr(0)) + 'share\proj';
  if not DirectoryExists(FSearchPath) then begin
    FSearchPath := '';
  end;

  FProj4Status := CProj4NotLoaded;
end;

function TProjConverterFactory._GetByInitString(
  const AArgs: AnsiString
): IProjConverter;
var
  VProj4Status: Integer;
begin
  Result := nil;

  if AArgs = '' then begin
    Exit;
  end;

  VProj4Status := InterlockedCompareExchange(FProj4Status, 0, 0);

  if VProj4Status = CProj4NotLoaded then begin
    try
      if init_proj4_dll(proj4_dll, True, FSearchPath) then begin
        VProj4Status := CProj4LoadedOK;
      end else begin
        VProj4Status := CProj4LoadedError;
      end;
      InterlockedExchange(FProj4Status, VProj4Status);
    except
      InterlockedExchange(FProj4Status, CProj4LoadedError);
      raise;
    end;
  end;

  if VProj4Status = CProj4LoadedOK then begin
    Result := TProjConverterByProj4.Create(AArgs);
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
  if SameTextA(Copy(AArgs, 1, Length(cEPSG)), cEPSG) then begin
    if TryStrToIntA(Copy(AArgs, Length(cEPSG) + 1, Length(AArgs)), VEPSG) then begin
      VArgs := Proj4ArgsByEpsg(VEPSG);
    end;
  end;
  Result := _GetByInitString(VArgs);
end;

end.
