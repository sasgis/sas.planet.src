{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_GPSModuleFactoryByVSAGPS;

interface

uses
  i_GPSPositionFactory,
  i_GPSModuleByCOM,
  i_GPSModuleByCOMFactory,
  u_BaseInterfacedObject;

type
  TGPSModuleFactoryByVSAGPS = class(TBaseInterfacedObject, IGPSModuleByCOMFactory)
  private
    FGPSPositionFactory: IGPSPositionFactory;
  private
    function CreateGPSModule: IGPSModuleByCOM;
  public
    constructor Create(const AGPSPositionFactory: IGPSPositionFactory);
  end;

implementation

uses
  u_GPSModuleByVSAGPS;

{ TGPSModuleFactoryByZylGPS }

constructor TGPSModuleFactoryByVSAGPS.Create(
  const AGPSPositionFactory: IGPSPositionFactory
);
begin
  inherited Create;
  FGPSPositionFactory := AGPSPositionFactory;
end;

function TGPSModuleFactoryByVSAGPS.CreateGPSModule: IGPSModuleByCOM;
begin
  Result := TGPSModuleByVSAGPS.Create(FGPSPositionFactory);
end;

end.
