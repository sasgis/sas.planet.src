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

unit i_GPSModuleByCOM;

interface

uses
  i_GPSModuleByCOMPortSettings,
  i_GPSConfig,
  i_GPSModule;

type
  IGPSModuleByCOM = interface(IGPSModule)
    ['{EFB18F84-3019-44D2-9525-A12B3D97B14B}']
    procedure Connect(
      const AConfig: IGPSModuleByCOMPortSettings;
      const ALogConfig: IGPSConfig
    ); safecall;
    procedure Disconnect; safecall;

    function GetIsReadyToConnect: Boolean; safecall;
    property IsReadyToConnect: Boolean read GetIsReadyToConnect;
  end;

implementation

end.
