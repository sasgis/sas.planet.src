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

unit i_ExternalTerrainsProvider;

interface

uses
  t_ExternalTerrainAPI,
  i_ProjConverter,
  i_TerrainProvider;

type
  IExternalTerrainsProvider = interface
    ['{395494F8-5793-4261-8AF8-259B1CAD2365}']
    function Available: Boolean;
    function Enum(
      const AHostPointer: Pointer;
      const AHostCallback: TExternalTerrainsEnumCallback
    ): Boolean;
  end;

  IExternalTerrainsProviderInternal = interface(IExternalTerrainsProvider)
    ['{F6DB1BA7-7EA7-4E5E-95C3-EB63A1782A11}']
    function OpenFunc: Pointer;
    function CloseFunc: Pointer;
    function ElevFunc: Pointer;
  end;

implementation

end.
