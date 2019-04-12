{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2019, SAS.Planet development team.                      *}
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

unit i_InternalDomainUrlHandlerConfig;

interface

uses
  i_ConfigDataElement;

type
  TUserAppRec = record
    ID: string;
    Path: string;
  end;

  TUserAppArray = array of TUserAppRec;

  IUserAppsConfig = interface(IConfigDataElement)
    ['{70057D19-F4DA-4CAD-85A9-EBF1A9717463}']
    function GetUserApps: TUserAppArray;
    property UserApps: TUserAppArray read GetUserApps;
  end;

  IInternalDomainUrlHandlerConfig = interface(IConfigDataElement)
    ['{A660C362-B083-42BD-81BC-4639B9A5FC92}']
    function GetAllowedExt: string;
    property AllowedExt: string read GetAllowedExt;

    function GetUserAppsConfig: IUserAppsConfig;
    property UserAppsConfig: IUserAppsConfig read GetUserAppsConfig;
  end;

implementation

end.
