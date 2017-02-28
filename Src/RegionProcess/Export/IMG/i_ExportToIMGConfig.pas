{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2017, SAS.Planet development team.                      *}
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

unit i_ExportToIMGConfig;

interface

uses
  i_ConfigDataElement;

type
  IExportToIMGConfig = interface(IConfigDataElement)
    ['{FAC377E0-EE21-4F01-BF3C-C0B9B66C9F38}']
    function GetMapCompilerPath: String;
    procedure SetMapCompilerPath(const AValue: String);
    property MapCompilerPath: String read GetMapCompilerPath write SetMapCompilerPath;

    function GetMapCompilerLicensePath: String;
    procedure SetMapCompilerLicensePath(const AValue: String);
    property MapCompilerLicensePath: String read GetMapCompilerLicensePath write SetMapCompilerLicensePath;

    function GetGMTPath: String;
    procedure SetGMTPath(const AValue: String);
    property GMTPath: String read GetGMTPath write SetGMTPath;

    function GetZoomOptionsVisible: Boolean;
    procedure SetZoomOptionsVisible(AValue: Boolean);
    property ZoomOptionsVisible: Boolean read GetZoomOptionsVisible write SetZoomOptionsVisible;

    function GetSASZoomList: String;
    procedure SetSASZoomList(const AValue: String);
    property SASZoomList: String read GetSASZoomList write SetSASZoomList;
  end;

implementation

end.
