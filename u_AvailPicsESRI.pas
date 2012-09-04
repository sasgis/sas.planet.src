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

unit u_AvailPicsESRI;

interface

uses
  SysUtils,
  Classes,
  u_AvailPicsAbstract;

type
  TAvailPicsESRI = class(TAvailPicsAbstract)
  public
    function ContentType: String; override;

    function ParseResponse(const AStream: TMemoryStream): Integer; override;

    function LinkToImages: String; override;
  end;

implementation

uses
  u_GeoToStr,
  u_TileRequestBuilderHelpers;

{ TAvailPicsESRI }

function TAvailPicsESRI.ContentType: String;
begin
  Result := 'text/html';
end;

function TAvailPicsESRI.LinkToImages: String;
begin
  Result := '';
end;

function TAvailPicsESRI.ParseResponse(const AStream: TMemoryStream): Integer;
begin
  Result:=0;
end;

end.
