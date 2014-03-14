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

unit u_MarkerSimpleConfigStatic;

interface

uses
  GR32,
  i_MarkerSimpleConfig,
  u_BaseInterfacedObject;

type
  TMarkerSimpleConfigStatic = class(TBaseInterfacedObject, IMarkerSimpleConfigStatic)
  private
    FMarkerSize: Integer;
    FMarkerColor: TColor32;
    FBorderColor: TColor32;
  private
    function GetMarkerSize: Integer;
    function GetMarkerColor: TColor32;
    function GetBorderColor: TColor32;
  public
    constructor Create(
      AMarkerSize: Integer;
      AMarkerColor: TColor32;
      ABorderColor: TColor32
    );
  end;

implementation

{ TMarkerSimpleConfigStatic }

constructor TMarkerSimpleConfigStatic.Create(
  AMarkerSize: Integer;
  AMarkerColor, ABorderColor: TColor32
);
begin
  inherited Create;
  FMarkerSize := AMarkerSize;
  FMarkerColor := AMarkerColor;
  FBorderColor := ABorderColor;
end;

function TMarkerSimpleConfigStatic.GetBorderColor: TColor32;
begin
  Result := FBorderColor;
end;

function TMarkerSimpleConfigStatic.GetMarkerColor: TColor32;
begin
  Result := FMarkerColor;
end;

function TMarkerSimpleConfigStatic.GetMarkerSize: Integer;
begin
  Result := FMarkerSize;
end;

end.


