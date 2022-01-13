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

unit u_UsedMarksConfigStatic;

interface

uses
  i_UsedMarksConfig,
  u_BaseInterfacedObject;

type
  TUsedMarksConfigStatic = class(TBaseInterfacedObject, IUsedMarksConfigStatic)
  private
    FIsUseMarks: Boolean;
    FIgnoreMarksVisible: Boolean;
    FIgnoreCategoriesVisible: Boolean;
  private
    function GetIsUseMarks: Boolean;
    function GetIgnoreCategoriesVisible: Boolean;
    function GetIgnoreMarksVisible: Boolean;
  public
    constructor Create(
      AIsUseMarks: Boolean;
      AIgnoreMarksVisible: Boolean;
      AIgnoreCategoriesVisible: Boolean
    );
  end;

implementation

{ TUsedMarksConfigStatic }

constructor TUsedMarksConfigStatic.Create(AIsUseMarks, AIgnoreMarksVisible,
  AIgnoreCategoriesVisible: Boolean);
begin
  inherited Create;
  FIsUseMarks := AIsUseMarks;
  FIgnoreCategoriesVisible := AIgnoreCategoriesVisible;
  FIgnoreMarksVisible := AIgnoreMarksVisible;
end;

function TUsedMarksConfigStatic.GetIgnoreCategoriesVisible: Boolean;
begin
  Result := FIgnoreCategoriesVisible;
end;

function TUsedMarksConfigStatic.GetIgnoreMarksVisible: Boolean;
begin
  Result := FIgnoreMarksVisible;
end;

function TUsedMarksConfigStatic.GetIsUseMarks: Boolean;
begin
  Result := FIsUseMarks;
end;

end.
