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

unit u_FillingMapLayerConfigStatic;

interface

uses
  t_Bitmap32,
  t_FillingMapModes,
  i_FillingMapLayerConfig,
  u_BaseInterfacedObject;

type
  TFillingMapLayerConfigStatic = class(TBaseInterfacedObject, IFillingMapLayerConfigStatic)
  private
    FVisible: Boolean;
    FSelectedMap: TGUID;
    FUseRelativeZoom: Boolean;
    FZoom: Byte;
    FNoTileColor: TColor32;
    FShowTNE: Boolean;
    FTNEColor: TColor32;
    FFillMode: TFillMode;
    FFilterMode: Boolean;
    FFillFirstDay: TDateTime;
    FFillLastDay: TDateTime;
    FFillColorPresetId: Integer;
  private
    { IFillingMapLayerConfigStatic }
    function GetVisible: Boolean;
    function GetSelectedMap: TGUID;
    function GetUseRelativeZoom: Boolean;
    function GetZoom: Byte;
    function GetNoTileColor: TColor32;
    function GetShowTNE: Boolean;
    function GetTNEColor: TColor32;
    function GetFillMode: TFillMode;
    function GetFilterMode: Boolean;
    function GetFillFirstDay: TDateTime;
    function GetFillLastDay: TDateTime;
    function GetFillColorPresetId: Integer;
  public
    constructor Create(
      const AVisible: Boolean;
      const ASelectedMap: TGUID;
      const AUseRelativeZoom: Boolean;
      const AZoom: Byte;
      const ANoTileColor: TColor32;
      const AShowTNE: Boolean;
      const ATNEColor: TColor32;
      const AFillMode: TFillMode;
      const AFilterMode: Boolean;
      const AFillFirstDay: TDateTime;
      const AFillLastDay: TDateTime;
      const AFillColorPresetId: Integer
    );
  end;

implementation

{ TFillingMapLayerConfigStatic }

constructor TFillingMapLayerConfigStatic.Create(
  const AVisible: Boolean;
  const ASelectedMap: TGUID;
  const AUseRelativeZoom: Boolean;
  const AZoom: Byte;
  const ANoTileColor: TColor32;
  const AShowTNE: Boolean;
  const ATNEColor: TColor32;
  const AFillMode: TFillMode;
  const AFilterMode: Boolean;
  const AFillFirstDay: TDateTime;
  const AFillLastDay: TDateTime;
  const AFillColorPresetId: Integer
);
begin
  inherited Create;
  FVisible := AVisible;
  FSelectedMap := ASelectedMap;
  FUseRelativeZoom := AUseRelativeZoom;
  FZoom := AZoom;
  FNoTileColor := ANoTileColor;
  FShowTNE := AShowTNE;
  FTNEColor := ATNEColor;
  FFillMode := AFillMode;
  FFilterMode := AFilterMode;
  FFillFirstDay := AFillFirstDay;
  FFillLastDay := AFillLastDay;
  FFillColorPresetId := AFillColorPresetId;
end;

function TFillingMapLayerConfigStatic.GetNoTileColor: TColor32;
begin
  Result := FNoTileColor;
end;

function TFillingMapLayerConfigStatic.GetSelectedMap: TGUID;
begin
  Result := FSelectedMap;
end;

function TFillingMapLayerConfigStatic.GetShowTNE: Boolean;
begin
  Result := FShowTNE;
end;

function TFillingMapLayerConfigStatic.GetZoom: Byte;
begin
  Result := FZoom;
end;

function TFillingMapLayerConfigStatic.GetTNEColor: TColor32;
begin
  Result := FTNEColor;
end;

function TFillingMapLayerConfigStatic.GetUseRelativeZoom: Boolean;
begin
  Result := FUseRelativeZoom;
end;

function TFillingMapLayerConfigStatic.GetVisible: Boolean;
begin
  Result := FVisible;
end;

function TFillingMapLayerConfigStatic.GetFillMode: TFillMode;
begin
  Result := FFillMode;
end;

function TFillingMapLayerConfigStatic.GetFilterMode: Boolean;
begin
  Result := FFilterMode;
end;

function TFillingMapLayerConfigStatic.GetFillColorPresetId: Integer;
begin
  Result := FFillColorPresetId;
end;

function TFillingMapLayerConfigStatic.GetFillFirstDay: TDateTime;
begin
  Result := FFillFirstDay;
end;

function TFillingMapLayerConfigStatic.GetFillLastDay: TDateTime;
begin
  Result := FFillLastDay;
end;

end.
