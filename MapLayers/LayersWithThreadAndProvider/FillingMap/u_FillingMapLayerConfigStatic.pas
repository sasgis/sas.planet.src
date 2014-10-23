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

unit u_FillingMapLayerConfigStatic;

interface

uses
  t_Bitmap32,
  t_FillingMapModes,
  i_MapType,
  i_FillingMapLayerConfig,
  u_BaseInterfacedObject;

type
  TFillingMapLayerConfigStatic = class(TBaseInterfacedObject, IFillingMapLayerConfigStatic)
  private
    FVisible: Boolean;
    FSelectedMap: IMapType;
    FActualMap: IMapType;
    FUseRelativeZoom: Boolean;
    FZoom: Byte;
    FNoTileColor: TColor32;
    FShowTNE: Boolean;
    FTNEColor: TColor32;
    FFillMode: TFillMode;
    FFilterMode: Boolean;
    FFillFirstDay: TDateTime;
    FFillLastDay: TDateTime;
  private
    function GetVisible: Boolean;
    function GetSelectedMap: IMapType;
    function GetActualMap: IMapType;
    function GetUseRelativeZoom: Boolean;
    function GetZoom: Byte;
    function GetNoTileColor: TColor32;
    function GetShowTNE: Boolean;
    function GetTNEColor: TColor32;
    function GetFillMode: TFillMode;
    function GetFilterMode: Boolean;
    function GetFillFirstDay: TDateTime;
    function GetFillLastDay: TDateTime;
  public
    constructor Create(
      AVisible: Boolean;
      const ASelectedMap: IMapType;
      const AActualMap: IMapType;
      AUseRelativeZoom: Boolean;
      AZoom: Byte;
      ANoTileColor: TColor32;
      AShowTNE: Boolean;
      ATNEColor: TColor32;
      AFillMode: TFillMode;
      AFilterMode: Boolean;
      const AFillFirstDay: TDateTime;
      const AFillLastDay: TDateTime
    );
  end;

implementation

{ TFillingMapLayerConfigStatic }

constructor TFillingMapLayerConfigStatic.Create(
  AVisible: Boolean;
  const ASelectedMap: IMapType;
  const AActualMap: IMapType;
  AUseRelativeZoom: Boolean;
  AZoom: Byte;
  ANoTileColor: TColor32;
  AShowTNE: Boolean;
  ATNEColor: TColor32;
  AFillMode: TFillMode;
  AFilterMode: Boolean;
  const AFillFirstDay: TDateTime;
  const AFillLastDay: TDateTime
);
begin
  inherited Create;
  FVisible := AVisible;
  FSelectedMap := ASelectedMap;
  FActualMap := AActualMap;
  FUseRelativeZoom := AUseRelativeZoom;
  FZoom := AZoom;
  FNoTileColor := ANoTileColor;
  FShowTNE := AShowTNE;
  FTNEColor := ATNEColor;
  FFillMode := AFillMode;
  FFilterMode := AFilterMode;
  FFillFirstDay := AFillFirstDay;
  FFillLastDay := AFillLastDay;
end;

function TFillingMapLayerConfigStatic.GetActualMap: IMapType;
begin
  Result := FActualMap;
end;

function TFillingMapLayerConfigStatic.GetNoTileColor: TColor32;
begin
  Result := FNoTileColor;
end;

function TFillingMapLayerConfigStatic.GetSelectedMap: IMapType;
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

function TFillingMapLayerConfigStatic.GetFillFirstDay: TDateTime;
begin
  Result := FFillFirstDay;
end;

function TFillingMapLayerConfigStatic.GetFillLastDay: TDateTime;
begin
  Result := FFillLastDay;
end;

end.
