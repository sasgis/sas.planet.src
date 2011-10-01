{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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

unit u_FillingMapLayerConfigStatic;

interface

uses
  GR32,
  i_LocalCoordConverter,
  i_MapTypes,
  i_FillingMapLayerConfig;

type
  TFillingMapLayerConfigStatic = class(TInterfacedObject, IFillingMapLayerConfigStatic)
  private
    FVisible: Boolean;
    FSourceMap: IMapType;
    FUseRelativeZoom: Boolean;
    FZoom: Byte;
    FNoTileColor: TColor32;
    FShowTNE: Boolean;
    FTNEColor: TColor32;
  protected
    function GetVisible: Boolean;
    function GetSourceMap: IMapType;
    function GetUseRelativeZoom: Boolean;
    function GetZoom: Byte;
    function GetNoTileColor: TColor32;
    function GetShowTNE: Boolean;
    function GetTNEColor: TColor32;

    function GetActualZoom(ALocalConverter: ILocalCoordConverter): Byte;
  public
    constructor Create(
      AVisible: Boolean;
      ASourceMap: IMapType;
      AUseRelativeZoom: Boolean;
      AZoom: Byte;
      ANoTileColor: TColor32;
      AShowTNE: Boolean;
      ATNEColor: TColor32
    );
  end;

implementation

{ TFillingMapLayerConfigStatic }

constructor TFillingMapLayerConfigStatic.Create(
  AVisible: Boolean;
  ASourceMap: IMapType;
  AUseRelativeZoom: Boolean;
  AZoom: Byte;
  ANoTileColor: TColor32;
  AShowTNE: Boolean;
  ATNEColor: TColor32
);
begin
  FVisible := AVisible;
  FSourceMap := ASourceMap;
  FUseRelativeZoom := AUseRelativeZoom;
  FZoom := AZoom;
  FNoTileColor := ANoTileColor;
  FShowTNE := AShowTNE;
  FTNEColor := ATNEColor;
end;

function TFillingMapLayerConfigStatic.GetActualZoom(
  ALocalConverter: ILocalCoordConverter): Byte;
var
  VZoom: Integer;
begin
  VZoom := FZoom;
  if FUseRelativeZoom then begin
    VZoom := FZoom + ALocalConverter.GetZoom;
  end;
  if VZoom < 0 then begin
    Result := 0;
  end else begin
    Result := VZoom;
    ALocalConverter.GetGeoConverter.CheckZoom(Result);
  end;
end;

function TFillingMapLayerConfigStatic.GetNoTileColor: TColor32;
begin
  Result := FNoTileColor;
end;

function TFillingMapLayerConfigStatic.GetShowTNE: Boolean;
begin
  Result := FShowTNE;
end;

function TFillingMapLayerConfigStatic.GetSourceMap: IMapType;
begin
  Result := FSourceMap;
end;

function TFillingMapLayerConfigStatic.GetZoom: Byte;
begin
  Result := FZoom
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

end.
