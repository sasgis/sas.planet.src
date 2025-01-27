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

unit u_FillingMapColorerSimple;

interface

uses
  t_Bitmap32,
  t_FillingMapModes,
  i_TileInfoBasic,
  i_FillingMapColorer,
  u_BaseInterfacedObject;

type
  TFillingMapColorerSimple = class(TBaseInterfacedObject, IFillingMapColorer)
  private
    FNoTileColor: TColor32;
    FShowTNE: Boolean;
    FTNEColor: TColor32;
    FFillMode: TFillMode;
    FFilterMode: Boolean;
    FFillFirstDay: TDateTime;
    FFillLastDay: TDateTime;
    FGradientDays: Integer;
    FFillColorPresetId: Integer;
  private
    { IFillingMapColorer }
    function GetColor(const ATileInfo: ITileInfoBasic): TColor32; overload;
    function GetColor(const ATileInfo: TTileInfo): TColor32; overload;
  public
    constructor Create(
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

uses
  Types,
  SysUtils,
  DateUtils,
  GR32;

{ TFillingMapColorerSimple }

constructor TFillingMapColorerSimple.Create(
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

  FNoTileColor := ANoTileColor;
  FShowTNE := AShowTNE;
  FTNEColor := ATNEColor;
  FFillMode := AFillMode;
  FFilterMode := AFilterMode;
  if FFilterMode then begin
    FFillFirstDay := AFillFirstDay;
    FFillLastDay := AFillLastDay;
  end else begin
    FFillFirstDay := EncodeDate(2000, 1, 1);
    FFillLastDay := DateOf(Now);
  end;
  FGradientDays := Trunc(FFillLastDay + 1.0 - FFillFirstDay);
  FFillColorPresetId := AFillColorPresetId;
end;

function TFillingMapColorerSimple.GetColor(const ATileInfo: ITileInfoBasic): TColor32;
var
  VTileInfo: TTileInfo;
  VInfoWithData: ITileInfoWithData;
begin
  if Assigned(ATileInfo) then begin
    VTileInfo.FLoadDate := ATileInfo.LoadDate;
    VTileInfo.FSize := ATileInfo.Size;
    VTileInfo.FVersionInfo := ATileInfo.VersionInfo;
    VTileInfo.FContentType := ATileInfo.ContentType;
    if Supports(ATileInfo, ITileInfoWithData, VInfoWithData) then begin
      VTileInfo.FData := VInfoWithData.TileData;
    end else begin
      VTileInfo.FData := nil;
    end;
    if ATileInfo.IsExists then begin
      VTileInfo.FInfoType := titExists;
    end else
    if ATileInfo.IsExistsTNE then begin
      VTileInfo.FInfoType := titTneExists;
    end else begin
      VTileInfo.FInfoType := titNotExists;
    end;
  end else begin
    VTileInfo.FInfoType := titNotExists;
    VTileInfo.FVersionInfo := nil;
    VTileInfo.FContentType := nil;
    VTileInfo.FData := nil;
  end;

  Result := GetColor(VTileInfo);
end;

function TFillingMapColorerSimple.GetColor(
  const ATileInfo: TTileInfo
): TColor32;
var
  VFileDate: TDateTime;
  VDateCompare: TValueRelationship;
  VC1, VC2: Double;
begin
  Result := 0;
  if ATileInfo.FInfoType = titExists then begin
    if FFillMode = fmExisting then begin
      if FFilterMode then begin
        VFileDate := ATileInfo.FLoadDate;
        VDateCompare := CompareDate(VFileDate, FFillLastDay);
        if VDateCompare < GreaterThanValue then begin
          VDateCompare := CompareDate(VFileDate, FFillFirstDay);
          if VDateCompare > LessThanValue then begin
            Result := FNoTileColor;
          end;
        end;
      end else begin
        Result := FNoTileColor;
      end;
    end else
    if FFillMode = fmGradient then begin
      VFileDate := ATileInfo.FLoadDate;
      VDateCompare := CompareDate(VFileDate, FFillLastDay);
      if VDateCompare <> GreaterThanValue then begin
        VDateCompare := CompareDate(VFileDate, FFillFirstDay);
        if VDateCompare <> LessThanValue then begin
          VFileDate := FFillLastDay + 1.0 - VFileDate;
          VC1 := 255.0 * (-1.0 + 2.0 * VFileDate / FGradientDays);
          if VC1 > 255.0 then begin
            VC1 := 255.0;
          end;
          if VC1 < 0.0 then begin
            VC1 := 0.0;
          end;
          VC2 := 255.0 * 2.0 * VFileDate / FGradientDays;
          if VC2 > 255.0 then begin
            VC2 := 255.0;
          end;
          if VC2 < 0.0 then begin
            VC2 := 0.0;
          end;
          case FFillColorPresetId of
            1: begin // purple
              Result := Color32(Trunc(255.0 - VC2), Trunc(VC1 * 0.5), Trunc(255.0 - VC1), AlphaComponent(FNoTileColor));
            end;
            2: begin // blue
              Result := Color32(Trunc(VC1), Trunc(VC2), Trunc(255.0 - VC1), AlphaComponent(FNoTileColor));
            end;
            3: begin // red
              Result := Color32(Trunc(255.0 - VC2), Trunc(VC1), Trunc(VC2 - VC1), AlphaComponent(FNoTileColor));
            end;
          else // green
            Result := Color32(Trunc(VC1), Trunc(255.0 - VC2), Trunc(VC2 - VC1), AlphaComponent(FNoTileColor));
          end;
        end;
      end;
    end;
  end else begin
    if FFillMode = fmUnexisting then begin
      Result := FNoTileColor;
    end;
    if FShowTNE then begin
      if ATileInfo.FInfoType = titTneExists then begin
        Result := FTNEColor;
      end;
    end;
  end;
end;

end.
