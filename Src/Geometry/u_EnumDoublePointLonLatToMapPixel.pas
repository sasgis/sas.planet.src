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

unit u_EnumDoublePointLonLatToMapPixel;

interface

uses
  t_GeoTypes,
  i_ProjectionType,
  i_Projection,
  i_DoublePointFilter,
  i_EnumDoublePoint,
  u_EnumDoublePointAbstract,
  u_BaseInterfacedObject;

type
  TEnumDoublePointLonLatToMapPixel = class(TEnumDoublePointAbstract, IEnumProjectedPoint)
  private
    FSourceEnum: IEnumLonLatPoint;
    FProjection: IProjection;
    FProjectionType: IProjectionType;
    FFinished: Boolean;
  protected
    function Next(out APoint: TDoublePoint): Boolean; override;
  public
    constructor Create(
      const AProjection: IProjection;
      const ASourceEnum: IEnumLonLatPoint
    );
  end;

type
  TLonLatPointConverter = class(TBaseInterfacedObject, ILonLatPointConverter)
  private
    FProjection: IProjection;
  private
    function CreateFilteredEnum(const ASource: IEnumLonLatPoint): IEnumProjectedPoint;
  public
    constructor Create(
      const AProjection: IProjection
    );
  end;

implementation

uses
  Math,
  u_GeoFunc;

{ TEnumDoublePointLonLatToMapPixels }

constructor TEnumDoublePointLonLatToMapPixel.Create(
  const AProjection: IProjection;
  const ASourceEnum: IEnumLonLatPoint
);
begin
  Assert(Assigned(AProjection));
  Assert(Assigned(ASourceEnum));
  inherited Create;
  FSourceEnum := ASourceEnum;
  FProjection := AProjection;
  FProjectionType := FProjection.ProjectionType;
  FFinished := False;
end;

function TEnumDoublePointLonLatToMapPixel.Next(
  out APoint: TDoublePoint): Boolean;
var
  VPoint: TDoublePoint;
begin
  if FFinished then begin
    Result := False;
    APoint := CEmptyDoublePoint;
  end else begin
    if FSourceEnum.Next(VPoint) then begin
      if PointIsEmpty(VPoint) then begin
        APoint := VPoint;
      end else begin
        FProjectionType.ValidateLonLatPos(VPoint);
        APoint := FProjection.LonLat2PixelPosFloat(VPoint);
      end;
      Result := True;
    end else begin
      FFinished := True;
      Result := False;
      APoint := CEmptyDoublePoint;
    end;
  end;
end;

{ TLonLatPointConverter }

constructor TLonLatPointConverter.Create(
  const AProjection: IProjection
);
begin
  Assert(Assigned(AProjection));
  inherited Create;
  FProjection := AProjection;
end;

function TLonLatPointConverter.CreateFilteredEnum(
  const ASource: IEnumLonLatPoint
): IEnumProjectedPoint;
begin
  Result := TEnumDoublePointLonLatToMapPixel.Create(FProjection, ASource);
end;

end.
