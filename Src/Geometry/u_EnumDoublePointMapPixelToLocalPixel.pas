{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2021, SAS.Planet development team.                      *}
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

unit u_EnumDoublePointMapPixelToLocalPixel;

interface

uses
  t_GeoTypes,
  i_LocalCoordConverter,
  i_DoublePointFilter,
  i_EnumDoublePoint,
  u_BaseInterfacedObject;

type
  TEnumDoublePointMapPixelToLocalPixel = class(TBaseInterfacedObject, IEnumLocalPoint)
  private
    FSourceEnum: IEnumProjectedPoint;
    FLocalConverter: ILocalCoordConverter;
    FFinished: Boolean;
  private
    function Next(out APoint: TDoublePoint): Boolean;
  public
    constructor Create(
      const ALocalConverter: ILocalCoordConverter;
      const ASourceEnum: IEnumProjectedPoint
    );
  end;

  TEnumDoublePointMapPixelToLocalPixelSimple = class(TBaseInterfacedObject, IEnumLocalPoint)
  private
    FSourceEnum: IEnumProjectedPoint;
    FTopLeft: TDoublePoint;
    FFinished: Boolean;
  private
    function Next(out APoint: TDoublePoint): Boolean;
  public
    constructor Create(
      const ATopLeft: TDoublePoint;
      const ASourceEnum: IEnumProjectedPoint
    );
  end;

  TProjectedPointConverter = class(TBaseInterfacedObject, IProjectedPointConverter)
  private
    FLocalConverter: ILocalCoordConverter;
  private
    function CreateFilteredEnum(const ASource: IEnumProjectedPoint): IEnumLocalPoint;
  public
    constructor Create(
      const ALocalConverter: ILocalCoordConverter
    );
  end;

implementation

uses
  Math,
  u_GeoFunc;

{ TEnumDoublePointMapPixelToLocalPixel }

constructor TEnumDoublePointMapPixelToLocalPixel.Create(
  const ALocalConverter: ILocalCoordConverter;
  const ASourceEnum: IEnumProjectedPoint
);
begin
  inherited Create;
  FSourceEnum := ASourceEnum;
  FLocalConverter := ALocalConverter;
  FFinished := False;
end;

function TEnumDoublePointMapPixelToLocalPixel.Next(
  out APoint: TDoublePoint
): Boolean;
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
        APoint := FLocalConverter.MapPixelFloat2LocalPixelFloat(VPoint);
      end;
      Result := True;
    end else begin
      FFinished := True;
      Result := False;
      APoint := CEmptyDoublePoint;
    end;
  end;
end;

{ TProjectedPointConverter }

constructor TProjectedPointConverter.Create(
  const ALocalConverter: ILocalCoordConverter
);
begin
  inherited Create;
  FLocalConverter := ALocalConverter;
end;

function TProjectedPointConverter.CreateFilteredEnum(
  const ASource: IEnumProjectedPoint
): IEnumLocalPoint;
begin
  Result := TEnumDoublePointMapPixelToLocalPixel.Create(FLocalConverter, ASource);
end;

{ TEnumDoublePointMapPixelToLocalPixelSimple }

constructor TEnumDoublePointMapPixelToLocalPixelSimple.Create(
  const ATopLeft: TDoublePoint;
  const ASourceEnum: IEnumProjectedPoint
);
begin
  inherited Create;
  FSourceEnum := ASourceEnum;
  FTopLeft := ATopLeft;
  FFinished := False;
end;

function TEnumDoublePointMapPixelToLocalPixelSimple.Next(
  out APoint: TDoublePoint
): Boolean;
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
        APoint := DoublePoint(VPoint.X - FTopLeft.X, VPoint.Y - FTopLeft.Y);
      end;
      Result := True;
    end else begin
      FFinished := True;
      Result := False;
      APoint := CEmptyDoublePoint;
    end;
  end;
end;

end.
