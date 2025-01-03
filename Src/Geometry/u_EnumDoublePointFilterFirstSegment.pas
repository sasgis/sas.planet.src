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

unit u_EnumDoublePointFilterFirstSegment;

interface

uses
  t_GeoTypes,
  i_DoublePointFilter,
  i_EnumDoublePoint,
  u_EnumDoublePointAbstract,
  u_BaseInterfacedObject;

type
  TEnumDoublePointFilterFirstSegment = class(TEnumDoublePointAbstract)
  private
    FSourceEnum: IEnumDoublePoint;
    FStarted: Boolean;
    FFinished: Boolean;
  protected
    function Next(out APoint: TDoublePoint): Boolean; override;
  public
    constructor Create(
      const ASourceEnum: IEnumDoublePoint
    );
  end;

  TEnumLonLatPointFilterFirstSegment = class(TEnumDoublePointFilterFirstSegment, IEnumLonLatPoint)
  public
    constructor Create(
      const ASourceEnum: IEnumLonLatPoint
    );
  end;

  TEnumProjectedPointFilterFirstSegment = class(TEnumDoublePointFilterFirstSegment, IEnumProjectedPoint)
  public
    constructor Create(
      const ASourceEnum: IEnumProjectedPoint
    );
  end;

  TDoublePointFilterFirstSegment = class(TBaseInterfacedObject, IDoublePointFilter)
  private
    function CreateFilteredEnum(const ASource: IEnumDoublePoint): IEnumDoublePoint;
  end;

  TLonLatPointFilterFirstSegment = class(TBaseInterfacedObject, ILonLatPointFilter)
  private
    function CreateFilteredEnum(const ASource: IEnumLonLatPoint): IEnumLonLatPoint;
  end;

  TProjectedPointFilterFirstSegment = class(TBaseInterfacedObject, IProjectedPointFilter)
  private
    function CreateFilteredEnum(const ASource: IEnumProjectedPoint): IEnumProjectedPoint;
  end;


implementation

uses
  Math,
  u_GeoFunc;

{ TEnumDoublePointFilterFirstSegment }

constructor TEnumDoublePointFilterFirstSegment.Create(
  const ASourceEnum: IEnumDoublePoint);
begin
  inherited Create;
  FSourceEnum := ASourceEnum;
  FStarted := False;
  FFinished := False;
end;

function TEnumDoublePointFilterFirstSegment.Next(
  out APoint: TDoublePoint): Boolean;
var
  VPoint: TDoublePoint;
begin
  while not FFinished do begin
    if FSourceEnum.Next(VPoint) then begin
      if FStarted then begin
        if PointIsEmpty(VPoint) then begin
          FFinished := True;
        end else begin
          APoint := VPoint;
          Break;
        end;
      end else begin
        if not PointIsEmpty(VPoint) then begin
          FStarted := True;
          APoint := VPoint;
          Break;
        end;
      end;
    end else begin
      FFinished := True;
    end;
  end;
  Result := not FFinished;
end;

{ TEnumLonLatPointFilterFirstSegment }

constructor TEnumLonLatPointFilterFirstSegment.Create(
  const ASourceEnum: IEnumLonLatPoint
);
begin
  inherited Create(ASourceEnum);
end;

{ TEnumProjectedPointFilterFirstSegment }

constructor TEnumProjectedPointFilterFirstSegment.Create(
  const ASourceEnum: IEnumProjectedPoint
);
begin
  inherited Create(ASourceEnum);
end;

{ TDoublePointFilterFirstSegment }

function TDoublePointFilterFirstSegment.CreateFilteredEnum(
  const ASource: IEnumDoublePoint
): IEnumDoublePoint;
begin
  Result := TEnumDoublePointFilterFirstSegment.Create(ASource);
end;

{ TLonLatPointFilterFirstSegment }

function TLonLatPointFilterFirstSegment.CreateFilteredEnum(
  const ASource: IEnumLonLatPoint
): IEnumLonLatPoint;
begin
  Result := TEnumLonLatPointFilterFirstSegment.Create(ASource);
end;

{ TProjectedPointFilterFirstSegment }

function TProjectedPointFilterFirstSegment.CreateFilteredEnum(
  const ASource: IEnumProjectedPoint
): IEnumProjectedPoint;
begin
  Result := TEnumProjectedPointFilterFirstSegment.Create(ASource);
end;

end.
