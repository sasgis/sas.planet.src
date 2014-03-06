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

unit u_EnumDoublePointFilterEqual;

interface

uses
  t_GeoTypes,
  i_DoublePointFilter,
  i_EnumDoublePoint,
  u_BaseInterfacedObject;

type
  TEnumDoublePointFilterEqual = class(TBaseInterfacedObject, IEnumDoublePoint)
  private
    FSourceEnum: IEnumDoublePoint;
    FPrevEmpty: Boolean;
    FPrevPoint: TDoublePoint;
    FFinished: Boolean;
  private
    function Next(out APoint: TDoublePoint): Boolean;
  public
    constructor Create(
      const ASourceEnum: IEnumDoublePoint
    );
  end;

  TEnumProjectedPointFilterEqual = class(TEnumDoublePointFilterEqual, IEnumProjectedPoint)
  public
    constructor Create(
      const ASourceEnum: IEnumProjectedPoint
    );
  end;

  TEnumLocalPointFilterEqual = class(TEnumDoublePointFilterEqual, IEnumLocalPoint)
  public
    constructor Create(
      const ASourceEnum: IEnumLocalPoint
    );
  end;

  TDoublePointFilterRemoveEqual = class(TBaseInterfacedObject, IDoublePointFilter)
  private
    function CreateFilteredEnum(const ASource: IEnumDoublePoint): IEnumDoublePoint;
  end;

  TProjectedPointFilterRemoveEqual = class(TBaseInterfacedObject, IProjectedPointFilter)
  private
    function CreateFilteredEnum(const ASource: IEnumProjectedPoint): IEnumProjectedPoint;
  end;

  TLocalPointFilterRemoveEqual = class(TBaseInterfacedObject, ILocalPointFilter)
  private
    function CreateFilteredEnum(const ASource: IEnumLocalPoint): IEnumLocalPoint;
  end;

implementation

uses
  u_GeoFunc;

{ TEnumDoublePointFilterEqual }

constructor TEnumDoublePointFilterEqual.Create(const ASourceEnum: IEnumDoublePoint);
begin
  inherited Create;
  FSourceEnum := ASourceEnum;
  FPrevEmpty := True;
  FFinished := False;
end;

function TEnumDoublePointFilterEqual.Next(out APoint: TDoublePoint): Boolean;
var
  VPoint: TDoublePoint;
  VPointIsEmpty: Boolean;
begin
  while not FFinished do begin
    if FSourceEnum.Next(VPoint) then begin
      VPointIsEmpty := PointIsEmpty(VPoint);
      if VPointIsEmpty then begin
        if not FPrevEmpty then begin
          FPrevEmpty := True;
          FPrevPoint := VPoint;
          APoint := VPoint;
          Break;
        end;
      end else begin
        if FPrevEmpty then begin
          FPrevEmpty := False;
          FPrevPoint := VPoint;
          APoint := VPoint;
          Break;
        end else begin
          if (abs(VPoint.X - FPrevPoint.X) > 1) or (abs(VPoint.Y - FPrevPoint.Y) > 1) then begin
            FPrevEmpty := False;
            FPrevPoint := VPoint;
            APoint := VPoint;
            Break;
          end;
        end;
      end;
    end else begin
      FFinished := True;
    end;
  end;
  Result := not FFinished;
end;

{ TEnumProjectedPointFilterEqual }

constructor TEnumProjectedPointFilterEqual.Create(
  const ASourceEnum: IEnumProjectedPoint
);
begin
  inherited Create(ASourceEnum);
end;

{ TEnumLocalPointFilterEqual }

constructor TEnumLocalPointFilterEqual.Create(
  const ASourceEnum: IEnumLocalPoint
);
begin
  inherited Create(ASourceEnum);
end;

{ TDoublePointFilterRemoveEqual }

function TDoublePointFilterRemoveEqual.CreateFilteredEnum(
  const ASource: IEnumDoublePoint
): IEnumDoublePoint;
begin
  Result := TEnumDoublePointFilterEqual.Create(ASource);
end;

{ TProjectedPointFilterRemoveEqual }

function TProjectedPointFilterRemoveEqual.CreateFilteredEnum(
  const ASource: IEnumProjectedPoint
): IEnumProjectedPoint;
begin
  Result := TEnumProjectedPointFilterEqual.Create(ASource);
end;

{ TLocalPointFilterRemoveEqual }

function TLocalPointFilterRemoveEqual.CreateFilteredEnum(
  const ASource: IEnumLocalPoint
): IEnumLocalPoint;
begin
  Result := TEnumLocalPointFilterEqual.Create(ASource);
end;

end.
