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

unit u_EnumDoublePointBySingleLine;

interface

uses
  t_GeoTypes,
  i_EnumDoublePoint,
  u_DoublePointsMetaFunc,
  u_EnumDoublePointAbstract;

type
  TEnumDoublePointBySingleLineBase = class(TEnumDoublePointAbstract)
  private
    FSourceLine: IInterface;
    FClosed: Boolean;
    FPoints: PDoublePointArray;
    FMeta: PDoublePointsMeta;
    FCount: Integer;
    FIndex: Integer;

    function DoNext(
      out APoint: TDoublePoint;
      const AMeta: PDoublePointsMetaItem
    ): Boolean; inline;
  protected
    function Next(
      out APoint: TDoublePoint
    ): Boolean; override;

    function Next(
      out APoint: TDoublePoint;
      out AMeta: TDoublePointsMetaItem
    ): Boolean; override;
  public
    constructor Create(
      const ADataOwner: IInterface;
      const AClosed: Boolean;
      const APoints: PDoublePointArray;
      const AMeta: PDoublePointsMeta;
      const ACount: Integer
    );
  end;

  TEnumDoublePointBySingleLonLatLine = class(TEnumDoublePointBySingleLineBase, IEnumLonLatPoint)
  end;

  TEnumDoublePointBySingleProjectedLine = class(TEnumDoublePointBySingleLineBase, IEnumProjectedPoint)
  public
    constructor Create(
      const ADataOwner: IInterface;
      const AClosed: Boolean;
      const APoints: PDoublePointArray;
      const ACount: Integer
    );
  end;

  TEnumLocalPointBySingleLocalLine = class(TEnumDoublePointBySingleLineBase, IEnumLocalPoint)
  public
    constructor Create(
      const ADataOwner: IInterface;
      const AClosed: Boolean;
      const APoints: PDoublePointArray;
      const ACount: Integer
    );
  end;

implementation

uses
  u_GeoFunc;

{ TEnumDoublePointBySingleLineBase }

constructor TEnumDoublePointBySingleLineBase.Create(
  const ADataOwner: IInterface;
  const AClosed: Boolean;
  const APoints: PDoublePointArray;
  const AMeta: PDoublePointsMeta;
  const ACount: Integer
);
begin
  Assert(ACount > 0, 'No points');

  inherited Create;

  FSourceLine := ADataOwner;
  FClosed := AClosed;
  FPoints := APoints;
  FMeta := AMeta;
  FCount := ACount;
  FIndex := 0;
end;

function TEnumDoublePointBySingleLineBase.Next(out APoint: TDoublePoint): Boolean;
begin
  Result := DoNext(APoint, nil);
end;

function TEnumDoublePointBySingleLineBase.Next(
  out APoint: TDoublePoint;
  out AMeta: TDoublePointsMetaItem
): Boolean;
begin
  Result := DoNext(APoint, @AMeta);
end;

function TEnumDoublePointBySingleLineBase.DoNext(
  out APoint: TDoublePoint;
  const AMeta: PDoublePointsMetaItem
): Boolean;
begin
  if FIndex < FCount then begin
    APoint := FPoints[FIndex];
    if AMeta <> nil then begin
      SetMetaItem(FMeta, FIndex, AMeta);
    end;
    Inc(FIndex);
    Result := True;
  end else begin
    if FIndex = FCount then begin
      if FClosed and (FCount > 1) then begin
        APoint := FPoints[0];
        if AMeta <> nil then begin
          SetMetaItem(FMeta, 0, AMeta);
        end;
        Result := True;
      end else begin
        APoint := CEmptyDoublePoint;
        if AMeta <> nil then begin
          ResetMetaItem(AMeta);
        end;
        Result := False;
      end;
      FPoints := nil;
      FMeta := nil;
      FSourceLine := nil;
      Inc(FIndex);
    end else begin
      APoint := CEmptyDoublePoint;
      if AMeta <> nil then begin
        ResetMetaItem(AMeta);
      end;
      Result := False;
    end;
  end;
end;

{ TEnumDoublePointBySingleProjectedLine }

constructor TEnumDoublePointBySingleProjectedLine.Create(
  const ADataOwner: IInterface;
  const AClosed: Boolean;
  const APoints: PDoublePointArray;
  const ACount: Integer
);
begin
  inherited Create(ADataOwner, AClosed, APoints, nil, ACount);
end;

{ TEnumLocalPointBySingleLocalLine }

constructor TEnumLocalPointBySingleLocalLine.Create(
  const ADataOwner: IInterface;
  const AClosed: Boolean;
  const APoints: PDoublePointArray;
  const ACount: Integer
);
begin
  inherited Create(ADataOwner, AClosed, APoints, nil, ACount);
end;

end.
