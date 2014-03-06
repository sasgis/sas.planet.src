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

unit u_EnumDoublePointBySingleLine;

interface

uses
  t_GeoTypes,
  i_EnumDoublePoint,
  u_BaseInterfacedObject;

type
  TEnumDoublePointBySingleLineBase = class(TBaseInterfacedObject, IEnumDoublePoint)
  private
    FSourceLine: IInterface;
    FClosed: Boolean;
    FPoints: PDoublePointArray;
    FCount: Integer;
    FIndex: Integer;
  private
    function Next(out APoint: TDoublePoint): Boolean;
  public
    constructor Create(
      const ADataOwner: IInterface;
      AClosed: Boolean;
      const APoints: PDoublePointArray;
      ACount: Integer
    );
  end;

  TEnumDoublePointBySingleLonLatLine = class(TEnumDoublePointBySingleLineBase, IEnumLonLatPoint)
  end;

  TEnumDoublePointBySingleProjectedLine = class(TEnumDoublePointBySingleLineBase, IEnumProjectedPoint)
  end;

  TEnumLocalPointBySingleLocalLine = class(TEnumDoublePointBySingleLineBase, IEnumLocalPoint)
  end;

implementation

uses
  u_GeoFunc;

{ TEnumDoublePointBySingleLineBase }

constructor TEnumDoublePointBySingleLineBase.Create(
  const ADataOwner: IInterface;
  AClosed: Boolean;
  const APoints: PDoublePointArray;
  ACount: Integer
);
begin
  inherited Create;
  FSourceLine := ADataOwner;
  FClosed := AClosed;
  FPoints := APoints;
  FCount := ACount;
  FIndex := 0;
  Assert(FCount > 0, 'No points');
end;

function TEnumDoublePointBySingleLineBase.Next(out APoint: TDoublePoint): Boolean;
begin
  if FIndex < FCount then begin
    APoint := FPoints[FIndex];
    Inc(FIndex);
    Result := True;
  end else begin
    if (FIndex = FCount) then begin
      if FClosed and (FCount > 1) then begin
        APoint := FPoints[0];
        Result := True;
      end else begin
        APoint := CEmptyDoublePoint;
        Result := False;
      end;
      FPoints := nil;
      FSourceLine := nil;
      Inc(FIndex);
    end else begin
      APoint := CEmptyDoublePoint;
      Result := False;
    end;
  end;
end;

end.
