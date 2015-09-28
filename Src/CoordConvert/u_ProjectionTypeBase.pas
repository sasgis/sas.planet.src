{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit u_ProjectionTypeBase;

interface

uses
  SysUtils,
  t_Hash,
  t_GeoTypes,
  i_Datum,
  i_ProjectionType,
  u_BaseInterfacedObject;

type
  TProjectionTypeBase = class(TBaseInterfacedObject, IProjectionType)
  private
    FHash: THashValue;
    FDatum: IDatum;
    FProjectionEPSG: Integer;

    FValidLonLatRect: TDoubleRect;
    procedure ValidateRelativePosInternal(var APoint: TDoublePoint); inline;
    procedure ValidateRelativeRectInternal(var ARect: TDoubleRect); inline;

    procedure ValidateLonLatPosInternal(var APoint: TDoublePoint); inline;
    procedure ValidateLonLatRectInternal(var ARect: TDoubleRect); inline;
  protected
    function Relative2LonLatInternal(const APoint: TDoublePoint): TDoublePoint; virtual; abstract;
    function LonLat2RelativeInternal(const APoint: TDoublePoint): TDoublePoint; virtual; abstract;
  private
    function GetHash: THashValue;
    function GetDatum: IDatum;
    function GetProjectionEPSG: Integer;

    function Relative2LonLat(const APoint: TDoublePoint): TDoublePoint;
    function RelativeRect2LonLatRect(const ARect: TDoubleRect): TDoubleRect;

    function LonLat2Relative(const APoint: TDoublePoint): TDoublePoint;
    function LonLatRect2RelativeRect(const ARect: TDoubleRect): TDoubleRect;

    function LonLat2Metr(const APoint: TDoublePoint): TDoublePoint;
    function Metr2LonLat(const APoint: TDoublePoint): TDoublePoint;

    procedure ValidateRelativePos(var APoint: TDoublePoint);
    procedure ValidateRelativeRect(var ARect: TDoubleRect);

    procedure ValidateLonLatPos(var APoint: TDoublePoint);
    procedure ValidateLonLatRect(var ARect: TDoubleRect);

    function CheckRelativePos(const APoint: TDoublePoint): boolean;
    function CheckRelativeRect(const ARect: TDoubleRect): boolean;

    function CheckLonLatPos(const APoint: TDoublePoint): boolean;
    function CheckLonLatRect(const ARect: TDoubleRect): boolean;

    function IsSame(const AOther: IProjectionType): Boolean;
  public
    constructor Create(
      const AHash: THashValue;
      const ADatum: IDatum;
      const AProjEPSG: integer
    );
  end;

implementation

uses
  u_GeoFunc;

{ TProjectionTypeBase }

constructor TProjectionTypeBase.Create(
  const AHash: THashValue;
  const ADatum: IDatum;
  const AProjEPSG: integer
);
begin
  inherited Create;
  FHash := AHash;
  FDatum := ADatum;
  FProjectionEPSG := AProjEPSG;
  FValidLonLatRect := RelativeRect2LonLatRect(DoubleRect(0, 0, 1, 1));
end;

function TProjectionTypeBase.CheckLonLatPos(const APoint: TDoublePoint): boolean;
begin
  Result := True;
  if APoint.X < FValidLonLatRect.Left then begin
    Result := False;
    Exit;
  end else begin
    if APoint.X > FValidLonLatRect.Right then begin
      Result := False;
      Exit;
    end;
  end;
  if APoint.Y < FValidLonLatRect.Bottom then begin
    Result := False;
    Exit;
  end else begin
    if APoint.Y > FValidLonLatRect.Top then begin
      Result := False;
      Exit;
    end;
  end;
end;

function TProjectionTypeBase.CheckLonLatRect(const ARect: TDoubleRect): boolean;
begin
  Result := True;
  if ARect.Left < FValidLonLatRect.Left then begin
    Result := False;
    Exit;
  end else begin
    if ARect.Left > FValidLonLatRect.Right then begin
      Result := False;
      Exit;
    end;
  end;
  if ARect.Bottom < FValidLonLatRect.Bottom then begin
    Result := False;
    Exit;
  end else begin
    if ARect.Bottom > FValidLonLatRect.Top then begin
      Result := False;
      Exit;
    end;
  end;

  if ARect.Right < FValidLonLatRect.Left then begin
    Result := False;
    Exit;
  end else begin
    if ARect.Right > FValidLonLatRect.Right then begin
      Result := False;
      Exit;
    end;
  end;
  if ARect.Top < FValidLonLatRect.Bottom then begin
    Result := False;
    Exit;
  end else begin
    if ARect.Top > FValidLonLatRect.Top then begin
      Result := False;
      Exit;
    end;
  end;
end;

function TProjectionTypeBase.CheckRelativePos(const APoint: TDoublePoint): boolean;
begin
  Result := True;
  if APoint.X < 0 then begin
    Result := False;
    Exit;
  end else begin
    if APoint.X > 1 then begin
      Result := False;
      Exit;
    end;
  end;

  if APoint.Y < 0 then begin
    Result := False;
    Exit;
  end else begin
    if APoint.Y > 1 then begin
      Result := False;
      Exit;
    end;
  end;
end;

function TProjectionTypeBase.CheckRelativeRect(const ARect: TDoubleRect): boolean;
begin
  Result := True;
  if ARect.Left < 0 then begin
    Result := False;
    Exit;
  end else begin
    if ARect.Left > 1 then begin
      Result := False;
      Exit;
    end;
  end;

  if ARect.Top < 0 then begin
    Result := False;
    Exit;
  end else begin
    if ARect.Top > 1 then begin
      Result := False;
      Exit;
    end;
  end;

  if ARect.Right < 0 then begin
    Result := False;
    Exit;
  end else begin
    if ARect.Right > 1 then begin
      Result := False;
      Exit;
    end;
  end;

  if ARect.Bottom < 0 then begin
    Result := False;
    Exit;
  end else begin
    if ARect.Bottom > 1 then begin
      Result := False;
      Exit;
    end;
  end;
end;

function TProjectionTypeBase.GetDatum: IDatum;
begin
  Result := FDatum;
end;

function TProjectionTypeBase.GetHash: THashValue;
begin
  Result := FHash;
end;

function TProjectionTypeBase.GetProjectionEPSG: Integer;
begin
  Result := FProjectionEPSG;
end;

function TProjectionTypeBase.IsSame(const AOther: IProjectionType): Boolean;
begin
  if not Assigned(AOther) then begin
    Result := False;
    Exit;
  end;
  if IProjectionType(Self) = AOther then begin
    Result := True;
  end else if (FHash <> 0) and (AOther.Hash <> 0) and (FHash <> AOther.Hash) then begin
    Result := False;
  end else if not FDatum.IsSameDatum(AOther.Datum) then begin
    Result := False;
  end else if AOther.GetProjectionEPSG <> FProjectionEPSG then begin
    Result := False;
  end else begin
    Result := True;
  end;
end;

function TProjectionTypeBase.LonLat2Metr(const APoint: TDoublePoint): TDoublePoint;
var
  VLonLat: TDoublePoint;
  VRelative: TDoublePoint;
  VMul: Double;
begin
  VLonLat := APoint;
  ValidateLonLatPosInternal(VLonLat);
  VRelative := LonLat2RelativeInternal(VLonLat);
  VMul := 2 * Pi * GetDatum.GetSpheroidRadiusA;
  Result.X := (VRelative.X - 0.5) * VMul;
  Result.Y := (0.5 - VRelative.Y) * VMul;
end;

function TProjectionTypeBase.LonLat2Relative(
  const APoint: TDoublePoint
): TDoublePoint;
var
  VXY: TDoublePoint;
begin
  VXY := APoint;
  ValidateLonLatPosInternal(VXY);
  Result := LonLat2RelativeInternal(VXY);
end;

function TProjectionTypeBase.LonLatRect2RelativeRect(
  const ARect: TDoubleRect
): TDoubleRect;
var
  VXY: TDoubleRect;
begin
  VXY := ARect;
  ValidateLonLatRectInternal(VXY);
  Result.TopLeft := LonLat2RelativeInternal(VXY.TopLeft);
  Result.BottomRight := LonLat2RelativeInternal(VXY.BottomRight);
end;

function TProjectionTypeBase.Metr2LonLat(const APoint: TDoublePoint): TDoublePoint;
var
  VRelative: TDoublePoint;
  VMul: Double;
begin
  VMul := 1.0 / (2 * Pi * GetDatum.GetSpheroidRadiusA);
  VRelative.X := APoint.X * VMul - 0.5;
  VRelative.Y := 0.5 - APoint.Y * VMul;
  Result := Relative2LonLatInternal(VRelative);
end;

function TProjectionTypeBase.Relative2LonLat(
  const APoint: TDoublePoint
): TDoublePoint;
var
  VXY: TDoublePoint;
begin
  VXY := APoint;
  ValidateRelativePosInternal(VXY);
  Result := Relative2LonLatInternal(VXY);
end;

function TProjectionTypeBase.RelativeRect2LonLatRect(
  const ARect: TDoubleRect
): TDoubleRect;
var
  VXY: TDoubleRect;
begin
  VXY := ARect;
  ValidateRelativeRectInternal(VXY);
  Result.TopLeft := Relative2LonLatInternal(VXY.TopLeft);
  Result.BottomRight := Relative2LonLatInternal(VXY.BottomRight);
end;

procedure TProjectionTypeBase.ValidateLonLatPos(var APoint: TDoublePoint);
begin
  if APoint.X < FValidLonLatRect.Left then begin
    APoint.X := FValidLonLatRect.Left;
  end else begin
    if APoint.X > FValidLonLatRect.Right then begin
      APoint.X := FValidLonLatRect.Right;
    end;
  end;
  if APoint.Y < FValidLonLatRect.Bottom then begin
    APoint.Y := FValidLonLatRect.Bottom;
  end else begin
    if APoint.Y > FValidLonLatRect.Top then begin
      APoint.Y := FValidLonLatRect.Top;
    end;
  end;
end;

procedure TProjectionTypeBase.ValidateLonLatPosInternal(var APoint: TDoublePoint);
begin
  if APoint.X < FValidLonLatRect.Left then begin
    Assert(False, 'Longitude must be not less then ' + FloatToStr(FValidLonLatRect.Left));
    APoint.X := FValidLonLatRect.Left;
  end else begin
    if APoint.X > FValidLonLatRect.Right then begin
      Assert(False, 'Longitude must be less then  ' + FloatToStr(FValidLonLatRect.Right));
      APoint.X := FValidLonLatRect.Right;
    end;
  end;
  if APoint.Y < FValidLonLatRect.Bottom then begin
    Assert(False, 'Latitude must be not less then ' + FloatToStr(FValidLonLatRect.Bottom));
    APoint.Y := FValidLonLatRect.Bottom;
  end else begin
    if APoint.Y > FValidLonLatRect.Top then begin
      Assert(False, 'Latitude must be less then ' + FloatToStr(FValidLonLatRect.Top));
      APoint.Y := FValidLonLatRect.Top;
    end;
  end;
end;

procedure TProjectionTypeBase.ValidateLonLatRect(var ARect: TDoubleRect);
begin
  if ARect.Left < FValidLonLatRect.Left then begin
    ARect.Left := FValidLonLatRect.Left;
  end else begin
    if ARect.Left > FValidLonLatRect.Right then begin
      ARect.Left := FValidLonLatRect.Right;
    end;
  end;
  if ARect.Bottom < FValidLonLatRect.Bottom then begin
    ARect.Bottom := FValidLonLatRect.Bottom;
  end else begin
    if ARect.Bottom > FValidLonLatRect.Top then begin
      ARect.Bottom := FValidLonLatRect.Top;
    end;
  end;

  if ARect.Right < FValidLonLatRect.Left then begin
    ARect.Right := FValidLonLatRect.Left;
  end else begin
    if ARect.Right > FValidLonLatRect.Right then begin
      ARect.Right := FValidLonLatRect.Right;
    end;
  end;
  if ARect.Top < FValidLonLatRect.Bottom then begin
    ARect.Top := FValidLonLatRect.Bottom;
  end else begin
    if ARect.Top > FValidLonLatRect.Top then begin
      ARect.Top := FValidLonLatRect.Top;
    end;
  end;
end;

procedure TProjectionTypeBase.ValidateLonLatRectInternal(var ARect: TDoubleRect);
begin
  if ARect.Left < FValidLonLatRect.Left then begin
    Assert(False, 'Longitude must be not less then ' + FloatToStr(FValidLonLatRect.Left));
    ARect.Left := FValidLonLatRect.Left;
  end else begin
    if ARect.Left > FValidLonLatRect.Right then begin
      Assert(False, 'Longitude must be less then ' + FloatToStr(FValidLonLatRect.Right));
      ARect.Left := FValidLonLatRect.Right;
    end;
  end;
  if ARect.Bottom < FValidLonLatRect.Bottom then begin
    Assert(False, 'Latitude must be not less then ' + FloatToStr(FValidLonLatRect.Bottom));
    ARect.Bottom := FValidLonLatRect.Bottom;
  end else begin
    if ARect.Bottom > FValidLonLatRect.Top then begin
      Assert(False, 'Latitude must be less then ' + FloatToStr(FValidLonLatRect.Top));
      ARect.Bottom := FValidLonLatRect.Top;
    end;
  end;

  if ARect.Right < FValidLonLatRect.Left then begin
    Assert(False, 'Longitude must be not less then ' + FloatToStr(FValidLonLatRect.Left));
    ARect.Right := FValidLonLatRect.Left;
  end else begin
    if ARect.Right > FValidLonLatRect.Right then begin
      Assert(False, 'Longitude must be less then ' + FloatToStr(FValidLonLatRect.Right));
      ARect.Right := FValidLonLatRect.Right;
    end;
  end;
  if ARect.Top < FValidLonLatRect.Bottom then begin
    Assert(False, 'Latitude must be not less then ' + FloatToStr(FValidLonLatRect.Bottom));
    ARect.Top := FValidLonLatRect.Bottom;
  end else begin
    if ARect.Top > FValidLonLatRect.Top then begin
      Assert(False, 'Latitude must be less then ' + FloatToStr(FValidLonLatRect.Top));
      ARect.Top := FValidLonLatRect.Top;
    end;
  end;
end;

procedure TProjectionTypeBase.ValidateRelativePos(var APoint: TDoublePoint);
begin
  if APoint.X < 0 then begin
    APoint.X := 0;
  end else begin
    if APoint.X > 1 then begin
      APoint.X := 1;
    end;
  end;

  if APoint.Y < 0 then begin
    APoint.Y := 0;
  end else begin
    if APoint.Y > 1 then begin
      APoint.Y := 1;
    end;
  end;
end;

procedure TProjectionTypeBase.ValidateRelativePosInternal(var APoint: TDoublePoint);
begin
  if APoint.X < 0 then begin
    Assert(False, 'Relative coordinate X must be not less then 0');
    APoint.X := 0;
  end else begin
    if APoint.X > 1 then begin
      Assert(False, 'Relative coordinate X must be not greater then 1');
      APoint.X := 1;
    end;
  end;

  if APoint.Y < 0 then begin
    Assert(False, 'Relative coordinate Y must be not less then 0');
    APoint.Y := 0;
  end else begin
    if APoint.Y > 1 then begin
      Assert(False, 'Relative coordinate Y must be not greater then 1');
      APoint.Y := 1;
    end;
  end;
end;

procedure TProjectionTypeBase.ValidateRelativeRect(var ARect: TDoubleRect);
begin
  if ARect.Left < 0 then begin
    ARect.Left := 0;
  end else begin
    if ARect.Left > 1 then begin
      ARect.Left := 1;
    end;
  end;

  if ARect.Top < 0 then begin
    ARect.Top := 0;
  end else begin
    if ARect.Top > 1 then begin
      ARect.Top := 1;
    end;
  end;

  if ARect.Right < 0 then begin
    ARect.Right := 0;
  end else begin
    if ARect.Right > 1 then begin
      ARect.Right := 1;
    end;
  end;

  if ARect.Bottom < 0 then begin
    ARect.Bottom := 0;
  end else begin
    if ARect.Bottom > 1 then begin
      ARect.Bottom := 1;
    end;
  end;
end;

procedure TProjectionTypeBase.ValidateRelativeRectInternal(var ARect: TDoubleRect);
begin
  if ARect.Left < 0 then begin
    Assert(False, 'Relative coordinate X must be not less then 0');
    ARect.Left := 0;
  end else begin
    if ARect.Left > 1 then begin
      Assert(False, 'Relative coordinate X must be not greater then 1');
      ARect.Left := 1;
    end;
  end;

  if ARect.Top < 0 then begin
    Assert(False, 'Relative coordinate Y must be not less then 0');
    ARect.Top := 0;
  end else begin
    if ARect.Top > 1 then begin
      Assert(False, 'Relative coordinate Y must be not greater then 1');
      ARect.Top := 1;
    end;
  end;

  if ARect.Right < 0 then begin
    Assert(False, 'Relative coordinate X must be not less then 0');
    ARect.Right := 0;
  end else begin
    if ARect.Right > 1 then begin
      Assert(False, 'Relative coordinate X must be not greater then 1');
      ARect.Right := 1;
    end;
  end;

  if ARect.Bottom < 0 then begin
    Assert(False, 'Relative coordinate Y must be not less then 0');
    ARect.Bottom := 0;
  end else begin
    if ARect.Bottom > 1 then begin
      Assert(False, 'Relative coordinate Y must be not greater then 1');
      ARect.Bottom := 1;
    end;
  end;
end;

end.
