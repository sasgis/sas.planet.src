{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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

unit u_SelectionRect;

interface

uses
  Classes,
  t_GeoTypes,
  i_SelectionRect,
  i_LocalCoordConverterChangeable,
  i_MapLayerGridsConfig,
  u_ConfigDataElementBase;

type
  TSelectionRect = class(TConfigDataElementBaseEmptySaveLoad, ISelectionRect)
  private
    FViewPortState: ILocalCoordConverterChangeable;
    FTileGridConfig: ITileGridConfig;
    FGenShtabGridConfig: IGenShtabGridConfig;
    FDegreeGridConfig: IDegreeGridConfig;

    FIsEmpty: Boolean;
    FFirstPoint: TDoublePoint;
    FSecondPoint: TDoublePoint;
    FShift: TShiftState;
    FResultRect: TDoubleRect;
    function PrepareSelectionRect(
      const APoint1, APoint2: TDoublePoint;
      Shift: TShiftState
    ): TDoubleRect;
  private
    function IsEmpty: Boolean;
    procedure SetNextPoint(
      const ALonLat: TDoublePoint;
      Shift: TShiftState
    );
    procedure Reset;
    function GetRect: TDoubleRect;
  public
    constructor Create(
      const AViewPortState: ILocalCoordConverterChangeable;
      const ATileGridConfig: ITileGridConfig;
      const AGenShtabGridConfig: IGenShtabGridConfig;
      const ADegreeGridConfig: IDegreeGridConfig
    );
  end;

implementation

uses
  i_LocalCoordConverter,
  i_CoordConverter,
  u_GeoFun;

{ TSelectionRect }

constructor TSelectionRect.Create(
  const AViewPortState: ILocalCoordConverterChangeable;
  const ATileGridConfig: ITileGridConfig;
  const AGenShtabGridConfig: IGenShtabGridConfig;
  const ADegreeGridConfig: IDegreeGridConfig
);
begin
  inherited Create;
  FTileGridConfig := ATileGridConfig;
  FGenShtabGridConfig := AGenShtabGridConfig;
  FDegreeGridConfig := ADegreeGridConfig;
  FViewPortState := AViewPortState;
  FIsEmpty := True;
end;

function TSelectionRect.GetRect: TDoubleRect;
begin
  LockRead;
  try
    Result := FResultRect;
  finally
    UnlockRead;
  end;
end;

function TSelectionRect.IsEmpty: Boolean;
begin
  LockRead;
  try
    Result := FIsEmpty;
  finally
    UnlockRead;
  end;
end;

function TSelectionRect.PrepareSelectionRect(
  const APoint1, APoint2: TDoublePoint;
  Shift: TShiftState
): TDoubleRect;
var
  VConverter: ICoordConverter;
  VTemp: Double;
  VLocalConverter: ILocalCoordConverter;
begin
  VLocalConverter := FViewPortState.GetStatic;
  VConverter := VLocalConverter.GetGeoConverter;

  Result.TopLeft := APoint1;
  Result.BottomRight := APoint2;

  VConverter.CheckLonLatRect(Result);
  if Result.Left > Result.Right then begin
    VTemp := Result.Left;
    Result.Left := Result.Right;
    Result.Right := VTemp;
  end;
  if Result.Top < Result.Bottom then begin
    VTemp := Result.Top;
    Result.Top := Result.Bottom;
    Result.Bottom := VTemp;
  end;
  if (ssCtrl in Shift) then begin
    Result := FTileGridConfig.GetRectStickToGrid(VLocalConverter, Result);
  end;
  if (ssShift in Shift) then begin
    if FGenShtabGridConfig.Scale <> 0 then begin
      Result := FGenShtabGridConfig.GetRectStickToGrid(VLocalConverter, Result);
    end else begin
      Result := FDegreeGridConfig.GetRectStickToGrid(VLocalConverter, Result);
    end;
  end;
  if (ssAlt in Shift) then begin
    Result := FDegreeGridConfig.GetRectStickToGrid(VLocalConverter, Result);
  end;
end;

procedure TSelectionRect.Reset;
begin
  LockWrite;
  try
    if not FIsEmpty then begin
      FFirstPoint.X := 0;
      FFirstPoint.Y := 0;
      FSecondPoint.X := 0;
      FSecondPoint.Y := 0;
      FResultRect.TopLeft := FFirstPoint;
      FResultRect.BottomRight := FSecondPoint;
      FIsEmpty := True;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TSelectionRect.SetNextPoint(
  const ALonLat: TDoublePoint;
  Shift: TShiftState
);
var
  VNewRect: TDoubleRect;
begin
  LockWrite;
  try
    if FIsEmpty then begin
      FIsEmpty := False;
      FFirstPoint := ALonLat;
      FSecondPoint := ALonLat;
      FShift := Shift;
      FResultRect := PrepareSelectionRect(FFirstPoint, FSecondPoint, FShift);
      SetChanged;
    end else begin
      FSecondPoint := ALonLat;
      FShift := Shift;
      VNewRect := PrepareSelectionRect(FFirstPoint, FSecondPoint, FShift);
      if not DoubleRectsEqual(FResultRect, VNewRect) then begin
        FResultRect := VNewRect;
        SetChanged;
      end;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
