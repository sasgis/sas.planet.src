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

unit u_LineOnMapEdit;

interface

uses
  t_GeoTypes,
  i_VectorItemLonLat,
  i_VectorItmesFactory,
  u_ConfigDataElementBase,
  i_LineOnMapEdit;

type
  TLineOnMapEdit = class(TConfigDataElementBaseEmptySaveLoad, ILineOnMapEdit)
  private
    FPoints: TArrayOfDoublePoint;
    FCount: Integer;
    FActiveIndex: Integer;
  protected
    function GetCount: Integer;
    function GetActiveIndex: Integer;
    function GetPoints: TArrayOfDoublePoint;
    function GetPointIndexInLonLatRect(ARect: TDoubleRect): Integer;
    procedure Empty;
    procedure SetActiveIndex(AValue: Integer);
    procedure DeleteActivePoint;
    procedure InsertPoint(APoint: TDoublePoint);
    procedure MoveActivePoint(APoint: TDoublePoint);
    procedure SetPoints(AValue: TArrayOfDoublePoint);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TLineOnMapEditNew = class(TConfigDataElementBaseEmptySaveLoad, ILineOnMapEditNew)
  private
    FFactory: IVectorItmesFactory;
    FPoints: TArrayOfDoublePoint;
    FCount: Integer;
    FSelectedSegmentIndex: Integer;
    FSelectedPointIndex: Integer;
  private
    function GetSelectedPoint: TDoublePoint;
    function GetSelectedSegmentIndex: Integer;
    function GetSelectedPointIndex: Integer;

    procedure SetSelectedPoint(ASegmentIndex: Integer; APointIndex: Integer);
    procedure SetSelectedNextPoint;
    procedure SetSelectedPrevPoint;
    function SelectPointInLonLatRect(ARect: TDoubleRect): Boolean;

    procedure Clear;
    procedure DeleteActivePoint;
    procedure InsertPoint(APoint: TDoublePoint);
    procedure MoveActivePoint(APoint: TDoublePoint);
  public
    constructor Create(AFactory: IVectorItmesFactory);
    destructor Destroy; override;
  end;

  TPathOnMapEdit = class(TLineOnMapEditNew, IPathOnMapEdit)
  private
  private
    function GetPath: ILonLatPath;
    procedure SetPath(AValue: ILonLatPath);
  end;

implementation

uses
  u_GeoFun;

{ TLineOnMapEdit }

constructor TLineOnMapEdit.Create;
begin
  inherited;
  SetLength(FPoints, 64);
  FCount := 0;
  FActiveIndex := -1;
end;

destructor TLineOnMapEdit.Destroy;
begin
  FPoints := nil;
  inherited;
end;

procedure TLineOnMapEdit.DeleteActivePoint;
begin
  LockWrite;
  try
    if FCount > 0 then begin
      if FActiveIndex >= FCount - 1 then begin
        Dec(FCount);
        FActiveIndex := FCount - 1;
      end else begin
        if FActiveIndex < 0 then begin
          FActiveIndex := 0;
        end;
        Move(FPoints[FActiveIndex + 1], FPoints[FActiveIndex], (FCount - FActiveIndex - 1) * SizeOf(FPoints[0]));
        Dec(FCount);
        Dec(FActiveIndex);
        if FActiveIndex < 0 then begin
          FActiveIndex := 0;
        end;
      end;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TLineOnMapEdit.Empty;
begin
  LockWrite;
  try
    if FCount > 0 then begin
      FCount := 0;
      FActiveIndex := -1;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

function TLineOnMapEdit.GetActiveIndex: Integer;
begin
  LockRead;
  try
    Result := FActiveIndex;
  finally
    UnlockRead;
  end;
end;

function TLineOnMapEdit.GetCount: Integer;
begin
  LockRead;
  try
    Result := FCount;
  finally
    UnlockRead;
  end;
end;

function TLineOnMapEdit.GetPointIndexInLonLatRect(ARect: TDoubleRect): Integer;
var
  i: Integer;
begin
  Result := -1;
  LockRead;
  try
    for i := FCount - 1 downto 0 do begin
      if LonLatPointInRect(FPoints[i], ARect) then begin
        Result := i;
        Break;
      end;
    end;
  finally
    UnlockRead;
  end;
end;

function TLineOnMapEdit.GetPoints: TArrayOfDoublePoint;
begin
  LockRead;
  try
    Result := Copy(FPoints, 0, FCount);
  finally
    UnlockRead;
  end;
end;

procedure TLineOnMapEdit.InsertPoint(APoint: TDoublePoint);
begin
  LockWrite;
  try
    if Length(FPoints) <= FCount then begin
      SetLength(FPoints, FCount * 2);
    end;
    if FActiveIndex >= FCount - 1 then begin
      FPoints[FCount] := APoint;
      FActiveIndex := FCount;
      Inc(FCount);
    end else begin
      if FActiveIndex < 0 then begin
        FActiveIndex := 0;
      end;
      Move(FPoints[FActiveIndex + 1], FPoints[FActiveIndex + 2], (FCount - FActiveIndex - 1) * SizeOf(FPoints[0]));
      FPoints[FActiveIndex + 1] := APoint;
      Inc(FActiveIndex);
      Inc(FCount);
    end;
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TLineOnMapEdit.MoveActivePoint(APoint: TDoublePoint);
begin
  LockWrite;
  try
    if (FCount > 0) then begin
      if FActiveIndex < 0 then begin
        FActiveIndex := 0;
      end;
      if FActiveIndex >= FCount then begin
        FActiveIndex := FCount - 1;
      end;
      if not DoublePointsEqual(APoint, FPoints[FActiveIndex]) then begin
        FPoints[FActiveIndex] := APoint;
        SetChanged;
      end;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TLineOnMapEdit.SetActiveIndex(AValue: Integer);
begin
  LockWrite;
  try
    if (AValue >= 0) and (AValue < FCount) then begin
      if FActiveIndex <> AValue then begin
        FActiveIndex := AValue;
        SetChanged;
      end;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TLineOnMapEdit.SetPoints(AValue: TArrayOfDoublePoint);
var
  VNewCount: Integer;
begin
  VNewCount := Length(AValue);
  if VNewCount > 1 then begin
    if DoublePointsEqual(AValue[0], AValue[VNewCount - 1]) then begin
      Dec(VNewCount);
    end;
  end;
  LockWrite;
  try
    if VNewCount > 0 then begin
    if Length(FPoints) < VNewCount then begin
        SetLength(FPoints, VNewCount);
      end;
      FCount := VNewCount;
      FActiveIndex := FCount - 1;
      Move(AValue[0], FPoints[0], FCount * SizeOf(FPoints[0]));
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

{ TLineOnMapEditNew }

procedure TLineOnMapEditNew.Clear;
begin

end;

constructor TLineOnMapEditNew.Create(AFactory: IVectorItmesFactory);
begin

end;

procedure TLineOnMapEditNew.DeleteActivePoint;
begin

end;

destructor TLineOnMapEditNew.Destroy;
begin

  inherited;
end;

function TLineOnMapEditNew.GetSelectedPoint: TDoublePoint;
begin

end;

function TLineOnMapEditNew.GetSelectedPointIndex: Integer;
begin

end;

function TLineOnMapEditNew.GetSelectedSegmentIndex: Integer;
begin

end;

procedure TLineOnMapEditNew.InsertPoint(APoint: TDoublePoint);
begin

end;

procedure TLineOnMapEditNew.MoveActivePoint(APoint: TDoublePoint);
begin

end;

function TLineOnMapEditNew.SelectPointInLonLatRect(ARect: TDoubleRect): Boolean;
begin

end;

procedure TLineOnMapEditNew.SetSelectedNextPoint;
begin

end;

procedure TLineOnMapEditNew.SetSelectedPoint(ASegmentIndex,
  APointIndex: Integer);
begin

end;

procedure TLineOnMapEditNew.SetSelectedPrevPoint;
begin

end;

{ TPathOnMapEdit }

function TPathOnMapEdit.GetPath: ILonLatPath;
begin

end;

procedure TPathOnMapEdit.SetPath(AValue: ILonLatPath);
begin

end;

end.
