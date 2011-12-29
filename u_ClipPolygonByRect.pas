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

unit u_ClipPolygonByRect;

interface

uses
  Types,
  t_GeoTypes;

type
  IPolygonClip = interface
    ['{DD70326E-B6E0-4550-91B4-8AA974AD2DE5}']
    function Clip(var AFirstPoint: TDoublePoint; APointsCount: Integer; var AResultPoints: TArrayOfDoublePoint): Integer;
  end;

  TPolygonClipAbstract = class(TInterfacedObject, IPolygonClip)
  protected
    procedure AppendPointToResult(
      const APoint: TDoublePoint;
      var AResultPoints: TArrayOfDoublePoint;
      var AOutPointsCount, AOutPointsCapacity: Integer
    );
  protected
    function Clip(var AFirstPoint: TDoublePoint; APointsCount: Integer; var AResultPoints: TArrayOfDoublePoint): Integer; virtual; abstract;
  end;

  TPolygonClipByLineAbstract = class(TPolygonClipAbstract)
  protected
    function GetPointCode(APoint: TDoublePoint): Byte; virtual; abstract;
    function GetIntersectPoint(APrevPoint,ACurrPoint: TDoublePoint): TDoublePoint; virtual; abstract;
  public
    function Clip(var AFirstPoint: TDoublePoint; APointsCount: Integer; var AResultPoints: TArrayOfDoublePoint): Integer; override;
  end;

  TPolygonClipByVerticalLine = class(TPolygonClipByLineAbstract)
  protected
    FX: Double;
  protected
    function GetIntersectPoint(APrevPoint,ACurrPoint: TDoublePoint): TDoublePoint; override;
  public
    constructor Create(AX: Double);
  end;

  TPolygonClipByLeftBorder = class(TPolygonClipByVerticalLine)
  protected
    function GetPointCode(APoint: TDoublePoint): Byte; override;
  end;

  TPolygonClipByRightBorder = class(TPolygonClipByVerticalLine)
  protected
    function GetPointCode(APoint: TDoublePoint): Byte; override;
  end;

  TPolygonClipByHorizontalLine = class(TPolygonClipByLineAbstract)
  protected
    FY: Double;
  protected
    function GetIntersectPoint(APrevPoint,ACurrPoint: TDoublePoint): TDoublePoint; override;
  public
    constructor Create(AY: Double);
  end;

  TPolygonClipByTopBorder = class(TPolygonClipByHorizontalLine)
  protected
    function GetPointCode(APoint: TDoublePoint): Byte; override;
  end;

  TPolygonClipByBottomBorder = class(TPolygonClipByHorizontalLine)
  protected
    function GetPointCode(APoint: TDoublePoint): Byte; override;
  end;

  TPolygonClipEqualPoints = class(TPolygonClipAbstract)
  protected
    function Clip(var AFirstPoint: TDoublePoint; APointsCount: Integer; var AResultPoints: TArrayOfDoublePoint): Integer; override;
  end;

  TPolygonClipByRect = class(TInterfacedObject, IPolygonClip)
  private
    FClipEqual: IPolygonClip;
    FClipLeft: IPolygonClip;
    FClipTop: IPolygonClip;
    FClipRight: IPolygonClip;
    FClipBottom: IPolygonClip;
  public
    constructor Create(ARect: TRect);
    destructor Destroy; override;
    function Clip(var AFirstPoint: TDoublePoint; APointsCount: Integer; var AResultPoints: TArrayOfDoublePoint): Integer;
  end;

implementation

uses
  u_GeoFun;

{ TPolygonClipAbstract }

procedure TPolygonClipAbstract.AppendPointToResult(
  const APoint: TDoublePoint;
  var AResultPoints: TArrayOfDoublePoint;
  var AOutPointsCount, AOutPointsCapacity: Integer
);
begin
  if AOutPointsCount >= AOutPointsCapacity then begin
    if AOutPointsCapacity  >= 32 then begin
      AOutPointsCapacity := AOutPointsCapacity * 2;
    end else begin
      AOutPointsCapacity := 32;
    end;
    SetLength(AResultPoints, AOutPointsCapacity);
  end;
  AResultPoints[AOutPointsCount] := APoint;
  Inc(AOutPointsCount);
end;

{ TPolygonClipByLineAbstract }

function TPolygonClipByLineAbstract.Clip(var AFirstPoint: TDoublePoint;
  APointsCount: Integer; var AResultPoints: TArrayOfDoublePoint): Integer;
var
  VPrevPoint: TDoublePoint;
  VPrevPointCode: Byte;
  VCurrPoint: TDoublePoint;
  VCurrPointCode: Byte;
  VLineCode: Byte;
  i: Integer;
  VIntersectPoint: TDoublePoint;
  VOutPointsCapacity: Integer;
begin
  Result := 0;
  if APointsCount > 0 then begin
    VOutPointsCapacity := Length(AResultPoints);
    VCurrPoint := PDoublePointArray(@AFirstPoint)[0];
    VCurrPointCode := GetPointCode(VCurrPoint);
    if APointsCount > 1 then begin
      case VCurrPointCode of
        1, 2: begin
          AppendPointToResult(VCurrPoint, AResultPoints, Result, VOutPointsCapacity);
        end;
      end;
      for i := 1 to APointsCount - 1 do begin
        VPrevPoint := VCurrPoint;
        VPrevPointCode := VCurrPointCode;
        VCurrPoint := PDoublePointArray(@AFirstPoint)[i];
        VCurrPointCode := GetPointCode(VCurrPoint);
        VLineCode := VPrevPointCode * 16 + VCurrPointCode;
        {
        Код      Стар Нов Выход
        $00:     вне-вне нет
        $01:     вне-на  конечная
        $02:     вне-вну перес,кон
        $03:
        $10:     на -вне нет
        $11:     на -на  конечная
        $12:     на -вну конечная
        $13:
        $20:     вну-вне пересечен
        $21:     вну-на  конечная
        $22:     вну-вну конечная
        $23:
        $30:
        $31:
        $32:
        $33:
        }
        case VLineCode of
          $01, $12, $21, $22, $03, $13, $23, $31, $32: begin
            AppendPointToResult(VCurrPoint, AResultPoints, Result, VOutPointsCapacity);
          end;
          $02: begin
            VIntersectPoint := GetIntersectPoint(VPrevPoint, VCurrPoint);
            AppendPointToResult(VIntersectPoint, AResultPoints, Result, VOutPointsCapacity);
            AppendPointToResult(VCurrPoint, AResultPoints, Result, VOutPointsCapacity);
          end;
          $20: begin
            VIntersectPoint := GetIntersectPoint(VPrevPoint, VCurrPoint);
            AppendPointToResult(VIntersectPoint, AResultPoints, Result, VOutPointsCapacity);
          end;
        end;
      end;
      if Result > 0 then begin
        if not PointIsEmpty(PDoublePointArray(@AFirstPoint)[0]) then // catch IFPO for some tracks
        if not PointIsEmpty(PDoublePointArray(@AFirstPoint)[APointsCount - 1]) then
        if DoublePointsEqual(PDoublePointArray(@AFirstPoint)[0], PDoublePointArray(@AFirstPoint)[APointsCount - 1]) then begin
          if not DoublePointsEqual(AResultPoints[0], AResultPoints[Result - 1]) then begin
            AppendPointToResult(AResultPoints[0], AResultPoints, Result, VOutPointsCapacity);
          end;
        end;
      end;
    end else begin
      if VCurrPointCode = 2 then begin
        if Result >= VOutPointsCapacity then begin
          VOutPointsCapacity := 1;
          SetLength(AResultPoints, VOutPointsCapacity);
        end;
        AResultPoints[Result] := VCurrPoint;
        Inc(Result);
      end;
    end;
  end;
end;

{ TPolygonClipByVerticalLine }

constructor TPolygonClipByVerticalLine.Create(AX: Double);
begin
  FX := AX;
end;

function TPolygonClipByVerticalLine.GetIntersectPoint(APrevPoint,
  ACurrPoint: TDoublePoint): TDoublePoint;
begin
  Result.X := FX;
  Result.Y := (ACurrPoint.Y - APrevPoint.Y) / (ACurrPoint.X - APrevPoint.X) * (FX - APrevPoint.X) + APrevPoint.Y;
end;

{ TPolygonClipByLeftBorder }

function TPolygonClipByLeftBorder.GetPointCode(APoint: TDoublePoint): Byte;
begin
  if PointIsEmpty(APoint) then begin
    Result := 3;
  end else if APoint.X < FX then begin
    Result := 0;
  end else if APoint.X > FX then begin
    Result := 2;
  end else begin
    Result := 1;
  end;
end;

{ TPolyClipByRightBorder }

function TPolygonClipByRightBorder.GetPointCode(APoint: TDoublePoint): Byte;
begin
  if PointIsEmpty(APoint) then begin
    Result := 3;
  end else if APoint.X > FX then begin
    Result := 0;
  end else if APoint.X < FX then begin
    Result := 2;
  end else begin
    Result := 1;
  end;
end;

{ TPolygonClipByHorizontalLine }

constructor TPolygonClipByHorizontalLine.Create(AY: Double);
begin
  FY := AY;
end;

function TPolygonClipByHorizontalLine.GetIntersectPoint(APrevPoint,
  ACurrPoint: TDoublePoint): TDoublePoint;
begin
  Result.X := (ACurrPoint.X - APrevPoint.X) / (ACurrPoint.Y - APrevPoint.Y) * (FY - APrevPoint.Y) + APrevPoint.X;
  Result.Y := FY;
end;

{ TPolygonClipByTopBorder }

function TPolygonClipByTopBorder.GetPointCode(APoint: TDoublePoint): Byte;
begin
  if PointIsEmpty(APoint) then begin
    Result := 3;
  end else   if APoint.Y < FY then begin
    Result := 0;
  end else if APoint.Y > FY then begin
    Result := 2;
  end else begin
    Result := 1;
  end;
end;

{ TPolygonClipByBottomBorder }

function TPolygonClipByBottomBorder.GetPointCode(APoint: TDoublePoint): Byte;
begin
  if PointIsEmpty(APoint) then begin
    Result := 3;
  end else   if APoint.Y > FY then begin
    Result := 0;
  end else if APoint.Y < FY then begin
    Result := 2;
  end else begin
    Result := 1;
  end;
end;

{ TPolygonClipByRect }

function TPolygonClipByRect.Clip(var AFirstPoint: TDoublePoint;
  APointsCount: Integer; var AResultPoints: TArrayOfDoublePoint): Integer;
var
  VTempArray: TArrayOfDoublePoint;
begin
  Result := 0;
  if APointsCount > 0 then begin
    SetLength(VTempArray, Length(AResultPoints));

    Result := FClipEqual.Clip(AFirstPoint, APointsCount, VTempArray);
    if Result > 0 then begin
      Result := FClipLeft.Clip(VTempArray[0], Result, AResultPoints);
      if Result > 0 then begin
        Result := FClipTop.Clip(AResultPoints[0], Result, VTempArray);
        if Result > 0 then begin
          Result := FClipRight.Clip(VTempArray[0], Result, AResultPoints);
          if Result > 0 then begin
            Result := FClipBottom.Clip(AResultPoints[0], Result, VTempArray);
            if Result > 0 then begin
              Result := FClipEqual.Clip(VTempArray[0], Result, AResultPoints);
            end;
          end;
        end;
      end;
    end;
  end;
end;

constructor TPolygonClipByRect.Create(ARect: TRect);
begin
  FClipEqual := TPolygonClipEqualPoints.Create;
  FClipLeft := TPolygonClipByLeftBorder.Create(ARect.Left);
  FClipTop := TPolygonClipByTopBorder.Create(ARect.Top);
  FClipRight := TPolygonClipByRightBorder.Create(ARect.Right);
  FClipBottom := TPolygonClipByBottomBorder.Create(ARect.Bottom);
end;

destructor TPolygonClipByRect.Destroy;
begin
  FClipEqual := nil;
  FClipLeft := nil;
  FClipTop := nil;
  FClipRight := nil;
  FClipBottom := nil;
  inherited;
end;

{ TPolygonClipEqualPoints }

function TPolygonClipEqualPoints.Clip(var AFirstPoint: TDoublePoint;
  APointsCount: Integer; var AResultPoints: TArrayOfDoublePoint): Integer;
var
  VOutPointsCapacity: Integer;
  VPrevPoint: TDoublePoint;
  VPrevEmpty: Boolean;
  VCurrPoint: TDoublePoint;
  VCurrEmpty: Boolean;
  i: Integer;
  VNeedAddPoint: Boolean;
begin
  Result := 0;
  if APointsCount > 0 then begin
    VOutPointsCapacity := Length(AResultPoints);
    VCurrPoint := PDoublePointArray(@AFirstPoint)[0];
    VCurrEmpty := PointIsEmpty(VCurrPoint);
    if APointsCount > 1 then begin
      if not VCurrEmpty then begin
        AppendPointToResult(VCurrPoint, AResultPoints, Result, VOutPointsCapacity);
      end;
      for i := 1 to APointsCount - 1 do begin
        VPrevPoint := VCurrPoint;
        VPrevEmpty := VCurrEmpty;
        VCurrPoint := PDoublePointArray(@AFirstPoint)[i];
        VCurrEmpty := PointIsEmpty(VCurrPoint);
        VNeedAddPoint := True;
        if VPrevEmpty then begin
          if VCurrEmpty then begin
            VNeedAddPoint := False;
          end;
        end else begin
          if not VCurrEmpty then begin
            if (abs(VCurrPoint.X - VPrevPoint.X) < 1) and (abs(VCurrPoint.Y - VPrevPoint.Y) < 1) then begin
              VNeedAddPoint := False;
            end;
          end;
        end;
        if VNeedAddPoint then begin
          AppendPointToResult(VCurrPoint, AResultPoints, Result, VOutPointsCapacity);
        end else begin
          VCurrPoint := VPrevPoint;
          VCurrEmpty := VPrevEmpty;
        end;
      end;
      if Result > 0 then begin
        if not PointIsEmpty(PDoublePointArray(@AFirstPoint)[0]) then
        if not PointIsEmpty(PDoublePointArray(@AFirstPoint)[APointsCount - 1]) then
        if DoublePointsEqual(PDoublePointArray(@AFirstPoint)[0], PDoublePointArray(@AFirstPoint)[APointsCount - 1]) then begin
          VCurrPoint := AResultPoints[0];
          if not DoublePointsEqual(VCurrPoint, AResultPoints[Result - 1]) then begin
            AppendPointToResult(VCurrPoint, AResultPoints, Result, VOutPointsCapacity);
          end;
        end;
      end;
    end else begin
      if not VCurrEmpty then begin
        if Result >= VOutPointsCapacity then begin
          VOutPointsCapacity := 1;
          SetLength(AResultPoints, VOutPointsCapacity);
        end;
        AResultPoints[Result] := VCurrPoint;
        Inc(Result);
      end;
    end;

  end;
end;

end.

