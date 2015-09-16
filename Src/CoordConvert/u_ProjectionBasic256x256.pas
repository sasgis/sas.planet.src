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

unit u_ProjectionBasic256x256;

interface

uses
  Types,
  t_Hash,
  t_GeoTypes,
  i_ProjectionType,
  i_ProjectionInfo,
  u_BaseInterfacedObject,
  u_GeoFunc;

type
  TProjectionBasic256x256 = class(TBaseInterfacedObject, IProjection)
  private
    FHash: THashValue;
    FZoom: Byte;
    FProjectionType: IProjectionType;

    FTiles: Integer;
    FPixels: Integer;
    FTilesFloat: Double;
    FPixelsFloat: Double;
    FTileSplitCode: Integer;
    procedure InternalValidatePixelPos(
      var APoint: TPoint
    ); inline;
    procedure InternalValidatePixelPosFloat(
      var APoint: TDoublePoint
    ); inline;
    procedure InternalValidatePixelRect(
      var ARect: TRect
    ); inline;
    procedure InternalValidatePixelRectFloat(
      var ARect: TDoubleRect
    ); inline;

    procedure InternalValidateTilePos(
      var APoint: TPoint
    ); inline;
    procedure InternalValidateTilePosStrict(
      var APoint: TPoint
    ); inline;
    procedure InternalValidateTilePosFloat(
      var APoint: TDoublePoint
    ); inline;
    procedure InternalValidateTileRect(
      var ARect: TRect
    ); inline;
    procedure InternalValidateTileRectFloat(
      var ARect: TDoubleRect
    ); inline;

    procedure InternalValidateRelativePos(var APoint: TDoublePoint); inline;
    procedure InternalValidateRelativeRect(var ARect: TDoubleRect); inline;

    function InternalRelative2PixelPosFloat(
      const APoint: TDoublePoint
    ): TDoublePoint; inline;
    function InternalRelativeRect2PixelRectFloat(
      const ARect: TDoubleRect
    ): TDoubleRect; inline;
    function InternalRelative2TilePosFloat(
      const APoint: TDoublePoint
    ): TDoublePoint; inline;
    function InternalRelativeRect2TileRectFloat(
      const ARect: TDoubleRect
    ): TDoubleRect; inline;

    function InternalPixelPos2Relative(
      const APoint: TPoint
    ): TDoublePoint; inline;
    function InternalPixelPosFloat2Relative(
      const APoint: TDoublePoint
    ): TDoublePoint; inline;
    function InternalPixelRect2RelativeRect(
      const ARect: TRect
    ): TDoubleRect; inline;
    function InternalPixelRectFloat2RelativeRect(
      const ARect: TDoubleRect
    ): TDoubleRect; inline;
    function InternalTilePos2Relative(
      const APoint: TPoint
    ): TDoublePoint; inline;
    function InternalTilePos2PixelRect(
      const APoint: TPoint
    ): TRect; inline;
    function InternalTilePosFloat2Relative(
      const APoint: TDoublePoint
    ): TDoublePoint; inline;
    function InternalTileRect2RelativeRect(
      const ARect: TRect
    ): TDoubleRect; inline;
    function InternalTileRectFloat2RelativeRect(
      const ARect: TDoubleRect
    ): TDoubleRect; inline;
  private
    function GetHash: THashValue;
    function GetZoom: Byte;
    function GetProjectionType: IProjectionType;
    function IsSame(const AProjection: IProjection): Boolean;

    // Возвращает прямоугольник тайлов допустимый в заданном зуме
    function GetTileRect: TRect;
    // Возвращает прямоугольник пикселов допустимый в заданном зуме
    function GetPixelRect: TRect;

    // Возвращает общее количество пикселей на заданном зуме
    function GetPixelsFloat: Double;

    // Возвращает код типа нарезки на тайлы (на будущее, вдруг реализую произвольный размер тайлов)
    function GetTileSplitCode: Integer;

    // Преобразует координаты пиксела в относительные координаты на карте (x/PixelsAtZoom)
    function PixelPos2Relative(
      const APoint: TPoint
    ): TDoublePoint;
    // Преобразует координаты пиксела в географические координаты
    function PixelPos2LonLat(
      const APoint: TPoint
    ): TDoublePoint;
    function PixelPos2TilePosFloat(
      const APoint: TPoint
    ): TDoublePoint;

    function PixelPosFloat2TilePosFloat(
      const APoint: TDoublePoint
    ): TDoublePoint;
    function PixelPosFloat2Relative(
      const APoint: TDoublePoint
    ): TDoublePoint;
    function PixelPosFloat2LonLat(
      const APoint: TDoublePoint
    ): TDoublePoint;

    // вычисляет прямоугольник тайлов покрывающий прямоугольник пикселов
    function PixelRect2TileRect(
      const ARect: TRect
    ): TRect;
    // Преобразует координаты прямоугольника пикселов в относительные координаты на карте (x/PixelsAtZoom)
    function PixelRect2RelativeRect(
      const ARect: TRect
    ): TDoubleRect;
    // Преобразует координаты прямоугольника пикселов в географические координаты на карте
    function PixelRect2LonLatRect(
      const ARect: TRect
    ): TDoubleRect;
    function PixelRect2TileRectFloat(
      const ARect: TRect
    ): TDoubleRect;

    function PixelRectFloat2TileRectFloat(
      const ARect: TDoubleRect
    ): TDoubleRect;
    function PixelRectFloat2RelativeRect(
      const ARect: TDoubleRect
    ): TDoubleRect;
    function PixelRectFloat2LonLatRect(
      const ARect: TDoubleRect
    ): TDoubleRect;

    // Преобразует позицию тайла заданного зума в координаты пиксела его левого верхнего угла
    function TilePos2PixelPos(
      const APoint: TPoint
    ): TPoint;
    // Преобразует позицию тайла заданного зума в номера пикселов его углов на заданном зуме
    function TilePos2PixelRect(
      const APoint: TPoint
    ): TRect;
    function TilePos2PixelRectFloat(
      const APoint: TPoint
    ): TDoubleRect;
    // Преобразует координаты тайла в относительные координаты на карте (x/PixelsAtZoom)
    function TilePos2Relative(
      const APoint: TPoint
    ): TDoublePoint;
    // Преобразует позицию тайла заданного зума в номера пикселов его углов на заданном зуме
    function TilePos2RelativeRect(
      const APoint: TPoint
    ): TDoubleRect;
    // Преобразует координаты тайла в географические координаты
    function TilePos2LonLat(
      const APoint: TPoint
    ): TDoublePoint;
    // Преобразует позицию тайла заданного зума в географические координаты его углов
    function TilePos2LonLatRect(
      const APoint: TPoint
    ): TDoubleRect;

    function TilePosFloat2PixelPosFloat(
      const APoint: TDoublePoint
    ): TDoublePoint;
    function TilePosFloat2Relative(
      const APoint: TDoublePoint
    ): TDoublePoint;
    function TilePosFloat2LonLat(
      const APoint: TDoublePoint
    ): TDoublePoint;

    // вычисляет координты пикселей вершин прямоугольника тайлов
    function TileRect2PixelRect(
      const ARect: TRect
    ): TRect;
    // вычисляет относительные координты вершин прямоугольника тайлов
    function TileRect2RelativeRect(
      const ARect: TRect
    ): TDoubleRect;
    // Преобразует прямоугольник тайлов заданного зума в географические координаты его углов
    function TileRect2LonLatRect(
      const ARect: TRect
    ): TDoubleRect;

    function TileRectFloat2PixelRectFloat(
      const ARect: TDoubleRect
    ): TDoubleRect;
    function TileRectFloat2RelativeRect(
      const ARect: TDoubleRect
    ): TDoubleRect;
    function TileRectFloat2LonLatRect(
      const ARect: TDoubleRect
    ): TDoubleRect;

    // Перобразует относительные координаты на карте в координаты пиксела
    function Relative2PixelPosFloat(
      const APoint: TDoublePoint
    ): TDoublePoint;
    // Перобразует относительные координаты на карте в координаты тайла
    function Relative2TilePosFloat(
      const APoint: TDoublePoint
    ): TDoublePoint;

    // Преобразует прямоугольник с относительными координатами в прямоугольник пикселов
    function RelativeRect2PixelRectFloat(
      const ARect: TDoubleRect
    ): TDoubleRect;
    // Преобразует прямоугольник с относительными координатами в прямоугольник тайлов
    function RelativeRect2TileRectFloat(
      const ARect: TDoubleRect
    ): TDoubleRect;

    // Преобразует георафические координаты в координаты пиксела на заданном зуме накрывающего данные координаты
    function LonLat2PixelPosFloat(
      const APoint: TDoublePoint
    ): TDoublePoint;
    // Преобразует георафические координаты в позицию тайла на заданном зуме накрывающего данные координаты
    function LonLat2TilePosFloat(
      const APoint: TDoublePoint
    ): TDoublePoint;

    function LonLatRect2PixelRectFloat(
      const ARect: TDoubleRect
    ): TDoubleRect;
    function LonLatRect2TileRectFloat(
      const ARect: TDoubleRect
    ): TDoubleRect;

    function GetTileSize(
      const APoint: TPoint
    ): TPoint;

    procedure ValidateTilePos(
      var APoint: TPoint;
      ACicleMap: Boolean
    );
    procedure ValidateTilePosStrict(
      var APoint: TPoint;
      ACicleMap: Boolean
    );
    procedure ValidateTileRect(
      var ARect: TRect
    );

    procedure ValidatePixelPos(
      var APoint: TPoint;
      ACicleMap: Boolean
    );
    procedure ValidatePixelPosFloat(
      var APoint: TDoublePoint;
      ACicleMap: Boolean
    );
    procedure ValidatePixelPosStrict(
      var APoint: TPoint;
      ACicleMap: Boolean
    );
    procedure ValidatePixelPosFloatStrict(
      var APoint: TDoublePoint;
      ACicleMap: Boolean
    );
    procedure ValidatePixelRect(
      var ARect: TRect
    );
    procedure ValidatePixelRectFloat(
      var ARect: TDoubleRect
    );

    function CheckTilePos(
      const APoint: TPoint
    ): boolean;
    function CheckTilePosStrict(
      const APoint: TPoint
    ): boolean;
    function CheckTileRect(
      const ARect: TRect
    ): boolean;

    function CheckPixelPos(
      const APoint: TPoint
    ): boolean;
    function CheckPixelPosFloat(
      const APoint: TDoublePoint
    ): boolean;
    function CheckPixelPosStrict(
      const APoint: TPoint
    ): boolean;
    function CheckPixelPosFloatStrict(
      const APoint: TDoublePoint
    ): boolean;
    function CheckPixelRect(
      const ARect: TRect
    ): boolean;
    function CheckPixelRectFloat(
      const ARect: TDoubleRect
    ): boolean;
  public
    constructor Create(
      const AHash: THashValue;
      const AProjectionType: IProjectionType;
      AZoom: Byte
    );
  end;

implementation

uses
  SysUtils,
  c_CoordConverter;

{ TProjectionInfo }

constructor TProjectionBasic256x256.Create(
  const AHash: THashValue;
  const AProjectionType: IProjectionType;
  AZoom: Byte
);
begin
  Assert(Assigned(AProjectionType));
  Assert(AZoom < 24);
  inherited Create;
  FHash := AHash;
  FProjectionType := AProjectionType;
  FZoom := AZoom;

  FTiles := 1 shl FZoom;
  if FZoom < 23 then begin
    FPixels := (1 shl FZoom) * 256;
  end else begin
    FPixels := MaxInt;
  end;
  FTilesFloat := FTiles;
  FPixelsFloat := FTilesFloat * 256.0;
  FTileSplitCode := CTileSplitQuadrate256x256;
end;

procedure TProjectionBasic256x256.InternalValidatePixelPosFloat(
  var APoint: TDoublePoint
);
begin
  if APoint.X < 0 then begin
    Assert(False, 'Координата X пиксела не может быть меньше нуля');
    APoint.X := 0;
  end else begin
    if APoint.X > FPixelsFloat then begin
      Assert(False, 'Координата X пиксела на этом зуме не может быть больше ' + FloatToStr(FPixelsFloat));
      APoint.X := FPixelsFloat;
    end;
  end;

  if APoint.Y < 0 then begin
    Assert(False, 'Координата Y пиксела не может быть меньше нуля');
    APoint.Y := 0;
  end else begin
    if APoint.Y > FPixelsFloat then begin
      Assert(False, 'Координата Y пиксела на этом зуме не может быть больше ' + FloatToStr(FPixelsFloat));
      APoint.Y := FPixelsFloat;
    end;
  end;
end;

procedure TProjectionBasic256x256.InternalValidatePixelPos(var APoint: TPoint);
begin
  if APoint.X < 0 then begin
    Assert(False, 'Координата X пиксела не может быть меньше нуля');
    APoint.X := 0;
  end else begin
    if APoint.X > FPixels then begin
      Assert(False, 'Координата X пиксела на этом зуме не может быть больше ' + IntToStr(FPixels));
      APoint.X := FPixels;
    end;
  end;

  if APoint.Y < 0 then begin
    Assert(False, 'Координата Y пиксела не может быть меньше нуля');
    APoint.Y := 0;
  end else begin
    if APoint.Y > FPixels then begin
      Assert(False, 'Координата Y пиксела на этом зуме не может быть больше ' + IntToStr(FPixels));
      APoint.Y := FPixels;
    end;
  end;
end;

procedure TProjectionBasic256x256.InternalValidatePixelRectFloat(
  var ARect: TDoubleRect
);
begin
  if ARect.Left < 0 then begin
    Assert(False, 'Координата X пиксела не может быть меньше нуля');
    ARect.Left := 0;
  end else begin
    if (ARect.Left > FPixelsFloat) then begin
      Assert(False, 'Координата X пиксела на этом зуме не может быть больше ' + FloatToStr(FPixelsFloat));
      ARect.Left := FPixelsFloat;
    end;
  end;

  if ARect.Top < 0 then begin
    Assert(False, 'Координата Y пиксела не может быть меньше нуля');
    ARect.Top := 0;
  end else begin
    if (ARect.Top > FPixelsFloat) then begin
      Assert(False, 'Координата Y пиксела на этом зуме не может быть больше ' + FloatToStr(FPixelsFloat));
      ARect.Top := FPixelsFloat;
    end;
  end;

  if ARect.Right < 0 then begin
    Assert(False, 'Координата X пиксела не может быть меньше нуля');
    ARect.Right := 0;
  end else begin
    if (ARect.Right > FPixelsFloat) then begin
      Assert(False, 'Координата X пиксела на этом зуме не может быть больше ' + FloatToStr(FPixelsFloat));
      ARect.Right := FPixelsFloat;
    end;
  end;

  if ARect.Bottom < 0 then begin
    Assert(False, 'Координата Y пиксела не может быть меньше нуля');
    ARect.Bottom := 0;
  end else begin
    if (ARect.Bottom > FPixelsFloat) then begin
      Assert(False, 'Координата Y пиксела на этом зуме не может быть больше ' + FloatToStr(FPixelsFloat));
      ARect.Bottom := FPixelsFloat;
    end;
  end;
end;

procedure TProjectionBasic256x256.InternalValidatePixelRect(var ARect: TRect);
begin
  if ARect.Left < 0 then begin
    Assert(False, 'Координата X пиксела не может быть меньше нуля');
    ARect.Left := 0;
  end else begin
    if ARect.Left > FPixels then begin
      Assert(False, 'Координата X пиксела на этом зуме не может быть больше ' + IntToStr(FPixels));
      ARect.Left := FPixels;
    end;
  end;

  if ARect.Top < 0 then begin
    Assert(False, 'Координата Y пиксела не может быть меньше нуля');
    ARect.Top := 0;
  end else begin
    if ARect.Top > FPixels then begin
      Assert(False, 'Координата Y пиксела на этом зуме не может быть больше ' + IntToStr(FPixels));
      ARect.Top := FPixels;
    end;
  end;

  if ARect.Right < 0 then begin
    Assert(False, 'Координата X пиксела не может быть меньше нуля');
    ARect.Right := 0;
  end else begin
    if ARect.Right > FPixels then begin
      Assert(False, 'Координата X пиксела на этом зуме не может быть больше ' + IntToStr(FPixels));
      ARect.Right := FPixels;
    end;
  end;

  if ARect.Bottom < 0 then begin
    Assert(False, 'Координата Y пиксела не может быть меньше нуля');
    ARect.Bottom := 0;
  end else begin
    if ARect.Bottom > FPixels then begin
      Assert(False, 'Координата Y пиксела на этом зуме не может быть больше ' + IntToStr(FPixels));
      ARect.Bottom := FPixels;
    end;
  end;
end;

procedure TProjectionBasic256x256.InternalValidateRelativePos(
  var APoint: TDoublePoint
);
begin
  if APoint.X < 0 then begin
    Assert(False, 'Относительная координата X не может быть меньше нуля');
    APoint.X := 0;
  end else begin
    if APoint.X > 1 then begin
      Assert(False, 'Относительная координата X не может быть больше единицы');
      APoint.X := 1;
    end;
  end;

  if APoint.Y < 0 then begin
    Assert(False, 'Относительная координата Y не может быть меньше нуля');
    APoint.Y := 0;
  end else begin
    if APoint.Y > 1 then begin
      Assert(False, 'Относительная координата Y не может быть больше единицы');
      APoint.Y := 1;
    end;
  end;
end;

procedure TProjectionBasic256x256.InternalValidateRelativeRect(
  var ARect: TDoubleRect
);
begin
  if ARect.Left < 0 then begin
    Assert(False, 'Относительная координата X не может быть меньше нуля');
    ARect.Left := 0;
  end else begin
    if ARect.Left > 1 then begin
      Assert(False, 'Относительная координата X не может быть больше единицы');
      ARect.Left := 1;
    end;
  end;

  if ARect.Top < 0 then begin
    Assert(False, 'Относительная координата Y не может быть меньше нуля');
    ARect.Top := 0;
  end else begin
    if ARect.Top > 1 then begin
      Assert(False, 'Относительная координата Y не может быть больше единицы');
      ARect.Top := 1;
    end;
  end;

  if ARect.Right < 0 then begin
    Assert(False, 'Относительная координата X не может быть меньше нуля');
    ARect.Right := 0;
  end else begin
    if ARect.Right > 1 then begin
      Assert(False, 'Относительная координата X не может быть больше единицы');
      ARect.Right := 1;
    end;
  end;

  if ARect.Bottom < 0 then begin
    Assert(False, 'Относительная координата Y не может быть меньше нуля');
    ARect.Bottom := 0;
  end else begin
    if ARect.Bottom > 1 then begin
      Assert(False, 'Относительная координата Y не может быть больше единицы');
      ARect.Bottom := 1;
    end;
  end;
end;

procedure TProjectionBasic256x256.InternalValidateTilePosFloat(
  var APoint: TDoublePoint
);
begin
  if APoint.X < 0 then begin
    Assert(False, 'Координата X тайла не может быть меньше нуля');
    APoint.X := 0;
  end else begin
    if APoint.X > FTilesFloat then begin
      Assert(False, 'Координата X тайла на этом зуме не может быть больше ' + FloatToStr(FTilesFloat));
      APoint.X := FTilesFloat;
    end;
  end;
  if APoint.Y < 0 then begin
    Assert(False, 'Координата Y тайла не может быть меньше нуля');
    APoint.Y := 0;
  end else begin
    if APoint.Y > FTilesFloat then begin
      Assert(False, 'Координата Y тайла на этом зуме не может быть больше ' + FloatToStr(FTilesFloat));
      APoint.Y := FTilesFloat;
    end;
  end;
end;

procedure TProjectionBasic256x256.InternalValidateTilePos(var APoint: TPoint);
begin
  if APoint.X < 0 then begin
    Assert(False, 'Координата X тайла не может быть меньше нуля');
    APoint.X := 0;
  end else begin
    if APoint.X > FTiles then begin
      Assert(False, 'Координата X тайла на этом зуме не может быть больше ' + IntToStr(FTiles));
      APoint.X := FTiles;
    end;
  end;

  if APoint.Y < 0 then begin
    Assert(False, 'Координата Y тайла не может быть меньше нуля');
    APoint.Y := 0;
  end else begin
    if APoint.Y > FTiles then begin
      Assert(False, 'Координата Y тайла на этом зуме не может быть больше ' + IntToStr(FTiles));
      APoint.Y := FTiles;
    end;
  end;
end;

procedure TProjectionBasic256x256.InternalValidateTilePosStrict(
  var APoint: TPoint
);
begin
  if APoint.X < 0 then begin
    Assert(False, 'Координата X тайла не может быть меньше нуля');
    APoint.X := 0;
  end else begin
    if APoint.X >= FTiles then begin
      Assert(False, 'Координата X тайла на этом зуме не может быть больше или равной ' + IntToStr(FTiles));
      APoint.X := FTiles - 1;
    end;
  end;
  if APoint.Y < 0 then begin
    Assert(False, 'Координата Y тайла не может быть меньше нуля');
    APoint.Y := 0;
  end else begin
    if APoint.Y >= FTiles then begin
      Assert(False, 'Координата Y тайла на этом зуме не может быть больше или равной ' + IntToStr(FTiles));
      APoint.Y := FTiles - 1;
    end;
  end;
end;

procedure TProjectionBasic256x256.InternalValidateTileRectFloat(
  var ARect: TDoubleRect
);
begin
  if ARect.Left < 0 then begin
    Assert(False, 'Координата X тайла не может быть меньше нуля');
    ARect.Left := 0;
  end else begin
    if ARect.Left > FTilesFloat then begin
      Assert(False, 'Координата X тайла на этом зуме не может быть больше или равна ' + FloatToStr(FTilesFloat));
      ARect.Left := FTilesFloat;
    end;
  end;
  if ARect.Top < 0 then begin
    Assert(False, 'Координата Y тайла не может быть меньше нуля');
    ARect.Top := 0;
  end else begin
    if ARect.Top > FTilesFloat then begin
      Assert(False, 'Координата Y тайла на этом зуме не может быть больше ' + FloatToStr(FTilesFloat));
      ARect.Top := FTilesFloat;
    end;
  end;
  if ARect.Right < 0 then begin
    Assert(False, 'Координата X тайла не может быть меньше нуля');
    ARect.Right := 0;
  end else begin
    if ARect.Right > FTilesFloat then begin
      Assert(False, 'Координата X тайла на этом зуме не может быть больше ' + FloatToStr(FTilesFloat));
      ARect.Right := FTilesFloat;
    end;
  end;
  if ARect.Bottom < 0 then begin
    Assert(False, 'Координата Y тайла не может быть меньше нуля');
    ARect.Bottom := 0;
  end else begin
    if ARect.Bottom > FTilesFloat then begin
      Assert(False, 'Координата Y тайла на этом зуме не может быть больше ' + FloatToStr(FTilesFloat));
      ARect.Bottom := FTilesFloat;
    end;
  end;
end;

procedure TProjectionBasic256x256.InternalValidateTileRect(var ARect: TRect);
begin
  if ARect.Left < 0 then begin
    Assert(False, 'Координата X тайла не может быть меньше нуля');
    ARect.Left := 0;
  end else begin
    if ARect.Left > FTiles then begin
      Assert(False, 'Координата X тайла на этом зуме не может быть больше ' + IntToStr(FTiles));
      ARect.Left := FTiles;
    end;
  end;
  if ARect.Top < 0 then begin
    Assert(False, 'Координата Y тайла не может быть меньше нуля');
    ARect.Top := 0;
  end else begin
    if ARect.Top > FTiles then begin
      Assert(False, 'Координата Y тайла на этом зуме не может быть больше ' + IntToStr(FTiles));
      ARect.Top := FTiles;
    end;
  end;
  if ARect.Right < 0 then begin
    Assert(False, 'Координата X тайла не может быть меньше нуля');
    ARect.Right := 0;
  end else begin
    if ARect.Right > FTiles then begin
      Assert(False, 'Координата X тайла на этом зуме не может быть больше ' + IntToStr(FTiles));
      ARect.Right := FTiles;
    end;
  end;
  if ARect.Bottom < 0 then begin
    Assert(False, 'Координата Y тайла не может быть меньше нуля');
    ARect.Bottom := 0;
  end else begin
    if ARect.Bottom > FTiles then begin
      Assert(False, 'Координата Y тайла на этом зуме не может быть больше ' + IntToStr(FTiles));
      ARect.Bottom := FTiles;
    end;
  end;
end;

function TProjectionBasic256x256.InternalPixelPos2Relative(
  const APoint: TPoint
): TDoublePoint;
begin
  Result := DoublePoint(APoint.X / FPixelsFloat, APoint.Y / FPixelsFloat);
end;

function TProjectionBasic256x256.InternalPixelPosFloat2Relative(
  const APoint: TDoublePoint
): TDoublePoint;
begin
  Result := DoublePoint(APoint.X / FPixelsFloat, APoint.Y / FPixelsFloat);
end;

function TProjectionBasic256x256.InternalPixelRect2RelativeRect(
  const ARect: TRect
): TDoubleRect;
begin
  Result :=
    DoubleRect(
      ARect.Left / FPixelsFloat,
      ARect.Top / FPixelsFloat,
      ARect.Right / FPixelsFloat,
      ARect.Bottom / FPixelsFloat
    );
end;

function TProjectionBasic256x256.InternalPixelRectFloat2RelativeRect(
  const ARect: TDoubleRect
): TDoubleRect;
begin
  Result :=
    DoubleRect(
      ARect.Left / FPixelsFloat,
      ARect.Top / FPixelsFloat,
      ARect.Right / FPixelsFloat,
      ARect.Bottom / FPixelsFloat
    );
end;

function TProjectionBasic256x256.InternalTilePos2PixelRect(
  const APoint: TPoint
): TRect;
begin
  Result.Left := APoint.X * 256;
  Result.Top := APoint.Y * 256;
  Result.Right := Result.Left + 256;
  Result.Bottom := Result.Top + 256;
  if FZoom = 23 then begin
    if Result.Right < 0 then begin
      Result.Right := MaxInt;
    end;
    if Result.Bottom < 0 then begin
      Result.Bottom := MaxInt;
    end;
  end;
end;

function TProjectionBasic256x256.InternalTilePos2Relative(
  const APoint: TPoint
): TDoublePoint;
begin
  Result := DoublePoint(APoint.X / FTilesFloat, APoint.Y / FTilesFloat);
end;

function TProjectionBasic256x256.InternalTilePosFloat2Relative(
  const APoint: TDoublePoint
): TDoublePoint;
begin
  Result := DoublePoint(APoint.X / FTilesFloat, APoint.Y / FTilesFloat);
end;

function TProjectionBasic256x256.InternalTileRect2RelativeRect(
  const ARect: TRect
): TDoubleRect;
begin
  Result :=
    DoubleRect(
      ARect.Left / FTilesFloat,
      ARect.Top / FTilesFloat,
      ARect.Right / FTilesFloat,
      ARect.Bottom / FTilesFloat
    );
end;

function TProjectionBasic256x256.InternalTileRectFloat2RelativeRect(
  const ARect: TDoubleRect
): TDoubleRect;
begin
  Result :=
    DoubleRect(
      ARect.Left / FTilesFloat,
      ARect.Top / FTilesFloat,
      ARect.Right / FTilesFloat,
      ARect.Bottom / FTilesFloat
    );
end;

function TProjectionBasic256x256.InternalRelative2PixelPosFloat(
  const APoint: TDoublePoint
): TDoublePoint;
begin
  Result := DoublePoint(APoint.X * FPixelsFloat, APoint.Y * FPixelsFloat);
end;

function TProjectionBasic256x256.InternalRelativeRect2PixelRectFloat(
  const ARect: TDoubleRect
): TDoubleRect;
begin
  Result :=
    DoubleRect(
      ARect.Left * FPixelsFloat,
      ARect.Top * FPixelsFloat,
      ARect.Right * FPixelsFloat,
      ARect.Bottom * FPixelsFloat
    );
end;

function TProjectionBasic256x256.InternalRelative2TilePosFloat(
  const APoint: TDoublePoint
): TDoublePoint;
begin
  Result := DoublePoint(APoint.X * FTilesFloat, APoint.Y * FTilesFloat);
end;

function TProjectionBasic256x256.InternalRelativeRect2TileRectFloat(
  const ARect: TDoubleRect
): TDoubleRect;
begin
  Result :=
    DoubleRect(
      ARect.Left * FTilesFloat,
      ARect.Top * FTilesFloat,
      ARect.Right * FTilesFloat,
      ARect.Bottom * FTilesFloat
    );
end;

function TProjectionBasic256x256.CheckPixelPos(const APoint: TPoint): boolean;
begin
  Result := True;
  if APoint.X < 0 then begin
    Result := False;
    Exit;
  end else begin
    if APoint.X > FPixels then begin
      Result := False;
      Exit;
    end;
  end;

  if APoint.Y < 0 then begin
    Result := False;
    Exit;
  end else begin
    if APoint.Y > FPixels then begin
      Result := False;
      Exit;
    end;
  end;
end;

function TProjectionBasic256x256.CheckPixelPosFloat(
  const APoint: TDoublePoint
): boolean;
begin
  Result := True;
  if APoint.X < 0 then begin
    Result := False;
    Exit;
  end else begin
    if APoint.X > FPixelsFloat then begin
      Result := False;
      Exit;
    end;
  end;

  if APoint.Y < 0 then begin
    Result := False;
    Exit;
  end else begin
    if APoint.Y > FPixelsFloat then begin
      Result := False;
      Exit;
    end;
  end;
end;

function TProjectionBasic256x256.CheckPixelPosFloatStrict(
  const APoint: TDoublePoint
): boolean;
begin
  Result := True;
  if APoint.X < 0 then begin
    Result := False;
    Exit;
  end else begin
    if APoint.X >= FPixelsFloat then begin
      Result := False;
      Exit;
    end;
  end;

  if APoint.Y < 0 then begin
    Result := False;
    Exit;
  end else begin
    if APoint.Y >= FPixelsFloat then begin
      Result := False;
      Exit;
    end;
  end;
end;

function TProjectionBasic256x256.CheckPixelPosStrict(const APoint: TPoint): boolean;
begin
  Result := True;
  if APoint.X < 0 then begin
    Result := False;
    Exit;
  end else begin
    if APoint.X >= FPixels then begin
      Result := False;
      Exit;
    end;
  end;

  if APoint.Y < 0 then begin
    Result := False;
    Exit;
  end else begin
    if APoint.Y >= FPixels then begin
      Result := False;
      Exit;
    end;
  end;
end;

function TProjectionBasic256x256.CheckPixelRect(const ARect: TRect): boolean;
begin
  Result := True;
  if ARect.Left < 0 then begin
    Result := False;
    Exit;
  end else begin
    if ARect.Left > FPixels then begin
      Result := False;
      Exit;
    end;
  end;

  if ARect.Top < 0 then begin
    Result := False;
    Exit;
  end else begin
    if ARect.Top > FPixels then begin
      Result := False;
      Exit;
    end;
  end;

  if ARect.Right < 0 then begin
    Result := False;
    Exit;
  end else begin
    if ARect.Right > FPixels then begin
      Result := False;
      Exit;
    end;
  end;

  if ARect.Bottom < 0 then begin
    Result := False;
    Exit;
  end else begin
    if ARect.Bottom > FPixels then begin
      Result := False;
      Exit;
    end;
  end;
end;

function TProjectionBasic256x256.CheckPixelRectFloat(const ARect: TDoubleRect): boolean;
begin
  Result := True;
  if ARect.Left < 0 then begin
    Result := False;
    Exit;
  end else begin
    if ARect.Left > FPixelsFloat then begin
      Result := False;
      Exit;
    end;
  end;

  if ARect.Top < 0 then begin
    Result := False;
    Exit;
  end else begin
    if ARect.Top > FPixelsFloat then begin
      Result := False;
      Exit;
    end;
  end;

  if ARect.Right < 0 then begin
    Result := False;
    Exit;
  end else begin
    if ARect.Right > FPixelsFloat then begin
      Result := False;
      Exit;
    end;
  end;

  if ARect.Bottom < 0 then begin
    Result := False;
    Exit;
  end else begin
    if ARect.Bottom > FPixelsFloat then begin
      Result := False;
      Exit;
    end;
  end;
end;

function TProjectionBasic256x256.CheckTilePos(const APoint: TPoint): boolean;
begin
  Result := True;
  if APoint.X < 0 then begin
    Result := False;
    Exit;
  end else begin
    if APoint.X > FTiles then begin
      Result := False;
      Exit;
    end;
  end;

  if APoint.Y < 0 then begin
    Result := False;
    Exit;
  end else begin
    if APoint.Y > FTiles then begin
      Result := False;
      Exit;
    end;
  end;
end;

function TProjectionBasic256x256.CheckTilePosStrict(const APoint: TPoint): boolean;
begin
  Result := True;
  if APoint.X < 0 then begin
    Result := False;
    Exit;
  end else begin
    if APoint.X >= FTiles then begin
      Result := False;
      Exit;
    end;
  end;

  if APoint.Y < 0 then begin
    Result := False;
    Exit;
  end else begin
    if APoint.Y >= FTiles then begin
      Result := False;
      Exit;
    end;
  end;
end;

function TProjectionBasic256x256.CheckTileRect(const ARect: TRect): boolean;
begin
  Result := True;
  if ARect.Left < 0 then begin
    Result := False;
    Exit;
  end else begin
    if ARect.Left > FTiles then begin
      Result := False;
      Exit;
    end;
  end;

  if ARect.Top < 0 then begin
    Result := False;
    Exit;
  end else begin
    if ARect.Top > FTiles then begin
      Result := False;
      Exit;
    end;
  end;

  if ARect.Right < 0 then begin
    Result := False;
    Exit;
  end else begin
    if ARect.Right > FTiles then begin
      Result := False;
      Exit;
    end;
  end;

  if ARect.Bottom < 0 then begin
    Result := False;
    Exit;
  end else begin
    if ARect.Bottom > FTiles then begin
      Result := False;
      Exit;
    end;
  end;
end;

function TProjectionBasic256x256.GetHash: THashValue;
begin
  Result := FHash;
end;

function TProjectionBasic256x256.IsSame(
  const AProjection: IProjection): Boolean;
var
  VSelf: IProjection;
begin
  VSelf := Self;
  if VSelf = AProjection then begin
    Result := True;
  end else if AProjection = nil then begin
    Result := False;
  end else begin
    if (FHash <> 0) and (AProjection.Hash <> 0) and (FHash <> AProjection.Hash) then begin
      Result := False;
      Exit;
    end;
    Result := False;
    if FTileSplitCode = AProjection.GetTileSplitCode then begin
      if FZoom = AProjection.Zoom then begin
        if FProjectionType.IsSame(AProjection.ProjectionType) then begin
          Result := True;
        end;
      end;
    end;
  end;
end;

function TProjectionBasic256x256.GetPixelRect: TRect;
begin
  Result := Rect(0, 0, FPixels, FPixels);
end;

function TProjectionBasic256x256.GetPixelsFloat: Double;
begin
  Result := FPixelsFloat;
end;

function TProjectionBasic256x256.GetProjectionType: IProjectionType;
begin
  Result := FProjectionType;
end;

function TProjectionBasic256x256.GetTileRect: TRect;
begin
  Result := Rect(0, 0, FTiles, FTiles);
end;

function TProjectionBasic256x256.GetTileSize(const APoint: TPoint): TPoint;
begin
  Result := Point(256, 256);
end;

function TProjectionBasic256x256.GetTileSplitCode: Integer;
begin
  Result := FTileSplitCode;
end;

function TProjectionBasic256x256.GetZoom: Byte;
begin
  Result := FZoom;
end;

function TProjectionBasic256x256.LonLat2PixelPosFloat(
  const APoint: TDoublePoint
): TDoublePoint;
var
  VRelative: TDoublePoint;
begin
  VRelative := FProjectionType.LonLat2Relative(APoint);
  Result := InternalRelative2PixelPosFloat(VRelative);
end;

function TProjectionBasic256x256.LonLat2TilePosFloat(
  const APoint: TDoublePoint
): TDoublePoint;
var
  VRelative: TDoublePoint;
begin
  VRelative := FProjectionType.LonLat2Relative(APoint);
  Result := InternalRelative2TilePosFloat(VRelative);
end;

function TProjectionBasic256x256.LonLatRect2PixelRectFloat(
  const ARect: TDoubleRect
): TDoubleRect;
var
  VRelative: TDoubleRect;
begin
  VRelative := FProjectionType.LonLatRect2RelativeRect(ARect);
  Result := RelativeRect2PixelRectFloat(VRelative);
end;

function TProjectionBasic256x256.LonLatRect2TileRectFloat(
  const ARect: TDoubleRect
): TDoubleRect;
var
  VRelative: TDoubleRect;
begin
  VRelative := FProjectionType.LonLatRect2RelativeRect(ARect);
  Result := InternalRelativeRect2TileRectFloat(VRelative);
end;

function TProjectionBasic256x256.PixelPos2LonLat(const APoint: TPoint): TDoublePoint;
var
  VPoint: TPoint;
  VRelative: TDoublePoint;
begin
  VPoint := APoint;
  InternalValidatePixelPos(VPoint);
  VRelative := InternalPixelPos2Relative(VPoint);
  Result := FProjectionType.Relative2LonLat(VRelative);
end;

function TProjectionBasic256x256.PixelPos2Relative(const APoint: TPoint): TDoublePoint;
var
  VPoint: TPoint;
begin
  VPoint := APoint;
  InternalValidatePixelPos(VPoint);
  Result := InternalPixelPos2Relative(VPoint)
end;

function TProjectionBasic256x256.PixelPos2TilePosFloat(
  const APoint: TPoint
): TDoublePoint;
begin
  Result := DoublePoint(APoint.X / 256.0, APoint.Y / 256.0);
end;

function TProjectionBasic256x256.PixelPosFloat2LonLat(
  const APoint: TDoublePoint
): TDoublePoint;
var
  VPoint: TDoublePoint;
  VRelative: TDoublePoint;
begin
  VPoint := APoint;
  InternalValidatePixelPosFloat(VPoint);
  VRelative := InternalPixelPosFloat2Relative(VPoint);
  Result := FProjectionType.Relative2LonLat(VRelative);
end;

function TProjectionBasic256x256.PixelPosFloat2Relative(
  const APoint: TDoublePoint
): TDoublePoint;
var
  VPoint: TDoublePoint;
begin
  VPoint := APoint;
  InternalValidatePixelPosFloat(VPoint);
  Result := InternalPixelPosFloat2Relative(VPoint)
end;

function TProjectionBasic256x256.PixelPosFloat2TilePosFloat(
  const APoint: TDoublePoint
): TDoublePoint;
begin
  Result := DoublePoint(APoint.X / 256.0, APoint.Y / 256.0);
end;

function TProjectionBasic256x256.PixelRect2LonLatRect(const ARect: TRect): TDoubleRect;
var
  VRect: TRect;
  VRelative: TDoubleRect;
begin
  VRect := ARect;
  InternalValidatePixelRect(VRect);
  VRelative := InternalPixelRect2RelativeRect(VRect);
  Result := FProjectionType.RelativeRect2LonLatRect(VRelative);
end;

function TProjectionBasic256x256.PixelRect2RelativeRect(
  const ARect: TRect
): TDoubleRect;
var
  VRect: TRect;
begin
  VRect := ARect;
  InternalValidatePixelRect(VRect);
  Result := InternalPixelRect2RelativeRect(VRect)
end;

function TProjectionBasic256x256.PixelRect2TileRect(const ARect: TRect): TRect;
var
  VRect: TRect;
begin
  VRect := ARect;
  InternalValidatePixelRect(VRect);
  Result.Left := VRect.Left shr 8;
  Result.Top := VRect.Top shr 8;
  Result.Right := (VRect.Right + 255) shr 8;
  Result.Bottom := (VRect.Bottom + 255) shr 8;
end;

function TProjectionBasic256x256.PixelRect2TileRectFloat(
  const ARect: TRect
): TDoubleRect;
var
  VRect: TRect;
begin
  VRect := ARect;
  InternalValidatePixelRect(VRect);
  Result.Left := VRect.Left / 256.0;
  Result.Top := VRect.Top / 256.0;
  Result.Right := VRect.Right / 256.0;
  Result.Bottom := VRect.Bottom / 256.0;
end;

function TProjectionBasic256x256.PixelRectFloat2LonLatRect(
  const ARect: TDoubleRect
): TDoubleRect;
var
  VRect: TDoubleRect;
  VRelative: TDoubleRect;
begin
  VRect := ARect;
  InternalValidatePixelRectFloat(VRect);
  VRelative := InternalPixelRectFloat2RelativeRect(VRect);
  Result := FProjectionType.RelativeRect2LonLatRect(VRelative);
end;

function TProjectionBasic256x256.PixelRectFloat2RelativeRect(
  const ARect: TDoubleRect
): TDoubleRect;
var
  VRect: TDoubleRect;
begin
  VRect := ARect;
  InternalValidatePixelRectFloat(VRect);
  Result := InternalPixelRectFloat2RelativeRect(VRect);
end;

function TProjectionBasic256x256.PixelRectFloat2TileRectFloat(
  const ARect: TDoubleRect
): TDoubleRect;
var
  VRect: TDoubleRect;
begin
  VRect := ARect;
  InternalValidatePixelRectFloat(VRect);
  Result.Left := VRect.Left / 256.0;
  Result.Top := VRect.Top / 256.0;
  Result.Right := VRect.Right / 256.0;
  Result.Bottom := VRect.Bottom / 256.0;
end;

function TProjectionBasic256x256.Relative2PixelPosFloat(
  const APoint: TDoublePoint
): TDoublePoint;
var
  VPoint: TDoublePoint;
begin
  VPoint := APoint;
  InternalValidateRelativePos(VPoint);
  Result := InternalRelative2PixelPosFloat(VPoint);
end;

function TProjectionBasic256x256.Relative2TilePosFloat(
  const APoint: TDoublePoint
): TDoublePoint;
var
  VPoint: TDoublePoint;
begin
  VPoint := APoint;
  InternalValidateRelativePos(VPoint);
  Result := InternalRelative2TilePosFloat(VPoint);
end;

function TProjectionBasic256x256.RelativeRect2PixelRectFloat(
  const ARect: TDoubleRect
): TDoubleRect;
var
  VRect: TDoubleRect;
begin
  VRect := ARect;
  InternalValidateRelativeRect(VRect);
  Result := InternalRelativeRect2PixelRectFloat(VRect);
end;

function TProjectionBasic256x256.RelativeRect2TileRectFloat(
  const ARect: TDoubleRect
): TDoubleRect;
var
  VRect: TDoubleRect;
begin
  VRect := ARect;
  InternalValidateRelativeRect(VRect);
  Result := InternalRelativeRect2TileRectFloat(VRect);
end;

function TProjectionBasic256x256.TilePos2LonLat(const APoint: TPoint): TDoublePoint;
var
  VPoint: TPoint;
  VRelative: TDoublePoint;
begin
  VPoint := APoint;
  InternalValidateTilePos(VPoint);
  VRelative := InternalTilePos2Relative(VPoint);
  Result := FProjectionType.Relative2LonLat(VRelative);
end;

function TProjectionBasic256x256.TilePos2LonLatRect(const APoint: TPoint): TDoubleRect;
var
  VPoint: TPoint;
  VPixelRect: TRect;
  VRelative: TDoubleRect;
begin
  VPoint := APoint;
  InternalValidateTilePosStrict(VPoint);
  VPixelRect := InternalTilePos2PixelRect(VPoint);
  VRelative := InternalPixelRect2RelativeRect(VPixelRect);
  Result := FProjectionType.RelativeRect2LonLatRect(VRelative);
end;

function TProjectionBasic256x256.TilePos2PixelPos(const APoint: TPoint): TPoint;
var
  VPoint: TPoint;
begin
  VPoint := APoint;
  InternalValidateTilePos(VPoint);
  Result.X := VPoint.X * 256;
  Result.Y := VPoint.Y * 256;
end;

function TProjectionBasic256x256.TilePos2PixelRect(const APoint: TPoint): TRect;
var
  VPoint: TPoint;
begin
  VPoint := APoint;
  InternalValidateTilePosStrict(VPoint);
  Result := InternalTilePos2PixelRect(VPoint);
end;

function TProjectionBasic256x256.TilePos2PixelRectFloat(
  const APoint: TPoint
): TDoubleRect;
var
  VPoint: TPoint;
begin
  VPoint := APoint;
  InternalValidateTilePosStrict(VPoint);
  Result := DoubleRect(InternalTilePos2PixelRect(VPoint));
end;

function TProjectionBasic256x256.TilePos2Relative(const APoint: TPoint): TDoublePoint;
var
  VPoint: TPoint;
begin
  VPoint := APoint;
  InternalValidateTilePos(VPoint);
  Result := InternalTilePos2Relative(VPoint);
end;

function TProjectionBasic256x256.TilePos2RelativeRect(
  const APoint: TPoint
): TDoubleRect;
var
  VPoint: TPoint;
begin
  VPoint := APoint;
  InternalValidateTilePosStrict(VPoint);
  Result := InternalPixelRect2RelativeRect(InternalTilePos2PixelRect(VPoint));
end;

function TProjectionBasic256x256.TilePosFloat2LonLat(
  const APoint: TDoublePoint
): TDoublePoint;
var
  VPoint: TDoublePoint;
  VRelative: TDoublePoint;
begin
  VPoint := APoint;
  InternalValidateTilePosFloat(VPoint);
  VRelative := InternalTilePosFloat2Relative(VPoint);
  Result := FProjectionType.Relative2LonLat(VRelative);
end;

function TProjectionBasic256x256.TilePosFloat2PixelPosFloat(
  const APoint: TDoublePoint
): TDoublePoint;
var
  VPoint: TDoublePoint;
begin
  VPoint := APoint;
  InternalValidateTilePosFloat(VPoint);
  Result.X := VPoint.X * 256;
  Result.Y := VPoint.Y * 256;
end;

function TProjectionBasic256x256.TilePosFloat2Relative(
  const APoint: TDoublePoint
): TDoublePoint;
var
  VPoint: TDoublePoint;
begin
  VPoint := APoint;
  InternalValidateTilePosFloat(VPoint);
  Result := InternalTilePosFloat2Relative(VPoint);
end;

function TProjectionBasic256x256.TileRect2LonLatRect(const ARect: TRect): TDoubleRect;
var
  VRect: TRect;
  VRelative: TDoubleRect;
begin
  VRect := ARect;
  InternalValidateTileRect(VRect);
  VRelative := InternalTileRect2RelativeRect(VRect);
  Result := FProjectionType.RelativeRect2LonLatRect(VRelative);
end;

function TProjectionBasic256x256.TileRect2PixelRect(const ARect: TRect): TRect;
var
  VRect: TRect;
begin
  VRect := ARect;
  InternalValidateTileRect(VRect);
  Result.Left := VRect.Left * 256;
  Result.Top := VRect.Top * 256;
  Result.Right := VRect.Right * 256;
  Result.Bottom := VRect.Bottom * 256;
  if FZoom = 23 then begin
    if Result.Right < 0 then begin
      Result.Right := MaxInt;
    end;
    if Result.Bottom < 0 then begin
      Result.Bottom := MaxInt;
    end;
  end;
end;

function TProjectionBasic256x256.TileRect2RelativeRect(const ARect: TRect): TDoubleRect;
var
  VRect: TRect;
begin
  VRect := ARect;
  InternalValidateTileRect(VRect);
  Result := InternalTileRect2RelativeRect(VRect);
end;

function TProjectionBasic256x256.TileRectFloat2LonLatRect(
  const ARect: TDoubleRect
): TDoubleRect;
var
  VRect: TDoubleRect;
  VRelative: TDoubleRect;
begin
  VRect := ARect;
  InternalValidateTileRectFloat(VRect);
  VRelative := InternalTileRectFloat2RelativeRect(VRect);
  Result := FProjectionType.RelativeRect2LonLatRect(VRelative);
end;

function TProjectionBasic256x256.TileRectFloat2PixelRectFloat(
  const ARect: TDoubleRect
): TDoubleRect;
var
  VRect: TDoubleRect;
begin
  VRect := ARect;
  InternalValidateTileRectFloat(VRect);
  Result.Left := VRect.Left * 256;
  Result.Top := VRect.Top * 256;
  Result.Right := VRect.Right * 256;
  Result.Bottom := VRect.Bottom * 256;
end;

function TProjectionBasic256x256.TileRectFloat2RelativeRect(
  const ARect: TDoubleRect
): TDoubleRect;
var
  VRect: TDoubleRect;
begin
  VRect := ARect;
  InternalValidateTileRectFloat(VRect);
  Result := InternalTileRectFloat2RelativeRect(VRect);
end;

procedure TProjectionBasic256x256.ValidatePixelPos(
  var APoint: TPoint;
  ACicleMap: Boolean
);
begin
  if APoint.X < 0 then begin
    if ACicleMap then begin
      APoint.X := APoint.X mod FPixels + FPixels;
    end else begin
      APoint.X := 0;
    end;
  end else begin
    if APoint.X > FPixels then begin
      if ACicleMap then begin
        APoint.X := APoint.X mod FPixels;
      end else begin
        APoint.X := FPixels;
      end;
    end;
  end;

  if APoint.Y < 0 then begin
    APoint.Y := 0;
  end else begin
    APoint.Y := FPixels;
  end;
end;

procedure TProjectionBasic256x256.ValidatePixelPosFloat(
  var APoint: TDoublePoint;
  ACicleMap: Boolean
);
begin
  if APoint.X < 0 then begin
    if ACicleMap then begin
      APoint.X := APoint.X - Int(APoint.X / FPixelsFloat) * FPixelsFloat + FPixelsFloat;
    end else begin
      APoint.X := 0;
    end;
  end else begin
    if (APoint.X > FPixelsFloat) then begin
      if ACicleMap then begin
        APoint.X := APoint.X - Int(APoint.X / FPixelsFloat) * FPixelsFloat;
      end else begin
        APoint.X := FPixelsFloat;
      end;
    end;
  end;

  if APoint.Y < 0 then begin
    APoint.Y := 0;
  end else begin
    if (APoint.Y > FPixelsFloat) then begin
      APoint.Y := FPixelsFloat;
    end;
  end;
end;

procedure TProjectionBasic256x256.ValidatePixelPosFloatStrict(
  var APoint: TDoublePoint;
  ACicleMap: Boolean
);
begin
  if APoint.X < 0 then begin
    if ACicleMap then begin
      APoint.X := APoint.X - Int(APoint.X / FPixelsFloat) * FPixelsFloat + FPixelsFloat;
    end else begin
      APoint.X := 0;
    end;
  end else begin
    if (APoint.X >= FPixelsFloat) then begin
      if ACicleMap then begin
        APoint.X := APoint.X - Int(APoint.X / FPixelsFloat) * FPixelsFloat;
      end else begin
        APoint.X := FPixelsFloat;
      end;
    end;
  end;

  if APoint.Y < 0 then begin
    APoint.Y := 0;
  end else begin
    if (APoint.Y >= FPixelsFloat) then begin
      APoint.Y := FPixelsFloat;
    end;
  end;
end;

procedure TProjectionBasic256x256.ValidatePixelPosStrict(
  var APoint: TPoint;
  ACicleMap: Boolean
);
begin
  if APoint.X < 0 then begin
    if ACicleMap then begin
      APoint.X := APoint.X mod FPixels + FPixels;
    end else begin
      APoint.X := 0;
    end;
  end else begin
    if APoint.X >= FPixels then begin
      if ACicleMap then begin
        APoint.X := APoint.X mod FPixels;
      end else begin
        APoint.X := FPixels - 1;
      end;
    end;
  end;

  if APoint.Y < 0 then begin
    APoint.Y := 0;
  end else begin
    if APoint.Y >= FPixels then begin
      APoint.Y := FPixels - 1;
    end;
  end;
end;

procedure TProjectionBasic256x256.ValidatePixelRect(var ARect: TRect);
begin
  if ARect.Left < 0 then begin
    ARect.Left := 0;
  end else begin
    if ARect.Left > FPixels then begin
      ARect.Left := FPixels;
    end;
  end;

  if ARect.Top < 0 then begin
    ARect.Top := 0;
  end else begin
    if ARect.Top > FPixels then begin
      ARect.Top := FPixels;
    end;
  end;

  if ARect.Right < 0 then begin
    ARect.Right := 0;
  end else begin
    if ARect.Right > FPixels then begin
      ARect.Right := FPixels;
    end;
  end;

  if ARect.Bottom < 0 then begin
    ARect.Bottom := 0;
  end else begin
    if ARect.Bottom > FPixels then begin
      ARect.Bottom := FPixels;
    end;
  end;
end;

procedure TProjectionBasic256x256.ValidatePixelRectFloat(var ARect: TDoubleRect);
begin
  if ARect.Left < 0 then begin
    ARect.Left := 0;
  end else begin
    if ARect.Left > FPixelsFloat then begin
      ARect.Left := FPixelsFloat;
    end;
  end;

  if ARect.Top < 0 then begin
    ARect.Top := 0;
  end else begin
    if ARect.Top > FPixelsFloat then begin
      ARect.Top := FPixelsFloat;
    end;
  end;

  if ARect.Right < 0 then begin
    ARect.Right := 0;
  end else begin
    if ARect.Right > FPixelsFloat then begin
      ARect.Right := FPixelsFloat;
    end;
  end;

  if ARect.Bottom < 0 then begin
    ARect.Bottom := 0;
  end else begin
    if ARect.Bottom > FPixelsFloat then begin
      ARect.Bottom := FPixelsFloat;
    end;
  end;
end;

procedure TProjectionBasic256x256.ValidateTilePos(
  var APoint: TPoint;
  ACicleMap: Boolean
);
begin
  if APoint.X < 0 then begin
    if ACicleMap then begin
      APoint.X := APoint.X mod FTiles + FTiles;
    end else begin
      APoint.X := 0;
    end;
  end else begin
    if APoint.X > FTiles then begin
      if ACicleMap then begin
        APoint.X := APoint.X mod FTiles;
      end else begin
        APoint.X := FTiles;
      end;
    end;
  end;

  if APoint.Y < 0 then begin
    APoint.Y := 0;
  end else begin
    if APoint.Y > FTiles then begin
      APoint.Y := FTiles;
    end;
  end;
end;

procedure TProjectionBasic256x256.ValidateTilePosStrict(
  var APoint: TPoint;
  ACicleMap: Boolean
);
begin
  if APoint.X < 0 then begin
    if ACicleMap then begin
      APoint.X := APoint.X mod FTiles + FTiles;
    end else begin
      APoint.X := 0;
    end;
  end else begin
    if APoint.X >= FTiles then begin
      if ACicleMap then begin
        APoint.X := APoint.X mod FTiles;
      end else begin
        APoint.X := FTiles - 1;
      end;
    end;
  end;

  if APoint.Y < 0 then begin
    APoint.Y := 0;
  end else begin
    if APoint.Y >= FTiles then begin
      APoint.Y := FTiles - 1;
    end;
  end;
end;

procedure TProjectionBasic256x256.ValidateTileRect(var ARect: TRect);
begin
  if ARect.Left < 0 then begin
    ARect.Left := 0;
  end else begin
    if ARect.Left > FTiles then begin
      ARect.Left := FTiles;
    end;
  end;

  if ARect.Top < 0 then begin
    ARect.Top := 0;
  end else begin
    if ARect.Top > FTiles then begin
      ARect.Top := FTiles;
    end;
  end;

  if ARect.Right < 0 then begin
    ARect.Right := 0;
  end else begin
    if ARect.Right > FTiles then begin
      ARect.Right := FTiles;
    end;
  end;

  if ARect.Bottom < 0 then begin
    ARect.Bottom := 0;
  end else begin
    if ARect.Bottom > FTiles then begin
      ARect.Bottom := FTiles;
    end;
  end;
end;

end.
