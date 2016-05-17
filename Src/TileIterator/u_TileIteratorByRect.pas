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

unit u_TileIteratorByRect;

interface

uses
  Types,
  SysUtils,
  i_TileRect,
  i_TileIterator,
  u_BaseInterfacedObject;

type
  TTileIteratorByRectBase = class(TBaseInterfacedObject)
  private
    FTilesTotal: Int64;
    FRect: TRect;
    FTilesRect: ITileRect;
  protected
    property TilesRect: TRect read FRect;
    function GetTilesTotal: Int64;
    function GetTilesRect: ITileRect;
  public
    constructor Create(const ARect: ITileRect);
  end;

  TTileIteratorByRect = class(TTileIteratorByRectBase, ITileIterator, ITileIteratorByRows)
  private
    FEOI: Boolean;
    FCurrent: TPoint;
  private
    function Next(out ATile: TPoint): Boolean;
    procedure Reset;
  public
    constructor Create(const ARect: ITileRect);
  end;

  TTileIteratorByRectRecord = record
  private
    FTilesRect: TRect;
    FCurrent: TPoint;
    FTilesTotal: Int64;
    FEOI: Boolean;
  public
    procedure Init(const ARect: TRect); inline;
    property TilesTotal: Int64 read FTilesTotal;
    function Next(out ATile: TPoint): Boolean; inline;
    procedure Reset; inline;
  end;

implementation

{ TTileIteratorByRectBase }

constructor TTileIteratorByRectBase.Create(const ARect: ITileRect);
begin
  inherited Create;
  FTilesRect := ARect;
  FRect := FTilesRect.Rect;
  if IsRectEmpty(FRect) then begin
    FTilesTotal := 0;
  end else begin
    FTilesTotal := (FRect.Right - FRect.Left);
    FTilesTotal := FTilesTotal * (FRect.Bottom - FRect.Top);
  end;
end;

function TTileIteratorByRectBase.GetTilesRect: ITileRect;
begin
  Result := FTilesRect;
end;

function TTileIteratorByRectBase.GetTilesTotal: Int64;
begin
  Result := FTilesTotal;
end;

{ TTileIteratorByRect }

constructor TTileIteratorByRect.Create(const ARect: ITileRect);
begin
  inherited Create(ARect);
  Reset;
end;

function TTileIteratorByRect.Next(out ATile: TPoint): Boolean;
begin
  Result := False;
  if not FEOI then begin
    Result := True;
    ATile := FCurrent;
    Inc(FCurrent.X);
    if FCurrent.X >= TilesRect.Right then begin
      FCurrent.X := TilesRect.Left;
      Inc(FCurrent.Y);
      if FCurrent.Y >= TilesRect.Bottom then begin
        FEOI := True;
      end;
    end;
  end;
end;

procedure TTileIteratorByRect.Reset;
begin
  FEOI := IsRectEmpty(TilesRect);
  if not FEOI then begin
    FCurrent := TilesRect.TopLeft;
  end;
end;

{ TTileIteratorByRectRecord }

procedure TTileIteratorByRectRecord.Init(const ARect: TRect);
begin
  FTilesRect := ARect;
  if IsRectEmpty(FTilesRect) then begin
    FTilesTotal := 0;
  end else begin
    FTilesTotal := (FTilesRect.Right - FTilesRect.Left);
    FTilesTotal := FTilesTotal * (FTilesRect.Bottom - FTilesRect.Top);
  end;
  Reset;
end;

function TTileIteratorByRectRecord.Next(out ATile: TPoint): Boolean;
begin
  Result := False;
  if not FEOI then begin
    Result := True;
    ATile := FCurrent;
    Inc(FCurrent.X);
    if FCurrent.X >= FTilesRect.Right then begin
      FCurrent.X := FTilesRect.Left;
      Inc(FCurrent.Y);
      if FCurrent.Y >= FTilesRect.Bottom then begin
        FEOI := True;
      end;
    end;
  end;
end;

procedure TTileIteratorByRectRecord.Reset;
begin
  FEOI := IsRectEmpty(FTilesRect);
  if not FEOI then begin
    FCurrent := FTilesRect.TopLeft;
  end;
end;

end.
