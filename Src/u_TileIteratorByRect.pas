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
  i_TileIterator,
  u_BaseInterfacedObject;

type
  TTileIteratorByRectBase = class(TBaseInterfacedObject)
  private
    FTilesTotal: Int64;
    FTilesRect: TRect;
  protected
    property TilesRect: TRect read FTilesRect;
    function GetTilesTotal: Int64;
    function GetTilesRect: TRect;
  public
    constructor Create(const ARect: TRect);
  end;

  TTileIteratorByRect = class(TTileIteratorByRectBase, ITileIterator, ITileIteratorByRows)
  private
    FEOI: Boolean;
    FCurrent: TPoint;
  private
    function Next(out ATile: TPoint): Boolean;
    procedure Reset;
  public
    constructor Create(const ARect: TRect);
  end;

implementation

{ TTileIteratorByRectBase }

constructor TTileIteratorByRectBase.Create(const ARect: TRect);
begin
  inherited Create;
  FTilesRect := ARect;
  if IsRectEmpty(FTilesRect) then begin
    FTilesTotal := 0;
  end else begin
    FTilesTotal := (FTilesRect.Right - FTilesRect.Left);
    FTilesTotal := FTilesTotal * (FTilesRect.Bottom - FTilesRect.Top);
  end;
end;

function TTileIteratorByRectBase.GetTilesRect: TRect;
begin
  Result := FTilesRect;
end;

function TTileIteratorByRectBase.GetTilesTotal: Int64;
begin
  Result := FTilesTotal;
end;

{ TTileIteratorByRect }

constructor TTileIteratorByRect.Create(const ARect: TRect);
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
  if IsRectEmpty(TilesRect) then begin
    FEOI := True;
  end else begin
    FEOI := False;
    FCurrent := TilesRect.TopLeft;
  end;
end;

end.
