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

unit u_TileIteratorByRect;

interface

uses
  Types,
  i_TileIterator,
  u_TileIteratorAbstract;

type
  TTileIteratorByRectBase = class(TTileIteratorAbstract)
  protected
    FTilesTotal: Int64;
    FTilesRect: TRect;
    function GetTilesTotal: Int64; override;
    function GetTilesRect: TRect; override;
  public
    constructor Create(const ARect: TRect); virtual;
  end;

  TTileIteratorByRect = class(TTileIteratorByRectBase, ITileIteratorByRows)
  protected
    FEOI: Boolean;
    FCurrent: TPoint;
    function Next(out ATile: TPoint): Boolean; override;
    procedure Reset; override;
  public
    constructor Create(const ARect: TRect); override;
  end;

implementation

{ TTileIteratorByRectBase }

constructor TTileIteratorByRectBase.Create(const ARect: TRect);
begin
  inherited Create;
  FTilesRect := ARect;
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
    if FCurrent.X >= FTilesRect.Right then begin
      FCurrent.X := FTilesRect.Left;
      Inc(FCurrent.Y);
      if FCurrent.Y >= FTilesRect.Bottom then begin
        FEOI := True;
      end;
    end;
  end;
end;

procedure TTileIteratorByRect.Reset;
begin
  if IsRectEmpty(FTilesRect) then begin
    FEOI := True;
    FTilesTotal := 0;
  end else begin
    FEOI := False;
    FTilesTotal := (FTilesRect.Right - FTilesRect.Left);
    FTilesTotal := FTilesTotal * (FTilesRect.Bottom - FTilesRect.Top);
    FCurrent := FTilesRect.TopLeft;
  end;
end;

end.
