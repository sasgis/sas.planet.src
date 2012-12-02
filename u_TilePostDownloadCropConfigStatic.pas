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

unit u_TilePostDownloadCropConfigStatic;

interface

uses
  Types,
  i_TilePostDownloadCropConfig,
  u_BaseInterfacedObject;

type
  TTilePostDownloadCropConfigStatic = class(TBaseInterfacedObject, ITilePostDownloadCropConfigStatic)
  private
    // crop params
    FIsCropOnDownload: Boolean;
    FCropRect: TRect;
    // cut params
    FIsCutOnDownload: Boolean;
    FCutCount: TPoint;
    FCutSize: TPoint;
    FCutTile: TPoint;
    FCutToSkip: String;
  private
    // crop image
    function GetIsCropOnDownload: Boolean;
    function GetCropRect: TRect;
    // cut image
    function GetIsCutOnDownload: Boolean;
    function GetCutCount: TPoint;
    function GetCutSize: TPoint;
    function GetCutTile: TPoint;
    function CutSkipItem(const AItem, ACount: TPoint): Boolean;
  public
    constructor Create(
      const ACropRect: TRect;
      const ACutCount: TPoint;
      const ACutSize: TPoint;
      const ACutTile: TPoint;
      const ACutToSkip: String
    );
  end;

implementation

uses
  SysUtils;

{ TTilePostDownloadCropConfigStatic }

constructor TTilePostDownloadCropConfigStatic.Create(
  const ACropRect: TRect;
  const ACutCount: TPoint;
  const ACutSize: TPoint;
  const ACutTile: TPoint;
  const ACutToSkip: String
);
begin
  inherited Create;

  // crop params
  if (ACropRect.Left >= 0) and
    (ACropRect.Top >= 0) and
    (ACropRect.Right > ACropRect.Left) and
    (ACropRect.Bottom > ACropRect.Top) and
    (ACropRect.Right < 10000) and
    (ACropRect.Bottom < 10000) then begin
    FIsCropOnDownload := True;
    FCropRect := ACropRect;
  end else begin
    FIsCropOnDownload := False;
    FCropRect := Rect(0, 0, 0, 0);
  end;

  // cut params (by Size or by Count)
  if (
    (ACutCount.X > 0) and
    (ACutCount.Y > 0) and
    (((ACutTile.X >= 0) and (ACutTile.X < ACutCount.X)) or (ACutTile.X < 0)) and
    (((ACutTile.Y >= 0) and (ACutTile.Y < ACutCount.Y)) or (ACutTile.Y < 0))
    ) OR
    (
    (ACutSize.X > 0) or (ACutSize.Y > 0) // if not defined - use 256 from config
    ) OR
    (
    (0 < Length(ACutToSkip)) // some tiles to exclude
    ) OR
    (
    (ACutTile.X <> 0) or (ACutTile.Y <> 0) // requested tile in big image
    ) then begin
    // do
    FIsCutOnDownload := TRUE;
    FCutCount := ACutCount;
    FCutSize := ACutSize;
    FCutTile := ACutTile;
    FCutToSkip := ACutToSkip;
  end else begin
    // no cutting
    FIsCutOnDownload := FALSE;
    FCutCount := Point(0, 0);
    FCutSize := FCutCount;
    FCutTile := FCutCount;
    FCutToSkip := '';
  end;
end;

function TTilePostDownloadCropConfigStatic.CutSkipItem(const AItem, ACount: TPoint): Boolean;
var
  S: String;
begin
  if (0 < Length(FCutToSkip)) then begin
    S := '(' + IntToStr(AItem.X) + ',' + IntToStr(AItem.Y) + ')';
    Result := (System.Pos(S, FCutToSkip) > 0);
    if Result then begin
      Exit;
    end;
    S := '(' + IntToStr(AItem.X) + ',' + IntToStr(AItem.Y - ACount.Y) + ')';
    Result := (System.Pos(S, FCutToSkip) > 0);
  end else begin
    Result := FALSE;
  end;
end;

function TTilePostDownloadCropConfigStatic.GetCropRect: TRect;
begin
  Result := FCropRect;
end;

function TTilePostDownloadCropConfigStatic.GetCutCount: TPoint;
begin
  Result := FCutCount;
end;

function TTilePostDownloadCropConfigStatic.GetCutSize: TPoint;
begin
  Result := FCutSize;
end;

function TTilePostDownloadCropConfigStatic.GetCutTile: TPoint;
begin
  Result := FCutTile;
end;

function TTilePostDownloadCropConfigStatic.GetIsCropOnDownload: Boolean;
begin
  Result := FIsCropOnDownload;
end;

function TTilePostDownloadCropConfigStatic.GetIsCutOnDownload: Boolean;
begin
  Result := FIsCutOnDownload;
end;

end.
