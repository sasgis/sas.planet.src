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
  i_TilePostDownloadCropConfig;

type
  TTilePostDownloadCropConfigStatic = class(TInterfacedObject, ITilePostDownloadCropConfigStatic)
  private
    FIsCropOnDownload: Boolean;
    FCropRect: TRect;
  protected
    function GetIsCropOnDownload: Boolean;
    function GetCropRect: TRect;
  public
    constructor Create(
      ACropRect: TRect
    );
  end;

implementation

{ TTilePostDownloadCropConfigStatic }

constructor TTilePostDownloadCropConfigStatic.Create(ACropRect: TRect);
begin
  inherited Create;
  if
    (ACropRect.Left >= 0) and
    (ACropRect.Top >= 0) and
    (ACropRect.Right > ACropRect.Left) and
    (ACropRect.Bottom > ACropRect.Top) and
    (ACropRect.Right < 10000) and
    (ACropRect.Bottom < 10000)
  then begin
    FIsCropOnDownload := True;
    FCropRect := ACropRect;
  end else begin
    FIsCropOnDownload := False;
    FCropRect := Rect(0, 0, 0, 0);
  end;
end;

function TTilePostDownloadCropConfigStatic.GetCropRect: TRect;
begin
  Result := FCropRect;
end;

function TTilePostDownloadCropConfigStatic.GetIsCropOnDownload: Boolean;
begin
  Result := FIsCropOnDownload;
end;

end.
