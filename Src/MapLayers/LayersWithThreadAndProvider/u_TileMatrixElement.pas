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

unit u_TileMatrixElement;

interface

uses
  Types,
  SysUtils,
  i_Bitmap32Static,
  i_LocalCoordConverter,
  i_TileMatrix,
  u_BaseInterfacedObject;

type
  TTileMatrixElement = class(TBaseInterfacedObject, ITileMatrixElement)
  private
    FSync: IReadWriteSync;

    FTile: TPoint;
    FLocalConverter: ILocalCoordConverter;

    FReadyID: Integer;
    FExpectedID: Integer;
    FBitmap: IBitmap32Static;
  private
    function GetTile: TPoint;
    function GetLocalConverter: ILocalCoordConverter;
    function GetReadyID: Integer;
    function GetExpectedID: Integer;
    function GetBitmap: IBitmap32Static;

    procedure IncExpectedID;
    procedure UpdateBitmap(
      AID: Integer;
      const ABitmap: IBitmap32Static
    );
  public
    constructor Create(
      const ATile: TPoint;
      const ALocalConverter: ILocalCoordConverter;
      const ABitmap: IBitmap32Static
    );
  end;

implementation

uses
  i_CoordConverter,
  u_Synchronizer;

{ TTileMatrixElement }

constructor TTileMatrixElement.Create(
  const ATile: TPoint;
  const ALocalConverter: ILocalCoordConverter;
  const ABitmap: IBitmap32Static
);
var
  VZoom: Byte;
  VConverter: ICoordConverter;
begin
  inherited Create;
  FTile := ATile;
  FLocalConverter := ALocalConverter;
  FBitmap := ABitmap;
  FSync := GSync.SyncVariable.Make(Self.ClassName);
  FReadyID := 0;
  FExpectedID := 1;
  VZoom := FLocalConverter.Zoom;
  VConverter := FLocalConverter.GeoConverter;
  Assert(EqualRect(FLocalConverter.GetRectInMapPixel, VConverter.TilePos2PixelRect(FTile, VZoom)));
end;

function TTileMatrixElement.GetBitmap: IBitmap32Static;
begin
  FSync.BeginRead;
  try
    Result := FBitmap;
  finally
    FSync.EndRead;
  end;
end;

function TTileMatrixElement.GetExpectedID: Integer;
begin
  FSync.BeginRead;
  try
    Result := FExpectedID;
  finally
    FSync.EndRead;
  end;
end;

function TTileMatrixElement.GetLocalConverter: ILocalCoordConverter;
begin
  Result := FLocalConverter;
end;

function TTileMatrixElement.GetReadyID: Integer;
begin
  FSync.BeginRead;
  try
    Result := FReadyID;
  finally
    FSync.EndRead;
  end;
end;

function TTileMatrixElement.GetTile: TPoint;
begin
  Result := FTile;
end;

procedure TTileMatrixElement.IncExpectedID;
begin
  FSync.BeginWrite;
  try
    Inc(FExpectedID);
  finally
    FSync.EndWrite;
  end;
end;

procedure TTileMatrixElement.UpdateBitmap(
  AID: Integer;
  const ABitmap: IBitmap32Static
);
begin
  FSync.BeginWrite;
  try
    FReadyID := AID;
    FBitmap := ABitmap;
  finally
    FSync.EndWrite;
  end;
end;

end.
