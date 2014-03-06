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

unit u_Bitmap8Static;

interface

uses
  Types,
  GR32,
  i_Bitmap8Static,
  u_BaseInterfacedObject;

type
  TBitmap8Static = class(TBaseInterfacedObject, IBitmap8Static)
  private
    FSize: TPoint;
    FData: PByte;
    FPalette: PColor32Array;
    FPaletteSize: Integer;
  private
    { IBitmap8Static }
    function GetSize: TPoint;
    function GetData: PByte;
    function GetPalette: PColor32Array;
    function GetPaletteSize: Integer;
  public
    constructor CreateWithOwn(
      var AData: PByte;
      const ASize: TPoint;
      const APalette: PColor32Array;
      const APaletteSize: Integer
    );
    destructor Destroy; override;
  end;

implementation

{ TBitmap8Static }

constructor TBitmap8Static.CreateWithOwn(
  var AData: PByte;
  const ASize: TPoint;
  const APalette: PColor32Array;
  const APaletteSize: Integer
);
begin
  inherited Create;

  Assert(ASize.X > 0);
  Assert(ASize.Y > 0);
  Assert(ASize.X < 1 shl 16);
  Assert(ASize.Y < 1 shl 16);
  Assert(ASize.X * ASize.Y < 1 shl 28);

  Assert(APaletteSize > 0);
  Assert(APaletteSize <= 256);

  if
    (ASize.X > 0) and (ASize.Y > 0) and
    (ASize.X < 1 shl 16) and (ASize.Y < 1 shl 16) and
    (ASize.X * ASize.Y < 1 shl 28) and
    (APaletteSize > 0) and (APaletteSize <= 256)
  then begin
    FData := AData;
    AData := nil;
    FSize := ASize;
    FPaletteSize := APaletteSize;
    FPalette := GetMemory(FPaletteSize * 4);
    Move(APalette^, FPalette^, FPaletteSize * 4);
  end;
end;

destructor TBitmap8Static.Destroy;
begin
  if Assigned(FData) then begin
    FreeMemory(FData);
    FData := nil;
  end;
  if Assigned(FPalette) then begin
    FreeMemory(FPalette);
    FPalette := nil;
  end;
  inherited;
end;

function TBitmap8Static.GetData: PByte;
begin
  Result := FData;
end;

function TBitmap8Static.GetSize: TPoint;
begin
  Result := FSize;
end;

function TBitmap8Static.GetPalette: PColor32Array;
begin
  Result := FPalette;
end;

function TBitmap8Static.GetPaletteSize: Integer;
begin
  Result := FPaletteSize;
end;

end.
