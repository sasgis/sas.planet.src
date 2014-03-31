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

unit u_Bitmap32BufferFactorySimple;

interface

uses
  Types,
  t_Bitmap32,
  i_Bitmap32Static,
  i_Bitmap32BufferFactory,
  u_BaseInterfacedObject;

type
  TBitmap32BufferFactorySimple = class(TBaseInterfacedObject, IBitmap32BufferFactory)
  private
    function Build(
      const ASize: TPoint;
      const AData: PColor32Array
    ): IBitmap32Static;
    function BuildEmpty(const ASize: TPoint): IBitmap32Static;
    function BuildEmptyClear(
      const ASize: TPoint;
      const AColor: TColor32
    ): IBitmap32Static;
  end;

implementation

uses
  GR32_LowLevel;

type
  TBitmap32StaticSimple = class(TBaseInterfacedObject, IBitmap32Static)
  private
    FSize: TPoint;
    FBits: PColor32Array;
  private
    function GetSize: TPoint;
    function GetData: PColor32Array;
  public
    constructor Create(
      const ASize: TPoint
    );
    destructor Destroy; override;
  end;

{ TBitmap32StaticSimple }


constructor TBitmap32StaticSimple.Create(
  const ASize: TPoint
);
begin
  inherited Create;
  Assert(ASize.X > 0);
  Assert(ASize.Y > 0);
  Assert(ASize.X < 1 shl 16);
  Assert(ASize.Y < 1 shl 16);
  Assert(ASize.X * ASize.Y < 1 shl 28);

  if
    (ASize.X > 0) and (ASize.Y > 0) and
    (ASize.X < 1 shl 16) and (ASize.Y < 1 shl 16) and
    (ASize.X * ASize.Y < 1 shl 28)
  then begin
    GetMem(FBits, ASize.X * ASize.Y * 4);
    FSize := ASize;
  end;
end;

destructor TBitmap32StaticSimple.Destroy;
begin
  if Assigned(FBits) then begin
    FreeMem(FBits);
    FBits := nil;
  end;
  inherited;
end;

function TBitmap32StaticSimple.GetData: PColor32Array;
begin
  Result := FBits;
end;

function TBitmap32StaticSimple.GetSize: TPoint;
begin
  Result := FSize;
end;


{ TBitmap32StaticFactorySimple }

function TBitmap32BufferFactorySimple.Build(
  const ASize: TPoint;
  const AData: PColor32Array
): IBitmap32Static;
begin
  Assert(ASize.X > 0);
  Assert(ASize.Y > 0);
  Assert(ASize.X < 1 shl 16);
  Assert(ASize.Y < 1 shl 16);
  Assert(ASize.X * ASize.Y < 1 shl 28);
  Assert(AData <> nil);

  Result := BuildEmpty(ASize);
  if (Result <> nil) and (AData <> nil) then begin
    if AData <> nil then begin
      MoveLongWord(AData^, Result.Data^, ASize.X * ASize.Y);
    end;
  end;
end;

function TBitmap32BufferFactorySimple.BuildEmpty(
  const ASize: TPoint
): IBitmap32Static;
begin
  Assert(ASize.X > 0);
  Assert(ASize.Y > 0);
  Assert(ASize.X < 1 shl 16);
  Assert(ASize.Y < 1 shl 16);
  Assert(ASize.X * ASize.Y < 1 shl 28);

  if
    (ASize.X > 0) and (ASize.Y > 0) and
    (ASize.X < 1 shl 16) and (ASize.Y < 1 shl 16) and
    (ASize.X * ASize.Y < 1 shl 28)
  then begin
    Result := TBitmap32StaticSimple.Create(ASize);
  end else begin
    Result := nil;
  end;
end;

function TBitmap32BufferFactorySimple.BuildEmptyClear(
  const ASize: TPoint;
  const AColor: TColor32
): IBitmap32Static;
begin
  Result := BuildEmpty(ASize);
  if Result <> nil then begin
    FillLongword(Result.Data^, ASize.X * ASize.Y, AColor);
  end;
end;

end.
