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

unit u_BitmapPostProcessing;

interface

uses
  t_Bitmap32,
  i_Bitmap32Static,
  i_Bitmap32BufferFactory,
  i_BitmapPostProcessing,
  u_BaseInterfacedObject;

type
  TComponentTable = array [0..255] of byte;

  TBitmapPostProcessingSimple = class(TBaseInterfacedObject, IBitmapPostProcessing)
  private
    function Process(const ABitmap: IBitmap32Static): IBitmap32Static;
  end;

  TBitmapPostProcessingByTable = class(TBaseInterfacedObject, IBitmapPostProcessing)
  private
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FTable: TComponentTable;
  private
    function Process(const ABitmap: IBitmap32Static): IBitmap32Static;
  public
    constructor Create(
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const ATable: TComponentTable
    );
  end;

  TBitmapPostProcessingMakeTransparetnByColor = class(TBaseInterfacedObject, IBitmapPostProcessing)
  private
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FMaskColor: TColor32;
  private
    function Process(const ABitmap: IBitmap32Static): IBitmap32Static;
  public
    constructor Create(
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const AMaskColor: TColor32
    );
  end;

implementation

uses
  GR32;

{ TBitmapPostProcessingSimple }

function TBitmapPostProcessingSimple.Process(
  const ABitmap: IBitmap32Static): IBitmap32Static;
begin
  Result := ABitmap;
end;

{ TBitmapPostProcessingByTable }

constructor TBitmapPostProcessingByTable.Create(
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const ATable: TComponentTable
);
begin
  Assert(Assigned(ABitmap32StaticFactory));
  inherited Create;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FTable := ATable;
end;

function TBitmapPostProcessingByTable.Process(
  const ABitmap: IBitmap32Static
): IBitmap32Static;
var
  i: Integer;
  VSize: TPoint;
  VBuffer: IBitmap32Buffer;
  VSource: PColor32Array;
  VTarget: PColor32Array;
  VColorEntry: TColor32Entry;
begin
  Result := nil;
  if ABitmap <> nil then begin
    VSize := ABitmap.Size;
    VBuffer := FBitmap32StaticFactory.BufferFactory.BuildEmpty(VSize);
    if VBuffer <> nil then begin
      VSource := ABitmap.Data;
      VTarget := VBuffer.Data;
      for i := 0 to VSize.X * VSize.Y - 1 do begin
        VColorEntry.ARGB := VSource[i];
        VTarget[i] :=
          Color32(
            FTable[VColorEntry.R],
            FTable[VColorEntry.G],
            FTable[VColorEntry.B],
            VColorEntry.A
          );
      end;
      Result := FBitmap32StaticFactory.BuildWithOwnBuffer(VBuffer);
    end;
  end;
end;

{ TBitmapPostProcessingMakeTransparetnByColor }

constructor TBitmapPostProcessingMakeTransparetnByColor.Create(
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const AMaskColor: TColor32
);
begin
  Assert(Assigned(ABitmap32StaticFactory));
  inherited Create;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FMaskColor := AMaskColor;
end;

function TBitmapPostProcessingMakeTransparetnByColor.Process(
  const ABitmap: IBitmap32Static
): IBitmap32Static;
var
  VSourceLine: PColor32Array;
  VTargetLine: PColor32Array;
  i: Integer;
  VSize: TPoint;
  VBuffer: IBitmap32Buffer;
  VIsChanged: Boolean;
begin
  Result := nil;
  if ABitmap <> nil then begin
    VSize := ABitmap.Size;
    VBuffer := FBitmap32StaticFactory.BufferFactory.BuildEmpty(VSize);
    if VBuffer <> nil then begin
      VIsChanged := False;
      VSourceLine := ABitmap.Data;
      VTargetLine := Result.Data;
      for i := 0 to VSize.X * VSize.Y - 1 do begin
        if VSourceLine[i] = FMaskColor then begin
          VTargetLine[i] := 0;
          VIsChanged := True;
        end else begin
          VTargetLine[i] := VSourceLine[i];
        end;
      end;
      if not VIsChanged then begin
        Result := ABitmap;
      end else begin
        Result := FBitmap32StaticFactory.BuildWithOwnBuffer(VBuffer);
      end;
    end;
  end;
end;

end.
