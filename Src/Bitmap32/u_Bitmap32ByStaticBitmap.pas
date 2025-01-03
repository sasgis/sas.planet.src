{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_Bitmap32ByStaticBitmap;

interface

uses
  GR32,
  i_Bitmap32Static,
  i_Bitmap32BufferFactory;

type
  TStaticBitmapBackend = class(TCustomBackend)
  private
    FBitmapStatic: IBitmap32Buffer;
  protected
    procedure InitializeSurface(
      NewWidth, NewHeight: Integer;
      ClearBuffer: Boolean
    ); override;
    procedure FinalizeSurface; override;
  public
    constructor Create(
      Owner: TCustomBitmap32
    ); override;
  end;

  TBitmap32ByStaticBitmap = class(TCustomBitmap32)
  private
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FBackEndByStatic: TStaticBitmapBackend;
  protected
    procedure SetBackend(const Backend: TCustomBackend); override;
  public
    function MakeAndClear: IBitmap32Static;
  public
    constructor Create(
      const ABitmap32StaticFactory: IBitmap32StaticFactory
    ); reintroduce;
  end;

implementation

uses
  Types;

{ TStaticBitmapBackend }

constructor TStaticBitmapBackend.Create(
  Owner: TCustomBitmap32
);
begin
  Assert(Owner is TBitmap32ByStaticBitmap);
  inherited Create(Owner);
end;

procedure TStaticBitmapBackend.FinalizeSurface;
begin
  inherited;
  FBitmapStatic := nil;
  FBits := nil;
end;

procedure TStaticBitmapBackend.InitializeSurface(
  NewWidth, NewHeight: Integer;
  ClearBuffer: Boolean
);
var
  VBufferFactory: IBitmap32BufferFactory;
begin
  VBufferFactory := TBitmap32ByStaticBitmap(FOwner).FBitmap32StaticFactory.BufferFactory;
  if ClearBuffer then begin
    FBitmapStatic := VBufferFactory.BuildEmptyClear(Types.Point(NewWidth, NewHeight), 0);
  end else begin
    FBitmapStatic := VBufferFactory.BuildEmpty(Types.Point(NewWidth, NewHeight));
  end;
  if FBitmapStatic <> nil then begin
    FBits := FBitmapStatic.Data;
  end else begin
    FBits := nil;
  end;
end;

{ TBitmap32ByStaticBitmap }

constructor TBitmap32ByStaticBitmap.Create(
  const ABitmap32StaticFactory: IBitmap32StaticFactory
);
begin
  Assert(Assigned(ABitmap32StaticFactory));
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  inherited Create(TStaticBitmapBackend);
end;

function TBitmap32ByStaticBitmap.MakeAndClear: IBitmap32Static;
begin
  Result := FBitmap32StaticFactory.BuildWithOwnBuffer(FBackEndByStatic.FBitmapStatic);
  Delete;
end;

procedure TBitmap32ByStaticBitmap.SetBackend(const Backend: TCustomBackend);
begin
  Assert(Backend is TStaticBitmapBackend);
  if Backend is TStaticBitmapBackend then begin
    FBackEndByStatic := TStaticBitmapBackend(Backend);
    inherited;
  end else begin
    Assert(False, 'Bad backend for this bitmap class');
  end;
end;

end.
