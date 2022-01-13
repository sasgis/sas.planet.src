{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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

unit u_Bitmap32StaticBuilderFactory;

interface

uses
  Types,
  t_Bitmap32,
  i_Bitmap32BufferFactory,
  i_Bitmap32Static,
  i_Bitmap32Surface,
  u_BaseInterfacedObject;

type
  TBitmap32StaticBuilderFactory = class(TBaseInterfacedObject, IBitmap32StaticBuilderFactory)
  private
    FBufferFactory: IBitmap32BufferFactory;
    FBitmap32StaticFactory: IBitmap32StaticFactory;
  private
    function BuildEmpty(const ASize: TPoint): IBitmap32StaticBuilder;
    function BuildFillColor(
      const ASize: TPoint;
      const AFillColor: TColor32
    ): IBitmap32StaticBuilder;
    function BuildByData(
      const ASize: TPoint;
      const AData: PColor32Array
    ): IBitmap32StaticBuilder;
    function BuildByBitmap32Static(const ASource: IBitmap32Static): IBitmap32StaticBuilder;
  public
    constructor Create(
      const ABufferFactory: IBitmap32BufferFactory;
      const ABitmap32StaticFactory: IBitmap32StaticFactory
    );
  end;

implementation

uses
  u_Bitmap32StaticBuilderByGR32;

{ TBitmap32StaticBuilderFactory }

constructor TBitmap32StaticBuilderFactory.Create(
  const ABufferFactory: IBitmap32BufferFactory;
  const ABitmap32StaticFactory: IBitmap32StaticFactory
);
begin
  Assert(Assigned(ABufferFactory));
  Assert(Assigned(ABitmap32StaticFactory));
  inherited Create;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FBufferFactory := ABufferFactory;
end;

function TBitmap32StaticBuilderFactory.BuildByBitmap32Static(
  const ASource: IBitmap32Static
): IBitmap32StaticBuilder;
begin
  Result :=
    TBitmap32StaticBuilderByGR32.CreateBySource(
      FBufferFactory,
      FBitmap32StaticFactory,
      ASource
    );
end;

function TBitmap32StaticBuilderFactory.BuildByData(
  const ASize: TPoint;
  const AData: PColor32Array
): IBitmap32StaticBuilder;
begin
  Result :=
    TBitmap32StaticBuilderByGR32.CreateByData(
      FBufferFactory,
      FBitmap32StaticFactory,
      ASize,
      AData
    );
end;

function TBitmap32StaticBuilderFactory.BuildEmpty(
  const ASize: TPoint
): IBitmap32StaticBuilder;
begin
  Result :=
    TBitmap32StaticBuilderByGR32.CreateEmpty(
      FBufferFactory,
      FBitmap32StaticFactory,
      ASize,
      0
    );
end;

function TBitmap32StaticBuilderFactory.BuildFillColor(
  const ASize: TPoint;
  const AFillColor: TColor32
): IBitmap32StaticBuilder;
begin
  Result :=
    TBitmap32StaticBuilderByGR32.CreateEmpty(
      FBufferFactory,
      FBitmap32StaticFactory,
      ASize,
      AFillColor
    );
end;

end.
