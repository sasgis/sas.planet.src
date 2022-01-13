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

unit u_Bitmap32Static;

interface

uses
  Types,
  t_Bitmap32,
  t_Hash,
  i_Bitmap32Static,
  u_BaseInterfacedObject;

type
  TBitmap32Static = class(TBaseInterfacedObject, IBitmap32Static)
  private
    FBuffer: IBitmap32Buffer;
    FData: PColor32Array;
    FSize: TPoint;
    FHash: THashValue;
  private
    function GetSize: TPoint;
    function GetData: PColor32Array;
    function GetHash: THashValue;
  public
    constructor Create(
      const AHash: THashValue;
      const ABuffer: IBitmap32Buffer
    );
  end;

implementation

{ TBitmap32Static }

constructor TBitmap32Static.Create(
  const AHash: THashValue;
  const ABuffer: IBitmap32Buffer
);
begin
  Assert(Assigned(ABuffer));
  inherited Create;
  FBuffer := ABuffer;
  FHash := AHash;
  FData := FBuffer.Data;
  FSize := FBuffer.Size;
end;

function TBitmap32Static.GetData: PColor32Array;
begin
  Result := FData;
end;

function TBitmap32Static.GetHash: THashValue;
begin
  Result := FHash;
end;

function TBitmap32Static.GetSize: TPoint;
begin
  Result := FSize;
end;

end.
