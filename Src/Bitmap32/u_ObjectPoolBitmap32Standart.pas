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

unit u_ObjectPoolBitmap32Standart;

interface

uses
  Types,
  i_Bitmap32Static,
  u_ObjectFromPoolAbstract,
  u_ObjectPoolBase;

type
  IObjectPoolBitmap32Standart = interface
    ['{31803812-FF36-4275-A4F7-B2E8CD136A30}']
    function Build: IBitmap32Buffer;
    function GetSize: TPoint;
    property Size: TPoint read GetSize;
  end;

  TObjectPoolBitmap32Standart = class(TObjectPoolBase, IObjectPoolBitmap32Standart)
  private
    function Build: IBitmap32Buffer;
    function GetSize: TPoint;
  protected
    function BuildNewObject(const AFreeProcedure: IFreeObjectProcedure): TObjectFromPoolAbstract; override;
  end;

implementation

uses
  t_Bitmap32,
  u_ObjectFromPoolBase;

const
  CStandartSize = 256;

type
  TBitmap32StaticStandartSize = class(TObjectFromPoolBase, IBitmap32Buffer)
  private
    FBits: array [0..(CStandartSize * CStandartSize - 1)] of TColor32;
  private
    function GetSize: TPoint;
    function GetData: PColor32Array;
  end;

{ TBitmap32StaticStandartSize }

function TBitmap32StaticStandartSize.GetData: PColor32Array;
begin
  Result := PColor32Array(@FBits[0]);
end;

function TBitmap32StaticStandartSize.GetSize: TPoint;
begin
  Result := Types.Point(CStandartSize, CStandartSize);
end;

{ TObjectPoolBitmap32Standart }

function TObjectPoolBitmap32Standart.Build: IBitmap32Buffer;
var
  VObject: TObjectFromPoolAbstract;
begin
  VObject := PullOrCreateObject;
  Result := VObject as IBitmap32Buffer;
end;

function TObjectPoolBitmap32Standart.BuildNewObject(
  const AFreeProcedure: IFreeObjectProcedure): TObjectFromPoolAbstract;
begin
  Result := TBitmap32StaticStandartSize.Create(AFreeProcedure);
end;

function TObjectPoolBitmap32Standart.GetSize: TPoint;
begin
  Result := Types.Point(CStandartSize, CStandartSize);
end;

end.
