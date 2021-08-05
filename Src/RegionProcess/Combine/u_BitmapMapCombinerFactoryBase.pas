{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2021, SAS.Planet development team.                      *}
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

unit u_BitmapMapCombinerFactoryBase;

interface

uses
  Types,
  t_CommonTypes,
  t_MapCombineOptions,
  i_GeometryLonLat,
  i_BitmapMapCombiner,
  i_RegionProcessParamsFrame,
  u_BaseInterfacedObject;

type
  TBitmapMapCombinerFactoryBase = class(TBaseInterfacedObject, IBitmapMapCombinerFactory)
  private
    FMinPartSize: TPoint;
    FMaxPartSize: TPoint;
    FCombinePathStringTypeSupport: TStringTypeSupport;
    FDefaultExt: string;
    FFormatName: string;
    FOptionsSet: TMapCombineOptionsSet;
  private
    function GetMinPartSize: TPoint;
    function GetMaxPartSize: TPoint;
    function GetCombinePathStringTypeSupport: TStringTypeSupport;
    function GetDefaultExt: string;
    function GetFormatName: string;
    function GetOptionsSet: TMapCombineOptionsSet;
  protected
    function Validate(
      const AParams: IRegionProcessParamsFrameMapCombine;
      const APolygon: IGeometryLonLatPolygon
    ): Boolean; virtual;
    function PrepareMapCombiner(
      const AParams: IRegionProcessParamsFrameMapCombine;
      const AProgressInfo: IBitmapCombineProgressUpdate
    ): IBitmapMapCombiner; virtual; abstract;
  public
    constructor Create(
      const AMinPartSize: TPoint;
      const AMaxPartSize: TPoint;
      const ACombinePathStringTypeSupport: TStringTypeSupport;
      const ADefaultExt: string;
      const AFormatName: string;
      const AOptionsSet: TMapCombineOptionsSet = []
    );
  end;

implementation

{ TBitmapMapCombinerFactoryBase }

constructor TBitmapMapCombinerFactoryBase.Create(
  const AMinPartSize, AMaxPartSize: TPoint;
  const ACombinePathStringTypeSupport: TStringTypeSupport;
  const ADefaultExt, AFormatName: string;
  const AOptionsSet: TMapCombineOptionsSet
);
begin
  Assert(AMinPartSize.X <= AMaxPartSize.X);
  Assert(AMinPartSize.Y <= AMaxPartSize.Y);
  inherited Create;
  FMinPartSize := AMinPartSize;
  FMaxPartSize := AMaxPartSize;
  FCombinePathStringTypeSupport := ACombinePathStringTypeSupport;
  FDefaultExt := ADefaultExt;
  FFormatName := AFormatName;
  FOptionsSet := AOptionsSet;
end;

function TBitmapMapCombinerFactoryBase.GetCombinePathStringTypeSupport: TStringTypeSupport;
begin
  Result := FCombinePathStringTypeSupport;
end;

function TBitmapMapCombinerFactoryBase.GetDefaultExt: string;
begin
  Result := FDefaultExt;
end;

function TBitmapMapCombinerFactoryBase.GetFormatName: string;
begin
  Result := FFormatName;
end;

function TBitmapMapCombinerFactoryBase.GetMaxPartSize: TPoint;
begin
  Result := FMaxPartSize;
end;

function TBitmapMapCombinerFactoryBase.GetMinPartSize: TPoint;
begin
  Result := FMinPartSize;
end;

function TBitmapMapCombinerFactoryBase.GetOptionsSet: TMapCombineOptionsSet;
begin
  Result := FOptionsSet;
end;

function TBitmapMapCombinerFactoryBase.Validate(
  const AParams: IRegionProcessParamsFrameMapCombine;
  const APolygon: IGeometryLonLatPolygon
): Boolean;
begin
  Result := True;
end;

end.
