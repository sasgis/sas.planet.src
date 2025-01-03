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

unit u_CoordTransformer;

interface

uses
  Proj4.GaussKruger,
  t_CoordRepresentation;

type
  TCoordTransformer = class
  private
    FSK42: TGaussKruger;
    FGSK2011: TGaussKruger;
    FItems: array [TCoordSysType] of TGaussKruger;
  public
    function GetItem(const AType: TCoordSysType): TGaussKruger; inline;
    property Item[const AType: TCoordSysType]: TGaussKruger read GetItem; default;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TCoordTransformer }

constructor TCoordTransformer.Create;
begin
  inherited Create;

  FSK42 := TGaussKrugerFactory.BuildSK42;
  FGSK2011 := TGaussKrugerFactory.BuildGSK2011;

  FItems[cstWGS84] := nil; // no any transformations needed

  FItems[cstUTM] := nil; // todo
  FItems[cstMGRS] := nil; // todo

  FItems[cstSK42] := FSK42;
  FItems[cstSK42GK] := FSK42;

  FItems[cstGSK2011] := FGSK2011;
  FItems[cstGSK2011GK] := FGSK2011;
end;

destructor TCoordTransformer.Destroy;
begin
  FreeAndNil(FSK42);
  FreeAndNil(FGSK2011);

  inherited Destroy;
end;

function TCoordTransformer.GetItem(const AType: TCoordSysType): TGaussKruger;
begin
  Result := FItems[AType];

  Assert(Result <> nil);
end;

end.
