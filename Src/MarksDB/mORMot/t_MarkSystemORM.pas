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

unit t_MarkSystemORM;

interface

uses
  SysUtils,
  mormot.core.base,
  i_GeometryLonLat,
  u_MarkSystemORMModel;

type
  EMarkSystemORMError = class(Exception);

  TID = mormot.core.base.TID;
  TIDDynArray = mormot.core.base.TIDDynArray;

  TOrmCategoryRec = record
    FCategoryId: TID;
    FName: string;
    FViewId: TID;
    FVisible: Boolean;
    FMinZoom: Byte;
    FMaxZoom: Byte;
  end;
  POrmCategoryRec = ^TOrmCategoryRec;
  TOrmCategoryRecDynArray = array of TOrmCategoryRec;

  TOrmMarkRec = record
    FMarkId: TID;
    FCategoryId: TID;
    FPicId: TID;
    FPicName: string;
    FAppearanceId: TID;
    FColor1: Cardinal;
    FColor2: Cardinal;
    FScale1: Integer;
    FScale2: Integer;
    FName: string;
    FDesc: string;
    FViewId: TID;
    FVisible: Boolean;
    FGeoLonSize: Cardinal;
    FGeoLatSize: Cardinal;
    FGeoType: TOrmGeoType;
    FGeoCount: Integer;
    FGeometry: IGeometryLonLat;
  end;
  POrmMarkRec = ^TOrmMarkRec;
  TOrmMarkRecDynArray = array of TOrmMarkRec;

const
  cEmptyID = 0;

  cEmptyOrmCategoryRec: TOrmCategoryRec = (
    FCategoryId : 0;
    FName       : '';
    FViewId     : 0;
    FVisible    : True;
    FMinZoom    : 3;
    FMaxZoom    : 23;
  );

  cEmptyOrmMarkRec: TOrmMarkRec = (
    FMarkId     : 0;
    FCategoryId : 0;
    FPicId      : 0;
    FPicName    : '';
    FAppearanceId : 0;
    FColor1     : 0;
    FColor2     : 0;
    FScale1     : 0;
    FScale2     : 0;
    FName       : '';
    FDesc       : '';
    FViewId     : 0;
    FVisible    : True;
    FGeoLonSize : 0;
    FGeoLatSize : 0;
    FGeoType    : gtUndef;
    FGeoCount   : 0;
    FGeometry   : nil;
  );

implementation

end.
