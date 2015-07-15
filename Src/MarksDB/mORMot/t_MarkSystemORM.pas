{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit t_MarkSystemORM;

interface

uses
  SysUtils,
  mORMot,
  i_GeometryLonLat,
  u_MarkSystemORMModel;

type
  EMarkSystemORMError = class(Exception);

  TID = mORMot.TID;
  TIDDynArray = mORMot.TIDDynArray;

  TSQLCategoryRec = record
    FCategoryId: TID;
    FName: string;
    FViewId: TID;
    FVisible: Boolean;
    FMinZoom: Byte;
    FMaxZoom: Byte;
  end;
  PSQLCategoryRec = ^TSQLCategoryRec;

  TSQLMarkRec = record
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
    FGeoType: TSQLGeoType;
    FGeoCount: Integer;
    FGeometry: IGeometryLonLat;
  end;
  PSQLMarkRec = ^TSQLMarkRec;
  TSQLMarkRecDynArray = array of TSQLMarkRec;

  TTransactionRec = record
    FSessionID: Cardinal;
    FIsInternal: Boolean;
  end;

const
  cEmptyID = 0;

  cEmptySQLCategoryRec: TSQLCategoryRec = (
    FCategoryId : 0;
    FName       : '';
    FViewId     : 0;
    FVisible    : True;
    FMinZoom    : 3;
    FMaxZoom    : 23;
  );

  cEmptySQLMarkRec: TSQLMarkRec = (
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
