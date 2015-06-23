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
  t_MarkSystemModelORM,
  i_GeometryLonLat;

type
  EMarkSystemORMError = class(Exception);

  TSQLMarkRecOption = (mrAll, mrGeometry, mrView, mrPic);
  TSQLMarkRecOptions = set of TSQLMarkRecOption;

  TID = mORMot.TID;

  TSQLMarkRec = record
    FMarkId: TID;
    FCategoryId: TID;
    FPicId: TID;
    FPicName: string;
    FColor1: Cardinal;
    FColor2: Cardinal;
    FScale1: Integer;
    FScale2: Integer;
    FName: string;
    FDesc: string;
    FVisible: Boolean;
    FGeoLon: Double;
    FGeoLat: Double;
    FGeoType: TSQLGeoType;
    FGeoCount: Integer;
    FGeometry: IGeometryLonLat;
  end;

  TTransactionRec = record
    FSessionID: Cardinal;
    FIsInternal: Boolean;
  end;

const
  cEmptyTID = 0;

  cEmptySQLMarkRec: TSQLMarkRec = (
    FMarkId     : 0;
    FCategoryId : 0;
    FPicId      : 0;
    FPicName    : '';
    FColor1     : 0;
    FColor2     : 0;
    FScale1     : 0;
    FScale2     : 0;
    FName       : '';
    FDesc       : '';
    FVisible    : True; // !
    FGeoLon     : 0;
    FGeoLat     : 0;
    FGeoType    : gtUndef;
    FGeoCount   : 0;
    FGeometry   : nil;
  );

implementation

end.
