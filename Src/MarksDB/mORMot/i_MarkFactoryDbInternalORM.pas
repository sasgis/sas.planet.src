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

unit i_MarkFactoryDbInternalORM;

interface

uses
  t_Bitmap32,
  t_MarkSystemORM,
  i_MarkId,
  i_GeometryLonLat,
  i_VectorDataItemSimple;

type
  IMarkFactoryDbInternalORM = interface
    ['{B346F0BF-7523-493B-83C6-0CA8640F8CC9}']
    function CreateMark(
      const AID: TID;
      const AName: string;
      const AVisible: Boolean;
      const APicName: string;
      const ACategoryId: TID;
      const ADesc: string;
      const AGeometry: IGeometryLonLat;
      const AColor1: TColor32;
      const AColor2: TColor32;
      const AScale1: Integer;
      const AScale2: Integer
    ): IVectorDataItem; overload;

    function CreateMark(
      const AMarkRec: TSQLMarkRec
    ): IVectorDataItem; overload;

    function CreateMarkId(
      const AMarkRec: TSQLMarkRec
    ): IMarkId;

    function CreateInternalMark(
      const AMark: IVectorDataItem
    ): IVectorDataItem;
  end;

implementation

end.
