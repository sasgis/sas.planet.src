{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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

unit u_AppearanceOfMarkFactory;

interface

uses
  t_Bitmap32,
  t_Hash,
  i_MarkPicture,
  i_HashFunction,
  i_Appearance,
  i_AppearanceOfMarkFactory,
  i_HashInterfaceCache,
  u_BaseInterfacedObject;

type
  TAppearanceOfMarkFactory = class(TBaseInterfacedObject, IAppearanceOfMarkFactory)
  private
    FHashFunction: IHashFunction;
    FCache: IHashInterfaceCache;
  private
    function CreateByKey(
      const AKey: THashValue;
      const AData: Pointer
    ): IInterface;
  private
    function CreatePointAppearance(
      const ATextColor: TColor32;
      const ATextBgColor: TColor32;
      const AFontSize: Integer;
      const APicName: string;
      const APic: IMarkPicture;
      const AMarkerSize: Integer
    ): IAppearance;
    function CreateLineAppearance(
      const ALineColor: TColor32;
      const ALineWidth: Integer
    ): IAppearance;
    function CreatePolygonAppearance(
      const ALineColor: TColor32;
      const ALineWidth: Integer;
      const AFillColor: TColor32
    ): IAppearance;
  public
    constructor Create(
      const AHashFunction: IHashFunction
    );
  end;

implementation

uses
  u_HashInterfaceCache2Q,
  u_AppearanceOfMarkPoint,
  u_AppearanceOfMarkLine,
  u_AppearanceOfMarkPolygon;

type
  PDataRecord = ^TDataRecord;
  TDataRecord = record
    Hash1: THashValue;
    Hash2: THashValue;
    Color1: TColor32;
    Color2: TColor32;
    Size1: Integer;
    Size2: Integer;
    Pic: IMarkPicture;
    PicName: string;
    MarkType: (mtPoint, mtPath, mtPolygon);
  end;

{ TAppearanceOfMarkFactory }

constructor TAppearanceOfMarkFactory.Create(const AHashFunction: IHashFunction);
begin
  inherited Create;
  FHashFunction := AHashFunction;
  FCache :=
    THashInterfaceCache2Q.Create(
      Self.CreateByKey,
      10,  // 2^10 elements in hash-table
      0,   // LRU 256 elements
      256,
      0
    );
end;

function TAppearanceOfMarkFactory.CreateByKey(
  const AKey: THashValue;
  const AData: Pointer
): IInterface;
var
  VData: PDataRecord;
  VResult: IAppearance;
begin
  inherited;
  VResult := nil;
  VData := PDataRecord(AData);
  case VData.MarkType of
    mtPoint: begin
      VResult :=
        TAppearanceOfMarkPoint.Create(
          AKey,
          VData^.Hash1,
          VData^.Hash2,
          VData^.Color1,
          VData^.Color2,
          VData^.Size1,
          VData^.Size2,
          VData^.PicName,
          VData^.Pic
        );
    end;
    mtPath: begin
      VResult :=
        TAppearanceOfMarkLine.Create(
          AKey,
          VData^.Color1,
          VData^.Size1
        );
    end;
    mtPolygon: begin
      VResult :=
        TAppearanceOfMarkPolygon.Create(
          AKey,
          VData^.Hash1,
          VData^.Hash2,
          VData^.Color1,
          VData^.Size1,
          VData^.Color2
        );
    end;
  end;

  Result := VResult;
end;

function TAppearanceOfMarkFactory.CreateLineAppearance(
  const ALineColor: TColor32;
  const ALineWidth: Integer
): IAppearance;
var
  VHash: THashValue;
  VData: TDataRecord;
begin
  VHash := $443e31d70873bb6b;
  FHashFunction.UpdateHashByInteger(VHash, ALineColor);
  FHashFunction.UpdateHashByInteger(VHash, ALineWidth);
  VData.MarkType := mtPath;
  VData.Color1 := ALineColor;
  VData.Size1 := ALineWidth;

  Result := IAppearance(FCache.GetOrCreateItem(VHash, @VData));
end;

function TAppearanceOfMarkFactory.CreatePointAppearance(
  const ATextColor, ATextBgColor: TColor32;
  const AFontSize: Integer;
  const APicName: string;
  const APic: IMarkPicture;
  const AMarkerSize: Integer
): IAppearance;
var
  VHash: THashValue;
  VData: TDataRecord;
begin
  VData.Hash1 := $df2118b946ed0b43;
  FHashFunction.UpdateHashByInteger(VData.Hash1, ATextColor);
  FHashFunction.UpdateHashByInteger(VData.Hash1, ATextBgColor);
  FHashFunction.UpdateHashByInteger(VData.Hash1, AFontSize);

  if Assigned(APic) then begin
    VData.Hash2 := $7498e432f9619b27;
    FHashFunction.UpdateHashByHash(VData.Hash2, APic.Hash);
  end else begin
    VData.Hash2 := $31bfcd0d9f48d1d3;
    FHashFunction.UpdateHashByString(VData.Hash2, APicName);
  end;
  FHashFunction.UpdateHashByInteger(VData.Hash2, AMarkerSize);

  VHash := $40fd28c43506c95d;
  FHashFunction.UpdateHashByHash(VHash, VData.Hash1);
  FHashFunction.UpdateHashByHash(VHash, VData.Hash2);

  VData.MarkType := mtPoint;
  VData.Color1 := ATextColor;
  VData.Color2 := ATextBgColor;
  VData.Size1 := AFontSize;
  VData.Size2 := AMarkerSize;
  VData.Pic := APic;
  VData.PicName := APicName;
  Result := IAppearance(FCache.GetOrCreateItem(VHash, @VData));
end;

function TAppearanceOfMarkFactory.CreatePolygonAppearance(
  const ALineColor: TColor32;
  const ALineWidth: Integer;
  const AFillColor: TColor32
): IAppearance;
var
  VHash: THashValue;
  VData: TDataRecord;
begin
  VData.Hash1 := $8184bab36bb79df0;
  FHashFunction.UpdateHashByInteger(VData.Hash1, ALineColor);
  FHashFunction.UpdateHashByInteger(VData.Hash1, ALineWidth);

  VData.Hash2 := $11b87fb1b900cc39;
  FHashFunction.UpdateHashByInteger(VData.Hash2, AFillColor);

  VHash := $501f3e9b18861e44;
  FHashFunction.UpdateHashByHash(VHash, VData.Hash1);
  FHashFunction.UpdateHashByHash(VHash, VData.Hash2);

  VData.MarkType := mtPolygon;
  VData.Color1 := ALineColor;
  VData.Color2 := AFillColor;
  VData.Size1 := ALineWidth;
  Result := IAppearance(FCache.GetOrCreateItem(VHash, @VData));
end;

end.
