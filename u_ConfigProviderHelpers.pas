{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_ConfigProviderHelpers;

interface

uses
  GR32,
  i_ContentTypeManager,
  i_Bitmap32Static,
  i_GeometryLonLat,
  i_VectorItemsFactory,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider;

procedure WriteColor32(
  const AConfigProvider: IConfigDataWriteProvider;
  const AIdent: string;
  AValue: TColor32
);
function ReadColor32(
  const AConfigProvider: IConfigDataProvider;
  const AIdent: string;
  ADefault: TColor32
): TColor32;

function ReadBitmapByFileRef(
  const AConfigProvider: IConfigDataProvider;
  const AFullFileName: string;
  const AContentTypeManager: IContentTypeManager;
  const ADefault: IBitmap32Static
): IBitmap32Static;

function ReadPolygon(
  const AConfigProvider: IConfigDataProvider;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory
): IGeometryLonLatMultiPolygon;

procedure WritePolygon(
  const AConfigProvider: IConfigDataWriteProvider;
  const APolygon: IGeometryLonLatMultiPolygon
);

implementation

uses
  SysUtils,
  Graphics,
  t_GeoTypes,
  i_BinaryData,
  i_EnumDoublePoint,
  i_DoublePointsAggregator,
  i_ContentTypeInfo,
  u_GeoFun,
  u_DoublePointsAggregator;

function ReadColor32(
  const AConfigProvider: IConfigDataProvider;
  const AIdent: string;
  ADefault: TColor32
): TColor32;
var
  VColor: TColor;
  VAlfa: Integer;
  VHexString: string;
  VIntColor: Integer;
begin
  Result := ADefault;
  if AConfigProvider <> nil then begin
    VHexString := AConfigProvider.ReadString(AIdent + 'Hex', '');
    if VHexString = '' then begin
      VAlfa := AlphaComponent(Result);
      VColor := WinColor(Result);
      VAlfa := AConfigProvider.ReadInteger(AIdent + 'Alfa', VAlfa);
      VColor := AConfigProvider.ReadInteger(AIdent, VColor);
      Result := SetAlpha(Color32(VColor), VAlfa);
    end else begin
      if TryStrToInt(VHexString, VIntColor) then begin
        Result := VIntColor;
      end;
    end;
  end;
end;

procedure WriteColor32(
  const AConfigProvider: IConfigDataWriteProvider;
  const AIdent: string;
  AValue: TColor32
);
begin
  AConfigProvider.WriteString(AIdent + 'Hex', HexDisplayPrefix + IntToHex(AValue, 8));
end;

function ReadBitmapByFileRef(
  const AConfigProvider: IConfigDataProvider;
  const AFullFileName: string;
  const AContentTypeManager: IContentTypeManager;
  const ADefault: IBitmap32Static
): IBitmap32Static;
var
  VFilePath: string;
  VFileName: string;
  VFileExt: string;
  VResourceProvider: IConfigDataProvider;
  VInfoBasic: IContentTypeInfoBasic;
  VBitmapContntType: IContentTypeInfoBitmap;
  VData: IBinaryData;
begin
  Result := ADefault;
  VFilePath := ExcludeTrailingPathDelimiter(ExtractFilePath(AFullFileName));
  VFileName := ExtractFileName(AFullFileName);
  VFileExt := ExtractFileExt(VFileName);

  if VFilePath = '' then begin
    VResourceProvider := AConfigProvider;
  end else begin
    try
      VResourceProvider := AConfigProvider.GetSubItem(VFilePath);
    except
      Assert(False, 'Ошибка при получении пути ' + VFilePath);
    end;
  end;

  if VResourceProvider <> nil then begin
    VData := VResourceProvider.ReadBinary(VFileName);
    if VData <> nil then begin
      VInfoBasic := AContentTypeManager.GetInfoByExt(VFileExt);
      if VInfoBasic <> nil then begin
        if Supports(VInfoBasic, IContentTypeInfoBitmap, VBitmapContntType) then begin
          try
            Result := VBitmapContntType.GetLoader.Load(VData);
          except
            Assert(False, 'Ошибка при загрузке картинки ' + AFullFileName);
          end;
        end;
      end;
    end;
  end;
end;

function ReadPolygon(
  const AConfigProvider: IConfigDataProvider;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory
): IGeometryLonLatMultiPolygon;
  function CheckIsValidPoint(
    const AConfigProvider: IConfigDataProvider;
    const AIdentLon: string;
    const AIdentLat: string;
    const AIndex: Integer
  ): Boolean;
  var
    VPoint: TDoublePoint;
  begin
    VPoint.X := AConfigProvider.ReadFloat(AIdentLon + inttostr(AIndex), -10000);
    VPoint.Y := AConfigProvider.ReadFloat(AIdentLat + inttostr(AIndex), -10000);

    Result := not PointIsEmpty(VPoint) and ((Abs(VPoint.X) < 360) and (Abs(VPoint.Y) < 360));
  end;
var
  i: Integer;
  VPoint: TDoublePoint;
  VPointsAggregator: IDoublePointsAggregator;
  VIdentLon: string;
  VIdentLat: string;
  VValidPoint: Boolean;
begin
  VIdentLon := 'PointLon_';
  VIdentLat := 'PointLat_';
  i := 0;
  if not CheckIsValidPoint(AConfigProvider, VIdentLon, VIdentLat, i) then begin
    i := 1;
    if not CheckIsValidPoint(AConfigProvider, VIdentLon, VIdentLat, i) then begin
      i := 0;
      VIdentLon := 'LLPointX_';
      VIdentLat := 'LLPointY_';
      if not CheckIsValidPoint(AConfigProvider, VIdentLon, VIdentLat, i) then begin
        i := 1;
        if not CheckIsValidPoint(AConfigProvider, VIdentLon, VIdentLat, i) then begin
          i := 0;
          VIdentLon := 'PointX_';
          VIdentLat := 'PointY_';
          if not CheckIsValidPoint(AConfigProvider, VIdentLon, VIdentLat, i) then begin
            i := 1;
            if not CheckIsValidPoint(AConfigProvider, VIdentLon, VIdentLat, i) then begin
              Result := AVectorGeometryLonLatFactory.CreateLonLatPolygon(nil, 0);
              Exit;
            end;
          end;
        end;
      end;
    end;
  end;

  VPointsAggregator := TDoublePointsAggregator.Create;
  repeat
    VPoint.X := AConfigProvider.ReadFloat(VIdentLon + inttostr(i), -10000);
    VPoint.Y := AConfigProvider.ReadFloat(VIdentLat + inttostr(i), -10000);
    VValidPoint := PointIsEmpty(VPoint) or ((Abs(VPoint.X) < 360) and (Abs(VPoint.Y) < 360));
    if VValidPoint then begin
      VPointsAggregator.Add(VPoint);
      Inc(i);
    end;
  until not VValidPoint;
  Result := AVectorGeometryLonLatFactory.CreateLonLatPolygon(VPointsAggregator.Points, VPointsAggregator.Count);
end;

procedure WritePolygon(
  const AConfigProvider: IConfigDataWriteProvider;
  const APolygon: IGeometryLonLatMultiPolygon
);
var
  VEnum: IEnumDoublePoint;
  i: Integer;
  VPoint: TDoublePoint;
begin
  if APolygon <> nil then begin
    VEnum := APolygon.GetEnum;
    i := 1;
    while VEnum.Next(VPoint) do begin
      AConfigProvider.WriteFloat('PointLon_' + IntToStr(i), VPoint.x);
      AConfigProvider.WriteFloat('PointLat_' + IntToStr(i), VPoint.y);
      Inc(i);
    end;
  end;
end;

end.

