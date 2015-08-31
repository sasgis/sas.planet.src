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

unit u_ConfigProviderHelpers;

interface

uses
  t_Bitmap32,
  i_ContentTypeManager,
  i_Bitmap32Static,
  i_GeometryLonLat,
  i_GeometryLonLatFactory,
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

procedure WriteGUID(
  const AConfigProvider: IConfigDataWriteProvider;
  const AIdent: string;
  const AValue: TGUID
);
function ReadGUID(
  const AConfigProvider: IConfigDataProvider;
  const AIdent: string;
  const ADefault: TGUID
): TGUID;

function ReadBitmapByFileRef(
  const AConfigProvider: IConfigDataProvider;
  const AFullFileName: string;
  const AContentTypeManager: IContentTypeManager;
  const ADefault: IBitmap32Static
): IBitmap32Static;

function ReadPolygon(
  const AConfigProvider: IConfigDataProvider;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory
): IGeometryLonLatPolygon;

procedure WritePolygon(
  const AConfigProvider: IConfigDataWriteProvider;
  const APolygon: IGeometryLonLatPolygon
);

implementation

uses
  SysUtils,
  Graphics,
  Math,
  GR32,
  t_GeoTypes,
  i_BinaryData,
  i_DoublePointsAggregator,
  i_ContentTypeInfo,
  u_GeoFunc,
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
): IGeometryLonLatPolygon;
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
  VIsValidX: Boolean;
  VIsValidY: Boolean;
  VIsFinish: Boolean;
  VBuilder: IGeometryLonLatPolygonBuilder;
  VIsOuter: Boolean;
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
              Result := nil;
              Exit;
            end;
          end;
        end;
      end;
    end;
  end;

  VPointsAggregator := TDoublePointsAggregator.Create;
  VBuilder := AVectorGeometryLonLatFactory.MakePolygonBuilder;
  VIsOuter := True;
  VIsFinish := False;
  repeat
    VPoint.X := AConfigProvider.ReadFloat(VIdentLon + inttostr(i), -10000);
    VPoint.Y := AConfigProvider.ReadFloat(VIdentLat + inttostr(i), -10000);
    Inc(i);
    VIsValidX := not IsNan(VPoint.X);
    VIsValidY := not IsNan(VPoint.Y);
    if (VIsValidX and (Abs(VPoint.X) > 360)) or (VIsValidY and (Abs(VPoint.Y) > 360)) then begin
      VIsFinish := True;
    end;
    if not VIsFinish and VIsValidX and VIsValidY then begin
      VPointsAggregator.Add(VPoint);
    end else begin
      if VPointsAggregator.Count > 0 then begin
        if VIsOuter then begin
          VBuilder.AddOuter(VPointsAggregator.MakeStaticAndClear);
        end else begin
          VBuilder.AddHole(VPointsAggregator.MakeStaticAndClear);
        end;
        VIsOuter := not VIsValidY;
      end;
    end;
  until VIsFinish;

  Result := VBuilder.MakeStaticAndClear;
end;

procedure WriteContour(
  const AConfigProvider: IConfigDataWriteProvider;
  const AContour: IGeometryLonLatContour;
  var AStartIndex: Integer
);
var
  i: Integer;
  VPoint: TDoublePoint;
  VStrIndex: string;
begin
  Assert(Assigned(AContour));
  for i := 0 to AContour.Count - 1 do begin
    VPoint := AContour.Points[i];
    VStrIndex := IntToStr(AStartIndex + i);
    AConfigProvider.WriteFloat('PointLon_' + VStrIndex, VPoint.x);
    AConfigProvider.WriteFloat('PointLat_' + VStrIndex, VPoint.y);
  end;
  Inc(AStartIndex, AContour.Count);
end;

procedure WriteSinglePolygon(
  const AConfigProvider: IConfigDataWriteProvider;
  const APolygon: IGeometryLonLatSinglePolygon;
  var AStartIndex: Integer
);
var
  i: Integer;
  VContour: IGeometryLonLatContour;
  VStrIndex: string;
begin
  Assert(Assigned(APolygon));
  VContour := APolygon.OuterBorder;
  WriteContour(AConfigProvider, VContour, AStartIndex);

  for i := 0 to APolygon.HoleCount - 1 do begin
    VStrIndex := IntToStr(AStartIndex + i);
    AConfigProvider.WriteFloat('PointLon_' + VStrIndex, NaN);
    AConfigProvider.WriteFloat('PointLat_' + VStrIndex, -1);
    Inc(AStartIndex);

    VContour := APolygon.HoleBorder[i];
    WriteContour(AConfigProvider, VContour, AStartIndex);
  end;
end;

procedure WriteMultiPolygon(
  const AConfigProvider: IConfigDataWriteProvider;
  const APolygon: IGeometryLonLatMultiPolygon;
  var AStartIndex: Integer
);
var
  i: Integer;
  VPolygon: IGeometryLonLatSinglePolygon;
  VStrIndex: string;
begin
  Assert(Assigned(APolygon));
  Assert(APolygon.Count > 0);
  VPolygon := APolygon.Item[0];
  WriteSinglePolygon(AConfigProvider, VPolygon, AStartIndex);

  for i := 1 to APolygon.Count - 1 do begin
    VStrIndex := IntToStr(AStartIndex + i);
    AConfigProvider.WriteFloat('PointLon_' + VStrIndex, NaN);
    AConfigProvider.WriteFloat('PointLat_' + VStrIndex, NaN);
    Inc(AStartIndex);

    VPolygon := APolygon.Item[i];
    WriteSinglePolygon(AConfigProvider, VPolygon, AStartIndex);
  end;
end;


procedure WritePolygon(
  const AConfigProvider: IConfigDataWriteProvider;
  const APolygon: IGeometryLonLatPolygon
);
var
  i: Integer;
  VMultiPolygon: IGeometryLonLatMultiPolygon;
  VSinglePolygon: IGeometryLonLatSinglePolygon;
begin
  if Assigned(APolygon) then begin
    i := 0;
    if Supports(APolygon, IGeometryLonLatSinglePolygon, VSinglePolygon) then begin
      WriteSinglePolygon(AConfigProvider, VSinglePolygon, i);
    end else if Supports(APolygon, IGeometryLonLatMultiPolygon, VMultiPolygon) then begin
      WriteMultiPolygon(AConfigProvider, VMultiPolygon, i);
    end else begin
      Assert(False);
    end;
  end;
end;

procedure WriteGUID(
  const AConfigProvider: IConfigDataWriteProvider;
  const AIdent: string;
  const AValue: TGUID
);
begin
  AConfigProvider.WriteString(AIdent, GUIDToString(AValue));
end;

function ReadGUID(
  const AConfigProvider: IConfigDataProvider;
  const AIdent: string;
  const ADefault: TGUID
): TGUID;
var
  VGUIDStr: string;
begin
  Result := ADefault;
  if AConfigProvider <> nil then begin
    VGUIDStr := AConfigProvider.ReadString(AIdent, '');
    if VGUIDStr <> '' then begin
      try
        Result := StringToGUID(VGUIDStr);
      except
        Result := ADefault;
      end;
    end;
  end;
end;

end.
