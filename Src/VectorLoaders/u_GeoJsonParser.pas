{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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

unit u_GeoJsonParser;

interface

uses
  SysUtils,
  superobject,
  t_GeoTypes,
  i_BinaryData,
  i_VectorDataLoader,
  i_GeometryLonLat,
  i_GeometryLonLatFactory,
  i_VectorItemSubset,
  i_VectorDataFactory,
  i_VectorItemSubsetBuilder,
  i_ProjConverter,
  u_BaseInterfacedObject;

type
  PGeoJsonCoordConverter = ^TGeoJsonCoordConverter;
  TGeoJsonCoordConverter = record
  private
    FConverter: IProjConverter;
    FParent: PGeoJsonCoordConverter;
  public
    procedure Init(
      const AProjConverterFactory: IProjConverterFactory;
      const AGeoJsonCRS: ISuperObject;
      const AParent: PGeoJsonCoordConverter
    );
    procedure ToWgs84(var APoint: TDoublePoint); {$IFNDEF DEBUG} inline; {$ENDIF}
    function GetPtr: PGeoJsonCoordConverter;
  end;

  TGeoJsonParser = class(TBaseInterfacedObject, IVectorDataLoader)
  private
    FVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FVectorDataFactory: IVectorDataFactory;
    FVectorGeometryLonLatFactory: IGeometryLonLatFactory;
    FProjConverterFactory: IProjConverterFactory;

    procedure ParseFeatureCollection(
      const AContext: TVectorLoadContext;
      const AJson: ISuperObject;
      const AList: IVectorItemSubsetBuilder
    ); inline;

    procedure ParseFeature(
      const AContext: TVectorLoadContext;
      const AFeature: ISuperObject;
      const AList: IVectorItemSubsetBuilder;
      const ACoordConverter: PGeoJsonCoordConverter
    );

    procedure ParseGeometry(
      const AGeometry: ISuperObject;
      const ACoordConverter: PGeoJsonCoordConverter;
      var AResult: TArrayOfIGeometryLonLat
    );
  private
    { IVectorDataLoader }
    function Load(
      const AContext: TVectorLoadContext;
      const AData: IBinaryData
    ): IVectorItemSubset;
  public
    constructor Create(
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AVectorDataFactory: IVectorDataFactory;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AProjConverterFactory: IProjConverterFactory
    );
  end;

  EGeoJsonParser = class(Exception);

implementation

uses
  Types,
  Math,
  StrUtils,
  i_DoublePointsAggregator,
  i_VectorDataItemSimple,
  u_StrFunc,
  u_DoublePointsAggregator;

{ TGeoJsonParser }

constructor TGeoJsonParser.Create(
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AVectorDataFactory: IVectorDataFactory;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AProjConverterFactory: IProjConverterFactory
);
begin
  inherited Create;
  FVectorItemSubsetBuilderFactory := AVectorItemSubsetBuilderFactory;
  FVectorDataFactory := AVectorDataFactory;
  FVectorGeometryLonLatFactory := AVectorGeometryLonLatFactory;
  FProjConverterFactory := AProjConverterFactory;
end;

function TGeoJsonParser.Load(
  const AContext: TVectorLoadContext;
  const AData: IBinaryData
): IVectorItemSubset;
var
  VStr: string;
  VType: string;
  VJson: ISuperObject;
  VSubsetBuilder: IVectorItemSubsetBuilder;
begin
  Result := nil;

  if (AData = nil) or (AData.Buffer = nil) or (AData.Size <= 0) then begin
    Exit;
  end;

  VStr := Utf8DataToUnicodeString(AData.Buffer, AData.Size);
  if VStr = '' then begin
    Exit;
  end;

  VJson := TSuperObject.ParseString(PChar(VStr), False);
  if VJson = nil then begin
    Exit;
  end;

  VSubsetBuilder := FVectorItemSubsetBuilderFactory.Build;

  VType := LowerCase(VJson.S['type']);
  if VType = 'featurecollection' then begin
    ParseFeatureCollection(AContext, VJson, VSubsetBuilder);
  end else
  if VType = 'feature' then begin
    ParseFeature(AContext, VJson, VSubsetBuilder, nil);
  end else begin
    raise EGeoJsonParser.Create('Invalid GeoJSON format!');
  end;

  if VSubsetBuilder.Count > 0 then begin
    Result := VSubsetBuilder.MakeStaticAndClear;
  end;
end;

procedure TGeoJsonParser.ParseFeatureCollection(
  const AContext: TVectorLoadContext;
  const AJson: ISuperObject;
  const AList: IVectorItemSubsetBuilder
);
var
  I: Integer;
  VCollection: TSuperArray;
  VCoordConverter: TGeoJsonCoordConverter;
begin
  VCollection := AJson.A['features'];
  if VCollection = nil then begin
    Exit;
  end;

  VCoordConverter.Init(FProjConverterFactory, AJson.O['crs'], nil);

  for I := 0 to VCollection.Length - 1 do begin
    ParseFeature(AContext, VCollection.O[I], AList, VCoordConverter.GetPtr);
  end;
end;

procedure TGeoJsonParser.ParseFeature(
  const AContext: TVectorLoadContext;
  const AFeature: ISuperObject;
  const AList: IVectorItemSubsetBuilder;
  const ACoordConverter: PGeoJsonCoordConverter
);
var
  I: Integer;
  VItem: IVectorDataItem;
  VProp: ISuperObject;
  VDesc: string;
  VGeometryLonLat: TArrayOfIGeometryLonLat;
  VCoordConverter: TGeoJsonCoordConverter;
begin
  if LowerCase(AFeature.S['type']) <> 'feature' then begin
    Exit;
  end;

  VCoordConverter.Init(FProjConverterFactory, AFeature.O['crs'], ACoordConverter);

  VGeometryLonLat := nil;
  ParseGeometry(AFeature.O['geometry'], VCoordConverter.GetPtr, VGeometryLonLat);

  VProp := AFeature.O['properties'];
  if VProp <> nil then begin
    VDesc := VProp.AsJSon(True, False);
  end else begin
    VDesc := '';
  end;

  for I := 0 to Length(VGeometryLonLat) - 1 do begin
    VItem :=
      FVectorDataFactory.BuildItem(
        AContext.MainInfoFactory.BuildMainInfo(AContext.IdData, '', VDesc),
        nil,
        VGeometryLonLat[I]
      );
    AList.Add(VItem);
  end;
end;

procedure TGeoJsonParser.ParseGeometry(
  const AGeometry: ISuperObject;
  const ACoordConverter: PGeoJsonCoordConverter;
  var AResult: TArrayOfIGeometryLonLat
);

var
  VCoordConverterPtr: PGeoJsonCoordConverter;

  function _ReadPoint(
    const ACoord: TSuperArray;
    out APoint: TDoublePoint;
    const AElevation: PDouble
  ): Boolean;
  begin
    Result := (ACoord <> nil) and (ACoord.Length >= 2);
    if not Result then begin
      Exit;
    end;

    APoint.X := ACoord.D[0];
    APoint.Y := ACoord.D[1];

    if VCoordConverterPtr <> nil then begin
      VCoordConverterPtr.ToWgs84(APoint);
    end;

    if AElevation = nil then begin
      Exit;
    end;

    if ACoord.Length >= 3 then begin
      AElevation^ := ACoord.D[2];
    end else begin
      AElevation^ := NaN;
    end;
  end;

  function _ReadPointsArray(
    const ACoord: TSuperArray;
    const AIncludeMeta: Boolean
  ): IDoublePointsAggregator;
  var
    I: Integer;
    VPoint: TDoublePoint;
    VMetaItem: TDoublePointsMetaItem;
    VMetaItemPtr: PDoublePointsMetaItem;
    VElevationPtr: PDouble;
  begin
    Result := TDoublePointsAggregator.Create;

    if (ACoord = nil) or (ACoord.Length <= 0) then begin
      Exit;
    end;

    if AIncludeMeta then begin
      VMetaItemPtr := @VMetaItem;
      VElevationPtr := @VMetaItem.Elevation;
    end else begin
      VMetaItemPtr := nil;
      VElevationPtr := nil;
    end;

    for I := 0 to ACoord.Length - 1 do begin
      if _ReadPoint(ACoord.O[I].AsArray, VPoint, VElevationPtr) then begin
        if AIncludeMeta then begin
          VMetaItem.IsElevationOk := not IsNan(VMetaItem.Elevation);
          VMetaItem.IsTimeStampOk := False;
        end;
        Result.Add(VPoint, VMetaItemPtr);
      end;
    end;
  end;

  function _ReadPolygon(const ACoord: TSuperArray): IGeometryLonLatPolygon;
  var
    I: Integer;
    VPoints: IDoublePointsAggregator;
    VBuilder: IGeometryLonLatPolygonBuilder;
  begin
    Result := nil;
    if ACoord = nil then begin
      Exit;
    end;
    VBuilder := FVectorGeometryLonLatFactory.MakePolygonBuilder;
    for I := 0 to ACoord.Length - 1 do begin
      VPoints := _ReadPointsArray(ACoord.O[I].AsArray, False);
      if (I = 0) and (VPoints.Count < 3) then begin
        Exit;
      end;
      if VPoints.Count >= 3 then begin
        if I = 0 then begin
          VBuilder.AddOuter(VPoints.MakeStaticAndClear);
        end else begin
          VBuilder.AddHole(VPoints.MakeStaticAndClear);
        end;
      end;
    end;
    Result := VBuilder.MakeStaticAndClear;
  end;

  function _ReadMultiPolygon(const ACoord: TSuperArray): IGeometryLonLatPolygon;
  var
    I: Integer;
    VPoly: IGeometryLonLatPolygon;
    VBuilder: IGeometryLonLatPolygonBuilder;
  begin
    Result := nil;
    if ACoord = nil then begin
      Exit;
    end;
    VBuilder := FVectorGeometryLonLatFactory.MakePolygonBuilder;
    for I := 0 to ACoord.Length - 1 do begin
      VPoly := _ReadPolygon(ACoord.O[I].AsArray);
      if VPoly <> nil then begin
        VBuilder.AddPolygon(VPoly);
      end;
    end;
    Result := VBuilder.MakeStaticAndClear;
  end;

  procedure _AddResult(const AGeometry: IGeometryLonLat);
  var
    I: Integer;
  begin
    if AGeometry = nil then begin
      Exit;
    end;
    I := Length(AResult);
    SetLength(AResult, I+1);
    AResult[I] := AGeometry;
  end;

var
  I: Integer;
  VType: string;
  VCoord: TSuperArray;
  VGeometries: TSuperArray;
  VPoint: TDoublePoint;
  VPoints: IDoublePointsAggregator;
  VCoordConverter: TGeoJsonCoordConverter;
  VLineBuilder: IGeometryLonLatLineBuilder;
begin
  if AGeometry = nil then begin
    Exit;
  end;

  VType := LowerCase(AGeometry.S['type']);
  if (VType = '') then begin
    Exit;
  end;

  VCoord := AGeometry.A['coordinates'];
  VGeometries := nil;

  if VCoord = nil then begin
    VGeometries := AGeometry.A['geometries'];
    if VGeometries = nil then begin
      Exit;
    end;
  end;

  VCoordConverter.Init(FProjConverterFactory, AGeometry.O['crs'], ACoordConverter);
  VCoordConverterPtr := VCoordConverter.GetPtr;

  if VType = 'point' then begin
    if _ReadPoint(VCoord, VPoint, nil) then begin
      _AddResult(
        FVectorGeometryLonLatFactory.CreateLonLatPoint(VPoint)
      );
    end;
  end else
  if VType = 'multipoint' then begin
    VPoints := _ReadPointsArray(VCoord, False);
    for I := 0 to VPoints.Count - 1 do begin
      _AddResult(
        FVectorGeometryLonLatFactory.CreateLonLatPoint(VPoints.Points[I])
      );
    end;
  end else
  if VType = 'linestring' then begin
    VPoints := _ReadPointsArray(VCoord, True);
    if VPoints.Count > 0 then begin
      _AddResult(
        FVectorGeometryLonLatFactory.CreateLonLatLine(VPoints.Points, VPoints.Meta, VPoints.Count)
      );
    end;
  end else
  if VType = 'multilinestring' then begin
    VLineBuilder := FVectorGeometryLonLatFactory.MakeLineBuilder;
    for I := 0 to VCoord.Length - 1 do begin
      VPoints := _ReadPointsArray(VCoord.O[I].AsArray, True);
      if VPoints.Count > 0 then begin
        VLineBuilder.AddLine(
          FVectorGeometryLonLatFactory.CreateLonLatLine(VPoints.Points, VPoints.Meta, VPoints.Count)
        );
      end;
    end;
    _AddResult(
      VLineBuilder.MakeStaticAndClear
    );
  end else
  if VType = 'polygon' then begin
    _AddResult(
      _ReadPolygon(VCoord)
    );
  end else
  if VType = 'multipolygon' then begin
    _AddResult(
      _ReadMultiPolygon(VCoord)
    );
  end else
  if VType = 'geometrycollection' then begin
    if VGeometries = nil then begin
      Assert(False);
      Exit;
    end;
    for I := 0 to VGeometries.Length - 1 do begin
      ParseGeometry(VGeometries.O[I], VCoordConverter.GetPtr, AResult);
    end;
  end;
end;

{ TGeoJsonCoordConverter }

procedure TGeoJsonCoordConverter.Init(
  const AProjConverterFactory: IProjConverterFactory;
  const AGeoJsonCRS: ISuperObject;
  const AParent: PGeoJsonCoordConverter
);
var
  I: Integer;
  VEPSG: Integer;
  VType: string;
  VName: string;
  VStr: TStringDynArray;
begin
  FConverter := nil;
  FParent := AParent;

  if AGeoJsonCRS = nil then begin
    Exit;
  end;

  VType := AGeoJsonCRS.S['type'];

  if LowerCase(VType) = 'name' then begin
    VName := AGeoJsonCRS.S['properties.name'];

    if VName = '' then begin
      Exit;
    end else
    if EndsText(':CRS84', VName) or EndsText('EPSG:4326', VName) then begin
      // nothing to do
      Exit;
    end else begin
      VStr := SplitString(UpperCase(VName), ':');
      I := Length(VStr);
      if (I >= 2) and (VStr[I-2] = 'EPSG') and TryStrToInt(VStr[I-1], VEPSG) then begin
        FConverter := AProjConverterFactory.GetByEPSG(VEPSG);
        if FConverter = nil then begin
          raise EGeoJsonParser.CreateFmt('Unsupported CRS EPSG: "%s"', [VName]);
        end;
      end else begin
        raise EGeoJsonParser.CreateFmt('Unsupported CRS: "%s"', [VName]);
      end;
    end;
  end else begin
    raise EGeoJsonParser.CreateFmt('Unsupported CRS type: "%s"', [VType]);
  end;
end;

procedure TGeoJsonCoordConverter.ToWgs84(var APoint: TDoublePoint);
var
  VLonLat: TDoublePoint;
begin
  Assert(FConverter <> nil);
  VLonLat := FConverter.XY2LonLat(APoint);
  APoint := VLonLat;
end;

function TGeoJsonCoordConverter.GetPtr: PGeoJsonCoordConverter;
begin
  if FConverter <> nil then begin
    Result := @Self;
  end else begin
    Result := FParent;
  end;
end;

end.
