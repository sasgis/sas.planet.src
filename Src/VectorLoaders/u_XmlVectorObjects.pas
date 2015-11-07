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

unit u_XmlVectorObjects;

interface

uses
  SysUtils,
  t_GeoTypes,
  i_Appearance,
  i_XmlVectorObjects,
  i_VectorItemSubsetBuilder,
  i_VectorItemSubset,
  i_VectorDataItemSimple,
  i_VectorDataFactory,
  i_GeometryLonLat,
  i_GeometryLonLatFactory,
  i_InterfaceListSimple,
  i_DoublePointsAggregator,
  u_BaseInterfacedObject;

type
  PFormatSettings = ^TFormatSettings;

  TXmlVectorObjects = class(TBaseInterfacedObject, IXmlVectorObjects)
  private
    FLineBuilder: IGeometryLonLatLineBuilder;
    FPolygonBuilder: IGeometryLonLatPolygonBuilder;
    FList: IInterfaceListSimple;
    FAllowMultiParts: Boolean;
    FCheckLineIsClosed: Boolean;
    FSkipPointInMultiObject: Boolean;
    FFormatPtr: PFormatSettings;
    FIdData: Pointer;
    FVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
    FDataFactory: IVectorDataFactory;
    FGeometryFactory: IGeometryLonLatFactory;
    FVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;

    // storage for coordinates
    FDoublePointsAggregator: IDoublePointsAggregator;
    // list of result objects
    FVectorDataItemsResultBuilder: IVectorItemSubsetBuilder;

    // check if in multigeometry
    FInMultiGeometry: Boolean;
    // check if in multitrack
    FInMultiTrack: Boolean;
    // check if in placemark object
    FInMarkObject: Boolean;

    // count of segments in array
    FClosedSegments: Integer;
    FOpenedSegments: Integer;
  private
    procedure SafeAddToResult(const AItem: IVectorDataItem);
    procedure InternalMakeTrackObject;
    procedure InternalCloseArrayPoints;
    function ParseKmlCoordinatesToArray(
      const ACoordinates: string
    ): Integer;
    function ParseCloseMarkObjectData(
      const AData: Pointer;
      const AMode: TCloseMarkObjectMode;
      out AAppearance: IAppearance;
      out AMarkName: string;
      out AMarkDesc: string
    ): Boolean;
  private
    { IXmlVectorObjects }
    function GetCount: Integer;
    function GetVectorDataItemsResult: IVectorItemSubset;

    procedure OpenMultiGeometry;
    procedure CloseMultiGeometry;

    procedure OpenMultiTrack;
    procedure CloseMultiTrack;

    procedure OpenTrackSegment;
    procedure CloseTrackSegment;

    procedure OpenMarkObject;
    procedure CloseMarkObject(
      const AData: Pointer;
      const AMode: TCloseMarkObjectMode
    );

    procedure CloseKmlLineString(const ACoordinates: string);
    procedure CloseKmlLinearRing(
      const ACoordinates: string;
      const AInner: Boolean
    );
    procedure CloseKmlPoint(const ACoordinates: string);
    procedure CloseGPXPoint(const APoint: TDoublePoint);
    procedure CloseKmlPolygon;

    procedure AddTrackPoint(const APoint: TDoublePoint);
  public
    constructor Create(
      const ACheckLineIsClosed, ASkipPointInMultiObject: Boolean;
      const AFormatPtr: PFormatSettings;
      const AIdData: Pointer;
      const AAllowMultiParts: Boolean;
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
      const ADataFactory: IVectorDataFactory;
      const AGeometryFactory: IGeometryLonLatFactory
    );
  end;

  EXmlVectorObjectsError = class(Exception);
  EXmlVectorObjectsMarkInMark = class(EXmlVectorObjectsError);
  EXmlVectorObjectsNotInMark = class(EXmlVectorObjectsError);
  EXmlVectorObjectsMultiInMulti = class(EXmlVectorObjectsError);
  EXmlVectorObjectsNotInMultiTrack = class(EXmlVectorObjectsError);
  EXmlVectorObjectsNotInMultiGeometry = class(EXmlVectorObjectsError);
  EXmlVectorObjectsUnclosedPolygon = class(EXmlVectorObjectsError);
  EXmlVectorObjectsFailedToCloseMark = class(EXmlVectorObjectsError);

implementation

uses
  Math,
  vsagps_public_base,
  vsagps_public_sysutils,
  vsagps_public_kml,
  vsagps_public_gpx,
  vsagps_public_parser,
  vsagps_public_print,
  u_GeoFunc,
  u_InterfaceListSimple,
  u_DoublePointsAggregator;

function FindNextDelimiterPos(
  const APrevDelimiterPos: Integer;
  const ASource: string
): Integer;
begin
  Result := APrevDelimiterPos + 1;
  while (Result <= Length(ASource)) do begin
    case Ord(ASource[Result]) of
      9, 10, 13, 32, 160: begin
        Exit;
      end;
    end;
    Inc(Result);
  end;
end;

{ TXmlVectorObjects }

procedure TXmlVectorObjects.AddTrackPoint(const APoint: TDoublePoint);
begin
  FDoublePointsAggregator.Add(APoint);
end;

procedure TXmlVectorObjects.CloseGPXPoint(const APoint: TDoublePoint);
var
  VPoint: IGeometryLonLatPoint;
begin
  // check if in multigeometry
  if FInMultiGeometry and FSkipPointInMultiObject then begin
    Exit;
  end;

  VPoint := FGeometryFactory.CreateLonLatPoint(APoint);
  FList.Add(VPoint);
end;

procedure TXmlVectorObjects.CloseKmlLinearRing(
  const ACoordinates: string;
  const AInner: Boolean
);
begin
  // check
  if (0 = Length(ACoordinates)) then begin
    Exit;
  end;

  // parse coordinates and add it to array
  ParseKmlCoordinatesToArray(ACoordinates);

  if FDoublePointsAggregator.Count > 0 then begin
    if AInner then begin
      FPolygonBuilder.AddHole(FDoublePointsAggregator.MakeStaticAndClear);
    end else begin
      FPolygonBuilder.AddOuter(FDoublePointsAggregator.MakeStaticAndClear);
    end;
    Inc(FClosedSegments);
  end;
end;

procedure TXmlVectorObjects.CloseKmlLineString(const ACoordinates: string);
begin
  // check
  if (0 = Length(ACoordinates)) then begin
    Exit;
  end;

  // parse coordinates and add it to array
  ParseKmlCoordinatesToArray(ACoordinates);
  if FDoublePointsAggregator.Count > 0 then begin
    FLineBuilder.AddLine(FDoublePointsAggregator.MakeStaticAndClear);
    Inc(FOpenedSegments);
  end;
end;

procedure TXmlVectorObjects.CloseKmlPoint(const ACoordinates: string);
var
  VData: TCoordLineData;
  VLonLatPoint: IGeometryLonLatPoint;
begin
  // check if in multigeometry
  if FInMultiGeometry and FSkipPointInMultiObject then begin
    Exit;
  end;

  // parse
  if parse_kml_coordinate(ACoordinates, @VData, FFormatPtr^) then begin
    // make point
    VLonLatPoint := FGeometryFactory.CreateLonLatPoint(DoublePoint(VData.lon1, VData.lat0));
    FList.Add(VLonLatPoint);
  end;
end;

procedure TXmlVectorObjects.CloseKmlPolygon;
var
  VLonLatPolygon: IGeometryLonLat;
begin
  // dont create polygons for every Polygon in MultiGeometry
  // if allow to create multisegment polygons
  if FAllowMultiParts and FInMultiGeometry then begin
    Exit;
  end;

  // make polygon object
  VLonLatPolygon := FPolygonBuilder.MakeStaticAndClear;

  if Assigned(VLonLatPolygon) then begin
    FList.Add(VLonLatPolygon);
  end;
end;

procedure TXmlVectorObjects.CloseMarkObject(
  const AData: Pointer;
  const AMode: TCloseMarkObjectMode
);
var
  i: Integer;
  // params
  VName, VDesc: string;
  VAppearance: IAppearance;
  // item
  VGeometry: IGeometryLonLat;
  VItem: IVectorDataItem;
begin
  if (not FInMarkObject) then begin
    raise EXmlVectorObjectsNotInMark.Create('');
  end;
  FInMarkObject := False;

  // check array
  InternalCloseArrayPoints;

  // get objects
  for i := 0 to FList.Count - 1 do begin
    VGeometry := FList[i] as IGeometryLonLat;
    if ParseCloseMarkObjectData(AData, AMode, VAppearance, VName, VDesc) then begin
      VItem :=
        FDataFactory.BuildItem(
          FVectorDataItemMainInfoFactory.BuildMainInfo(FIdData, VName, VDesc),
          VAppearance,
          VGeometry
        );
      SafeAddToResult(VItem);
    end;
  end;

  // reset
  FList.Clear;
  FDoublePointsAggregator.Clear;
end;

procedure TXmlVectorObjects.CloseMultiGeometry;
begin
  if (not FInMultiGeometry) then begin
    raise EXmlVectorObjectsNotInMultiGeometry.Create('');
  end;
  FInMultiGeometry := False;
  // convert array to some object
  InternalCloseArrayPoints;
end;

procedure TXmlVectorObjects.CloseMultiTrack;
begin
  if (not FInMultiTrack) then begin
    raise EXmlVectorObjectsNotInMultiTrack.Create('');
  end;
  FInMultiTrack := False;
  // convert array to polyline object
  InternalMakeTrackObject;
end;

procedure TXmlVectorObjects.CloseTrackSegment;
begin
  if FDoublePointsAggregator.Count > 0 then begin
    FLineBuilder.AddLine(FDoublePointsAggregator.MakeStaticAndClear);
    Inc(FOpenedSegments);
  end;
  InternalMakeTrackObject;
end;

constructor TXmlVectorObjects.Create(
  const ACheckLineIsClosed, ASkipPointInMultiObject: Boolean;
  const AFormatPtr: PFormatSettings;
  const AIdData: Pointer;
  const AAllowMultiParts: Boolean;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
  const ADataFactory: IVectorDataFactory;
  const AGeometryFactory: IGeometryLonLatFactory
);
begin
  Assert(AVectorItemSubsetBuilderFactory <> nil);
  Assert(AGeometryFactory <> nil);
  Assert(ADataFactory <> nil);
  inherited Create;
  FList := TInterfaceListSimple.Create;
  FLineBuilder := AGeometryFactory.MakeLineBuilder;
  FPolygonBuilder := AGeometryFactory.MakePolygonBuilder;
  FAllowMultiParts := AAllowMultiParts;
  FCheckLineIsClosed := ACheckLineIsClosed;
  FSkipPointInMultiObject := ASkipPointInMultiObject;
  FFormatPtr := AFormatPtr;
  FIdData := AIdData;
  FVectorItemSubsetBuilderFactory := AVectorItemSubsetBuilderFactory;
  FDataFactory := ADataFactory;
  FGeometryFactory := AGeometryFactory;
  FVectorDataItemMainInfoFactory := AVectorDataItemMainInfoFactory;

  FDoublePointsAggregator := TDoublePointsAggregator.Create;
  FClosedSegments := 0;
  FOpenedSegments := 0;
  FInMarkObject := False;
  FInMultiGeometry := False;
  FInMultiTrack := False;
end;

function TXmlVectorObjects.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TXmlVectorObjects.GetVectorDataItemsResult: IVectorItemSubset;
begin
  Result := nil;
  if Assigned(FVectorDataItemsResultBuilder) then begin
    Result := FVectorDataItemsResultBuilder.MakeStaticAndClear;
    FVectorDataItemsResultBuilder := nil;
  end;
end;

procedure TXmlVectorObjects.InternalCloseArrayPoints;
var
  VPolygon: IGeometryLonLatPolygon;
  VLine: IGeometryLonLatLine;
begin
  VPolygon := FPolygonBuilder.MakeStaticAndClear;
  if Assigned(VPolygon) then begin
    FList.Add(VPolygon);
  end;
  VLine := FLineBuilder.MakeStaticAndClear;
  if Assigned(VLine) then begin
    FList.Add(VLine);
  end;
end;

procedure TXmlVectorObjects.InternalMakeTrackObject;
var
  VLonLatPath: IGeometryLonLat;
begin
  // dont create tracks for every gx:Track in gx:MultiTrack
  // if allow to create multisegment polylines
  if FAllowMultiParts and FInMultiTrack then begin
    Exit;
  end;

  // make polyline object
  VLonLatPath := FLineBuilder.MakeStaticAndClear;

  if Assigned(VLonLatPath) then begin
    FList.Add(VLonLatPath);
  end;
end;

procedure TXmlVectorObjects.OpenMarkObject;
begin
  if (FInMarkObject) then begin
    raise EXmlVectorObjectsMarkInMark.Create('');
  end;
  FInMarkObject := True;
  FOpenedSegments := 0;
  FClosedSegments := 0;
end;

procedure TXmlVectorObjects.OpenMultiGeometry;
begin
  if (FInMultiGeometry) then begin
    raise EXmlVectorObjectsMultiInMulti.Create('');
  end;
  FInMultiGeometry := True;
end;

procedure TXmlVectorObjects.OpenMultiTrack;
begin
  if (FInMultiTrack) then begin
    raise EXmlVectorObjectsMultiInMulti.Create('');
  end;
  FInMultiTrack := True;
end;

procedure TXmlVectorObjects.OpenTrackSegment;
begin
  CloseTrackSegment;
end;

function TXmlVectorObjects.ParseCloseMarkObjectData(
  const AData: Pointer;
  const AMode: TCloseMarkObjectMode;
  out AAppearance: IAppearance;
  out AMarkName: string;
  out AMarkDesc: string
): Boolean;

  procedure _AddToDesc(const AParamName, AParamValue: string);
  begin
    if (0 < Length(AParamValue)) then begin
      if (0 < Length(AMarkDesc)) then begin
        AMarkDesc := AMarkDesc + '<br>';
      end;
      AMarkDesc := AMarkDesc + AParamName + ': ' + AParamValue;
    end;
  end;

var
  i: Tvsagps_KML_str;
  j: Tvsagps_GPX_trk_str;
  k: Tvsagps_GPX_wpt_str;
  x: Tvsagps_GPX_ext_sasx_str;
  y: Tvsagps_GPX_trk_ext;
  z: Tvsagps_GPX_wpt_ext;
  VParamName: string;
  VParamValue: string;
begin
  Result := False;
  AAppearance := nil;
  AMarkName := '';
  AMarkDesc := '';

  Assert(AData <> nil);

  case AMode of
    cmom_KML: begin
      // kml
      with Pvsagps_KML_ParserData(AData)^ do begin
        // name
        if (kml_name in fAvail_strs) then begin
          AMarkName := SafeSetStringP(fParamsStrs[kml_name]);
        end;

        // description
        if (kml_description in fAvail_strs) then begin
          AMarkDesc := SafeSetStringP(fParamsStrs[kml_description]);
        end;

        // others
        for i := Low(i) to High(i) do begin
          if (not (i in [kml_name, kml_description])) then begin
            if (i in fAvail_strs) then begin
              VParamName := c_KML_str[i];
              VParamValue := SafeSetStringP(fParamsStrs[i]);
          // add to description
              _AddToDesc(VParamName, VParamValue);
            end;
          end;
        end;
      end;
      Inc(Result);
    end;

    cmom_GPX_TRK: begin
      // trk in gpx
      with Pvsagps_GPX_ParserData(AData)^.trk_data do begin
        // name
        if (trk_name in fAvail_trk_strs) then begin
          AMarkName := SafeSetStringP(fStrs[trk_name]);
        end;

        // description
        if (trk_desc in fAvail_trk_strs) then begin
          AMarkDesc := SafeSetStringP(fStrs[trk_desc]);
        end;

        // others
        for j := Low(j) to High(j) do begin
          if (not (j in [trk_name, trk_desc])) then begin
            if (j in fAvail_trk_strs) then begin
              VParamName := c_GPX_trk_subtag[j];
              VParamValue := SafeSetStringP(fStrs[j]);
            // add to description
              _AddToDesc(VParamName, VParamValue);
            end;
          end;
        end;
          // gpxx:TrackExtension
        for y := Low(y) to High(y) do begin
          if (y in fAvail_trk_exts) then begin
            VParamName := c_GPX_trk_ext_subtag[y];
            VParamValue := SafeSetStringP(fExts[y]);
            // add to description
            _AddToDesc(VParamName, VParamValue);
          end;
        end;
      end;
      Inc(Result);
    end;

    cmom_GPX_WPT: begin
      // wpt in gpx
      with Pvsagps_GPX_ParserData(AData)^.wpt_data do begin
        // name
        if (wpt_name in fAvail_wpt_strs) then begin
          AMarkName := SafeSetStringP(fStrs[wpt_name]);
        end;

        // description
        if (wpt_desc in fAvail_wpt_strs) then begin
          AMarkDesc := SafeSetStringP(fStrs[wpt_desc]);
        end;

        // others
        for k := Low(k) to High(k) do begin
          if (not (k in [wpt_name, wpt_desc])) then begin
            if (k in fAvail_wpt_strs) then begin
              VParamName := c_GPX_wpt_str_subtag[k];
              VParamValue := SafeSetStringP(fStrs[k]);
            // add to description
              _AddToDesc(VParamName, VParamValue);
            end;
          end;
        end;

          // fPos
        with fPos do begin
            // time
          if (wpt_time in fAvail_wpt_params) and UTCDateOK and UTCTimeOK then begin
            VParamName := 'time';
            VParamValue := DateTime_To_ISO8601(UTCDate + UTCTime, False);
              // add to description
            _AddToDesc(VParamName, VParamValue);
          end;

            // ele
          if (wpt_ele in fAvail_wpt_params) and (not NoData_Float64(Altitude)) then begin
            VParamName := 'ele';
            VParamValue := Round_Float64_to_String(Altitude, FFormatPtr^, round_ele);
              // add to description
            _AddToDesc(VParamName, VParamValue);
          end;
        end;

          // gpxx:*
        for z := Low(z) to High(z) do begin
          if (z in fAvail_wpt_exts) then begin
            VParamName := c_GPX_wpt_ext_subtag[z];
            VParamValue := SafeSetStringP(fExts[z]);
            // add to description
            _AddToDesc(VParamName, VParamValue);
          end;
        end;
      end;
      // extension
      with Pvsagps_GPX_ParserData(AData)^.extensions_data do begin
        for x := Low(x) to High(x) do begin
          if (x in fAvail_strs) then begin
            VParamName := c_GPX_ext_sasx_subtag[x];
            VParamValue := SafeSetStringP(sasx_strs[x]);
          // add to description
            _AddToDesc(VParamName, VParamValue);
          end;
        end;
      end;
      Inc(Result);
    end;
  end;
end;

function TXmlVectorObjects.ParseKmlCoordinatesToArray(
  const ACoordinates: string
): Integer;
var
  VPosPrev, VPosCur: Integer;
  VCoordLine: string;
  VData: TCoordLineData;
  VPoint: TDoublePoint;
begin
  Result := 0;
  VPosPrev := 0;
  // loop through points
  repeat
    if (VPosPrev >= Length(ACoordinates)) then begin
      break;
    end;

    // get part
    VPosCur := FindNextDelimiterPos(VPosPrev, ACoordinates);
    VCoordLine := System.Copy(
      ACoordinates,
      (VPosPrev + 1),
      (VPosCur - VPosPrev - 1)
    );

    // parse and add
    if (Length(VCoordLine) > 0) then begin
      if parse_kml_coordinate(VCoordLine, @VData, FFormatPtr^) then begin
        VPoint.X := VData.lon1;
        VPoint.Y := VData.lat0;
        // add to array
        FDoublePointsAggregator.Add(VPoint);
        Inc(Result);
      end;
    end;

    // next
    VPosPrev := VPosCur;
  until False;
end;

procedure TXmlVectorObjects.SafeAddToResult(
  const AItem: IVectorDataItem
);
begin
  if Assigned(AItem) then begin
    if not Assigned(FVectorDataItemsResultBuilder) then begin
      FVectorDataItemsResultBuilder := FVectorItemSubsetBuilderFactory.Build;
    end;
    FVectorDataItemsResultBuilder.Add(AItem);
  end;
end;

end.
