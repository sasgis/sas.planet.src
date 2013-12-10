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
  i_VectorItemsFactory,
  i_InterfaceListSimple,
  i_DoublePointsAggregator,
  u_BaseInterfacedObject;

type
  PFormatSettings = ^TFormatSettings;

  TXmlVectorObjects = class(TBaseInterfacedObject, IXmlVectorObjects)
  private
    FList: IInterfaceListSimple;
    FAllowMultiParts: Boolean;
    FCheckLineIsClosed: Boolean;
    FSkipPointInMultiObject: Boolean;
    FFormatPtr: PFormatSettings;
    FIdData: Pointer;
    FDataFactory: IVectorDataFactory;
    FItemsFactory: IVectorGeometryLonLatFactory;
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
    procedure SafeMakeResultList;
    procedure SafeAddToResult(const AItem: IVectorDataItemSimple);
    procedure InternalAddPoint(const APoint: TDoublePoint);
    procedure InternalMakeTrackObject(const AForMultiTrack: Boolean);
    procedure InternalMakePolygonObject(const AForMultiObject, AInner: Boolean);
    procedure InternalCloseArrayPoints;
    function LastSegmentIsClosed(const AFirstIndexOfLastSegment: Integer): Boolean;
    function ParseKmlCoordinatesToArray(
      const ACoordinates: WideString;
      const AForceClose: Boolean
    ): Integer;
    function PrepareArrayOfPoints: Integer;
    function ParseCloseMarkObjectData(
      const AData: Pointer;
      const AMode: TCloseMarkObjectMode;
      out AAppearance: IAppearance;
      out AMarkName: string;
      out AMarkDesc: string;
      const AMarkType: TGUID
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

    procedure CloseKmlLineString(const ACoordinates: WideString);
    procedure CloseKmlLinearRing(
      const ACoordinates: WideString;
      const AInner: Boolean
    );
    procedure CloseKmlPoint(const ACoordinates: WideString);
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
      const ADataFactory: IVectorDataFactory;
      const AItemsFactory: IVectorGeometryLonLatFactory
    );
  end;

  EXmlVectorObjectsError               = class(Exception);
  EXmlVectorObjectsMarkInMark          = class(EXmlVectorObjectsError);
  EXmlVectorObjectsNotInMark           = class(EXmlVectorObjectsError);
  EXmlVectorObjectsMultiInMulti        = class(EXmlVectorObjectsError);
  EXmlVectorObjectsNotInMultiTrack     = class(EXmlVectorObjectsError);
  EXmlVectorObjectsNotInMultiGeometry  = class(EXmlVectorObjectsError);
  EXmlVectorObjectsUnclosedPolygon     = class(EXmlVectorObjectsError);
  EXmlVectorObjectsFailedToCloseMark   = class(EXmlVectorObjectsError);

implementation

uses
  vsagps_public_base,
  vsagps_public_sysutils,
  vsagps_public_kml,
  vsagps_public_gpx,
  vsagps_public_parser,
  vsagps_public_print,
  i_LonLatRect,
  u_GeoFun,
  u_LonLatRectByPoint,
  u_InterfaceListSimple,
  u_DoublePointsAggregator;

function FindNextDelimiterPos(
  const APrevDelimiterPos: Integer;
  const ASource: WideString
): Integer;
begin
  Result := APrevDelimiterPos+1;
  while (Result <= Length(ASource)) do begin
    case Ord(ASource[Result]) of
      9,10,13,32,160: Exit;
    end;
    Inc(Result);
  end;
end;

{ TXmlVectorObjects }

procedure TXmlVectorObjects.AddTrackPoint(const APoint: TDoublePoint);
begin
  InternalAddPoint(APoint);
end;

procedure TXmlVectorObjects.CloseGPXPoint(const APoint: TDoublePoint);
var
  VPoint: IGeometryLonLatPoint;
begin
  // check if in multigeometry
  if FInMultiGeometry and FSkipPointInMultiObject then
    Exit;

  VPoint := FItemsFactory.CreateLonLatPoint(APoint);
  FList.Add(VPoint);
end;

procedure TXmlVectorObjects.CloseKmlLinearRing(
  const ACoordinates: WideString;
  const AInner: Boolean
);
var
  VOldCount: Integer;
  VAdded: Integer;
  VClosed: Boolean;
begin
  // check
  if (0 = Length(ACoordinates)) then
    Exit;

  // count of existing points
  VOldCount := PrepareArrayOfPoints;
  
  // parse coordinates and add it to array
  VAdded := ParseKmlCoordinatesToArray(ACoordinates, True);

  // check segment closure
  if (VAdded > 1) then begin
    VClosed := LastSegmentIsClosed(VOldCount);
    if FAllowMultiParts then begin
      // prepare for multigeometry or placemark
      if VClosed then begin
        Inc(FClosedSegments);
      end else begin
        // Inc(FOpenedSegments);
        raise EXmlVectorObjectsUnclosedPolygon.Create('');
      end;
    end else begin
      // make object here
      if VClosed then begin
        // make polygon
        InternalMakePolygonObject(False, AInner);
      end else begin
        // make polyline
        // InternalMakeTrackObject(False);
        raise EXmlVectorObjectsUnclosedPolygon.Create('');
      end;
    end;
  end;
end;

procedure TXmlVectorObjects.CloseKmlLineString(const ACoordinates: WideString);
var
  VOldCount: Integer;
  VAdded: Integer;
  VClosed: Boolean;
begin
  // check
  if (0 = Length(ACoordinates)) then
    Exit;

  // count of existing points
  VOldCount := PrepareArrayOfPoints;
  
  // parse coordinates and add it to array
  VAdded := ParseKmlCoordinatesToArray(ACoordinates, False);

  // check segment closure
  if (VAdded > 1) then begin
    VClosed := LastSegmentIsClosed(VOldCount);
    if FAllowMultiParts then begin
      // prepare for multigeometry or placemark
      if VClosed then begin
        Inc(FClosedSegments);
      end else begin
        Inc(FOpenedSegments);
      end;
    end else begin
      // make object here
      if VClosed and FCheckLineIsClosed then begin
        // make polygon
        InternalMakePolygonObject(False, False);
      end else begin
        // make polyline
        InternalMakeTrackObject(False);
      end;
    end;
  end;
end;

procedure TXmlVectorObjects.CloseKmlPoint(const ACoordinates: WideString);
var
  VData: TCoordLineData;
  VLonLatPoint: IGeometryLonLatPoint;
begin
  // check if in multigeometry
  if FInMultiGeometry and FSkipPointInMultiObject then
    Exit;

  // parse
  if parse_kml_coordinate(ACoordinates, @VData, FFormatPtr^) then begin
    // make point
    VLonLatPoint := FItemsFactory.CreateLonLatPoint(DoublePoint(VData.lon1, VData.lat0));
    FList.Add(VLonLatPoint);
  end;
end;

procedure TXmlVectorObjects.CloseKmlPolygon;
begin
  InternalMakePolygonObject(False, False);
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
  VObject: IInterface;
  // for point
  VLonLatPoint: IGeometryLonLatPoint;
  VPointResult: IVectorDataItemPoint;
  // for line
  VLonLatPath: IGeometryLonLatMultiLine;
  VLineResult: IVectorDataItemLine;
  // for polygon
  VLonLatPolygon: IGeometryLonLatMultiPolygon;
  VPolygonResult: IVectorDataItemPoly;
begin
  if (not FInMarkObject) then
    raise EXmlVectorObjectsNotInMark.Create('');
  FInMarkObject := False;

  // check array
  InternalCloseArrayPoints;

  // get objects
  for i := 0 to FList.Count - 1 do begin
    VObject := FList[i];
    if Supports(VObject, IGeometryLonLatPoint, VLonLatPoint) then begin
      // point
      if ParseCloseMarkObjectData(AData, AMode, VAppearance, VName, VDesc, IVectorDataItemPoint) then begin
        VPointResult := FDataFactory.BuildPoint(FIdData, VAppearance, VName, VDesc, VLonLatPoint);
        SafeAddToResult(VPointResult);
      end;
    end else if Supports(VObject, IGeometryLonLatMultiLine, VLonLatPath) then begin
      // line
      if ParseCloseMarkObjectData(AData, AMode, VAppearance, VName, VDesc, IVectorDataItemLine) then begin
        VLineResult := FDataFactory.BuildPath(FIdData, VAppearance, VName, VDesc, VLonLatPath);
        SafeAddToResult(VLineResult);
      end;
    end else if Supports(VObject, IGeometryLonLatMultiPolygon, VLonLatPolygon) then begin
      // polygon
      if ParseCloseMarkObjectData(AData, AMode, VAppearance, VName, VDesc, IVectorDataItemPoly) then begin
        VPolygonResult := FDataFactory.BuildPoly(FIdData, VAppearance, VName, VDesc, VLonLatPolygon);
        SafeAddToResult(VPolygonResult);
      end;
    end;
  end;

  // reset
  FList.Clear;
  FDoublePointsAggregator.Clear;
end;

procedure TXmlVectorObjects.CloseMultiGeometry;
begin
  if (not FInMultiGeometry) then
    raise EXmlVectorObjectsNotInMultiGeometry.Create('');
  FInMultiGeometry := False;
  // convert array to some object
  InternalCloseArrayPoints;
end;

procedure TXmlVectorObjects.CloseMultiTrack;
begin
  if (not FInMultiTrack) then
    raise EXmlVectorObjectsNotInMultiTrack.Create('');
  FInMultiTrack := False;
  // convert array to polyline object
  InternalMakeTrackObject(True);
end;

procedure TXmlVectorObjects.CloseTrackSegment;
begin
  InternalMakeTrackObject(False);
end;

constructor TXmlVectorObjects.Create(
  const ACheckLineIsClosed, ASkipPointInMultiObject: Boolean;
  const AFormatPtr: PFormatSettings;
  const AIdData: Pointer;
  const AAllowMultiParts: Boolean;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const ADataFactory: IVectorDataFactory;
  const AItemsFactory: IVectorGeometryLonLatFactory
);
begin
  Assert(AVectorItemSubsetBuilderFactory<>nil);
  Assert(ADataFactory<>nil);
  Assert(AItemsFactory<>nil);
  inherited Create;
  FList := TInterfaceListSimple.Create;
  FAllowMultiParts := AAllowMultiParts;
  FCheckLineIsClosed := ACheckLineIsClosed;
  FSkipPointInMultiObject := ASkipPointInMultiObject;
  FFormatPtr := AFormatPtr;
  FIdData := AIdData;
  FVectorItemSubsetBuilderFactory := AVectorItemSubsetBuilderFactory;
  FDataFactory := ADataFactory;
  FItemsFactory := AItemsFactory;

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

procedure TXmlVectorObjects.InternalAddPoint(const APoint: TDoublePoint);
begin
  FDoublePointsAggregator.Add(APoint);
end;

procedure TXmlVectorObjects.InternalCloseArrayPoints;
begin
  if (0 = FDoublePointsAggregator.Count) then
    Exit;
  if (FOpenedSegments > 0) then begin
    // have unclosed segments
    InternalMakeTrackObject(True);
  end else if (FClosedSegments > 0) then begin
    // have only closed segments
    InternalMakePolygonObject(True, False);
  end else begin
    // no segments at all
    Assert(False);
    if LastSegmentIsClosed(0) then begin
      // array is closed
      InternalMakePolygonObject(True, False);
    end else begin
      // unclosed
      InternalMakeTrackObject(True);
    end;
  end;
end;

procedure TXmlVectorObjects.InternalMakePolygonObject(
  const AForMultiObject, AInner: Boolean);
var
  VLonLatPolygon: IGeometryLonLatMultiPolygon;
begin
  // dont create polygons for every Polygon in MultiGeometry
  // if allow to create multisegment polygons
  if FAllowMultiParts and FInMultiGeometry and (not AForMultiObject) then
    Exit;

  // convert array to polygon object
  if (0 = FDoublePointsAggregator.Count) then
    Exit;

  // make polygon object
  VLonLatPolygon := FItemsFactory.CreateLonLatPolygon(
    FDoublePointsAggregator.Points,
    FDoublePointsAggregator.Count
  );

  if Assigned(VLonLatPolygon) then begin
    FList.Add(VLonLatPolygon);
  end;

  // init
  FDoublePointsAggregator.Clear;
end;

procedure TXmlVectorObjects.InternalMakeTrackObject(
  const AForMultiTrack: Boolean);
var
  VLonLatPath: IGeometryLonLatMultiLine;
begin
  // dont create tracks for every gx:Track in gx:MultiTrack
  // if allow to create multisegment polylines
  if FAllowMultiParts and FInMultiTrack and (not AForMultiTrack) then
    Exit;

  // convert array to object
  if (0 = FDoublePointsAggregator.Count) then
    Exit;

  // make polyline object
  VLonLatPath := FItemsFactory.CreateLonLatPath(
    FDoublePointsAggregator.Points,
    FDoublePointsAggregator.Count
  );

  if Assigned(VLonLatPath) then begin
    FList.Add(VLonLatPath);
  end;

  // init
  FDoublePointsAggregator.Clear;
end;

function TXmlVectorObjects.LastSegmentIsClosed(
  const AFirstIndexOfLastSegment: Integer): Boolean;
var
  VArray: PDoublePointArray;
begin
  VArray := FDoublePointsAggregator.Points;
  Result := DoublePointsEqual(
    VArray^[AFirstIndexOfLastSegment],
    VArray^[FDoublePointsAggregator.Count-1]
  );
end;

procedure TXmlVectorObjects.OpenMarkObject;
begin
  if (FInMarkObject) then
    raise EXmlVectorObjectsMarkInMark.Create('');
  FInMarkObject := True;
end;

procedure TXmlVectorObjects.OpenMultiGeometry;
begin
  if (FInMultiGeometry) then
    raise EXmlVectorObjectsMultiInMulti.Create('');
  FInMultiGeometry := True;
end;

procedure TXmlVectorObjects.OpenMultiTrack;
begin
  if (FInMultiTrack) then
    raise EXmlVectorObjectsMultiInMulti.Create('');
  FInMultiTrack := True;
end;

procedure TXmlVectorObjects.OpenTrackSegment;
begin
  // add delimiter if array is not empty
  PrepareArrayOfPoints;
  Inc(FOpenedSegments);
end;

function TXmlVectorObjects.ParseCloseMarkObjectData(
  const AData: Pointer;
  const AMode: TCloseMarkObjectMode;
  out AAppearance: IAppearance;
  out AMarkName: string;
  out AMarkDesc: string;
  const AMarkType: TGUID
): Boolean;

  procedure _AddToDesc(const AParamName, AParamValue: WideString);
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
  VParamName: WideString;
  VParamValue: WideString;
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
        for i := Low(i) to High(i) do
        if (not (i in [kml_name, kml_description])) then
        if (i in fAvail_strs) then begin
          VParamName := c_KML_str[i];
          VParamValue := SafeSetStringP(fParamsStrs[i]);
          // add to description
          _AddToDesc(VParamName, VParamValue);
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
          for j := Low(j) to High(j) do
          if (not (j in [trk_name, trk_desc])) then
          if (j in fAvail_trk_strs) then begin
            VParamName := c_GPX_trk_subtag[j];
            VParamValue := SafeSetStringP(fStrs[j]);
            // add to description
            _AddToDesc(VParamName, VParamValue);
          end;
          // gpxx:TrackExtension
          for y := Low(y) to High(y) do
          if (y in fAvail_trk_exts) then begin
            VParamName := c_GPX_trk_ext_subtag[y];
            VParamValue := SafeSetStringP(fExts[y]);
            // add to description
            _AddToDesc(VParamName, VParamValue);
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
          for k := Low(k) to High(k) do
          if (not (k in [wpt_name, wpt_desc])) then
          if (k in fAvail_wpt_strs) then begin
            VParamName := c_GPX_wpt_str_subtag[k];
            VParamValue := SafeSetStringP(fStrs[k]);
            // add to description
            _AddToDesc(VParamName, VParamValue);
          end;

          // fPos
          with fPos do begin
            // time
            if (wpt_time in fAvail_wpt_params) and UTCDateOK and UTCTimeOK then begin
              VParamName := 'time';
              VParamValue := DateTime_To_ISO8601(UTCDate+UTCTime, False);
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
          for z := Low(z) to High(z) do
          if (z in fAvail_wpt_exts) then begin
            VParamName := c_GPX_wpt_ext_subtag[z];
            VParamValue := SafeSetStringP(fExts[z]);
            // add to description
            _AddToDesc(VParamName, VParamValue);
          end;
      end;
      // extension
      with Pvsagps_GPX_ParserData(AData)^.extensions_data do begin
        for x := Low(x) to High(x) do
        if (x in fAvail_strs) then begin
          VParamName := c_GPX_ext_sasx_subtag[x];
          VParamValue := SafeSetStringP(sasx_strs[x]);
          // add to description
          _AddToDesc(VParamName, VParamValue);
        end;
      end;
      Inc(Result);
    end;
  end;
end;

function TXmlVectorObjects.ParseKmlCoordinatesToArray(
  const ACoordinates: WideString;
  const AForceClose: Boolean
): Integer;
var
  VPosPrev, VPosCur: Integer;
  VCoordLine: WideString;
  VData: TCoordLineData;
  VPoint: TDoublePoint;
  VFirstPos: Integer;
begin
  Result := 0;
  VPosPrev := 0;
  VFirstPos := -1;
  //VPosCur := 0;
  // loop through points
  repeat
    if (VPosPrev >= Length(ACoordinates)) then
      break;

    // get part
    VPosCur := FindNextDelimiterPos(VPosPrev, ACoordinates);
    VCoordLine := System.Copy(
      ACoordinates,
      (VPosPrev+1),
      (VPosCur-VPosPrev-1)
    );

    // parse and add
    if (Length(VCoordLine) > 0) then begin
      if parse_kml_coordinate(VCoordLine, @VData, FFormatPtr^) then begin
        VPoint.X := VData.lon1;
        VPoint.Y := VData.lat0;
        // check closure
        if AForceClose then
        if (VFirstPos < 0) then begin
          VFirstPos := FDoublePointsAggregator.Count;
        end;
        // add to array
        InternalAddPoint(VPoint);
        Inc(Result);
      end;
    end;

    // next
    VPosPrev := VPosCur;
  until False;

  // check closure
  if AForceClose then
  if (Result > 0) then
  if (VFirstPos >= 0) then
  if (not LastSegmentIsClosed(VFirstPos)) then begin
    VPoint := FDoublePointsAggregator.Points^[VFirstPos];
    InternalAddPoint(VPoint);
  end;
end;

function TXmlVectorObjects.PrepareArrayOfPoints: Integer;
begin
  Result := FDoublePointsAggregator.Count;
  if (0 = Result) then begin
    // very first item
    FClosedSegments := 0;
    FOpenedSegments := 0;
  end else begin
    // has some points
    InternalAddPoint(CEmptyDoublePoint);
    Inc(Result);
  end;
end;

procedure TXmlVectorObjects.SafeAddToResult(
  const AItem: IVectorDataItemSimple
);
begin
  if (AItem <> nil) then begin
    SafeMakeResultList;
    FVectorDataItemsResultBuilder.Add(AItem);
  end;
end;

procedure TXmlVectorObjects.SafeMakeResultList;
begin
  if (nil = FVectorDataItemsResultBuilder) then begin
    FVectorDataItemsResultBuilder := FVectorItemSubsetBuilderFactory.Build;
  end;
end;

end.
