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

unit u_XmlInfoSimpleParser;

interface

uses
  Windows,
  Classes,
  SysUtils,
  t_GeoTypes,
  i_BinaryData,
  i_VectorDataFactory,
  i_VectorItmesFactory,
  i_VectorDataItemSimple,
  i_ArchiveReadWriteFactory,
  i_ArchiveReadWrite,
  i_InternalPerformanceCounter,
  i_VectorDataLoader,
  vsagps_public_sysutils,
  vsagps_public_print,
  vsagps_public_parser,
  vsagps_public_gpx,
  vsagps_public_kml,
  vsagps_public_xml_parser;

type
  TXmlInfoSimpleParser = class(TInterfacedObject, IVectorDataLoader)
  private
    FFactory: IVectorItmesFactory;
    FLoadXmlStreamCounter: IInternalPerformanceCounter;
    FFormat: TFormatSettings;
    FArchiveReadWriteFactory: IArchiveReadWriteFactory;
  private
    procedure Internal_ParseXML_UserProc(
      const pUserAuxPointer: Pointer;
      const pPX_Result: Pvsagps_XML_ParserResult;
      const pPX_State: Pvsagps_XML_ParserState
    );
    function Internal_LoadFromStream_Original(
      AStream: TStream;
      const AFactory: IVectorDataFactory
    ): IVectorDataItemList;
  private
    function LoadFromStream(
      AStream: TStream;
      const AFactory: IVectorDataFactory
    ): IVectorDataItemList;
    function Load(
      const AData: IBinaryData;
      const AFactory: IVectorDataFactory
    ): IVectorDataItemList;
  public
    constructor Create(
      const AFactory: IVectorItmesFactory;
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
      const APerfCounterList: IInternalPerformanceCounterList
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_StreamReadOnlyByBinaryData,
  u_VectorDataItemList,
  u_GeoFun;

type
  TParseXML_Aux = record
    opt: Tvsagps_XML_ParserOptions;
    Factory: IVectorDataFactory;
    list: IInterfaceList;
    segment_counter: Integer;
    array_count: Integer;
    array_capacity: Integer;
    array_rect: TDoubleRect;
    array_points: array of TDoublePoint;
  end;
  PParseXML_Aux = ^TParseXML_Aux;

procedure ParseXML_Aux_Cleanup_Array(p: PParseXML_Aux);
begin
  with p^ do begin
    array_count := 0;
    array_capacity := 0;
    SetLength(array_points, 0);
  end;
end;

procedure ParseXML_Aux_AddPointTo_Array(AParseXML_Aux: PParseXML_Aux; const Awpt_point: TDoublePoint);
begin
    // calc rect
    with AParseXML_Aux^ do begin
      if (0 = array_count) then begin
        // very first point of segment
        array_rect.TopLeft := Awpt_point;
        array_rect.BottomRight := Awpt_point;
      end else begin
        // compare bounds
        if array_rect.Left > Awpt_point.X then begin
          array_rect.Left := Awpt_point.X;
        end;
        if array_rect.Right < Awpt_point.X then begin
          array_rect.Right := Awpt_point.X;
        end;
        if array_rect.Top < Awpt_point.y then begin
          array_rect.Top := Awpt_point.y;
        end;
        if array_rect.Bottom > Awpt_point.y then begin
          array_rect.Bottom := Awpt_point.y;
        end;
      end;
    end;

    // allocate more array
    with AParseXML_Aux^ do begin
      if (array_count >= array_capacity) then begin
        if (0 = array_capacity) then begin
          array_capacity := 32;
        end else begin
          array_capacity := array_capacity * 2;
        end;
        SetLength(array_points, array_capacity);
      end;
    end;

    // add
    with AParseXML_Aux^ do begin
      array_points[array_count] := Awpt_point;
      Inc(array_count);
    end;
end;

procedure ParseXML_Aux_AddTrackSegmentToList(
  AParseXML_Aux: PParseXML_Aux;
  const AForcePolyLine: Boolean;
  const AWideStrName, AWideStrDesc: WideString;
  const AItemsFactory: IVectorItmesFactory
);
var
  trk_obj: IVectorDataItemSimple;
begin
  // make list object
  if not Assigned(AParseXML_Aux^.list) then begin
    AParseXML_Aux^.list := TInterfaceList.Create;
  end;
  // trim points to count
  with AParseXML_Aux^ do begin
    if (array_capacity > array_count) then begin
      array_capacity := array_count;
      SetLength(array_points, array_count);
    end;
  end;
  // make object and add it to list
  with AParseXML_Aux^ do begin
    if (0 < array_count) then begin
      if (1 = array_count) then begin
        // single point in track segment - make as point
        trk_obj := AParseXML_Aux^.Factory.BuildPoint('', AWideStrName, AWideStrDesc, array_points[0]);
      end else if (not AForcePolyLine) and DoublePointsEqual(array_points[0], array_points[array_count - 1]) then begin
        // polygon
        trk_obj :=
          AParseXML_Aux^.Factory.BuildPoly(
            '',
            AWideStrName,
            AWideStrDesc,
            AItemsFactory.CreateLonLatPolygon(@array_points[0], array_count)
          );
      end else begin
        // polyline
        trk_obj :=
          AParseXML_Aux^.Factory.BuildPath(
            '',
            AWideStrName,
            AWideStrDesc,
            AItemsFactory.CreateLonLatPath(@array_points[0], array_count)
          );
      end;
      list.Add(trk_obj);
    end;
  end;
end;

procedure rTVSAGPS_ParseXML_UserProc(
  const pUserObjPointer: Pointer;
  const pUserAuxPointer: Pointer;
  const pPX_Options: Pvsagps_XML_ParserOptions;
  const pPX_Result: Pvsagps_XML_ParserResult;
  const pPX_State: Pvsagps_XML_ParserState
); stdcall;
begin
  TXmlInfoSimpleParser(pUserObjPointer).Internal_ParseXML_UserProc(pUserAuxPointer, pPX_Result, pPX_State);
end;

{ TXmlInfoSimpleParser }

constructor TXmlInfoSimpleParser.Create(
  const AFactory: IVectorItmesFactory;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
  const APerfCounterList: IInternalPerformanceCounterList
);
var
  VCounterName: String;
begin
  inherited Create;
  FFactory := AFactory;
  FArchiveReadWriteFactory := AArchiveReadWriteFactory;

  if Assigned(FArchiveReadWriteFactory) then
    VCounterName := 'LoadXmlZipStream'
  else
    VCounterName := 'LoadXmlStream';

  FLoadXmlStreamCounter := APerfCounterList.CreateAndAddNewCounter(VCounterName);
  VSAGPS_PrepareFormatSettings(FFormat);
end;

destructor TXmlInfoSimpleParser.Destroy;
begin
  FLoadXmlStreamCounter := nil;
  inherited;
end;

function TXmlInfoSimpleParser.Internal_LoadFromStream_Original(
  AStream: TStream;
  const AFactory: IVectorDataFactory
): IVectorDataItemList;
var
  tAux: TParseXML_Aux;
begin
  // init
  ZeroMemory(@tAux, sizeof(tAux));
  // for wpt and trk
  Inc(tAux.opt.gpx_options.bParse_trk);
  Inc(tAux.opt.gpx_options.bParse_wpt);
  tAux.Factory := AFactory;
  try
    // parse
    VSAGPS_LoadAndParseXML(Self, @tAux, '', AStream, TRUE, @(tAux.opt), rTVSAGPS_ParseXML_UserProc, FFormat);
    // output result
    if Assigned(tAux.list) then begin
      Result := TVectorDataItemList.Create(tAux.list);
      tAux.list := nil;
    end;
  finally
    tAux.Factory := nil;
    tAux.list := nil;
  end;
end;

procedure TXmlInfoSimpleParser.Internal_ParseXML_UserProc(
  const pUserAuxPointer: Pointer;
  const pPX_Result: Pvsagps_XML_ParserResult;
  const pPX_State: Pvsagps_XML_ParserState
);

var
  VWSName, VWSDesc: WideString;
  wpt_point: TDoublePoint;

  procedure _SetFromParentPlacemark(
  const a_type: Tvsagps_KML_str;
  const a_to_description: Boolean
  );
  var
    VPX_Result: Pvsagps_XML_ParserResult;
  begin
    VPX_Result := pPX_Result;
    repeat
      // noway
      if (nil=VPX_Result) then
        break;

      // check tag
      if (VPX_Result^.kml_data.current_tag = kml_Placemark) then begin
        // Placemark found
        if (a_type in VPX_Result^.kml_data.fAvail_strs) then begin
          // has string
          SafeSetWideStringP(VWSName, VPX_Result^.kml_data.fParamsStrs[a_type]);
          if a_to_description and (0 < Length(VWSName)) then begin
            if (0 < Length(VWSDesc)) then begin
              VWSDesc := VWSDesc + #13#10;
            end;
            VWSDesc := VWSDesc + VWSName;
          end;
        end;

        // done
        Exit;
      end;

      // prev level
      VPX_Result := VPX_Result^.prev_data;
    until FALSE;
  end;

  procedure _SetFromParentTrk(
  const a_type: Tvsagps_GPX_trk_str;
  const a_to_description: Boolean
  );
  begin
    if (nil <> pPX_Result^.prev_data) then begin
      with pPX_Result^.prev_data^.gpx_data.trk_data do begin
        if (a_type in fAvail_trk_strs) then begin
          SafeSetWideStringP(VWSName, fStrs[a_type]);
          if a_to_description and (0 < Length(VWSName)) then begin
            if (0 < Length(VWSDesc)) then begin
              VWSDesc := VWSDesc + #13#10;
            end;
            VWSDesc := VWSDesc + VWSName;
          end;
        end;
      end;
    end;
  end;

  procedure _SetFromWpt(
  const a_type: Tvsagps_GPX_wpt_str;
  const a_to_description: Boolean
  );
  begin
    with pPX_Result^.gpx_data.wpt_data do begin
      if (a_type in fAvail_wpt_strs) then begin
        SafeSetWideStringP(VWSName, fStrs[a_type]);
        if a_to_description and (0 < Length(VWSName)) then begin
          if (0 < Length(VWSDesc)) then begin
            VWSDesc := VWSDesc + #13#10;
          end;
          VWSDesc := VWSDesc + VWSName;
        end;
      end;
    end;
  end;

  procedure _AddSasxFile;
  begin
    if (sasx_file_name in pPX_Result^.gpx_data.extensions_data.fAvail_strs) then begin
      SafeSetWideStringP(VWSName, pPX_Result^.gpx_data.extensions_data.sasx_strs[sasx_file_name]);
      if (0 < Length(VWSName)) then begin
        if (0 < Length(VWSDesc)) then begin
          VWSDesc := VWSDesc + #13#10;
        end;
        VWSDesc := VWSDesc + VWSName;
      end;
    end;
  end;

  procedure _AddWptToList;
  var
    wpt_iface: IVectorDataItemPoint;
  begin
    // make list object
    if not Assigned(PParseXML_Aux(pUserAuxPointer)^.list) then begin
      PParseXML_Aux(pUserAuxPointer)^.list := TInterfaceList.Create;
    end;
    // create object
    wpt_iface := PParseXML_Aux(pUserAuxPointer)^.Factory.BuildPoint('', VWSName, VWSDesc, wpt_point);
    // add object to list
    PParseXML_Aux(pUserAuxPointer)^.list.Add(wpt_iface);
  end;

  function _GetPointForGPX(const AWptData: Tvsagps_GPX_wpt_data): Boolean;
  begin
    with AWptData.fPos do begin
      Result := PositionOK;
      if Result then begin
        wpt_point.X := PositionLon;
        wpt_point.Y := PositionLat;
      end;
    end;
  end;

  function _GetPointForKML(const AKmlData: Tvsagps_KML_ParserData): Boolean;
  var
    VCoordinates: WideString;
    VData: TCoordLineData;
  begin
    Result := (AKmlData.fParamsStrs[kml_coordinates] <> nil);
    if Result then begin
      SafeSetWideStringP(VCoordinates, AKmlData.fParamsStrs[kml_coordinates]);
      Result := parse_kml_coordinate(VCoordinates, @VData, FFormat);
      if Result then begin
        wpt_point.X := VData.lon1;
        wpt_point.Y := VData.lat0;
      end;
    end;
  end;

  function _ParseCoordinatesForKML(const AKmlData: Tvsagps_KML_ParserData): Boolean;
  var
    VCoordinates: WideString;
    VData: TCoordLineData;
    VPointsAdded: Integer;
    VSepPos: Integer;
    VCoordLine: String;
  begin
    Result := (AKmlData.fParamsStrs[kml_coordinates] <> nil);
    if Result then begin
      SafeSetWideStringP(VCoordinates, AKmlData.fParamsStrs[kml_coordinates]);
      VPointsAdded := 0;
      // loop through points
      repeat
        if Length(VCoordinates)=0 then
          break;
        VSepPos:=System.Pos(' ',VCoordinates);
        if (VSepPos>0) then begin
          // with delimiter
          VCoordLine := System.Copy(VCoordinates, 1, VSepPos-1);
          System.Delete(VCoordinates, 1, VSepPos);
        end else begin
          // no delimiter
          VCoordLine := VCoordinates;
          VCoordinates := '';
        end;

        if (Length(VCoordLine)>0) then
        if parse_kml_coordinate(VCoordLine, @VData, FFormat) then begin
          wpt_point.X := VData.lon1;
          wpt_point.Y := VData.lat0;
          ParseXML_Aux_AddPointTo_Array(PParseXML_Aux(pUserAuxPointer), wpt_point);
          Inc(VPointsAdded);
        end;
      until FALSE;
      // check
      Result := (VPointsAdded>0);
    end;
  end;

begin
  // if aborted
  if pPX_State^.aborted_by_user then begin
    Exit;
  end;

  // kml
  if (xsf_KML = pPX_State^.src_fmt) then begin
    // skip some tags
    if (xtd_BeforeSub = pPX_State^.tag_disposition) then begin
      if (pPX_Result^.kml_data.subitem_tag in [kml_NetworkLink, kml_NetworkLinkControl]) then begin
        pPX_State^.skip_sub := TRUE;
        Exit;
      end;
    end;

    // check some tags with coordinates on Closing
    if (xtd_Close = pPX_State^.tag_disposition) then
    case pPX_Result^.kml_data.current_tag of
      kml_LinearRing,kml_LineString: begin
        // <Placemark><MultiGeometry><Polygon><outerBoundaryIs><LinearRing><coordinates>
        // <Placemark><LineString><coordinates>
        // make description
        _SetFromParentPlacemark(kml_description, TRUE);
        // make name
        _SetFromParentPlacemark(kml_name, FALSE);

        // make polyline (path) object
        // if LineString - force PolyLine mode
        if _ParseCoordinatesForKML(pPX_Result^.kml_data) then
          ParseXML_Aux_AddTrackSegmentToList(
            PParseXML_Aux(pUserAuxPointer),
            (pPX_Result^.kml_data.current_tag=kml_LineString),
            VWSName, VWSDesc,
            FFactory);

        // clear points
        ParseXML_Aux_Cleanup_Array(pUserAuxPointer);
      end;
      kml_Point: begin
        // <Placemark><Point><coordinates>
        // make description
        _SetFromParentPlacemark(kml_description, TRUE);
        //_SetFromParentPlacemark(kml_href, TRUE);
        //_SetFromParentPlacemark(kml_key, TRUE);
        //_SetFromParentPlacemark(kml_phoneNumber, TRUE);
        //_SetFromParentPlacemark(kml_address, TRUE);
        //_SetFromParentPlacemark(kml_sourceHref, TRUE);
        //_SetFromParentPlacemark(kml_text, TRUE);
        // make name
        _SetFromParentPlacemark(kml_name, FALSE);
        // make point object
        if _GetPointForKML(pPX_Result^.kml_data) then begin
          _AddWptToList;
        end;
      end;
    end;

    // done
    Exit;
  end;

  // only gpx
  if (xsf_GPX <> pPX_State^.src_fmt) then begin
    Exit;
  end;

  // skip some tags
  if (xtd_BeforeSub = pPX_State^.tag_disposition) then begin
    if (pPX_Result^.gpx_data.subitem_tag in [gpx_rte, gpx_metadata]) then begin
      pPX_State^.skip_sub := TRUE;
      Exit;
    end;
  end;

  // switch by tag
  case pPX_Result^.gpx_data.current_tag of
    gpx_trk: begin
      // start new track - reset segment counter
      if (xtd_Open = pPX_State^.tag_disposition) then begin
        with PParseXML_Aux(pUserAuxPointer)^ do begin
          segment_counter := 0;
        end;
      end;
    end;
    gpx_trkseg: begin
      // track segment header - create path or polygon when completed
      if (xtd_Close = pPX_State^.tag_disposition) then begin
        if (0 < PParseXML_Aux(pUserAuxPointer)^.array_count) then begin
          // make track name and desc (get data from parent)
          _SetFromParentTrk(trk_desc, TRUE);
          _SetFromParentTrk(trk_cmt, TRUE);
          _SetFromParentTrk(trk_src, TRUE);
          _SetFromParentTrk(trk_name, FALSE);

          // make track segment object
          ParseXML_Aux_AddTrackSegmentToList(
            PParseXML_Aux(pUserAuxPointer),
            FALSE,
            VWSName, VWSDesc,
            FFactory);
            
          // clear points and increment segment counter
          ParseXML_Aux_Cleanup_Array(pUserAuxPointer);
          Inc(PParseXML_Aux(pUserAuxPointer)^.segment_counter);
        end;
      end;
    end;
    gpx_trkpt: begin
      // track point - lon/lat as attributes
      if (xtd_ReadAttributes = pPX_State^.tag_disposition) then begin
        // add to current track object and jump to end of track
        pPX_State^.skip_current := TRUE;
        if _GetPointForGPX(pPX_Result^.gpx_data.wpt_data) then begin
          // add to array of points
          ParseXML_Aux_AddPointTo_Array(PParseXML_Aux(pUserAuxPointer), wpt_point);
        end;
      end;
    end;
    gpx_wpt: begin
      // waypoint - single item
      if (xtd_Close = pPX_State^.tag_disposition) then begin
        // make description
        _SetFromWpt(wpt_desc, TRUE);
        _SetFromWpt(wpt_cmt, TRUE);
        _SetFromWpt(wpt_src, TRUE);
        // add file from sasx
        _AddSasxFile;
        // make name
        _SetFromWpt(wpt_name, FALSE);
        // make point object
        if _GetPointForGPX(pPX_Result^.gpx_data.wpt_data) then begin
          _AddWptToList;
        end;
      end;
    end;
  end;
end;

function TXmlInfoSimpleParser.Load(
  const AData: IBinaryData;
  const AFactory: IVectorDataFactory
): IVectorDataItemList;
var
  VStream: TStreamReadOnlyByBinaryData;
begin
  Result := nil;
  VStream := TStreamReadOnlyByBinaryData.Create(AData);
  try
    Result := LoadFromStream(VStream, AFactory);
  finally
    VStream.Free;
  end;
end;

function TXmlInfoSimpleParser.LoadFromStream(
  AStream: TStream;
  const AFactory: IVectorDataFactory
): IVectorDataItemList;
var
  VCounterContext: TInternalPerformanceCounterContext;
  // for unzip
  I: Integer;
  VZip: IArchiveReader;
  VItemsCount: Integer;
  VMemStream: TMemoryStream;
  VStreamKml: TStream;
  VIndex: Integer;
  VData: IBinaryData;
  VFileName: string;
begin
  Result := nil;
  VCounterContext := FLoadXmlStreamCounter.StartOperation;
  try
    if Assigned(FArchiveReadWriteFactory) then begin
      // read from archive
      VMemStream := TMemoryStream.Create;
      try
        VMemStream.LoadFromStream(AStream);
        VMemStream.Position := 0;
        VZip := FArchiveReadWriteFactory.CreateZipReaderByStream(VMemStream);
        VItemsCount := VZip.GetItemsCount;
        if VItemsCount > 0 then begin
          VData := VZip.GetItemByName('doc.kml');
          if VData = nil then begin
            VIndex := 0;
            for I := 0 to VItemsCount - 1 do begin
              if ExtractFileExt(VZip.GetItemNameByIndex(I)) = '.kml' then begin
                VIndex := I;
                Break;
              end;
            end;
            VData := VZip.GetItemByIndex(VIndex, VFileName);
          end;
          VStreamKml := TStreamReadOnlyByBinaryData.Create(VData);
          try
            Result := Internal_LoadFromStream_Original(VStreamKml, AFactory);
          finally
            VStreamKml.Free;
          end;
        end;
      finally
        FreeAndNil(VMemStream);
      end;
      
    end else begin
      // read from single simple source
      Result := Internal_LoadFromStream_Original(AStream, AFactory);
    end;
  finally
    FLoadXmlStreamCounter.FinishOperation(VCounterContext);
  end;
end;

end.
