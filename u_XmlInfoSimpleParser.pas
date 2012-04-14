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
  i_InternalPerformanceCounter,
  i_VectorDataLoader,
  vsagps_public_sysutils,
  vsagps_public_print,
  vsagps_public_gpx,
  vsagps_public_xml_parser;

type
  TXmlInfoSimpleParser = class(TInterfacedObject, IVectorDataLoader)
  private
    FFactory: IVectorItmesFactory;
    FLoadXmlStreamCounter: IInternalPerformanceCounter;
    FFormat: TFormatSettings;
  protected
    procedure Internal_ParseXML_UserProc(
      const pUserAuxPointer: Pointer;
      const pPX_Result: Pvsagps_XML_ParserResult;
      const pPX_State: Pvsagps_XML_ParserState
    );
  protected
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
    array_count:=0;
    array_capacity:=0;
    SetLength(array_points,0);
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
  const APerfCounterList: IInternalPerformanceCounterList
);
begin
  FFactory := AFactory;
  FLoadXmlStreamCounter := APerfCounterList.CreateAndAddNewCounter('LoadXmlStream');
  VSAGPS_PrepareFormatSettings(FFormat);
end;

destructor TXmlInfoSimpleParser.Destroy;
begin
  FLoadXmlStreamCounter:=nil;
  inherited;
end;

procedure TXmlInfoSimpleParser.Internal_ParseXML_UserProc(
  const pUserAuxPointer: Pointer;
  const pPX_Result: Pvsagps_XML_ParserResult;
  const pPX_State: Pvsagps_XML_ParserState
);

var
  VWSName, VWSDesc: WideString;
  wpt_point: TDoublePoint;

  procedure _SetFromParentTrk(const a_type: Tvsagps_GPX_trk_str; const a_to_description: Boolean);
  begin
    if (nil<>pPX_Result^.prev_data) then
    with pPX_Result^.prev_data^.gpx_data.trk_data do
    if (a_type in fAvail_trk_strs) then begin
      SafeSetWideStringP(VWSName, fStrs[a_type]);
      if a_to_description and (0<Length(VWSName)) then begin
        if (0<Length(VWSDesc)) then
          VWSDesc:=VWSDesc+#13#10;
        VWSDesc:=VWSDesc+VWSName;
      end;
    end;
  end;

  procedure _SetFromWpt(const a_type: Tvsagps_GPX_wpt_str; const a_to_description: Boolean);
  begin
    with pPX_Result^.gpx_data.wpt_data do
    if (a_type in fAvail_wpt_strs) then begin
      SafeSetWideStringP(VWSName, fStrs[a_type]);
      if a_to_description and (0<Length(VWSName)) then begin
        if (0<Length(VWSDesc)) then
          VWSDesc:=VWSDesc+#13#10;
        VWSDesc:=VWSDesc+VWSName;
      end;
    end;
  end;

  procedure _AddSasxFile;
  begin
    if (sasx_file_name in pPX_Result^.gpx_data.extensions_data.fAvail_strs) then begin
      SafeSetWideStringP(VWSName, pPX_Result^.gpx_data.extensions_data.sasx_strs[sasx_file_name]);
      if (0<Length(VWSName)) then begin
        if (0<Length(VWSDesc)) then
          VWSDesc:=VWSDesc+#13#10;
        VWSDesc:=VWSDesc+VWSName;
      end;
    end;
  end;

  procedure _AddWptToList;
  var wpt_iface: IVectorDataItemPoint;
  begin
    // make list object
    if not Assigned(PParseXML_Aux(pUserAuxPointer)^.list) then
      PParseXML_Aux(pUserAuxPointer)^.list:=TInterfaceList.Create;
    // create object
    wpt_iface:=PParseXML_Aux(pUserAuxPointer)^.Factory.BuildPoint('', VWSName, VWSDesc, wpt_point);
    // add object to list
    PParseXML_Aux(pUserAuxPointer)^.list.Add(wpt_iface);
  end;

  procedure _AddTrackSegmentToList;
  var
    trk_obj: IVectorDataItemSimple;
  begin
    // make list object
    if not Assigned(PParseXML_Aux(pUserAuxPointer)^.list) then
      PParseXML_Aux(pUserAuxPointer)^.list:=TInterfaceList.Create;
    // trim points to count
    with PParseXML_Aux(pUserAuxPointer)^ do
    if (array_capacity>array_count) then begin
      array_capacity:=array_count;
      SetLength(array_points, array_count);
    end;
    // make object and add it to list
    with PParseXML_Aux(pUserAuxPointer)^ do
    if (0<array_count) then begin
      if (1=array_count) then begin
        // single point in track segment - make as point
        trk_obj:=PParseXML_Aux(pUserAuxPointer)^.Factory.BuildPoint('', VWSName, VWSDesc, array_points[0]);
      end else if DoublePointsEqual(array_points[0], array_points[array_count-1]) then begin
        // polygon
        trk_obj :=
          PParseXML_Aux(pUserAuxPointer)^.Factory.BuildPoly(
            '',
            VWSName,
            VWSDesc,
            FFactory.CreateLonLatPolygon(@array_points[0], array_count)
          );
      end else begin
        // polyline
        trk_obj :=
          PParseXML_Aux(pUserAuxPointer)^.Factory.BuildPath(
            '',
            VWSName,
            VWSDesc,
            FFactory.CreateLonLatPath(@array_points[0], array_count)
          );
      end;
      list.Add(trk_obj);
    end;
  end;

  function _GetPoint: Boolean;
  begin
    with pPX_Result^.gpx_data.wpt_data.fPos do begin
      Result := PositionOK;
      if Result then begin
        wpt_point.X:=PositionLon;
        wpt_point.Y:=PositionLat;
      end;
    end;
  end;

  procedure _AddToTrack;
  begin
    // calc rect
    with PParseXML_Aux(pUserAuxPointer)^ do begin
      if (0=array_count) then begin
        // very first point of segment
        array_rect.TopLeft:=wpt_point;
        array_rect.BottomRight:=wpt_point;
      end else begin
        // compare bounds
        if array_rect.Left > wpt_point.X then begin
          array_rect.Left := wpt_point.X;
        end;
        if array_rect.Right < wpt_point.X then begin
          array_rect.Right := wpt_point.X;
        end;
        if array_rect.Top < wpt_point.y then begin
          array_rect.Top := wpt_point.y;
        end;
        if array_rect.Bottom > wpt_point.y then begin
          array_rect.Bottom := wpt_point.y;
        end;
      end;
    end;
    
    // allocate more array
    with PParseXML_Aux(pUserAuxPointer)^ do
    if (array_count>=array_capacity) then begin
      if (0=array_capacity) then
        array_capacity:=32
      else
        array_capacity:=array_capacity*2;
      SetLength(array_points, array_capacity);
    end;

    // add
    with PParseXML_Aux(pUserAuxPointer)^ do begin
      array_points[array_count] := wpt_point;
      Inc(array_count);
    end;
  end;


begin
  // if aborted
  if pPX_State^.aborted_by_user then
    Exit;

  // only gpx
  if (xsf_GPX<>pPX_State^.src_fmt) then
    Exit;

  // skip some tags
  if (xtd_BeforeSub=pPX_State^.tag_disposition) then
  if (pPX_Result^.gpx_data.subitem_tag in [gpx_rte, gpx_metadata]) then begin
    pPX_State^.skip_sub:=TRUE;
    Exit;
  end;

  // switch by tag
  case pPX_Result^.gpx_data.current_tag of
    gpx_trk: begin
      // start new track - reset segment counter
      if (xtd_Open=pPX_State^.tag_disposition) then
        with PParseXML_Aux(pUserAuxPointer)^ do
          segment_counter := 0;
    end;
    gpx_trkseg: begin
      // track segment header - create path or polygon when completed
      if (xtd_Close=pPX_State^.tag_disposition) then
      if (0<PParseXML_Aux(pUserAuxPointer)^.array_count) then begin
        // make track name and desc (get data from parent)
        _SetFromParentTrk(trk_desc, TRUE);
        _SetFromParentTrk(trk_cmt, TRUE);
        _SetFromParentTrk(trk_src, TRUE);
        _SetFromParentTrk(trk_name, FALSE);
        // make track segment object
        _AddTrackSegmentToList;
        // clear points and increment segment counter
        ParseXML_Aux_Cleanup_Array(pUserAuxPointer);
        Inc(PParseXML_Aux(pUserAuxPointer)^.segment_counter);
      end;
    end;
    gpx_trkpt: begin
      // track point - lon/lat as attributes
      if (xtd_ReadAttributes=pPX_State^.tag_disposition) then begin
        // add to current track object and jump to end of track
        pPX_State^.skip_current:=TRUE;
        if _GetPoint then begin
          // add to array of points
          _AddToTrack;
        end;
      end;
    end;
    gpx_wpt: begin
      // waypoint - single item
      if (xtd_Close=pPX_State^.tag_disposition) then begin
        // make description
        _SetFromWpt(wpt_desc, TRUE);
        _SetFromWpt(wpt_cmt, TRUE);
        _SetFromWpt(wpt_src, TRUE);
        // add file from sasx
        _AddSasxFile;
        // make name
        _SetFromWpt(wpt_name, FALSE);
        // make point object
        if _GetPoint then
          _AddWptToList;
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
  tAux: TParseXML_Aux;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  Result := nil;
  VCounterContext := FLoadXmlStreamCounter.StartOperation;
  try
    // init
    ZeroMemory(@tAux, sizeof(tAux));
    // for wpt and trk
    Inc(tAux.opt.gpx_options.bParse_trk);
    Inc(tAux.opt.gpx_options.bParse_wpt);
    tAux.Factory := AFactory;
    // parse
    VSAGPS_LoadAndParseXML(Self, @tAux, '', AStream, TRUE, @(tAux.opt), rTVSAGPS_ParseXML_UserProc, FFormat);
    // output result
    if Assigned(tAux.list) then begin
      Result := TVectorDataItemList.Create(tAux.list);
      tAux.list := nil;
    end;
    tAux.Factory := nil;
  finally
    FLoadXmlStreamCounter.FinishOperation(VCounterContext);
  end;
end;

end.
