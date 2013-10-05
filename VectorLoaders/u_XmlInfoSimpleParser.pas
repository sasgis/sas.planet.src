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
  i_VectorItemSubsetBuilder,
  i_VectorItemSubset,
  i_VectorItemsFactory,
  i_VectorDataItemSimple,
  i_ArchiveReadWriteFactory,
  i_ArchiveReadWrite,
  i_InternalPerformanceCounter,
  i_VectorDataLoader,
  i_XmlVectorObjects,
  i_InterfaceListSimple,
  u_BaseInterfacedObject,
  vsagps_public_sysutils,
  vsagps_public_print,
  vsagps_public_parser,
  vsagps_public_gpx,
  vsagps_public_kml,
  vsagps_public_xml_parser;

type
  TXmlInfoSimpleParser = class(TBaseInterfacedObject, IVectorDataLoader)
  private
    FVectorGeometryLonLatFactory: IVectorGeometryLonLatFactory;
    FVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FLoadXmlStreamCounter: IInternalPerformanceCounter;
    FAllowMultiParts: Boolean;
    FFormat: TFormatSettings;
  private
    procedure Internal_ParseXML_UserProc(
      const AXmlVectorObjects: IXmlVectorObjects;
      const pPX_Result: Pvsagps_XML_ParserResult;
      const pPX_State: Pvsagps_XML_ParserState
    );
  private
    procedure Internal_CloseTRK(
      const AXmlVectorObjects: IXmlVectorObjects;
      const pPX_Result: Pvsagps_XML_ParserResult
    );
    procedure Internal_CloseWPT(
      const AXmlVectorObjects: IXmlVectorObjects;
      const pPX_Result: Pvsagps_XML_ParserResult
    );
    procedure Internal_CloseMark(
      const AXmlVectorObjects: IXmlVectorObjects;
      const pPX_Result: Pvsagps_XML_ParserResult
    );
    procedure Internal_CloseLineString(
      const AXmlVectorObjects: IXmlVectorObjects;
      const pPX_Result: Pvsagps_XML_ParserResult
    );
    procedure Internal_CloseLinearRing(
      const AXmlVectorObjects: IXmlVectorObjects;
      const pPX_Result: Pvsagps_XML_ParserResult
    );
    procedure Internal_ClosePoint(
      const AXmlVectorObjects: IXmlVectorObjects;
      const pPX_Result: Pvsagps_XML_ParserResult
    );
  private
    function Internal_LoadFromStream_Original(
      const AStream: TStream;
      const AIdData: Pointer;
      const AVectorGeometryLonLatFactory: IVectorDataFactory
    ): IVectorItemSubset;
    function LoadFromStream(
      const AStream: TStream;
      const AIdData: Pointer;
      const AVectorGeometryLonLatFactory: IVectorDataFactory
    ): IVectorItemSubset;
  private
    function Load(
      const AData: IBinaryData;
      const AIdData: Pointer;
      const AVectorGeometryLonLatFactory: IVectorDataFactory
    ): IVectorItemSubset;
  public
    constructor Create(
      const AVectorGeometryLonLatFactory: IVectorGeometryLonLatFactory;
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AAllowMultiParts: Boolean;
      const APerfCounterList: IInternalPerformanceCounterList
    );
  end;

implementation

uses
  u_XmlVectorObjects,
  u_StreamReadOnlyByBinaryData,
  u_GeoFun;

procedure rTVSAGPS_ParseXML_UserProc(
  const pUserObjPointer: Pointer;
  const pUserAuxPointer: Pointer;
  const pPX_Options: Pvsagps_XML_ParserOptions;
  const pPX_Result: Pvsagps_XML_ParserResult;
  const pPX_State: Pvsagps_XML_ParserState
); stdcall;
begin
  TXmlInfoSimpleParser(pUserObjPointer).Internal_ParseXML_UserProc(
    IXmlVectorObjects(pUserAuxPointer),
    pPX_Result,
    pPX_State
  );
end;

function GetPointForGPX(const AWptData: Tvsagps_GPX_wpt_data; out AWptPoint: TDoublePoint): Boolean;
begin
  with AWptData.fPos do begin
    Result := PositionOK;
    if Result then begin
      AWptPoint.X := PositionLon;
      AWptPoint.Y := PositionLat;
    end;
  end;
end;

{ TXmlInfoSimpleParser }

constructor TXmlInfoSimpleParser.Create(
  const AVectorGeometryLonLatFactory: IVectorGeometryLonLatFactory;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AAllowMultiParts: Boolean;
  const APerfCounterList: IInternalPerformanceCounterList
);
begin
  inherited Create;
  FVectorGeometryLonLatFactory := AVectorGeometryLonLatFactory;
  FVectorItemSubsetBuilderFactory := AVectorItemSubsetBuilderFactory;
  FAllowMultiParts := AAllowMultiParts;

  if APerfCounterList <> nil then begin
    FLoadXmlStreamCounter := APerfCounterList.CreateAndAddNewCounter('LoadXmlStream');
  end;
  VSAGPS_PrepareFormatSettings(FFormat);
end;

procedure TXmlInfoSimpleParser.Internal_CloseLinearRing(
  const AXmlVectorObjects: IXmlVectorObjects;
  const pPX_Result: Pvsagps_XML_ParserResult
);
var
  VCoordinates: WideString;
  VInner: Boolean;
  VPX_Result: Pvsagps_XML_ParserResult;
begin
  with pPX_Result^.kml_data do begin
    if (fParamsStrs[kml_coordinates] <> nil) then begin
      VCoordinates := SafeSetStringP(fParamsStrs[kml_coordinates]);

      // check if inner
      VInner := False;
      VPX_Result := pPX_Result;
      repeat
        // check
        if (nil = VPX_Result) then begin
          break;
        end;

        // check tag
        case VPX_Result^.kml_data.current_tag of
          kml_innerBoundaryIs: begin
            VInner := True;
            break;
          end;
          kml_outerBoundaryIs, kml_Placemark: begin
            break;
          end;
        end;

        // prev level
        VPX_Result := VPX_Result^.prev_data;
      until False;

      // call
      AXmlVectorObjects.CloseKmlLinearRing(VCoordinates, VInner);
    end;
  end;
end;

procedure TXmlInfoSimpleParser.Internal_CloseLineString(
  const AXmlVectorObjects: IXmlVectorObjects;
  const pPX_Result: Pvsagps_XML_ParserResult
);
var
  VCoordinates: WideString;
begin
  with pPX_Result^.kml_data do begin
    if (fParamsStrs[kml_coordinates] <> nil) then begin
      VCoordinates := SafeSetStringP(fParamsStrs[kml_coordinates]);
      AXmlVectorObjects.CloseKmlLineString(VCoordinates);
    end;
  end;
end;

procedure TXmlInfoSimpleParser.Internal_CloseMark(
  const AXmlVectorObjects: IXmlVectorObjects;
  const pPX_Result: Pvsagps_XML_ParserResult
);
begin
  // do it
  AXmlVectorObjects.CloseMarkObject(
    @(pPX_Result^.kml_data),
    cmom_KML
  );
end;

procedure TXmlInfoSimpleParser.Internal_ClosePoint(
  const AXmlVectorObjects: IXmlVectorObjects;
  const pPX_Result: Pvsagps_XML_ParserResult
);
var
  VCoordinates: WideString;
begin
  with pPX_Result^.kml_data do begin
    if (fParamsStrs[kml_coordinates] <> nil) then begin
      VCoordinates := SafeSetStringP(fParamsStrs[kml_coordinates]);
      AXmlVectorObjects.CloseKmlPoint(VCoordinates);
    end;
  end;
end;

procedure TXmlInfoSimpleParser.Internal_CloseTRK(
  const AXmlVectorObjects: IXmlVectorObjects;
  const pPX_Result: Pvsagps_XML_ParserResult
);
begin
  // do it
  AXmlVectorObjects.CloseMarkObject(
    @(pPX_Result^.gpx_data),
    cmom_GPX_TRK
  );
end;

procedure TXmlInfoSimpleParser.Internal_CloseWPT(
  const AXmlVectorObjects: IXmlVectorObjects;
  const pPX_Result: Pvsagps_XML_ParserResult
);
begin
  // do it
  AXmlVectorObjects.CloseMarkObject(
    @(pPX_Result^.gpx_data),
    cmom_GPX_WPT
  );
end;

function TXmlInfoSimpleParser.Internal_LoadFromStream_Original(
  const AStream: TStream;
  const AIdData: Pointer;
  const AVectorGeometryLonLatFactory: IVectorDataFactory
): IVectorItemSubset;
var
  VXmlVectorObjects: IXmlVectorObjects;
  VParserOptions: Tvsagps_XML_ParserOptions;
begin
  Result := nil;

  // init
  VXmlVectorObjects := TXmlVectorObjects.Create(
    False, // use True for wiki
    False, // use True for wiki
    @FFormat,
    AIdData,
    FAllowMultiParts,
    FVectorItemSubsetBuilderFactory,
    AVectorGeometryLonLatFactory,
    FVectorGeometryLonLatFactory
  );

  // xml parser options
  FillChar(VParserOptions, SizeOf(VParserOptions), 0);

  // for wpt and trk
  Inc(VParserOptions.gpx_options.bParse_trk);
  Inc(VParserOptions.gpx_options.bParse_wpt);

  // parse
  VSAGPS_LoadAndParseXML(
    Self,
    Pointer(VXmlVectorObjects),
    '',
    AStream,
    True,
    @VParserOptions,
    rTVSAGPS_ParseXML_UserProc,
    FFormat
  );

  // output result
  Result := VXmlVectorObjects.VectorDataItemsResult;
end;

procedure TXmlInfoSimpleParser.Internal_ParseXML_UserProc(
  const AXmlVectorObjects: IXmlVectorObjects;
  const pPX_Result: Pvsagps_XML_ParserResult;
  const pPX_State: Pvsagps_XML_ParserState
);
const
  c_KML_Skipped: set of Tvsagps_KML_main_tag = [
    kml_LookAt,
    kml_NetworkLink,
    kml_NetworkLinkControl,
    kml_Region
  ];
  c_GPX_Skipped: set of Tvsagps_GPX_main_tag = [
    gpx_metadata,
    gpx_rte
  ];
var
  VWptPoint: TDoublePoint;
begin
  // if aborted
  if pPX_State^.aborted_by_user then begin
    Exit;
  end;

  // kml
  if (xsf_KML = pPX_State^.src_fmt) then begin
    // skip some tags
    if (xtd_BeforeSub = pPX_State^.tag_disposition) then begin
      if (pPX_Result^.kml_data.subitem_tag in c_KML_Skipped) then begin
        pPX_State^.skip_sub := True;
        Exit;
      end;
    end;

    case pPX_Result^.kml_data.current_tag of
      //kml_innerBoundaryIs: begin
        // только полигон
      //end;
      kml_LinearRing: begin
        // только полигон
        case pPX_State^.tag_disposition of
          xtd_Close: begin
            Internal_CloseLinearRing(AXmlVectorObjects, pPX_Result);
          end;
        end;
      end;
      kml_LineString: begin
        // обычно полилиния, но в вики - полигон
        case pPX_State^.tag_disposition of
          xtd_Close: begin
            Internal_CloseLineString(AXmlVectorObjects, pPX_Result);
          end;
        end;
      end;
      kml_MultiGeometry: begin
        // внутри может быть что угодно
        case pPX_State^.tag_disposition of
          xtd_Open: begin
            AXmlVectorObjects.OpenMultiGeometry;
          end;
          xtd_Close: begin
            AXmlVectorObjects.CloseMultiGeometry;
          end;
        end;
      end;
      //kml_outerBoundaryIs: begin
        // только полигон
      //end;
      kml_Placemark: begin
        // объект метки
        case pPX_State^.tag_disposition of
          xtd_Open: begin
            AXmlVectorObjects.OpenMarkObject;
          end;
          xtd_Close: begin
            // get info
            Internal_CloseMark(AXmlVectorObjects, pPX_Result);
          end;
        end;
      end;
      kml_Point: begin
        // точка (может быть внутри MultiGeometry)
        case pPX_State^.tag_disposition of
          xtd_Close: begin
            Internal_ClosePoint(AXmlVectorObjects, pPX_Result);
          end;
        end;
      end;
      kml_Polygon: begin
        // полигон (может быть внутри MultiGeometry)
        case pPX_State^.tag_disposition of
          xtd_Close: begin
            AXmlVectorObjects.CloseKmlPolygon;
          end;
        end;
      end;

      // gx:
      kml_gx_coord: begin
        // точка (мульти)трека
        case pPX_State^.tag_disposition of
          xtd_Close: begin
            with pPX_Result^.kml_data do
            if (kml_latitude in fAvail_params) and (kml_longitude in fAvail_params) then begin
              VWptPoint.X := fValues.longitude;
              VWptPoint.Y := fValues.latitude;
              AXmlVectorObjects.AddTrackPoint(VWptPoint);
            end;
          end;
        end;
      end;
      kml_gx_MultiTrack: begin
        // мультитрек
        case pPX_State^.tag_disposition of
          xtd_Open: begin
            AXmlVectorObjects.OpenMultiTrack;
          end;
          xtd_Close: begin
            AXmlVectorObjects.CloseMultiTrack;
          end;
        end;
      end;
      kml_gx_Track: begin
        // отдельный трек или кусок мультитрека
        case pPX_State^.tag_disposition of
          xtd_Open: begin
            // open new track segment or open single track
            AXmlVectorObjects.OpenTrackSegment;
          end;
          xtd_Close: begin
            // close track segment or close single track
            AXmlVectorObjects.CloseTrackSegment;
          end;
        end;
      end;

      // appearance
      kml_LineStyle: begin
        // параметры рисования линии
        case pPX_State^.tag_disposition of
          xtd_Close: begin
            // пропихнуть наверх color и width
            if (pPX_Result^.prev_data <> nil) then begin
              VSAGPS_KML_ShiftParam(pPX_Result, kml_color);
              VSAGPS_KML_ShiftParam(pPX_Result, kml_width);
            end;
          end;
        end;
      end;
      kml_PolyStyle: begin
        // параметры рисования полигона
        case pPX_State^.tag_disposition of
          xtd_Close: begin
            // пропихнуть наверх bgColor и fill
            if (pPX_Result^.prev_data <> nil) then begin
              VSAGPS_KML_ShiftParam(pPX_Result, kml_bgColor);
              VSAGPS_KML_ShiftParam(pPX_Result, kml_fill);
            end;
          end;
        end;
      end;
      kml_LabelStyle: begin
        // параметры рисования текстовой метки
        case pPX_State^.tag_disposition of
          xtd_Close: begin
            // пропихнуть наверх textColor и tileSize (вместо scale)
            if (pPX_Result^.prev_data <> nil) then begin
              VSAGPS_KML_ShiftParam(pPX_Result, kml_textColor);
              VSAGPS_KML_ShiftParam(pPX_Result, kml_tileSize);
            end;
          end;
        end;
      end;
      kml_BalloonStyle: begin
        // параметры рисования иконки
        case pPX_State^.tag_disposition of
          xtd_Close: begin
            // пропихнуть наверх bgColor
            if (pPX_Result^.prev_data <> nil) then begin
              VSAGPS_KML_ShiftParam(pPX_Result, kml_bgColor);
            end;
          end;
        end;
      end;
      kml_IconStyle: begin
        // параметры рисования иконки
        case pPX_State^.tag_disposition of
          xtd_Close: begin
            // пропихнуть наверх scale
            if (pPX_Result^.prev_data <> nil) then begin
              VSAGPS_KML_ShiftParam(pPX_Result, kml_scale_);
            end;
          end;
        end;
      end;
      kml_Style: begin
        // параметры рисования
        case pPX_State^.tag_disposition of
          xtd_Close: begin
            // пропихнуть наверх все параметры *Style
            if (pPX_Result^.prev_data <> nil) then begin
              VSAGPS_KML_ShiftParam(pPX_Result, kml_color);
              VSAGPS_KML_ShiftParam(pPX_Result, kml_width);
              VSAGPS_KML_ShiftParam(pPX_Result, kml_bgColor);
              VSAGPS_KML_ShiftParam(pPX_Result, kml_fill);
              VSAGPS_KML_ShiftParam(pPX_Result, kml_textColor);
              VSAGPS_KML_ShiftParam(pPX_Result, kml_tileSize);
              VSAGPS_KML_ShiftParam(pPX_Result, kml_scale_);
            end;
          end;
        end;
      end;
    end;

    // done
    Exit;
  end; { end of kml }

  // gpx
  if (xsf_GPX = pPX_State^.src_fmt) then begin
    // skip some tags
    if (xtd_BeforeSub = pPX_State^.tag_disposition) then begin
      if (pPX_Result^.gpx_data.subitem_tag in c_GPX_Skipped) then begin
        pPX_State^.skip_sub := True;
        Exit;
      end;
    end;

    // switch by tag
    case pPX_Result^.gpx_data.current_tag of
      gpx_trk: begin
        // trk - entire track object
        case pPX_State^.tag_disposition of
          xtd_Open: begin
            // тут будет новая метка
            AXmlVectorObjects.OpenMarkObject;
            // начинаем мультиобъект в терминах KML
            AXmlVectorObjects.OpenMultiGeometry;
          end;
          xtd_Close: begin
            // заканчиваем мультиобъект в терминах KML
            AXmlVectorObjects.CloseMultiGeometry;
            // заканчиваем объект метки
            Internal_CloseTRK(AXmlVectorObjects, pPX_Result);
          end;
        end;
      end;
      gpx_trkpt: begin
        // single track point - lon/lat as attributes
        case pPX_State^.tag_disposition of
          xtd_ReadAttributes: begin
            // add point to array and skip other params
            pPX_State^.skip_current := True;
            if GetPointForGPX(pPX_Result^.gpx_data.wpt_data, VWptPoint) then begin
              // add to array of points
              AXmlVectorObjects.AddTrackPoint(VWptPoint);
            end;
          end;
        end;
      end;
      gpx_trkseg: begin
        // track segment
        case pPX_State^.tag_disposition of
          xtd_Open: begin
            AXmlVectorObjects.OpenTrackSegment;
          end;
          xtd_Close: begin
            AXmlVectorObjects.CloseTrackSegment;
          end;
        end;
      end;
      gpx_wpt: begin
        // single waypoint
        case pPX_State^.tag_disposition of
          xtd_Open: begin
            // тут будет новая метка
            AXmlVectorObjects.OpenMarkObject;
          end;
          xtd_Close: begin
            // close waypoint
            if GetPointForGPX(pPX_Result^.gpx_data.wpt_data, VWptPoint) then begin
              // add single point object
              AXmlVectorObjects.CloseGPXPoint(VWptPoint);
              // close mark object
              Internal_CloseWPT(AXmlVectorObjects, pPX_Result);
            end;
          end;
        end;
      end;
    end;
  end; { end of gpx }
end;

function TXmlInfoSimpleParser.Load(
  const AData: IBinaryData;
  const AIdData: Pointer;
  const AVectorGeometryLonLatFactory: IVectorDataFactory
): IVectorItemSubset;
var
  VStream: TStreamReadOnlyByBinaryData;
begin
  VStream := TStreamReadOnlyByBinaryData.Create(AData);
  try
    Result := LoadFromStream(
      VStream,
      AIdData,
      AVectorGeometryLonLatFactory
    );
  finally
    VStream.Free;
  end;
end;

function TXmlInfoSimpleParser.LoadFromStream(
  const AStream: TStream;
  const AIdData: Pointer;
  const AVectorGeometryLonLatFactory: IVectorDataFactory
): IVectorItemSubset;
var
  VCounterContext: TInternalPerformanceCounterContext;
begin
  Result := nil;
  VCounterContext := FLoadXmlStreamCounter.StartOperation;
  try
    // read from single simple source
    Result := Internal_LoadFromStream_Original(
      AStream,
      AIdData,
      AVectorGeometryLonLatFactory
    );
  finally
    FLoadXmlStreamCounter.FinishOperation(VCounterContext);
  end;
end;

end.
