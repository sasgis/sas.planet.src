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
  Classes,
  SysUtils,
  t_GeoTypes,
  i_BinaryData,
  i_VectorDataFactory,
  i_VectorItemsFactory,
  i_VectorItemSubset,
  i_InternalPerformanceCounter,
  i_VectorDataLoader,
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
    FFactory: IVectorItemsFactory;
    FLoadXmlStreamCounter: IInternalPerformanceCounter;
    FAllowMultiParts: Boolean;
    FFormat: TFormatSettings;
  private
    procedure Internal_ParseXML_UserProc(
      const pUserAuxPointer: Pointer;
      const pPX_Result: Pvsagps_XML_ParserResult;
      const pPX_State: Pvsagps_XML_ParserState
    );
    function Internal_LoadFromStream_Original(
      AStream: TStream;
      const AIdData: Pointer;
      const AFactory: IVectorDataFactory
    ): IVectorItemSubset;
  private
    function LoadFromStream(
      AStream: TStream;
      const AIdData: Pointer;
      const AFactory: IVectorDataFactory
    ): IVectorItemSubset;
    function Load(
      const AData: IBinaryData;
      const AIdData: Pointer;
      const AFactory: IVectorDataFactory
    ): IVectorItemSubset;
  public
    constructor Create(
      const AFactory: IVectorItemsFactory;
      const AAllowMultiParts: Boolean;
      const APerfCounterList: IInternalPerformanceCounterList
    );
  end;

implementation

uses
  i_VectorDataItemSimple,
  i_InterfaceListSimple,
  u_InterfaceListSimple,
  u_StreamReadOnlyByBinaryData,
  u_VectorDataItemSubset,
  u_GeoFun;

type
  TForceObjectType = (fotUnknown, fotPolygon, fotPolyLine);

  TParseXML_Aux = record
  strict private
    FAllowMultiParts: Boolean;
    FInMultiObject: Boolean;
    FCurrentFOT: TForceObjectType;
    FSegmentCounter: Integer;
    FArrayCount: Integer;
    (*
    FArrayRect: TDoubleRect;
    *)
  private
    opt: Tvsagps_XML_ParserOptions;
    IdData: Pointer;
    Factory: IVectorDataFactory;
    list: IInterfaceListSimple;
    array_capacity: Integer;
    array_points: array of TDoublePoint;
  public
    procedure Init(
      const AAllowMultiParts: Boolean
    );
    procedure Uninit;
    // сбрасывает массив точек
    procedure Cleanup_Array;
    // проверка замкнутости массива
    function IsClosed_Array: Boolean;
    // добавляет точку в массив
    procedure AddPointTo_Array(const Awpt_point: TDoublePoint);
    // добавляет объект в список
    procedure AddCoordsToList(
      const AWideStrName, AWideStrDesc: WideString;
      const AItemsFactory: IVectorItemsFactory
    );
    // определение типа объекта по тэгу при открытии тэга
    procedure ApplyKmlTag(const Atag: Tvsagps_KML_main_tag);

    // если можно начать мультисегментный объект - начинает (и закрывает) его
    procedure SafeStartMultiObject;
    procedure SafeCloseMultiObject;
    // проверка что в мультисегментном объекте
    property InMultiObject: Boolean read FInMultiObject;
    // начинает сегмент (для мультика - добавляет разделитель)
    procedure SafeStartSegment;
  end;
  PParseXML_Aux = ^TParseXML_Aux;

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
  const AFactory: IVectorItemsFactory;
  const AAllowMultiParts: Boolean;
  const APerfCounterList: IInternalPerformanceCounterList
);
begin
  inherited Create;
  FFactory := AFactory;
  FAllowMultiParts := AAllowMultiParts;

  if APerfCounterList <> nil then begin
    FLoadXmlStreamCounter := APerfCounterList.CreateAndAddNewCounter('LoadXmlStream');
  end;
  VSAGPS_PrepareFormatSettings(FFormat);
end;

function TXmlInfoSimpleParser.Internal_LoadFromStream_Original(
  AStream: TStream;
  const AIdData: Pointer;
  const AFactory: IVectorDataFactory
): IVectorItemSubset;
var
  tAux: TParseXML_Aux;
begin
  // init
  tAux.Init(
    // делаем или нет многосегментные объекты (если нет - создаём их как отдельные)
    FAllowMultiParts
  );
  // for wpt and trk
  Inc(tAux.opt.gpx_options.bParse_trk);
  Inc(tAux.opt.gpx_options.bParse_wpt);
  tAux.Factory := AFactory;
  tAux.IdData := AIdData;
  try
    // parse
    VSAGPS_LoadAndParseXML(Self, @tAux, '', AStream, TRUE, @(tAux.opt), rTVSAGPS_ParseXML_UserProc, FFormat);
    // output result
    if Assigned(tAux.list) then begin
      Result := TVectorItemSubset.Create(tAux.list.MakeStaticAndClear);
      tAux.list := nil;
    end;
  finally
    tAux.Uninit;
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

  procedure _SetFromTagValue(
    const ABuffer: PWideChar;
    const a_to_description: Boolean
  );
  begin
    VWSName := SafeSetStringP(ABuffer);
    if a_to_description and (0 < Length(VWSName)) then begin
      if (0 < Length(VWSDesc)) then begin
        VWSDesc := VWSDesc + #13#10;
      end;
      VWSDesc := VWSDesc + VWSName;
    end;
  end;

  procedure _SetFromParentPlacemark(
  const a_type: Tvsagps_KML_str;
  const a_to_description: Boolean
  );
  var
    VPX_Result: Pvsagps_XML_ParserResult;
    VInner: Boolean;
  begin
    VWSName := '';
    VInner := FALSE;

    VPX_Result := pPX_Result;
    repeat
      // noway
      if (nil = VPX_Result) then begin
        break;
      end;

      // check tag
      if (VPX_Result^.kml_data.current_tag = kml_Placemark) then begin
        // Placemark found
        if (a_type in VPX_Result^.kml_data.fAvail_strs) then begin
          // has string
          _SetFromTagValue(VPX_Result^.kml_data.fParamsStrs[a_type], a_to_description);
        end;

        // done
        break;
      end else if (VPX_Result^.kml_data.current_tag = kml_innerBoundaryIs) then begin
        // Inner boundary
        VInner := TRUE;
      end;

      // prev level
      VPX_Result := VPX_Result^.prev_data;
    until FALSE;

    if VInner and (not a_to_description) then begin
      // add prefix to name
      VWSName := 'inner - ' + VWSName;
    end;
  end;

  procedure _SetFromTrk(
    const a_type: Tvsagps_GPX_trk_str;
    const a_to_description: Boolean
  );
  begin
    with pPX_Result^.gpx_data.trk_data do
    if (a_type in fAvail_trk_strs) then begin
      _SetFromTagValue(fStrs[a_type], a_to_description);
    end;
  end;

  procedure _SetFromParentTrk(
    const a_type: Tvsagps_GPX_trk_str;
    const a_to_description: Boolean
  );
  begin
    if (nil <> pPX_Result^.prev_data) then
    with pPX_Result^.prev_data^.gpx_data.trk_data do
    if (a_type in fAvail_trk_strs) then begin
      _SetFromTagValue(fStrs[a_type], a_to_description);
    end;
  end;

  procedure _SetFromWpt(
    const a_type: Tvsagps_GPX_wpt_str;
    const a_to_description: Boolean
  );
  begin
    with pPX_Result^.gpx_data.wpt_data do
    if (a_type in fAvail_wpt_strs) then begin
      _SetFromTagValue(fStrs[a_type], a_to_description);
    end;
  end;

  procedure _AddSasxFile;
  begin
    if (sasx_file_name in pPX_Result^.gpx_data.extensions_data.fAvail_strs) then begin
      _SetFromTagValue(pPX_Result^.gpx_data.extensions_data.sasx_strs[sasx_file_name], TRUE);
    end;
  end;

  procedure _AddWptToList;
  var
    wpt_iface: IVectorDataItemPoint;
    VAUX: PParseXML_Aux;
  begin
    VAUX := PParseXML_Aux(pUserAuxPointer);
    // make list object
    if not Assigned(VAUX^.list) then begin
      VAUX^.list := TInterfaceListSimple.Create;
    end;
    // create object
    wpt_iface := VAUX^.Factory.BuildPoint(VAUX^.IdData, VWSName, VWSDesc, wpt_point);
    // add object to list
    VAUX^.list.Add(wpt_iface);
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
      VCoordinates := SafeSetStringP(AKmlData.fParamsStrs[kml_coordinates]);
      Result := parse_kml_coordinate(VCoordinates, @VData, FFormat);
      if Result then begin
        wpt_point.X := VData.lon1;
        wpt_point.Y := VData.lat0;
      end;
    end;
  end;

  function _GetFirstDelimiterPos(const ASource: WideString): Integer;
  begin
    Result := 1;
    while (Result <=Length(ASource)) do begin
      case Ord(ASource[Result]) of
        9,10,13,32,160: Exit;
      end;
      Inc(Result);
    end;
    Result := 0;
  end;

  function _ParseCoordinatesForKML(const AKmlData: Tvsagps_KML_ParserData): Boolean;
  var
    VCoordinates, VCoordLine: WideString;
    VData: TCoordLineData;
    VPointsAdded: Integer;
    VSepPos: Integer;
  begin
    Result := (AKmlData.fParamsStrs[kml_coordinates] <> nil);
    if Result then begin
      VCoordinates := SafeSetStringP(AKmlData.fParamsStrs[kml_coordinates]);
      VPointsAdded := 0;

      // loop through points
      repeat
        if Length(VCoordinates) = 0 then begin
          break;
        end;

        VSepPos := _GetFirstDelimiterPos(VCoordinates);
        if (VSepPos > 0) then begin
          // with delimiter
          VCoordLine := System.Copy(VCoordinates, 1, VSepPos - 1);
          System.Delete(VCoordinates, 1, VSepPos);
        end else begin
          // no delimiter
          VCoordLine := VCoordinates;
          VCoordinates := '';
        end;

        if (Length(VCoordLine) > 0) then begin
          if parse_kml_coordinate(VCoordLine, @VData, FFormat) then begin
            wpt_point.X := VData.lon1;
            wpt_point.Y := VData.lat0;
            // добавляем точку в массив
            PParseXML_Aux(pUserAuxPointer)^.AddPointTo_Array(wpt_point);
            Inc(VPointsAdded);
          end;
        end;
      until FALSE;
      // check
      Result := (VPointsAdded > 0);
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

    // некоторые тэги обрабатываем при открытии
    if (xtd_Open = pPX_State^.tag_disposition) then begin
      PParseXML_Aux(pUserAuxPointer)^.ApplyKmlTag(pPX_Result^.kml_data.current_tag);
    end;

    // check some tags with coordinates on Closing
    if (xtd_Close = pPX_State^.tag_disposition) then begin
      case pPX_Result^.kml_data.current_tag of
        kml_LinearRing, kml_LineString: begin
          // <Placemark><MultiGeometry><Polygon><outerBoundaryIs><LinearRing><coordinates>
          // <Placemark><MultiGeometry><LineString></LineString><LineString>...
          // <Placemark><LineString><coordinates>
          // создаём тут объект, только если он не многосегментный
          // если многосегментный - только лишь пропихнём координаты
          if PParseXML_Aux(pUserAuxPointer)^.InMultiObject then begin
            // только лишь распарсим и добавим координаты в массив
            _ParseCoordinatesForKML(pPX_Result^.kml_data);
          end else begin
            // make description
            _SetFromParentPlacemark(kml_description, TRUE);
            // make name
            _SetFromParentPlacemark(kml_name, FALSE);

            // make polyline (path) object
            // if LineString - force PolyLine mode
            if _ParseCoordinatesForKML(pPX_Result^.kml_data) then
            with PParseXML_Aux(pUserAuxPointer)^ do begin
              AddCoordsToList(
                VWSName, VWSDesc,
                FFactory);
            end;

            // clear points
            PParseXML_Aux(pUserAuxPointer)^.Cleanup_Array;
          end;
        end;
        kml_MultiGeometry: begin
          // создаём тут объект, только если он был многосегментный
          if PParseXML_Aux(pUserAuxPointer)^.InMultiObject then begin
            // get params from parent (Placemark)
            _SetFromParentPlacemark(kml_description, TRUE);
            _SetFromParentPlacemark(kml_name, FALSE);

            with PParseXML_Aux(pUserAuxPointer)^ do begin
              // make track segment object
              AddCoordsToList(
                VWSName, VWSDesc,
                FFactory);

              // clear points (no segment counter)
              Cleanup_Array;
            end;
          end;
          PParseXML_Aux(pUserAuxPointer)^.SafeCloseMultiObject;
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
        kml_gx_coord: begin
          if (kml_latitude in pPX_Result^.kml_data.fAvail_params) and
             (kml_longitude in pPX_Result^.kml_data.fAvail_params) then begin
            // add single coord
            wpt_point.X := pPX_Result^.kml_data.fValues.longitude;
            wpt_point.Y := pPX_Result^.kml_data.fValues.latitude;
            // add to array of points
            PParseXML_Aux(pUserAuxPointer)^.AddPointTo_Array(wpt_point);
          end;
        end;
        kml_gx_MultiTrack: begin
          // https://developers.google.com/kml/documentation/kmlreference?hl=en#gxmultitrack
          // <gx:MultiTrack id="ID">
          // <gx:interpolate>0<gx:interpolate> // boolean
          // <gx:Track>...</gx:Track>          // one or more gx:Track elements
          // в конце мультитрека создаём объект, только если создавали многосегментный
          if PParseXML_Aux(pUserAuxPointer)^.InMultiObject then begin
            // get params from parent (Placemark)
            _SetFromParentPlacemark(kml_description, TRUE);
            _SetFromParentPlacemark(kml_name, FALSE);

            with PParseXML_Aux(pUserAuxPointer)^ do begin
              // make track segment object
              AddCoordsToList(
                VWSName, VWSDesc,
                FFactory);

              // clear points (no segment counter)
              Cleanup_Array;
            end;
          end;
          PParseXML_Aux(pUserAuxPointer)^.SafeCloseMultiObject;
        end;
        kml_gx_Track: begin
          // <Placemark><gx:Track>
          // <gx:coord>60.798387 56.748476 230.85376</gx:coord><gx:coord>60.798634 56.748772 247.196045</gx:coord>
          // </gx:Track></Placemark>
          // создаём тут объект, только если он не многосегментный
          if (not PParseXML_Aux(pUserAuxPointer)^.InMultiObject) then begin
            // get params from parent (Placemark)
            _SetFromParentPlacemark(kml_description, TRUE);
            _SetFromParentPlacemark(kml_name, FALSE);

            with PParseXML_Aux(pUserAuxPointer)^ do begin
              // make track segment object
              AddCoordsToList(
                VWSName, VWSDesc,
                FFactory);

              // clear points (no segment counter)
              Cleanup_Array;
            end;
          end;
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

  // обработка открытия тэгов
  if (xtd_Open = pPX_State^.tag_disposition) then
  case pPX_Result^.gpx_data.current_tag of
    gpx_trk: begin
      // возможно надо начать многосегментный объект
      PParseXML_Aux(pUserAuxPointer)^.SafeStartMultiObject;
    end;
    gpx_trkseg: begin
      // может быть как одиночный тэг внутри trk
      // а может быть первый или очередной из многих внутри trk
      // во втором случае надо добавить разделитель
      PParseXML_Aux(pUserAuxPointer)^.SafeStartSegment;
    end;
  end;

  if (xtd_ReadAttributes = pPX_State^.tag_disposition) then
  case pPX_Result^.gpx_data.current_tag of
    gpx_trkpt: begin
      // track point - lon/lat as attributes
      // add to current track object and jump to end of track
      pPX_State^.skip_current := TRUE;
      if _GetPointForGPX(pPX_Result^.gpx_data.wpt_data) then begin
        // add to array of points
        PParseXML_Aux(pUserAuxPointer)^.AddPointTo_Array(wpt_point);
      end;
    end;
  end;

  // switch by tag on closing
  if (xtd_Close = pPX_State^.tag_disposition) then
  case pPX_Result^.gpx_data.current_tag of
    gpx_trk: begin
      // если создаём многосегментные - создание объекта выполняется в самом конце
      if PParseXML_Aux(pUserAuxPointer)^.InMultiObject then begin
        // make track name and desc (get data from trk)
        _SetFromTrk(trk_desc, TRUE);
        _SetFromTrk(trk_cmt, TRUE);
        _SetFromTrk(trk_src, TRUE);
        _SetFromTrk(trk_name, FALSE);

        with PParseXML_Aux(pUserAuxPointer)^ do begin
          // make track object
          AddCoordsToList(
            VWSName, VWSDesc,
            FFactory
          );

          // clear points and segment counter
          Cleanup_Array;
        end;
      end;
      PParseXML_Aux(pUserAuxPointer)^.SafeCloseMultiObject;
    end;
    gpx_trkseg: begin
      // на событие закрытия сегмента создаём объект, только если запрещены многосегментные объекты
      // в этом случае признак многосегментности фактически не установится
      if (not PParseXML_Aux(pUserAuxPointer)^.InMultiObject) then begin
        // make track name and desc (get data from parent)
        _SetFromParentTrk(trk_desc, TRUE);
        _SetFromParentTrk(trk_cmt, TRUE);
        _SetFromParentTrk(trk_src, TRUE);
        _SetFromParentTrk(trk_name, FALSE);

        with PParseXML_Aux(pUserAuxPointer)^ do begin
          // make track segment object
          AddCoordsToList(
            VWSName, VWSDesc,
            FFactory
          );

          // clear points and increment segment counter
          Cleanup_Array;
        end;
      end;
    end;
    gpx_wpt: begin
      // waypoint - single item
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

function TXmlInfoSimpleParser.Load(
  const AData: IBinaryData;
  const AIdData: Pointer;
  const AFactory: IVectorDataFactory
): IVectorItemSubset;
var
  VStream: TStreamReadOnlyByBinaryData;
begin
  Result := nil;
  VStream := TStreamReadOnlyByBinaryData.Create(AData);
  try
    Result := LoadFromStream(VStream, AIdData, AFactory);
  finally
    VStream.Free;
  end;
end;

function TXmlInfoSimpleParser.LoadFromStream(
  AStream: TStream;
  const AIdData: Pointer;
  const AFactory: IVectorDataFactory
): IVectorItemSubset;
var
  VCounterContext: TInternalPerformanceCounterContext;
begin
  Result := nil;
  if FLoadXmlStreamCounter <> nil then begin
    VCounterContext := FLoadXmlStreamCounter.StartOperation;
    try
      // read from single simple source
      Result := Internal_LoadFromStream_Original(AStream, AIdData, AFactory);
    finally
      FLoadXmlStreamCounter.FinishOperation(VCounterContext);
    end;
  end else begin
    Result := Internal_LoadFromStream_Original(AStream, AIdData, AFactory);
  end;
end;

{ TParseXML_Aux }

procedure TParseXML_Aux.AddPointTo_Array(const Awpt_point: TDoublePoint);
begin
  // calc rect
  (*
  if (0 = FArrayCount) then begin
    // very first point of segment
    FArrayRect.TopLeft := Awpt_point;
    FArrayRect.BottomRight := Awpt_point;
  end else begin
    // compare bounds
    if FArrayRect.Left > Awpt_point.X then begin
      FArrayRect.Left := Awpt_point.X;
    end;
    if FArrayRect.Right < Awpt_point.X then begin
      FArrayRect.Right := Awpt_point.X;
    end;
    if FArrayRect.Top < Awpt_point.y then begin
      FArrayRect.Top := Awpt_point.y;
    end;
    if FArrayRect.Bottom > Awpt_point.y then begin
      FArrayRect.Bottom := Awpt_point.y;
    end;
  end;
  *)

  // allocate more array
  if (FArrayCount >= array_capacity) then begin
    if (0 = array_capacity) then begin
      array_capacity := 32;
    end else begin
      array_capacity := array_capacity * 2;
    end;
    SetLength(array_points, array_capacity);
  end;

  // add
  array_points[FArrayCount] := Awpt_point;
  Inc(FArrayCount);
end;

procedure TParseXML_Aux.AddCoordsToList(
  const AWideStrName, AWideStrDesc: WideString;
  const AItemsFactory: IVectorItemsFactory
);
var
  trk_obj: IVectorDataItemSimple;
begin
  // make list object
  if (nil=list) then begin
    list := TInterfaceListSimple.Create;
  end;

  // make object and add it to list
  if (0 < FArrayCount) then begin
    if (1 = FArrayCount) then begin
      // single point in track segment - make as point
      trk_obj := Factory.BuildPoint(IdData, AWideStrName, AWideStrDesc, array_points[0]);
    end else if ((FCurrentFOT = fotPolygon))
                OR
                ((FCurrentFOT = fotUnknown) and
                  IsClosed_Array) then begin
      // polygon
      trk_obj := Factory.BuildPoly(
          IdData,
          AWideStrName,
          AWideStrDesc,
          AItemsFactory.CreateLonLatPolygon(@array_points[0], FArrayCount)
        );
    end else begin
      // polyline
      trk_obj := Factory.BuildPath(
          IdData,
          AWideStrName,
          AWideStrDesc,
          AItemsFactory.CreateLonLatPath(@array_points[0], FArrayCount)
        );
    end;
    list.Add(trk_obj);
  end;
end;

procedure TParseXML_Aux.ApplyKmlTag(const Atag: Tvsagps_KML_main_tag);
var
  VForceObjectType: TForceObjectType;
begin
  case Atag of
    kml_gx_MultiTrack: begin
      // только трек
      SafeStartMultiObject;
      VForceObjectType := fotPolyLine;
    end;
    kml_gx_Track: begin
      // только трек
      SafeStartSegment;
      VForceObjectType := fotPolyLine;
    end;
    kml_LinearRing: begin
      // только полигон
      SafeStartSegment;
      VForceObjectType := fotPolygon;
    end;
    kml_LineString: begin
      // только трек
      SafeStartSegment;
      VForceObjectType := fotPolyLine;
    end;
    kml_MultiGeometry: begin
      // тип тут пока неизвестен
      SafeStartMultiObject;
      Exit;
    end;
    kml_outerBoundaryIs: begin
      // только полигон
      VForceObjectType := fotPolygon;
    end;
    else begin
      Exit;
    end;
  end;

  // тип не понижаем
  if FCurrentFOT<VForceObjectType then
    FCurrentFOT:=VForceObjectType;
end;

procedure TParseXML_Aux.Cleanup_Array;
begin
  FArrayCount := 0;
end;

procedure TParseXML_Aux.Init(const AAllowMultiParts: Boolean);
begin
  FillChar(Self, SizeOf(Self) ,0);
  FAllowMultiParts := AAllowMultiParts;
end;

function TParseXML_Aux.IsClosed_Array: Boolean;
begin
  Result := DoublePointsEqual(array_points[0], array_points[FArrayCount-1]);
end;

procedure TParseXML_Aux.SafeCloseMultiObject;
begin
  FInMultiObject := FALSE;
  FSegmentCounter := 0;
end;

procedure TParseXML_Aux.SafeStartMultiObject;
begin
  // вложения не допускаются
  Assert(not FInMultiObject);
  // только если разрешено
  if FAllowMultiParts then begin
    FInMultiObject := TRUE;
  end;
  FSegmentCounter := 0;
  FCurrentFOT := fotUnknown;
end;

procedure TParseXML_Aux.SafeStartSegment;
begin
  // если создаём многосегментный объект, и уже есть хотя бы одна точка
  if FInMultiObject and (FArrayCount>0) then begin
    // для многосегментного надо определить его тип (иначе будет всегда полилиния)
    // но только если ещё не определено
    if (fotUnknown=FCurrentFOT) then begin
      if IsClosed_Array then
        FCurrentFOT := fotPolygon
      else
        FCurrentFOT := fotPolyLine;
    end;
    // разделитель как начало нового сегмента
    AddPointTo_Array(CEmptyDoublePoint);
  end;
  Inc(FSegmentCounter);
end;

procedure TParseXML_Aux.Uninit;
begin
  Cleanup_Array;

  array_capacity := 0;
  SetLength(array_points, 0);
  array_points:=nil;

  Factory := nil;

  if (list<>nil) then begin
    list.Clear;
    list := nil;
  end;
end;

end.
