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

unit u_AvailPicsNMC;

interface

uses
  Windows,
  SysUtils,
  Classes,
  i_InetConfig,
  i_ProjectionSet,
  i_DownloadResult,
  i_DownloadRequest,
  i_MapSvcScanStorage,
  u_DownloadRequest,
  u_AvailPicsAbstract;

type
  TAvailPicsNMC = class(TAvailPicsByKey)
  private
    FWorkingZoom: Byte;
    FProfile: AnsiString;
  private
    function GetQuadKey: AnsiString;
    function ParseExifXml(const AStream: TMemoryStream): Integer;
  public
    procedure AfterConstruction; override;

    function ContentType: String; override;

    function ParseResponse(const AResultOk: IDownloadResultOk): Integer; override;

    function GetRequest(const AInetConfig: IInetConfig): IDownloadRequest; override;

    property WorkingZoom: Byte read FWorkingZoom write FWorkingZoom;
  end;

  TAvailPicsNMCZoom = (nmcz13=13, nmcz15=15, nmcz16=16, nmcz18=18, nmcz20=20);
  // treat boolean as FALSE = Recency,TRUE = ColorOnly
  TAvailPicsNMCs = array [TAvailPicsNMCZoom, Boolean] of TAvailPicsNMC;

procedure GenerateAvailPicsNMC(
  var ADGs: TAvailPicsNMCs;
  const ATileInfoPtr: PAvailPicsTileInfo;
  const AProjectionSet: IProjectionSet;
  const AMapSvcScanStorage: IMapSvcScanStorage
);

function FindExifInJpeg(const AJpegBuffer: Pointer;
                        const AJpegSize: Cardinal;
                        const AForGE: Boolean;
                        const AExifTag: Word;
                        out AOffset: PByte;
                        out ASize: DWORD): Boolean;

implementation

uses
  Types,
  Math,
  xmldom,
  t_GeoTypes,
  u_GeoToStrFunc,
  u_GeoFunc,
  u_InetFunc,
  u_XmlLoaderByVSAGPS,
  u_StreamReadOnlyByBinaryData,
  u_ETS_Tiles;

type
  // 4.6.2 IFD Structure
  // The IFD used in this standard consists of:
  // a 2-byte count (number of fields),
  // 12-byte field Interoperability arrays,
  // and 4-byte offset to the next IFD, in conformance with TIFF Rev. 6.0.
  TIFD_12 = packed record
    tag: Word;     // Bytes 0-1 Tag
    type_: Word;      // Bytes 2-3 Type
    count: DWORD;  // Bytes 4-7 Count
    offset: DWORD; // Bytes 8-11 Value Offset
  end;
  PIFD_12 = ^TIFD_12;

  TIFD_NN = packed record
    number_of_fields: Word;
    items: array of TIFD_12;
    offset_to_next: DWORD;
  end;
  PIFD_NN = ^TIFD_NN;

  TPointedMemoryStream = class(TMemoryStream)
  end;

function FindExifInJpeg(const AJpegBuffer: Pointer;
                        const AJpegSize: Cardinal;
                        const AForGE: Boolean;
                        const AExifTag: Word;
                        out AOffset: PByte;
                        out ASize: DWORD): Boolean;
const
  c_SOI_Size = 1024;

  function _GetNextWord(ASrcPtr: PByte): Word;
  begin
    //CopyMemory(@Result, ASrcPtr, sizeof(Result));
    Result := ASrcPtr^;
    Result := (Result shl 8);
    Inc(ASrcPtr);
    Result := Result + ASrcPtr^;
  end;

  function _GetNextDWORD(ASrcPtr: PByte): DWORD;
  begin
    //CopyMemory(@Result, ASrcPtr, sizeof(Result));
    Result := ASrcPtr^;

    Result := (Result shl 8);
    Inc(ASrcPtr);
    Result := Result + ASrcPtr^;

    Result := (Result shl 8);
    Inc(ASrcPtr);
    Result := Result + ASrcPtr^;

    Result := (Result shl 8);
    Inc(ASrcPtr);
    Result := Result + ASrcPtr^;
  end;

  procedure _ReadIFD12(var ASrcPtr: PByte;
                       p: PIFD_12);
  begin
    p^.tag := _GetNextWord(ASrcPtr);
    Inc(ASrcPtr, 2);
    p^.type_ := _GetNextWord(ASrcPtr);
    Inc(ASrcPtr, 2);
    p^.count := _GetNextDWORD(ASrcPtr);
    Inc(ASrcPtr, 4);
    p^.offset := _GetNextDWORD(ASrcPtr);
    Inc(ASrcPtr, 4);
  end;

  function _FindSection(const ASrcPtr: PByte;
                        const AByte1, AByte2: Byte;
                        const AMaxSteps: Word;
                        out ANewPtr: PByte): Boolean;
  var i: Word;
  begin
    Result := FALSE;
    ANewPtr := ASrcPtr;
    i := AMaxSteps;
    while (i > 0) do begin
      // check
      if (AByte1=ANewPtr^) then begin
        Inc(ANewPtr);
        if (AByte2=ANewPtr^) then begin
          Inc(ANewPtr);
          Result := TRUE;
          Exit;
        end;
      end;
      // next
      Inc(ANewPtr);
      Dec(i);
    end;
  end;

var
  VChkLen, VEndian, V42: Word;
  VIFD0: DWORD;
  VSOIPtr, VAPP1Ptr, VPointer: PByte;
  VIFD_NN: TIFD_NN;
  VTagFound: Boolean;

begin
  Result := FALSE;
  AOffset := nil;
  ASize := 0;

  if (nil=AJpegBuffer) or (0=AJpegSize) then
    Exit;

  // test stream - get SOI section
  if (AJpegSize >= c_SOI_Size) then
    VChkLen := c_SOI_Size
  else
    VChkLen := AJpegSize;

  if not _FindSection(AJpegBuffer, $FF, $D8, VChkLen, VSOIPtr) then
    Exit;

  // get JFIF as $FF $E0
  // skipped

  if AForGE then begin
    // get $FF $FE
    if not _FindSection(VSOIPtr, $FF, $FE, VChkLen, AOffset) then
      Exit;

    // get size of section
    VEndian := _GetNextWord(AOffset);

    Inc(AOffset, 2);

    // done
    ASize := VEndian;
    Inc(Result);

    Exit;
  end;

  // get APP1
  if not _FindSection(VSOIPtr, $FF, $E1, VChkLen, VAPP1Ptr) then
    Exit;

  VPointer := VAPP1Ptr;
  Inc(VPointer, 2); // size of APP1

  // check Exif tag
  VTagFound := FALSE;
  if ({'E'}$45=VPointer^) then begin
    Inc(VPointer);
    if ({'x'}$78=VPointer^) then begin
      Inc(VPointer);
      if ({'i'}$69=VPointer^) then begin
        Inc(VPointer);
        if ({'f'}$66=VPointer^) then begin
          Inc(VPointer);
          VTagFound := TRUE;
        end;
      end;
    end;
  end;

  if (not VTagFound) then
    Exit;

  // check next zeroes
  VTagFound := FALSE;
  if ($00=VPointer^) then begin
    Inc(VPointer);
    if ($00=VPointer^) then begin
      Inc(VPointer);
      VTagFound := TRUE;
    end;
  end;

  if (not VTagFound) then
    Exit;

  // Attribute information goes here
  AOffset := VPointer;

  // get (4949.H) (little endian) or "MM" (4D4D.H) (big endian)
  VEndian := _GetNextWord(VPointer);

  if ($4949<>VEndian) and ($4D4D<>VEndian) then
    Exit;

  Inc(VPointer, 2);

  // get 42 (2 bytes) = 002A.H (fixed)
  V42 := _GetNextWord(VPointer);

  if ($002A<>V42) then
    Exit;

  Inc(VPointer, 2);

  // get 0th IFD offset (4 bytes).
  // If the TIFF header is followed immediately by the 0th IFD, it is written as 00000008.H.
  VIFD0 := _GetNextDWORD(VPointer);
  Inc(VPointer, 4);

  if (VIFD0<>$00000008) then
    Inc(VPointer, (VIFD0 - $00000008));

  // IFD 0th
  // The IFD used in this standard consists of:
  // a 2-byte count (number of fields),
  // 12-byte field Interoperability arrays,
  // and 4-byte offset to the next IFD, in conformance with TIFF Rev. 6.0.
  VIFD_NN.number_of_fields := _GetNextWord(VPointer);
  if (0=VIFD_NN.number_of_fields) then
    Exit;

  // $00 $02
  SetLength(VIFD_NN.items, VIFD_NN.number_of_fields);
  Inc(VPointer, 2);

  // read items
  // $82 $98 $00 $02 $00 $00 $00 $20 $00 $00 $00 $26
  // $87 $69 $00 $04 $00 $00 $00 $01 $00 $00 $00 $46
  for V42 := 0 to VIFD_NN.number_of_fields - 1 do begin
    _ReadIFD12(VPointer, @(VIFD_NN.items[V42]));
  end;

  // read next IFD offset
  // $00 $00 $00 $00
  VIFD_NN.offset_to_next := _GetNextDWORD(VPointer);
  Inc(VPointer, 4);

  // TODO: check next IFDs in loop

  VTagFound := FALSE;
  // loop items for Exif
  for V42 := 0 to VIFD_NN.number_of_fields - 1 do
  with VIFD_NN.items[V42] do
  if ($8769=tag) then
  if ($0004=type_) then begin
    // Exif IFD Pointer
    // Tag = 34665 (8769.H)
    // Type = LONG (treat as UNSIGNED LONG WORD)
    // Count = 1
    // Default = none

    // goes to offset = $00 $00 $00 $46
    VPointer := AOffset;
    Inc(VPointer, offset);
    VTagFound := TRUE;
    break;
  end;

  // no exif
  if (not VTagFound) then
    Exit;

  // do it again
  VIFD_NN.number_of_fields := _GetNextWord(VPointer);
  if (0=VIFD_NN.number_of_fields) then
    Exit;

  SetLength(VIFD_NN.items, VIFD_NN.number_of_fields);
  Inc(VPointer, 2);

  // read items
  // $00 $01
  // $92 $86 $00 $07 $00 $00 $10 $EB $00 $00 $00 $54
  // type = $00 $07
  // count = $00 $00 $10 $EB
  // offset = $00 $00 $00 $54
  // $41 $53 $43 $49 $49 $00 $00 $00 $3C $3F $78 $6D $6C $20 $76 $65 $72 $73 $69 $6F $6E $3D $22 $31 $2E $30 $22 $20 $65 $6E $63 $6F $64 $69
  //                                 <   ?
  for V42 := 0 to VIFD_NN.number_of_fields - 1 do begin
    _ReadIFD12(VPointer, @(VIFD_NN.items[V42]));
  end;

  // loop items for AExifTag ($9286 = UserComment)
  VTagFound := FALSE;
  for V42 := 0 to VIFD_NN.number_of_fields - 1 do
  with VIFD_NN.items[V42] do
  if (AExifTag=tag) then begin
    Inc(AOffset, offset + 8);
    VTagFound := TRUE;
    break;
  end;

  if (not VTagFound) then
    Exit;

  // treat VExifAttr as PAnsiChar
  ASize := StrLen(PAnsiChar(AOffset));
  if (0=ASize) then
    Exit;

  Inc(Result);
end;

procedure GenerateAvailPicsNMC(
  var ADGs: TAvailPicsNMCs;
  const ATileInfoPtr: PAvailPicsTileInfo;
  const AProjectionSet: IProjectionSet;
  const AMapSvcScanStorage: IMapSvcScanStorage
);
var
  j: TAvailPicsNMCZoom;
begin
  for j := Low(TAvailPicsNMCZoom) to High(TAvailPicsNMCZoom) do begin
    if (nil=ADGs[j, FALSE]) then begin
      ADGs[j, FALSE] := TAvailPicsNMC.Create(
        AProjectionSet,
        ATileInfoPtr,
        AMapSvcScanStorage
      );
      ADGs[j, FALSE].WorkingZoom := Ord(j);
      ADGs[j, FALSE].FProfile := 'Recency';
    end;
    if (nil=ADGs[j, TRUE]) then begin
      ADGs[j, TRUE] := TAvailPicsNMC.Create(
        AProjectionSet,
        ATileInfoPtr,
        AMapSvcScanStorage
      );
      ADGs[j, TRUE].WorkingZoom := Ord(j);
      ADGs[j, TRUE].FProfile := 'ColorOnly';
    end;
  end;
end;

{ TAvailPicsNMC }

procedure TAvailPicsNMC.AfterConstruction;
begin
  inherited;
  FWorkingZoom := 0; // real value
  FDefaultKey:='LBSP_DEV_ALL';
end;

function TAvailPicsNMC.ContentType: String;
begin
  Result := 'image/jpeg';
end;

function TAvailPicsNMC.GetQuadKey: AnsiString;
var
  VZoom: Byte;
  VTilePos: Tpoint;
begin
  // check working zoom
  if (0=FWorkingZoom) then begin
    // actual zoom (decremented)
    VZoom := FTileInfoPtr.Zoom;
  end else begin
    // fixed zoom (decremented)
    VZoom := FWorkingZoom - 1;
  end;

  // get tile coords (use decremented zoom)
  VTilePos :=
    PointFromDoublePoint(
      FProjectionSet.Zooms[VZoom].LonLat2TilePosFloat(FTileInfoPtr.LonLat),
      prToTopLeft
    );

  // to quadkey (use real zoom)
  Result := XYZ_to_QuadKey(VTilePos, (VZoom + 1)); // like 1210211331322
end;

function TAvailPicsNMC.ParseExifXml(const AStream: TMemoryStream): Integer;
const
  cdgprefix = 'digitalglobe';

  function _NodeHasDGName(const ANode: IDOMNode;
                          out ANodeNameTail: String;
                          out AIsGeometry: Boolean): Boolean;
  var VName: String;
  begin
    Result := FALSE;
    AIsGeometry := FALSE;
    VName := ANode.nodeName;
    if SameText(System.Copy(VName, 1, Length(cdgprefix)), cdgprefix) then begin
      Inc(Result);
      // skip ':' here
      ANodeNameTail := System.Copy(VName, Length(cdgprefix) + 2, Length(VName));
      if (0=Length(ANodeNameTail)) then
        Result := FALSE // skip errors
      else if SameText(ANodeNameTail, 'geometry') then begin
        Inc(AIsGeometry);
        Result := FALSE; // skip 'digitalglobe:geometry'
      end;
    end;
  end;

  procedure _AddToSL(const SL: TStrings; const AValueName: String; const ANode: IDOMNode);
  var S: String;
  begin
    S := GetXmlNodeText(ANode);
    if (0<Length(S)) then begin
      SL.Values[AValueName] := S;
    end;
  end;

  procedure _ParsePosList(const SL_Common: TStrings;
                          const AValueName: String;
                          const ANode: IDOMNode;
                          var ANeedGeometry: Boolean);
  var
    VCoords: String;
    VSLCoords: TStringList;
    i: Integer;
    // pair
    Vc1,Vc2: String;
    VMM: TDoublePoint;
    VLonLat: TDoublePoint;
  begin
    VCoords := GetXmlNodeText(ANode);
    if (0<Length(VCoords)) then begin
      // parse coords (string with metrical values) like:
      // 6247045.446821287 8487567.61960449
      // 6247045.446821287 8492459.589414064
      // 6251937.41663086 8492459.589414064
      // 6251937.41663086 8487567.61960449
      // 6247045.446821287 8487567.61960449
      VSLCoords := TStringList.Create;
      try
        Vc1:='';
        Vc2:='';

        // fetch coordinates
        while (0<Length(VCoords)) do begin
          // get x and y
          i := System.Pos(' ', VCoords);
          if (i<=1) then
            break;
          Vc1 := System.Copy(VCoords, 1, i - 1);
          System.Delete(VCoords, 1, i);
          if (0=Length(VCoords)) then
            break;
          i := System.Pos(' ', VCoords);
          if (i<=1) then begin
            Vc2 := VCoords;
            VCoords:='';
          end else begin
            Vc2 := System.Copy(VCoords, 1, i - 1);
            System.Delete(VCoords, 1, i);
          end;

          // parse and convert
          try
            if (DecimalSeparator<>'.') then begin
              Vc1 := StringReplace(Vc1, '.',DecimalSeparator, []);
              Vc2 := StringReplace(Vc2, '.',DecimalSeparator, []);
            end;

            // TODO: check EPSG from
            // <digitalglobe:tileMatrix>EPSG:3857:13</digitalglobe:tileMatrix>

            // convert to lonlat
            VMM.X := StrToFloat(Vc1);
            VMM.Y := StrToFloat(Vc2);
            VLonLat := FLocalConverter.Projection.ProjectionType.Metr2LonLat(VMM);

            // add to list
            VSLCoords.Append(RoundEx(VLonLat.X, 8) + ',' + RoundEx(VLonLat.Y, 8));
            Vc1:='';
            Vc2:='';
          except
            // epic fail
            Exit;
          end;
        end;

        // check coordinates
        if (2<VSLCoords.Count) then
        if (0=Length(VCoords)) then
        if (0=Length(Vc1)) then
        if (0=Length(Vc2)) then
        if SameText(VSLCoords[0], VSLCoords[VSLCoords.Count - 1]) then begin
          // ok - add to result params
          VSLCoords.Delimiter := ' ';
          VCoords := VSLCoords.DelimitedText;
          SL_Common.Values[AValueName] := VCoords;
          ANeedGeometry := FALSE; // fetch only first geometry
        end;
      finally
        VSLCoords.Free;
      end;
    end;
  end;

  // recursive!
  procedure _ParseGeometry(const SL_Common: TStrings;
                           const AValueName: String;
                           const ANode: IDOMNode;
                           var ANeedGeometry: Boolean);
  var
    VSubItem: IDOMNode;
  begin
    VSubItem := ANode.firstChild;
    while Assigned(VSubItem) do begin
      // parse
      if SameText(VSubItem.nodeName, 'gml:posList') then begin
        // parse positions
        _ParsePosList(SL_Common, AValueName, VSubItem, ANeedGeometry);
      end else if VSubItem.hasChildNodes then begin
        // dive in tag
        _ParseGeometry(SL_Common, AValueName, VSubItem, ANeedGeometry);
      end;

      // check geometry found
      if (not ANeedGeometry) then
        Exit;

      // next
      VSubItem := VSubItem.nextSibling;
    end;
  end;

  procedure _ParseFinishedFeature(const SL_Common: TStrings; const AFinishedFeature: IDOMNode);
  var
    VItem: IDOMNode;
    VTail, VDate: String;
    VSLParams: TStrings;
    VIsGeometry: Boolean;
    VItemExisting: Boolean;
    VItemFetched: TDateTime;
  begin
    VSLParams := TStringList.Create;
    try

      VItem := AFinishedFeature.firstChild;
      while Assigned(VItem) do begin
        // parse
        if _NodeHasDGName(VItem, VTail, VIsGeometry) then begin
          _AddToSL(VSLParams, VTail, VItem);
        end else if VIsGeometry then begin
          // parse geometry
          _ParseGeometry(VSLParams, VTail, VItem, VIsGeometry);
        end;
        // next
        VItem := VItem.nextSibling;
      end;

      // check some critical values
      if FTileInfoPtr.LowResToo or CheckHiResResolution(VSLParams.Values['groundSampleDistance']) then begin
//        VTail := VSLParams.Values['featureId'];
        // get date(s) for date caption
        VDate := GetDateCaptionFromParams(VSLParams);
        if (0<Length(VDate)) then begin

          // add params from common
          //VSLParams.AddStrings(SL_Common);

          // add working zoom
          (*
          if (FWorkingZoom<>0) then begin
            VSLParams.Values[SAS_STR_Zoom + ' ' + IntToStr(FWorkingZoom)] := SAS_STR_Yes;
          end;
          *)

          // check existing
          VItemExisting := ItemExists(
            FBaseStorageName + '_' + FProfile,
            VSLParams.Values['featureId'],
            @VItemFetched
          );

          // add item
          FTileInfoPtr.AddImageProc(
            Self,
            VDate,
            'Nokia z'+IntToStr(FWorkingZoom),
            VItemExisting,
            VItemFetched,
            VSLParams
          );
        end;
      end;
    finally
      FreeAndNil(VSLParams);
    end;
  end;

  procedure _ParseFeatures(const SL_Common: TStrings; const Afeatures: IDOMNode);
  var
    VFinishedFeature: IDOMNode;
    VTail: String;
    VIsGeometry: Boolean;
  begin
    VFinishedFeature := Afeatures.firstChild;
    while Assigned(VFinishedFeature) do begin
      // parse
      if _NodeHasDGName(VFinishedFeature, VTail, VIsGeometry) then
      if SameText(VTail, 'FinishedFeature') then begin
        _ParseFinishedFeature(SL_Common, VFinishedFeature);
      end;
      // next
      VFinishedFeature := VFinishedFeature.nextSibling;
    end;
  end;

var
  VTileNode: IDOMNode;
  VTileMatrixFeature: IDOMNode;
  VfeatureMember: IDOMNode;
  VFeatureCollection: IDOMNode;
  VDOMDocument: IDOMDocument;
  VSLCommon: TStrings;
  VNodeNameTail: String;
  VFound, VIsGeometry: Boolean;
begin
  Result := 0;

  if not LoadXmlDomDocFromStream(VDOMDocument, AStream) then
    Exit;

  // wfs:FeatureCollection
  VFeatureCollection := VDOMDocument.lastChild;

  // gml:featureMember
  VfeatureMember := VFeatureCollection.lastChild; // VfeatureMember.nodeType=1

  // digitalglobe:TileMatrixFeature
  VFound := False;
  VTileMatrixFeature := VfeatureMember.lastChild;
  while Assigned(VTileMatrixFeature) do begin
    // check
    if _NodeHasDGName(VTileMatrixFeature, VNodeNameTail, VIsGeometry) then begin
      VFound := True;
      break;
    end;
    // prev
    VTileMatrixFeature := VTileMatrixFeature.previousSibling;
  end;

  // loop subnodes (skip and dive in '<digitalglobe:features>')
  if VFound then begin
    VSLCommon := TStringList.Create;
    try
      VTileNode := VTileMatrixFeature.firstChild;
      while Assigned(VTileNode) do begin
        // parse node
        if _NodeHasDGName(VTileNode, VNodeNameTail, VIsGeometry) then begin
          if SameText(VNodeNameTail, 'features') then begin
            // dive in features
            _ParseFeatures(VSLCommon, VTileNode);
          end else begin
            // add to common
            _AddToSL(VSLCommon, VNodeNameTail, VTileNode);
          end;
        end;
        // next
        VTileNode := VTileNode.nextSibling;
      end;
    finally
      FreeAndNil(VSLCommon);
    end;
  end;
end;


function TAvailPicsNMC.ParseResponse(const AResultOk: IDownloadResultOk): Integer;
var
  VExifAttr: PByte;
  VLen: DWORD;
  VExifStream: TPointedMemoryStream;
  VStream: TStreamReadOnlyByBinaryData;
begin
  Result := 0;

  if (not Assigned(FTileInfoPtr.AddImageProc)) then
    Exit;

  Assert(not IsGZipped(AResultOk.RawResponseHeader));

  VStream := TStreamReadOnlyByBinaryData.Create(AResultOk.Data);
  try
    if (0 = VStream.Size) then
      Exit;

    // get item for $9286 (UserComment)
    if not FindExifInJpeg(VStream.Memory, VStream.Size, False, $9286, VExifAttr, VLen) then
      Exit;

    // parse xml
    VExifStream := TPointedMemoryStream.Create;
    try
      // set stream
      VExifStream.SetPointer(VExifAttr, VLen);
      // apply stream
      Result := ParseExifXml(VExifStream);
    finally
      VExifStream.Free;
    end;
  finally
    VStream.Free;
  end;
end;

function TAvailPicsNMC.GetRequest(const AInetConfig: IInetConfig): IDownloadRequest;
begin
  Result := TDownloadRequest.Create(
    'http://stg.lbsp.navteq.com/satellite/6.0/images/?token=' + FDefaultKey+
    '&profile=' + FProfile+
    '&quadkey=' + GetQuadKey,
    '',
    AInetConfig.GetStatic
  );
end;

end.
