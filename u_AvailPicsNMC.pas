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

unit u_AvailPicsNMC;

interface

uses
  Windows,
  SysUtils,
  Classes,
  u_AvailPicsAbstract;

type
  TAvailPicsNMC = class(TAvailPicsByKey)
  private
    function GetQuadKey: String;
    function ParseExifXml(const AStream: TMemoryStream): Integer;
  public
    procedure AfterConstruction; override;

    function ContentType: String; override;

    function ParseResponse(const AStream: TMemoryStream): Integer; override;

    function LinkToImages: String; override;
  end;

implementation

uses
  t_GeoTypes,
  u_GeoToStr,
  t_ETS_Tiles,
  u_ETS_Tiles,
  xmldom,
  vsagps_public_xml_dom,
  vsagps_public_xml_parser;

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

{ TAvailPicsNMC }

procedure TAvailPicsNMC.AfterConstruction;
begin
  inherited;
  FDefaultKey:='88a38a0c5e5cf5e340b95dd5eacae3a065d0275a68e71fe6';
end;

function TAvailPicsNMC.ContentType: String;
begin
  Result := 'image/jpeg';
end;

function TAvailPicsNMC.GetQuadKey: String;
var
  VXYZ: TTILE_ID_XYZ;
  VTilePos: Tpoint;
begin
  // get quadkey for zoom>=14 (decremented here!)
  // workaround for MOSAICs - EXIF only from 15th zoom
  VXYZ.z := FTileInfoPtr.Zoom;
  if (VXYZ.z<14) then
    VXYZ.z:=14;

  // get tile coords
  VTilePos := FLocalConverter.GeoConverter.LonLat2TilePos(FTileInfoPtr.LonLat, VXYZ.z);
  VXYZ.x := VTilePos.X; // 5372
  VXYZ.y := VTilePos.Y; // 2359

  // to quadkey
  Inc(VXYZ.z);
  Result:=Convert_XYZ_to_0123(@VXYZ); // 1210211331322
end;

function TAvailPicsNMC.LinkToImages: String;
begin
  // get tile (for exif)
  Result := 'http://0.stl.prd.lbsp.navteq.com/satellite/6.0/images/?profile=ColorOnly&syn=1&appid=jsapi&token='+FDefaultKey+
            '&quadkey='+GetQuadKey;
end;

function TAvailPicsNMC.ParseExifXml(const AStream: TMemoryStream): Integer;
const
  cdgprefix = 'digitalglobe';

  function _NodeHasDGName(const ANode: IDOMNode;
                          out ANodeNameTail: String;
                          out AIsGeometry: Boolean): Boolean;
  var VName: String;
  begin
    Result:=FALSE;
    AIsGeometry:=FALSE;
    VName:=ANode.nodeName;
    if SameText(System.Copy(VName,1,Length(cdgprefix)),cdgprefix) then begin
      Inc(Result);
      // skip ':' here
      ANodeNameTail:=System.Copy(VName, Length(cdgprefix)+2, Length(VName));
      if (0=Length(ANodeNameTail)) then
        Result:=FALSE // skip errors
      else if SameText(ANodeNameTail,'geometry') then begin
        Inc(AIsGeometry);
        Result:=FALSE; // skip 'digitalglobe:geometry'
      end;
    end;
  end;

  procedure _AddToSL(const SL: TStrings; const AValueName: String; const ANode: IDOMNode);
  var S: String;
  begin
    S := VSAGPS_XML_DOMNodeValue(ANode);
    if (0<Length(S)) then begin
      SL.Values[AValueName]:=S;
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
    VCoords := VSAGPS_XML_DOMNodeValue(ANode);
    if (0<Length(VCoords)) then begin
      // parse coords (string with metrical values) like:
      // 6247045.446821287 8487567.61960449
      // 6247045.446821287 8492459.589414064
      // 6251937.41663086 8492459.589414064
      // 6251937.41663086 8487567.61960449
      // 6247045.446821287 8487567.61960449
      VSLCoords:=TStringList.Create;
      try
        Vc1:='';
        Vc2:='';

        // fetch coordinates
        while (0<Length(VCoords)) do begin
          // get x and y
          i:=System.Pos(' ', VCoords);
          if (i<=1) then
            break;
          Vc1:=System.Copy(VCoords, 1, i-1);
          System.Delete(VCoords, 1, i);
          if (0=Length(VCoords)) then
            break;
          i:=System.Pos(' ', VCoords);
          if (i<=1) then begin
            Vc2:=VCoords;
            VCoords:='';
          end else begin
            Vc2:=System.Copy(VCoords, 1, i-1);
            System.Delete(VCoords, 1, i);
          end;

          // parse and convert
          try
            if (DecimalSeparator<>'.') then begin
              Vc1:=StringReplace(Vc1,'.',DecimalSeparator,[]);
              Vc2:=StringReplace(Vc2,'.',DecimalSeparator,[]);
            end;

            // TODO: check EPSG from
            // <digitalglobe:tileMatrix>EPSG:3857:13</digitalglobe:tileMatrix>

            // convert to lonlat
            VMM.X:=StrToFloat(Vc1);
            VMM.Y:=StrToFloat(Vc2);
            VLonLat:=FLocalConverter.GeoConverter.Metr2LonLat(VMM);

            // add to list
            VSLCoords.Append(RoundEx(VLonLat.X, 8)+','+RoundEx(VLonLat.Y, 8));
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
        if SameText(VSLCoords[0],VSLCoords[VSLCoords.Count-1]) then begin
          // ok - add to result params
          VSLCoords.Delimiter := ' ';
          VCoords := VSLCoords.DelimitedText;
          SL_Common.Values[AValueName]:=VCoords;
          ANeedGeometry:=FALSE; // fetch only first geometry
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
    VSubItem:=ANode.firstChild;
    while Assigned(VSubItem) do begin
      // parse
      if SameText(VSubItem.nodeName,'gml:posList') then begin
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
      VSubItem:=VSubItem.nextSibling;
    end;
  end;

  procedure _ParseFinishedFeature(const SL_Common: TStrings; const AFinishedFeature: IDOMNode);
  var
    VItem: IDOMNode;
    VTail, VDate: String;
    VSLParams: TStrings;
    VIsGeometry: Boolean;
  begin
    VSLParams:=TStringList.Create;
    try

      VItem:=AFinishedFeature.firstChild;
      while Assigned(VItem) do begin
        // parse
        if _NodeHasDGName(VItem,VTail,VIsGeometry) then begin
          _AddToSL(VSLParams, VTail, VItem);
        end else if VIsGeometry then begin
          // parse geometry
          _ParseGeometry(VSLParams, VTail, VItem, VIsGeometry);
        end;
        // next
        VItem:=VItem.nextSibling;
      end;

      // check some critical values
      VTail := VSLParams.Values['featureId'];
      VDate := VSLParams.Values['acquisitionDate'];
      if (0<Length(VDate)) and (0<Length(VTail)) then begin
        // subst date separators
        try
          VDate[5]:=DateSeparator;
          VDate[8]:=DateSeparator;
        except
        end;
        // add params from common
        //VSLParams.AddStrings(SL_Common);
        // add item
        FTileInfoPtr.AddImageProc(Self, VDate, VTail, VSLParams);
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
    VFinishedFeature:=Afeatures.firstChild;
    while Assigned(VFinishedFeature) do begin
      // parse
      if _NodeHasDGName(VFinishedFeature,VTail,VIsGeometry) then
      if SameText(VTail,'FinishedFeature') then begin
        _ParseFinishedFeature(SL_Common, VFinishedFeature);
      end;
      // next
      VFinishedFeature:=VFinishedFeature.nextSibling;
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
  Result:=0;
  VTileNode:=nil;
  VTileMatrixFeature:=nil;
  VfeatureMember:=nil;
  VFeatureCollection:=nil;
  VDOMDocument:=nil;
  try
    // create xml
    if VSAGPS_Create_DOMDocument(VDOMDocument, FALSE {no exception if false}, 1 {no validation}) then
    if VSAGPS_Load_DOMDocument_FromStream(VDOMDocument, AStream, FALSE {no exception if false}) then
    try
      // wfs:FeatureCollection
      VFeatureCollection := VDOMDocument.lastChild;

      // gml:featureMember
      VfeatureMember := VFeatureCollection.lastChild; // VfeatureMember.nodeType=1

      // digitalglobe:TileMatrixFeature
      VFound:=FALSE;
      VTileMatrixFeature := VfeatureMember.lastChild;
      while Assigned(VTileMatrixFeature) do begin
        // check
        if _NodeHasDGName(VTileMatrixFeature,VNodeNameTail,VIsGeometry) then begin
          VFound:=TRUE;
          break;
        end;
        // prev
        VTileMatrixFeature:=VTileMatrixFeature.previousSibling;
      end;

      // loop subnodes (skip  and dive in '<digitalglobe:features>')
      if VFound then begin
        VSLCommon := TStringList.Create;
        try
          VTileNode := VTileMatrixFeature.firstChild;
          while Assigned(VTileNode) do begin
            // parse node
            if _NodeHasDGName(VTileNode, VNodeNameTail, VIsGeometry) then begin
              if SameText(VNodeNameTail,'features') then begin
                // dive in features
                _ParseFeatures(VSLCommon, VTileNode);
              end else begin
                // add to common
                _AddToSL(VSLCommon, VNodeNameTail, VTileNode);
              end;
            end;
            // next
            VTileNode:=VTileNode.nextSibling;
          end;
        finally
          FreeAndNil(VSLCommon);
        end;
      end;
    except
    end;
  finally
    VTileNode:=nil;
    VTileMatrixFeature:=nil;
    VfeatureMember:=nil;
    VFeatureCollection:=nil;
    VDOMDocument:=nil;
  end;
end;

function TAvailPicsNMC.ParseResponse(const AStream: TMemoryStream): Integer;
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
    Inc(ASrcPtr,2);
    p^.type_ := _GetNextWord(ASrcPtr);
    Inc(ASrcPtr,2);
    p^.count := _GetNextDWORD(ASrcPtr);
    Inc(ASrcPtr,4);
    p^.offset := _GetNextDWORD(ASrcPtr);
    Inc(ASrcPtr,4);
  end;

  function _FindSection(const ASrcPtr: PByte;
                        const AByte1, AByte2: Byte;
                        const AMaxSteps: Word;
                        out ANewPtr: PByte): Boolean;
  var i: Word;
  begin
    Result:=FALSE;
    ANewPtr:=ASrcPtr;
    i:=AMaxSteps;
    while (i>0) do begin
      // check
      if (AByte1=ANewPtr^) then begin
        Inc(ANewPtr);
        if (AByte2=ANewPtr^) then begin
          Inc(ANewPtr);
          Result:=TRUE;
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
  VSOIPtr, VAPP1Ptr, VExifAttr, VPointer: PByte;
  VIFD_NN: TIFD_NN;
  VTagFound: Boolean;
  VLen: DWORD;
  VStream: TPointedMemoryStream;
begin
  Result := 0;

  if (not Assigned(FTileInfoPtr.AddImageProc)) then
    Exit;

  if (nil=AStream) or (0=AStream.Size) then
    Exit;

  // test stream - get SOI section
  if (AStream.Size>=c_SOI_Size) then
    VChkLen:=c_SOI_Size
  else
    VChkLen:=AStream.Size;

  if not _FindSection(AStream.Memory, $FF, $D8, VChkLen, VSOIPtr) then
    Exit;

  // get JFIF as $FF $E0
  // skipped

  // get APP1
  if not _FindSection(VSOIPtr, $FF, $E1, VChkLen, VAPP1Ptr) then
    Exit;

  VPointer:=VAPP1Ptr;
  Inc(VPointer,2); // size of APP1

  // check Exif tag
  VTagFound:=FALSE;
  if ({'E'}$45=VPointer^) then begin
    Inc(VPointer);
    if ({'x'}$78=VPointer^) then begin
      Inc(VPointer);
      if ({'i'}$69=VPointer^) then begin
        Inc(VPointer);
        if ({'f'}$66=VPointer^) then begin
          Inc(VPointer);
          VTagFound:=TRUE;
        end;
      end;
    end;
  end;

  if (not VTagFound) then
    Exit;

  // check next zeroes
  VTagFound:=FALSE;
  if ($00=VPointer^) then begin
    Inc(VPointer);
    if ($00=VPointer^) then begin
      Inc(VPointer);
      VTagFound:=TRUE;
    end;
  end;
    
  if (not VTagFound) then
    Exit;

  // Attribute information goes here
  VExifAttr := VPointer;

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
    Inc(VPointer,(VIFD0-$00000008));

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
  for V42 := 0 to VIFD_NN.number_of_fields-1 do begin
    _ReadIFD12(VPointer, @(VIFD_NN.items[V42]));
  end;

  // read next IFD offset
  // $00 $00 $00 $00
  VIFD_NN.offset_to_next := _GetNextDWORD(VPointer);
  Inc(VPointer, 4);

  // TODO: check next IFDs in loop

  VTagFound:=FALSE;
  // loop items for Exif
  for V42 := 0 to VIFD_NN.number_of_fields-1 do
  with VIFD_NN.items[V42] do
  if ($8769=tag) then
  if ($0004=type_) then begin
    // Exif IFD Pointer
    // Tag = 34665 (8769.H)
    // Type = LONG (treat as UNSIGNED LONG WORD)
    // Count = 1
    // Default = none

    // goes to offset = $00 $00 $00 $46
    VPointer:=VExifAttr;
    Inc(VPointer, offset);
    VTagFound:=TRUE;
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
  for V42 := 0 to VIFD_NN.number_of_fields-1 do begin
    _ReadIFD12(VPointer, @(VIFD_NN.items[V42]));
  end;

  // // loop items for $9286 (UserComment)
  VTagFound:=FALSE;
  for V42 := 0 to VIFD_NN.number_of_fields-1 do
  with VIFD_NN.items[V42] do
  if ($9286=tag) then begin
    Inc(VExifAttr, offset+8);
    VTagFound:=TRUE;
    break;
  end;

  if (not VTagFound) then
    Exit;

  // treat VExifAttr as PAnsiChar
  VLen := StrLen(PAnsiChar(VExifAttr));
  if (0=VLen) then
    Exit;

  // parse xml
  VStream:=TPointedMemoryStream.Create;
  try
    // set stream
    VStream.SetPointer(VExifAttr, VLen+1);
    // apply stream
    Result := ParseExifXml(VStream);
  finally
    VStream.Free;
  end;
end;

end.
