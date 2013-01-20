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

unit u_ImportCSV;

interface

uses
  Types,
  Classes,
  t_GeoTypes,
  i_VectorItemsFactory,
  i_DoublePointsAggregator,
  i_ImportFile,
  i_ImportConfig,
  u_BaseInterfacedObject;

type
  TImportCSV = class(TBaseInterfacedObject, IImportFile)
  private
    FFactory: IVectorItemsFactory;
  private
    function ProcessImport(
      const AFileName: string;
      const AConfig: IImportConfig
    ): IInterfaceList;
  public
    constructor Create(
      const AFactory: IVectorItemsFactory
    );
  end;

implementation

uses
  SysUtils,
  StrUtils,
  i_MarksSimple,
  u_DoublePointsAggregator,
  u_GeoFun,
  u_GeoToStr;

{ TImportCSV }

constructor TImportCSV.Create(const AFactory: IVectorItemsFactory);
begin
  inherited Create;
  FFactory := AFactory;
end;

function TImportCSV.ProcessImport(
  const AFileName: string;
  const AConfig: IImportConfig
): IInterfaceList;

  type
    TCSVPointFieldType = (
      csvpft_NAME,
      csvpft_DESC,
      csvpft_DESCRIPTION,
      csvpft_COMMENT,
      csvpft_NOTES,
      csvpft_FILENAME,
      csvpft_URL,
      // changeable fields:
      csvpft_DATE,
      csvpft_UTC_D,
      csvpft_TIME,
      csvpft_UTC_T
    );

    TCSVPointFieldIndicesA = array [TCSVPointFieldType] of Integer;
    TCSVPointFieldIndices = record
      Items: TCSVPointFieldIndicesA;
    end;
    PCSVPointFieldIndices = ^TCSVPointFieldIndices;

    TCSVPointFieldValuesA = array [TCSVPointFieldType] of String;
    TCSVPointFieldValues = record
      Items: TCSVPointFieldValuesA;
    end;
    PCSVPointFieldValues = ^TCSVPointFieldValues;

  const
    c_CSVPointFieldNames: TCSVPointFieldValuesA = (
      'NAME',
      'DESC',
      'DESCRIPTION',
      'COMMENT',
      'NOTES',
      'FILENAME',
      'URL',
      'DATE',
      'UTC_D',
      'TIME',
      'UTC_T'
    );

  procedure _ClearFieldValues(const AValues: PCSVPointFieldValues);
  var i: TCSVPointFieldType;
  begin
    for i := Low(TCSVPointFieldType) to High(TCSVPointFieldType) do begin
      AValues^.Items[i] := '';
    end;
  end;

  procedure _ObtainFieldIndices(const AList: TStrings; const AIndices: PCSVPointFieldIndices);
  var i: TCSVPointFieldType;
  begin
    for i := Low(TCSVPointFieldType) to High(TCSVPointFieldType) do begin
      AIndices^.Items[i] := AList.IndexOf(c_CSVPointFieldNames[i]);
    end;
  end;

  procedure _FillFieldValues(
    const AList: TStrings;
    const AIndices: PCSVPointFieldIndices;
    const AValues: PCSVPointFieldValues
  );
  var i: TCSVPointFieldType;
  begin
    // date: yyyy/mm/dd or yyyymmdd
    // time: hh:mm:ss[.msec] or hh:mm:ss or hhmmss
    for i := Low(TCSVPointFieldType) to High(TCSVPointFieldType) do
    if (AIndices^.Items[i] >= 0) then begin
      AValues^.Items[i] := Trim(AList[AIndices^.Items[i]]);
    end;
  end;

  function _FieldsHaveChanged(
    const AIndices: PCSVPointFieldIndices;
    const AOldValues, ANewValues: PCSVPointFieldValues
  ): Boolean;
  var i: TCSVPointFieldType;
  begin
    // do not check fields after 'URL'
    for i := Low(TCSVPointFieldType) to csvpft_URL {High(TCSVPointFieldType)} do
    if (AIndices^.Items[i] >= 0) then begin
      if (not SameText(AOldValues^.Items[i], ANewValues^.Items[i])) then begin
        Result := TRUE;
        Exit;
      end;
    end;
    Result := FALSE;
  end;

  procedure _CopyNewToOld(const AOldValues, ANewValues: PCSVPointFieldValues);
  var i: TCSVPointFieldType;
  begin
    for i := Low(TCSVPointFieldType) to High(TCSVPointFieldType) do begin
      AOldValues^.Items[i] := ANewValues^.Items[i];
    end;
  end;

  function _HasNonEmptyValue(const AList: TStrings; const AIndex: Integer): Boolean;
  begin
    Result := (AIndex>=0) and (AIndex<AList.Count);
    if Result then
      Result := (0<Length(Trim(AList[AIndex])));
  end;

  function _TryTextToFloat(const S: String; out AValue: Double): Boolean;
  var
    VText: String;
    VNeg: Boolean;
  begin
    Result := (0<Length(S));
    if Result then begin
      VNeg := FALSE;
      VText := UpperCase(S);
      case VText[Length(VText)] of
        'N','E': begin
          // positive
          SetLength(VText, Length(VText)-1);
        end;
        'S','W': begin
          // negative
          VNeg := TRUE;
          SetLength(VText, Length(VText)-1);
        end;
        else begin
          // with sign - nothing
        end;
      end;
      Result := TryStrPointToFloat(VText, AValue);
      if Result and VNeg then
        AValue := -AValue;
    end;
  end;

  function _ParseCoordinates(const AList: TStrings; const AIndices: TPoint; var ACoords: TDoublePoint): Boolean;
  begin
    // 56.218050N or 041.325721E or 35.972033 or -87.134700
    Result := (AIndices.X>=0) and (AIndices.Y>=0) and (AIndices.Y<AList.Count) and (AIndices.X<AList.Count);
    if Result then
      Result := _TryTextToFloat(Trim(AList[AIndices.X]), ACoords.X);
    if Result then
      Result := _TryTextToFloat(Trim(AList[AIndices.Y]), ACoords.Y);
  end;

  (*
  procedure _SafeCheckFileExists(const AFilePath: String; var AFileName: String);
  var
    VFullName: String;
    sr: TSearchRec;
  begin
    VFullName := AFilePath + AFileName;
    if FileExists(VFullName) then begin
      AFileName := VFullName;
      Exit;
    end;

    // file not exists - try to enum
    if FindFirst(AFilePath+AFileName+'.*', faAnyFile, sr)=0 then
    try
      repeat
        // file found - obtain full filename
        AFileName := AFilePath + sr.Name;
        break;
      until FindNext(sr) <> 0;
    finally
      FindClose(sr);
    end;
  end;
  *)

  procedure _AppendStr(var ACollector: String; const ADelimiter, ANewItem: String);
  begin
    if (0<Length(ACollector)) then
      ACollector := ACollector + ADelimiter;
    ACollector := ACollector + ANewItem;
  end;

  procedure _MakeObjectFromArray(
    const AHead{, AList}: TStrings;
    const AOldValues: PCSVPointFieldValues;
    const AIndices: PCSVPointFieldIndices;
    const APointsAggregator: IDoublePointsAggregator;
    const AAllNewMarks: IInterfaceList
  );
  var
    i: TCSVPointFieldType;
    VPointName, VPointDesc: String;
    VMark: IMark;
  begin
    if APointsAggregator.Count=0 then
      Exit;

    // make name and description - only from AOldValues
    VPointName := '';
    VPointDesc := '';
    for i := Low(TCSVPointFieldType) to High(TCSVPointFieldType) do
    if (AIndices^.Items[i] >= 0) then
    if (0<Length(AOldValues^.Items[i])) then begin
      // always add to descript
      _AppendStr(VPointDesc, '<br>', AHead[AIndices^.Items[i]] + ': ' + AOldValues^.Items[i]);
      // set to name (always add date and time)
      if (0=Length(VPointName)) or (i>csvpft_URL) then begin
        // assuming no more than one date and one time
        _AppendStr(VPointName, ' ', AOldValues^.Items[i]);
      end;
    end;

    if APointsAggregator.Count=1 then begin
      // check template
      if (nil=AConfig.TemplateNewPoint) then
        Exit;
      // make
      VMark := AConfig.MarkDB.Factory.CreateNewPoint(
        APointsAggregator.Points[0],
        VPointName,
        VPointDesc,
        AConfig.TemplateNewPoint
      );
    end else if (APointsAggregator.Count>2) and DoublePointsEqual(APointsAggregator.Points[0], APointsAggregator.Points[APointsAggregator.Count-1]) then begin
      // check template
      if (nil=AConfig.TemplateNewPoly) then
        Exit;
      // make
      VMark := AConfig.MarkDB.Factory.CreateNewPoly(
        FFactory.CreateLonLatPolygon(APointsAggregator.Points, APointsAggregator.Count),
        VPointName,
        VPointDesc,
        AConfig.TemplateNewPoly
      );
    end else begin
      // check template
      if (nil=AConfig.TemplateNewLine) then
        Exit;
      // make
      VMark := AConfig.MarkDB.Factory.CreateNewLine(
        FFactory.CreateLonLatPath(APointsAggregator.Points, APointsAggregator.Count),
        VPointName,
        VPointDesc,
        AConfig.TemplateNewLine
      );
    end;

    if (VMark <> nil) then begin
      // add mark to array
      AAllNewMarks.Add(VMark);
    end;
  end;

  procedure _MakeNewPointWithFullInfo(
    const AHead, AList: TStrings;
    const ACoords: TDoublePoint;
    const AIndices: PCSVPointFieldIndices;
    const AVoxFieldIndex: Integer;
    const AFilePath: String;
    const AAllNewMarks: IInterfaceList
  );
  var
    i: TCSVPointFieldType;
    j: Integer;
    VPointName, VPointDesc, VText: String;
    VMark: IMark;
  begin
    if not Assigned(AConfig.TemplateNewPoint) then
      Exit;

    // make name

    VPointName := '';
    for i := Low(TCSVPointFieldType) to High(TCSVPointFieldType) do
    if (AIndices^.Items[i] >= 0) then begin
      VPointName := Trim(AList[AIndices^.Items[i]]);
      if (0<Length(VPointName)) then begin
        // if DATE only - add TIME
        // if UTC_D only - add UTC_T
        if (i=csvpft_DATE) then begin
          if (AIndices^.Items[csvpft_TIME]>=0) then
            _AppendStr(VPointName, ' ', Trim(AList[AIndices^.Items[csvpft_TIME]]));
        end else if (i=csvpft_UTC_D) then begin
          if (AIndices^.Items[csvpft_UTC_T]>=0) then
            _AppendStr(VPointName, ' ', Trim(AList[AIndices^.Items[csvpft_UTC_T]]));
        end;
        break;
      end;
    end;

    // add vox to name
    if (AVoxFieldIndex>=0) then begin
      _AppendStr(VPointName, ' ', Trim(AList[AVoxFieldIndex]));
    end;

    // make description - use all fields (vox too)
    VPointDesc := '';
    for j := 0 to AList.Count-1 do begin
      VText := Trim(AList[j]);
      if (0<Length(VText)) then begin
        (*
        // check value is filename
        if (j=AVoxFieldIndex) or (j=AIndices^.Items[csvpft_FILENAME]) then begin
          // make full path if file exists with full path
          _SafeCheckFileExists(AFilePath, VText);
        end;
        *)
        // make 'name: value' pair
        VText := AHead[j] + ': ' + VText;
        // add to description
        _AppendStr(VPointDesc, '<br>', VText);
      end;
    end;

    // add path to description
    _AppendStr(VPointDesc, '<br>', '"IMPORTED FROM": "' + AFilePath + '"');

    // make simple point
    VMark := AConfig.MarkDB.Factory.CreateNewPoint(ACoords, VPointName, VPointDesc, AConfig.TemplateNewPoint);
    if (VMark <> nil) then begin
      // add mark to array
      AAllNewMarks.Add(VMark);
    end;
  end;

  procedure _LoadFileBodyFromFile(
    const AFileBody: TStrings;
    const AFileName: String
  );
  var
    VFileStream: TFileStream;
    S: String;
    i: Integer;
    VLineCounter: Byte;
  begin
    // read
    VFileStream:=TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
    try
      VFileStream.Position := 0;
      SetString(S, nil, VFileStream.Size);
      VFileStream.Read(Pointer(S)^, VFileStream.Size);
    finally
      VFileStream.Free;
    end;

    // replace #0 by SPACE
    VLineCounter := 8;
    for i := 1 to Length(S) do begin
      if S[i]=#0 then begin
        // has #0
        S[i]:=' ';
        VLineCounter := $FF;
      end else if S[i] in [#10,#13] then begin
        Dec(VLineCounter);
        if (0=VLineCounter) then begin
          // there are no #0 in many lines
          break;
        end;
      end;
    end;
    
    // apply as text
    AFileBody.Text := S;
  end;

var
  VFileBody, VFileHeader, VParsedLine: TStringList;
  VFilePath, VFileName: String;
  i, VIndexVoxField: Integer;
  // for some fields to check changing of values
  VPointFieldIndices: TCSVPointFieldIndices;
  VOldValues, VNewValues: TCSVPointFieldValues;
  // for coordinates
  VCoord: TPoint;
  VPoint: TDoublePoint;
  // to collect points for polyline
  VPointsAggregator: IDoublePointsAggregator;
  // to collect all new marks
  VAllNewMarks: IInterfaceList;
begin
  Result := nil;

  if (nil=AConfig) then
    Exit;

  VFileBody:=TStringList.Create;
  VFileHeader:=TStringList.Create;
  VParsedLine:=TStringList.Create;
  try
    // read file (do not use LoadFromFile because of #0 chars)
    // VFileBody.LoadFromFile(AFileName);
    _LoadFileBodyFromFile(VFileBody, AFileName);

    // check count of lines (with header!)
    if VFileBody.Count<=1 then
      Exit;

    // get header and parse into fields
    VFilePath := UpperCase(VFileBody[0]);
    if (System.Pos(#9, VFilePath)>0) then begin
      // TAB-separated
      VFileHeader.Delimiter := #9;
    end else if (System.Pos(';', VFilePath)>0) then begin
      // COMMA-separated with russian locale
      VFileHeader.Delimiter := ';';
    end else begin
      // COMMA-separated
      VFileHeader.Delimiter := ',';
    end;
    VFileHeader.StrictDelimiter := TRUE;
    VFileHeader.QuoteChar := '"';
    VFileHeader.DelimitedText := VFilePath;

    // check header
    if VFileHeader.Count<=1 then
      Exit;

    // get index of header fields for coordinates
    VCoord.X := VFileHeader.IndexOf('LONGITUDE E/W');
    if VCoord.X<0 then
      VCoord.X := VFileHeader.IndexOf('LONGITUDE');
    if VCoord.X<0 then
      VCoord.X := VFileHeader.IndexOf('LON');
    if VCoord.X<0 then
      VCoord.X := VFileHeader.IndexOf('X_POS');
    if VCoord.X<0 then
      VCoord.X := VFileHeader.IndexOf('X');
    if VCoord.X<0 then
      Exit;

    VCoord.Y := VFileHeader.IndexOf('LATITUDE N/S');
    if VCoord.Y<0 then
      VCoord.Y := VFileHeader.IndexOf('LATITUDE');
    if VCoord.Y<0 then
      VCoord.Y := VFileHeader.IndexOf('LAT');
    if VCoord.Y<0 then
      VCoord.Y := VFileHeader.IndexOf('Y_POS');
    if VCoord.Y<0 then
      VCoord.Y := VFileHeader.IndexOf('Y');
    if VCoord.Y<0 then
      Exit;

    // get position of special fields and clear values
    _ObtainFieldIndices(VFileHeader, @VPointFieldIndices);
    _ClearFieldValues(@VOldValues);
    _ClearFieldValues(@VNewValues);

    // check position of special field VOX (Columbus V-990 and other devices with voice support)
    VIndexVoxField := VFileHeader.IndexOf('VOX');

    // get name and path
    VFilePath := ExtractFilePath(AFileName);
    VFileName := ExtractFileName(AFileName);

    // and prepare to parse lines
    VParsedLine.StrictDelimiter := TRUE;
    VParsedLine.QuoteChar := '"';
    VParsedLine.Delimiter := VFileHeader.Delimiter;
    VPointsAggregator := TDoublePointsAggregator.Create;
    VAllNewMarks := TInterfaceList.Create;
    
    // loop through
    for i := 1 to VFileBody.Count-1 do begin
      // parse line by specified delimiter
      VParsedLine.DelimitedText := VFileBody[i];

      // obtain coordinates
      if _ParseCoordinates(VParsedLine, VCoord, VPoint) then begin
        // check special vox field exists
        if (VIndexVoxField<0) then begin
          // common mode: check some important fields were changed
          _FillFieldValues(VParsedLine, @VPointFieldIndices, @VNewValues);
          if _FieldsHaveChanged(@VPointFieldIndices, @VOldValues, @VNewValues) then begin
            // changed - make object from previous points (in array)
            if (VPointsAggregator.Count>0) then begin
              _MakeObjectFromArray(
                VFileHeader,
                @VOldValues,
                @VPointFieldIndices,
                VPointsAggregator,
                VAllNewMarks
              );
              VPointsAggregator.Clear;
            end;
            // start new object
            VPointsAggregator.Add(VPoint);
            _CopyNewToOld(@VOldValues, @VNewValues);
          end else begin
            // just add point to array
            VPointsAggregator.Add(VPoint);
          end;
        end else begin
          // special mode: single track with several points
          // check and create point with external vox link
          if _HasNonEmptyValue(VParsedLine, VIndexVoxField) then begin
            _MakeNewPointWithFullInfo(
              VFileHeader,
              VParsedLine,
              VPoint,
              @VPointFieldIndices,
              VIndexVoxField,
              VFilePath,
              VAllNewMarks
            );
          end;
          // always add point to default polyline
          VPointsAggregator.Add(VPoint);
        end;
      end;
    end;

    // make object if has points after loop
    if (VPointsAggregator.Count>0) then begin
      // fill values for special mode
      if (VIndexVoxField>=0) then begin
        _FillFieldValues(VParsedLine, @VPointFieldIndices, @VOldValues);
      end;
      // make
      _MakeObjectFromArray(
        VFileHeader,
        @VOldValues,
        @VPointFieldIndices,
        VPointsAggregator,
        VAllNewMarks
      );
      VPointsAggregator.Clear;
    end;
  finally
    VFileBody.Free;
    VFileHeader.Free;
    VParsedLine.Free;
  end;

  if Assigned(VAllNewMarks) then
  if (VAllNewMarks.Count>0) then
  if (nil<>AConfig.MarkDB) then begin
    Result := AConfig.MarkDB.UpdateMarksList(nil, VAllNewMarks);
  end;
end;

end.
