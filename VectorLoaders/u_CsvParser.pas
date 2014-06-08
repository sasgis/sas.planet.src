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

unit u_CsvParser;

interface

uses
  i_BinaryData,
  i_VectorDataLoader,
  i_GeometryLonLatFactory,
  i_VectorItemSubset,
  i_VectorDataFactory,
  i_VectorItemSubsetBuilder,
  u_BaseInterfacedObject;

type
  TCsvParser = class(TBaseInterfacedObject, IVectorDataLoader)
  private
    FVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FVectorDataFactory: IVectorDataFactory;
    FVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  private
    function Load(
      const AData: IBinaryData;
      const AIdData: Pointer;
      const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory
    ): IVectorItemSubset;
  public
    constructor Create(
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AVectorDataFactory: IVectorDataFactory;
      const AGeometryFactory: IGeometryLonLatFactory
    );
  end;

implementation

uses
  Types,
  SysUtils,
  StrUtils,
  Math,
  Classes,
  t_GeoTypes,
  i_GeometryLonLat,
  i_VectorDataItemSimple,
  i_DoublePointsAggregator,
  u_DoublePointsAggregator,
  u_GeoFunc,
  u_GeoToStrFunc;

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

function _TryTextToFloat(const AStr: string; out AValue: Double): Boolean;
var
  llat: boolean;
  Vpos: integer;
  Vdelitel: single;
  Vgms: double;
  VText: string;
  Vminus: boolean;
  VTextTogms: string;
begin
  Result := (0<Length(Astr));
  if Result then begin
    AValue := 0;
    VText := Trim(AnsiUpperCase(Astr));
    llat := false;

    VText := ReplaceStr(VText, 'LON','');
    VText := ReplaceStr(VText, 'LN','');

    if PosEx('S', VText, 1) > 0 then llat := true;
    if PosEx('N', VText, 1) > 0 then llat := true;
    if PosEx('Ю', VText, 1) > 0 then llat := true;
    if PosEx('С', VText, 1) > 0 then llat := true;
    if PosEx('LAT', VText, 1) > 0 then llat := true;
    if PosEx('LL', VText, 1) > 0 then llat := true;
    VText := ReplaceStr(VText, 'LAT', '');
    VText := ReplaceStr(VText, 'LL', '');
    VText := ReplaceStr(VText, 'Ш.', '');
    VText := ReplaceStr(VText, 'Ш', '');
    VText := ReplaceStr(VText, 'Д.', '');
    VText := ReplaceStr(VText, 'Д', '');
    VText := ReplaceStr(VText, '=', '');
    VText := ReplaceStr(VText, 'S', '-');
    VText := ReplaceStr(VText, 'W', '-');
    VText := ReplaceStr(VText, 'N', '+');
    VText := ReplaceStr(VText, 'E', '+');
    VText := ReplaceStr(VText, 'Ю', '-');
    VText := ReplaceStr(VText, 'З', '-');
    VText := ReplaceStr(VText, 'В', '+');
    VText := ReplaceStr(VText, 'С', '+');
    Vminus := false;
    if posEx('-', VText, 1)>0 then Vminus := true;

    if (VText[length(VText)] = '.') then VText := copy(VText, 1, length(VText)-1);
    if (VText[length(VText)] = ',') then VText := copy(VText, 1, length(VText)-1);
    if (VText[length(VText)] = '+') or (VText[length(VText)] = '-') then
      VText := VText[length(VText)] +copy(VText, 0, length(VText) - 1);

    if PosEx('+-', VText, 1) > 0 then begin // WE123 NS123
      llat := true;
      VText := ReplaceStr(VText,'+-','+');
    end;
    if PosEx('-+', VText, 1) > 0 then begin // EW123 SN123
      llat := true;
      VText := ReplaceStr(VText,'-+','-');
    end;
    if PosEx('--', VText, 1) > 0 then begin // -123S
      VText := ReplaceStr(VText,'--','-');
    end;
    Vpos :=1;
    while Vpos <= length(VText) do begin
      if (not(VText[Vpos] in ['0'..'9', '-', '+', '.', ',', ' '])) then begin
        VText[Vpos] := ' ';
        dec(Vpos);
      end;
      if ((Vpos = 1)and(VText[Vpos] = ' '))or
        ((Vpos = length(VText)) and (VText[Vpos] = ' ')) or
        ((Vpos < length(VText) - 1) and (VText[Vpos] = ' ') and (VText[Vpos + 1] = ' ')) or
        ((Vpos > 1) and (VText[Vpos] = ' ') and (not(VText[Vpos - 1] in ['0'..'9']))) or
        ((Vpos < length(VText) - 1) and (VText[Vpos]=',') and (VText[Vpos + 1] = ' '))
      then begin
        Delete(VText, Vpos, 1);
        dec(Vpos);
      end;
      inc(Vpos);
    end;
    AValue := 0;
    Vdelitel := 1;
    repeat
      Vpos := posEx(' ', VText, 1);
      if Vpos = 0 then begin
        VTextTogms := VText;
      end else begin
        VTextTogms := copy(VText, 1, Vpos-1);
        Delete(VText, 1, Vpos);
      end;
      if not TryStrPointToFloat(VTextTogms, Vgms) then Vgms := 0;

      if ((Vdelitel>1) and (abs(Vgms) > 60))or
        ((Vdelitel=1) and (llat) and (abs(Vgms)>90))or
        ((Vdelitel=1) and (not llat) and (abs(Vgms)>180))
      then begin
        if (Vdelitel = 60) and (Vgms > 60) then begin //  37 6298475265502
          Vdelitel := Power(10,length(VText));
        end else begin
          Result := false;
        end;
      end;
      if (Vgms > Vdelitel) and (Vdelitel > 1) then Vgms := 0;
      if Vgms <> 0 then begin
        if AValue < 0 then begin
          AValue := AValue - Vgms/Vdelitel;
        end else begin
          AValue := AValue + Vgms/Vdelitel;
        end;
      end;
      if (Vminus) and (AValue > 0) then AValue := -AValue;
      Vdelitel := Vdelitel*60;
    until (Vpos = 0) or (Vdelitel > 3600) or (not result);
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

procedure _AppendStr(var ACollector: String; const ADelimiter, ANewItem: String);
begin
  if (0<Length(ACollector)) then
    ACollector := ACollector + ADelimiter;
  ACollector := ACollector + ANewItem;
end;

procedure _MakeObjectFromArray(
  const AVectorDataFactory: IVectorDataFactory;
  const AIdData: Pointer;
  const AGeometryFactory: IGeometryLonLatFactory;
  const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
  const AHead{, AList}: TStrings;
  const AOldValues: PCSVPointFieldValues;
  const AIndices: PCSVPointFieldIndices;
  const APointsAggregator: IDoublePointsAggregator;
  const AAllItems: IVectorItemSubsetBuilder
);
var
  i: TCSVPointFieldType;
  VPointName, VPointDesc: String;
  VItem: IVectorDataItemSimple;
  VPoint: IGeometryLonLatPoint;
  VPath: IGeometryLonLatMultiLine;
  VPoly: IGeometryLonLatMultiPolygon;
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
    // make
    if not PointIsEmpty(APointsAggregator.Points[0]) then begin
      VPoint := AGeometryFactory.CreateLonLatPoint(APointsAggregator.Points[0]);
      VItem :=
        AVectorDataFactory.BuildItem(
          AVectorDataItemMainInfoFactory.BuildMainInfo(AIdData, VPointName, VPointDesc),
          nil,
          VPoint
        );
    end;
  end else if (APointsAggregator.Count>2) and DoublePointsEqual(APointsAggregator.Points[0], APointsAggregator.Points[APointsAggregator.Count-1]) then begin
    VPoly := AGeometryFactory.CreateLonLatMultiPolygon(APointsAggregator.Points, APointsAggregator.Count);
    if Assigned(VPoly) then begin
      // make
      VItem :=
        AVectorDataFactory.BuildItem(
          AVectorDataItemMainInfoFactory.BuildMainInfo(AIdData, VPointName, VPointDesc),
          nil,
          VPoly
        );
    end;
  end else begin
    VPath := AGeometryFactory.CreateLonLatMultiLine(APointsAggregator.Points, APointsAggregator.Count);
    if Assigned(VPath) then begin
      // make
      VItem :=
        AVectorDataFactory.BuildItem(
          AVectorDataItemMainInfoFactory.BuildMainInfo(AIdData, VPointName, VPointDesc),
          nil,
          VPath
        );
    end;
  end;

  if (VItem <> nil) then begin
    // add mark to array
    AAllItems.Add(VItem);
  end;
end;

procedure _MakeNewPointWithFullInfo(
  const AVectorDataFactory: IVectorDataFactory;
  const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
  const AGeometryFactory: IGeometryLonLatFactory;
  const AIdData: Pointer;
  const AHead, AList: TStrings;
  const ACoords: TDoublePoint;
  const AIndices: PCSVPointFieldIndices;
  const AVoxFieldIndex: Integer;
  const AAllItems: IVectorItemSubsetBuilder
);
var
  i: TCSVPointFieldType;
  j: Integer;
  VPointName, VPointDesc, VText: String;
  VItem: IVectorDataItemSimple;
  VPoint: IGeometryLonLatPoint;
begin
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
      // make 'name: value' pair
      VText := AHead[j] + ': ' + VText;
      // add to description
      _AppendStr(VPointDesc, '<br>', VText);
    end;
  end;

  VPoint := AGeometryFactory.CreateLonLatPoint(ACoords);
  // make simple point
  VItem :=
    AVectorDataFactory.BuildItem(
      AVectorDataItemMainInfoFactory.BuildMainInfo(AIdData, VPointName, VPointDesc),
      nil,
      VPoint
    );
  if (VItem <> nil) then begin
    // add mark to array
    AAllItems.Add(VItem);
  end;
end;

procedure _LoadFileBodyFromFile(
  const AFileBody: TStrings;
  const AData: IBinaryData
);
var
  S: AnsiString;
  i: Integer;
  VLineCounter: Byte;
begin
  // read
  SetString(S, PAnsiChar(AData.Buffer), AData.Size);

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

{ TCsvParser }

constructor TCsvParser.Create(
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AVectorDataFactory: IVectorDataFactory;
  const AGeometryFactory: IGeometryLonLatFactory
);
begin
  inherited Create;
  FVectorItemSubsetBuilderFactory := AVectorItemSubsetBuilderFactory;
  FVectorDataFactory := AVectorDataFactory;
  FVectorGeometryLonLatFactory := AGeometryFactory;
end;

function TCsvParser.Load(
  const AData: IBinaryData;
  const AIdData: Pointer;
  const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory
): IVectorItemSubset;
var
  VFileBody, VFileHeader, VParsedLine: TStringList;
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
  VAllItems: IVectorItemSubsetBuilder;
  VHeaders: string;
begin
  Result := nil;

  VFileBody := TStringList.Create;
  VFileHeader := TStringList.Create;
  VParsedLine := TStringList.Create;
  try
    // read file (do not use LoadFromFile because of #0 chars)
    // VFileBody.LoadFromFile(AFileName);
    _LoadFileBodyFromFile(VFileBody, AData);

    // check count of lines (with header!)
    if VFileBody.Count<=1 then
      Exit;

    // get header and parse into fields
    VHeaders := UpperCase(VFileBody[0]);
    if (System.Pos(#9, VHeaders)>0) then begin
      // TAB-separated
      VFileHeader.Delimiter := #9;
    end else if (System.Pos(';', VHeaders)>0) then begin
      // COMMA-separated with russian locale
      VFileHeader.Delimiter := ';';
    end else begin
      // COMMA-separated
      VFileHeader.Delimiter := ',';
    end;
    VFileHeader.StrictDelimiter := TRUE;
    VFileHeader.QuoteChar := '"';
    VFileHeader.DelimitedText := VHeaders;

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

    // and prepare to parse lines
    VParsedLine.StrictDelimiter := TRUE;
    VParsedLine.QuoteChar := '"';
    VParsedLine.Delimiter := VFileHeader.Delimiter;
    VPointsAggregator := TDoublePointsAggregator.Create;
    VAllItems := FVectorItemSubsetBuilderFactory.Build;

    // loop through
    for i := 1 to VFileBody.Count - 1 do begin
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
                FVectorDataFactory,
                AIdData,
                FVectorGeometryLonLatFactory,
                AVectorDataItemMainInfoFactory,
                VFileHeader,
                @VOldValues,
                @VPointFieldIndices,
                VPointsAggregator,
                VAllItems
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
              FVectorDataFactory,
              AVectorDataItemMainInfoFactory,
              FVectorGeometryLonLatFactory,
              AIdData,
              VFileHeader,
              VParsedLine,
              VPoint,
              @VPointFieldIndices,
              VIndexVoxField,
              VAllItems
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
        FVectorDataFactory,
        AIdData,
        FVectorGeometryLonLatFactory,
        AVectorDataItemMainInfoFactory,
        VFileHeader,
        @VOldValues,
        @VPointFieldIndices,
        VPointsAggregator,
        VAllItems
      );
      VPointsAggregator.Clear;
    end;
  finally
    VFileBody.Free;
    VFileHeader.Free;
    VParsedLine.Free;
  end;
  Result := VAllItems.MakeStaticAndClear;
end;

end.
