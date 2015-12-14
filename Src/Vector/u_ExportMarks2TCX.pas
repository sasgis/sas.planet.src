{******************************************************************************}
{* SAS.Planet (SAS.Ïëàíåòà)                                                   *}
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

unit u_ExportMarks2TCX;

interface

uses
  Windows,
  SysUtils,
  Classes,
  ActiveX,
  ALXmlDoc,
  t_Bitmap32,
  i_GeoCalc,
  i_BuildInfo,
  i_GeometryLonLat,
  i_AppearanceOfVectorItem,
  i_VectorDataItemSimple,
  i_VectorItemSubset,
  i_VectorItemTree;

type
  TExportMarks2TCX = class
  private
    FTCXDoc: TALXMLDocument;
    FGeoCalc: IGeoCalc;
    FBuildInfo: IBuildInfo;
    FFileName: string;
    FTCXNode: TALXMLNode;
    FFolders: TALXMLNode;
    FCourseFolder: TALXMLNode;
    FHistoryFolder: TALXMLNode;
    FCourses: TALXMLNode;
    FActivities: TALXMLNode;
    FNow: TDateTime;
    function AddTree(
      const ACategory: String;
      const ATree: IVectorItemTree
    ): boolean;
    function AddMarks(
      const ACategory: String;
      const AMarksSubset: IVectorItemSubset
    ): Boolean;
    function AddMark(
      const ACategory: String;
      const AMark, ARoot: IVectorDataItem;
      const ARootNode: TALXMLNode
    ): TALXMLNode;
    procedure PrepareExportToFile(const AFileName: string; const ATree: IVectorItemTree);
    procedure SaveToFile;
    procedure AddAuthor;
    function XMLDateTime(const ADateTime: TDateTime; const ADetailed: Boolean = False): AnsiString;
    function XMLText(const AStr: String): AnsiString;
    function TCXName(const AName: String): AnsiString;
    function FindSymByName(const AName: String): AnsiString;
    function FindSymByMark(const AMark: IVectorDataItem): AnsiString;
    function IsActivity(const AMark: IVectorDataItem): Boolean; // same as IsHistory; False means "Course"
    function GetActivityName(const AMark: IVectorDataItem): AnsiString;
    function ExtractDesc(const ADesc: String): String;
    function ExtractCmt(var ADesc: String): String;
    function ExtractTime(var ADesc: String; const AName: String): TDateTime;
    function ExtractType(var ADesc: String): String;
    function ExtractSym(var ADesc: String): String;
  public
    procedure ExportTreeToTCX(
      const AGeoCalc: IGeoCalc;
      const ABuildInfo: IBuildInfo;
      const ATree: IVectorItemTree;
      const AFileName: string
    );
  end;

implementation

uses
  DateUtils,
  ALString,
  {$IFNDef UNICODE}
  Compatibility,
  {$ENDIF}
  t_GeoTypes,
  i_BinaryData,
  i_LonLatRect,
  i_EnumDoublePoint,
  u_GeoToStrFunc,
  u_GeoFunc,
  u_StrFunc,
  u_StreamReadOnlyByBinaryData;

{ TExportMarks2TCX }

procedure TExportMarks2TCX.PrepareExportToFile(const AFileName: string; const ATree: IVectorItemTree);
begin
  FNow := Now;

  FTCXDoc.Options := [doNodeAutoIndent, doNodeAutoCreate];
  FTCXDoc.Active := True;
  FTCXDoc.Version := '1.0';
  FTCXDoc.Encoding := 'UTF-8';
  FTCXDoc.StandAlone := 'no';
  FTCXNode := FTCXDoc.AddChild('TrainingCenterDatabase');
  FTCXNode.Attributes['xmlns'] := 'http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2';
  FTCXNode.Attributes['xmlns:xsi'] := 'http://www.w3.org/2001/XMLSchema-instance';
  FTCXNode.Attributes['xsi:schemaLocation'] := 'http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2 http://www.garmin.com/xmlschemas/TrainingCenterDatabasev2.xsd';

  FFolders := nil;
  FCourses := nil;
  FActivities := nil;

  FFileName := AFileName;
end;

procedure TExportMarks2TCX.AddAuthor;

  {$WARNINGS OFF}
  // Can be replaced with TFormatSettings.Create($0409) on DXE+ (and possibly on D2009/D2010)
  function EnglishFormatSettings: TFormatSettings;
  begin
    FillChar(Result, SizeOf(Result), 0);

    // Result := TFormatSettings.Create($0409);
    Result.CurrencyString := '$';
    Result.CurrencyDecimals := 2;
    Result.DateSeparator := '-';
    Result.TimeSeparator := ':';
    Result.ListSeparator := ',';
    Result.ShortDateFormat := 'yyyy/MM/dd';
    Result.LongDateFormat := 'dddd, MMMM d, yyyy';
    Result.TimeAMString := 'AM';
    Result.TimePMString := 'PM';
    Result.ShortTimeFormat := 'hh:mm';
    Result.LongTimeFormat := 'hh:mm:ss';
    Result.ThousandSeparator := ',';
    Result.DecimalSeparator := '.';

    Result.ShortMonthNames[1]  := 'Jan';
    Result.ShortMonthNames[2]  := 'Feb';
    Result.ShortMonthNames[3]  := 'Mar';
    Result.ShortMonthNames[4]  := 'Apr';
    Result.ShortMonthNames[5]  := 'May';
    Result.ShortMonthNames[6]  := 'Jun';
    Result.ShortMonthNames[7]  := 'Jul';
    Result.ShortMonthNames[8]  := 'Aug';
    Result.ShortMonthNames[9]  := 'Sep';
    Result.ShortMonthNames[10] := 'Oct';
    Result.ShortMonthNames[11] := 'Nov';
    Result.ShortMonthNames[12] := 'Dec';

    Result.LongMonthNames[1]  := 'January';
    Result.LongMonthNames[2]  := 'February';
    Result.LongMonthNames[3]  := 'March';
    Result.LongMonthNames[4]  := 'April';
    Result.LongMonthNames[5]  := 'May';
    Result.LongMonthNames[6]  := 'June';
    Result.LongMonthNames[7]  := 'July';
    Result.LongMonthNames[8]  := 'August';
    Result.LongMonthNames[9]  := 'September';
    Result.LongMonthNames[10] := 'October';
    Result.LongMonthNames[11] := 'November';
    Result.LongMonthNames[12] := 'December';

    Result.ShortDayNames[1] := 'Sun';
    Result.ShortDayNames[2] := 'Mon';
    Result.ShortDayNames[3] := 'Tue';
    Result.ShortDayNames[4] := 'Wed';
    Result.ShortDayNames[5] := 'Thu';
    Result.ShortDayNames[6] := 'Fri';
    Result.ShortDayNames[7] := 'Sat';

    Result.LongDayNames[1] := 'Sunday';
    Result.LongDayNames[2] := 'Monday';
    Result.LongDayNames[3] := 'Tuesday';
    Result.LongDayNames[4] := 'Wednesday';
    Result.LongDayNames[5] := 'Thursday';
    Result.LongDayNames[6] := 'Friday';
    Result.LongDayNames[7] := 'Saturday';
  end;
  {$WARNINGS ON}

var
  VAuthor: TALXMLNode;
  VBuild: TALXMLNode;
  VVersion: TALXMLNode;
  FS: TFormatSettings;
  VBT: String;
  VDT: TDateTime;
  VVer: String;
  VVerMajor: String;
  VVerMinor: String;
  VVerBuild: String;
  VSrcRev: Integer;
  VDummy: String;
begin
  // <Author xsi:type="Application_t">
  //   <Name>Garmin Training Center(r)</Name>
  //   <Build>
  //     <Version>
  //       <VersionMajor>3</VersionMajor>
  //       <VersionMinor>6</VersionMinor>
  //       <BuildMajor>5</BuildMajor>
  //       <BuildMinor>0</BuildMinor>
  //     </Version>
  //     <Type>Release</Type>
  //     <Time>Aug 17 2011, 11:13:24</Time>
  //     <Builder>sqa</Builder>
  //   </Build>
  //   <LangID>EN</LangID>
  //   <PartNumber>006-A0119-00</PartNumber>
  // </Author>

  VAuthor := FTCXNode.AddChild('Author');
  VAuthor.Attributes['xsi:type'] := 'Application_t';
  VAuthor.AddChild('Name').Text := XMLText('SAS.Planet');

  VBuild := VAuthor.AddChild('Build');
  VVersion := VBuild.AddChild('Version');

  FBuildInfo.GetBuildSrcInfo(VSrcRev, VDummy);
  VDT := FBuildInfo.GetBuildDate;
  if VDT = 0 then
    VDT := FNow;
  VVer := FBuildInfo.GetVersion;
  VVerMajor := Trim(Copy(VVer, 1, 2)); if VVerMajor = '' then VVerMajor := IntToStr(YearOf(VDT) mod 100);
  VVerMinor := Trim(Copy(VVer, 3, 2)); if VVerMinor = '' then VVerMinor := IntToStr(MonthOf(VDT));
  VVerBuild := Trim(Copy(VVer, 5, 2)); if VVerBuild = '' then VVerBuild := IntToStr(DayOf(VDT));
  VVersion.AddChild('VersionMajor').Text := XMLText(VVerMajor);
  VVersion.AddChild('VersionMinor').Text := XMLText(VVerMinor);
  VVersion.AddChild('BuildMajor').Text   := XMLText(VVerBuild);
  VVersion.AddChild('BuildMinor').Text   := XMLText(IntToStr(VSrcRev));

  VBT := LowerCase(FBuildInfo.GetBuildType);
  // <xsd:enumeration value="Internal"/>
  // <xsd:enumeration value="Alpha"/>
  // <xsd:enumeration value="Beta"/>
  // <xsd:enumeration value="Release"/>
  if VBT = 'nightly' then
    VBuild.AddChild('Type').Text := 'Beta'
  else
  if VBT = 'stable' then
    VBuild.AddChild('Type').Text := 'Release'
  else  // GetBuildType = 'Custom'
    VBuild.AddChild('Type').Text := 'Internal';

  FS := EnglishFormatSettings; // can be replaced with TFormatSettings.Create($0409) on XE+ (and possibly on D2009/D2010)
  VBuild.AddChild('Time').Text := XMLText(FormatDateTime('mmm d yyyy, hh:nn:ss', VDT, FS));
  if (VBT = 'nightly') or (VBT = 'stable') then
    VBuild.AddChild('Builder').Text := XMLText('SAS.Team'); // marked as optional
  VAuthor.AddChild('LangID').Text := XMLText('EN');
  VAuthor.AddChild('PartNumber').Text := XMLText('006-A0119-00'); // seems to be mandatory? Set it to value for Garmin Training Center software
end;

procedure TExportMarks2TCX.SaveToFile;
var
  VFileStream: TFileStream;
begin
  VFileStream := TFileStream.Create(FFileName, fmCreate);
  try
    FTCXDoc.SaveToStream(VFileStream);
  finally
    VFileStream.Free;
  end;
end;

procedure TExportMarks2TCX.ExportTreeToTCX(
  const AGeoCalc: IGeoCalc;
  const ABuildInfo: IBuildInfo;
  const ATree: IVectorItemTree;
  const AFileName: string
);
begin
  FGeoCalc := AGeoCalc;
  FBuildInfo := ABuildInfo;
  FTCXDoc := TALXMLDocument.Create;
  try
    PrepareExportToFile(AFileName, ATree);
    AddTree('', ATree);
    AddAuthor;
    SaveToFile;
  finally
    FreeAndNil(FTCXDoc);
    FGeoCalc := nil;
  end;
end;

function TExportMarks2TCX.AddMarks(
  const ACategory: String;
  const AMarksSubset: IVectorItemSubset
): Boolean;
var
  VMark: IVectorDataItem;
  VEnumMarks: IEnumUnknown;
  VPath: IVectorDataItem;
  VPathNode: TALXMLNode;
  VTmp: TALXMLNode;
  i: integer;
  VPathSaved: Boolean;
begin
  Result := False;
  if Assigned(AMarksSubset) then begin
    VPathSaved := False;
    VPath := nil;
    VPathNode := nil;

    // Export lines/multi-lines first
    VEnumMarks := AMarksSubset.GetEnum;
    while (VEnumMarks.Next(1, VMark, @i) = S_OK) do begin
      // Skip points
      if Supports(VMark.Geometry, IGeometryLonLatPoint) then
      begin
        VMark := nil;
        Continue;
      end;

      VTmp := AddMark(ACategory, VMark, nil, nil);

      // Save first line/multi-line into VPath - but only if it is the only line in a group
      if VTmp <> nil then
      begin
        if VPathSaved then begin
          VPath := nil;
          VPathNode := nil;
        end
        else begin
          VPath := VMark;
          VPathNode := VTmp;
          VPathSaved := True;
        end;
      end;

      VMark := nil;
      Result := True;
    end;

    // Export points only (after lines/multi-lines)
    VEnumMarks.Reset;
    while (VEnumMarks.Next(1, VMark, @i) = S_OK) do begin
      if Supports(VMark.Geometry, IGeometryLonLatPoint) then
        AddMark(ACategory, VMark, VPath, VPathNode);
      VMark := nil;
      Result := True;
    end;
  end;
end;

function TExportMarks2TCX.AddTree(
  const ACategory: String;
  const ATree: IVectorItemTree
): boolean;
var
  i: Integer;
  VSubTree: IVectorItemTree;
begin
  Result := False;
  if not Assigned(ATree) then
    Exit;

  for i := 0 to ATree.SubTreeItemCount - 1 do begin
    VSubTree := ATree.GetSubTreeItem(i);
    if AddTree(ACategory + '\' + VSubTree.Name, VSubTree) then
      Result := True;
  end;
  if AddMarks(ACategory, ATree.Items) then
    Result := True;
end;

function TExportMarks2TCX.AddMark(
  const ACategory: String;
  const AMark, ARoot: IVectorDataItem;
  const ARootNode: TALXMLNode
): TALXMLNode;
const
  DummySpeedKMH = 5; // 5 Km / Hour - dummy speed for export TRK/track
  DummySpeedMS  = DummySpeedKMH * 1000 / (60 * 60); // dummy speed in meters per second

  procedure AddFolders(const ACategory, AName: String; const AIsActivity: Boolean);
  var
    X: Integer;
    VCategories: string;
    VCategory: string;
    VFolder: TALXMLNode;
    VSubFolder: TALXMLNode;
    VNameRef: TALXMLNode;
    VTemp: TALXMLNode;
    VRef: AnsiString;
    VCatXML: AnsiString;
  begin
    VCategories := ACategory;
    if (VCategories <> '') and (VCategories[1] = '\') then
      Delete(VCategories, 1, 1);
    VCategories := Trim(VCategories);

    if VCategories = '' then
      Exit;

    if AIsActivity then begin
      if FHistoryFolder = nil then
      begin
        if FFolders = nil then
          FFolders := FTCXNode.AddChild('Folders');
        FHistoryFolder := FFolders.AddChild('History');
        FHistoryFolder := FHistoryFolder.AddChild('Other');
      end;
    end
    else begin
      if FCourseFolder = nil then
      begin
        if FFolders = nil then
          FFolders := FTCXNode.AddChild('Folders');
        FCourseFolder := FFolders.AddChild('Courses');
        FCourseFolder := FCourseFolder.AddChild('CourseFolder');
      end;
    end;

    VFolder := nil;
    repeat
      X := Pos('\', VCategories);
      if X > 0 then
      begin
        VCategory := Trim(Copy(VCategories, 1, X - 1));
        VCategories := Trim(Copy(VCategories, X + 1, MaxInt));
      end
      else
      begin
        VCategory := VCategories;
        VCategories := '';
      end;

      if VCategory <> '' then
      begin
        VCatXML := XMLText(VCategory);
        if VFolder = nil then
        begin
          if AIsActivity then
            VFolder := FHistoryFolder
          else
            VFolder := FCourseFolder;
          VFolder.Attributes['Name'] := VCatXML;
        end
        else
        begin
          VSubFolder := nil;
          for X := 0 to VFolder.ChildNodes.Count - 1 do
          begin
            VTemp := VFolder.ChildNodes[X];
            if VTemp.Attributes['Name'] = VCatXML then
            begin
              VSubFolder := VTemp;
              Break;
            end;
          end;
          if VSubFolder = nil then
          begin
            VSubFolder := VFolder.AddChild('Folder');
            VSubFolder.Attributes['Name'] := VCatXML;
          end;

          VFolder := VSubFolder;
        end;

        // VFolder.Attributes['Notes'] - can fill this with notes/description

        if VCategories = '' then
        begin
          if AIsActivity then
          begin
            VNameRef := VFolder.AddChild('ActivityRef');
            VRef := GetActivityName(AMark);
          end
          else
          begin
            VNameRef := VFolder.AddChild('CourseNameRef');
            VRef := TCXName(AName);
          end;
          VNameRef.AddChild('Id').Text := VRef;
        end;
      end;
    until VCategories = '';
  end;

  function AddPoint(
    const AMark, ARoot: IVectorDataItem;
    const ARootNode: TALXMLNode;
    const ALonLatPoint: IGeometryLonLatPoint;
    const AIsActivity: Boolean): TALXMLNode;
  var
    VCurrentNode: TALXMLNode;
    VPosition: TALXMLNode;
    VDesc: String;
    VCmt: String;
    VLength: Double;
    VDT: TDateTime;
    VLonLatLine: IGeometryLonLatSingleLine;
  begin
       // Unclear which course should be associated with these points
    if (ARoot = nil) or (ARootNode = nil) or
       // Activity does not have any additional points
       AIsActivity then begin
      Result := nil;
      Exit;
    end;

    // <CoursePoint>
    //   <Name>Налево</Name>
    //   <Time>2015-12-12T23:59:35Z</Time>
    //   <Position>
    //     <LatitudeDegrees>56.8586142</LatitudeDegrees>
    //     <LongitudeDegrees>35.9078204</LongitudeDegrees>
    //   </Position>
    //   <PointType>Left</PointType>
    //   <Notes>Поверните налево на просп. Тверской</Notes>
    // </CoursePoint>

    VCurrentNode := ARootNode.AddChild('CoursePoint');
    VCurrentNode.AddChild('Name').Text := TCXName(AMark.Name);
    Result := VCurrentNode;

    // Order of extraction is important
    VDesc := ExtractDesc(AMark.Desc);
    VCmt := ExtractCmt(VDesc);
    VDT := ExtractTime(VDesc, AMark.Name);
    ExtractType(VDesc);
    ExtractSym(VDesc);
    if (VCmt = '') and (VDesc <> '') then
    begin
      VCmt := VDesc;
      VDesc := '';
    end;
    VDesc := Trim(VDesc + sLineBreak + VCmt);

    if VDT = 0 then
    begin
      VLength := FGeoCalc.CalcLineLength(VLonLatLine); // distance in meters
      VDT := IncSecond(FNow, -Round(VLength / DummySpeedMS)); // VStartTime from AddLine
    end;
    VCurrentNode.AddChild('Time').Text := XMLDateTime(VDT);

    VPosition := VCurrentNode.AddChild('Position');
    VPosition.AddChild('LatitudeDegrees').Text := R2AnsiStrPoint(ALonLatPoint.Point.Y);
    VPosition.AddChild('LongitudeDegrees').Text := R2AnsiStrPoint(ALonLatPoint.Point.X);

    VPosition.AddChild('PointType').Text := FindSymByMark(AMark);

    if VDesc <> '' then
      VCurrentNode.ChildNodes['Notes'].Text := XMLText(VDesc);
  end;

  function AddLine(
    const AMark: IVectorDataItem;
    const ALonLatLine: IGeometryLonLatSingleLine;
    const AIsActivity: Boolean): TALXMLNode;
  var
    VCurrentNode: TALXMLNode;
    VTrack: TALXMLNode;
    VPoints: IEnumLonLatPoint;
    VPoint: TDoublePoint;
    VLast: TDoublePoint;
    VPointNode: TALXMLNode;
    VPosition: TALXMLNode;
    VLength: Double;
    VStartTime: TDateTime;
    VDelta: TDateTime;
    VDesc: String;
    VCmt: String;
    VDT: TDateTime;
    VFirst: Boolean;
  begin
    if AIsActivity then begin
      if FActivities = nil then
        FActivities := FTCXNode.AddChild('Activities');

      VCurrentNode := FActivities.AddChild('Activity');
      VCurrentNode.AddChild('Id').Text := GetActivityName(AMark);
      VCurrentNode.Attributes['Sport'] := 'Other';
      Result := VCurrentNode;

      // Order of extraction is important
      VDesc := ExtractDesc(AMark.Desc);
      VCmt := ExtractCmt(VDesc);
      VDT := ExtractTime(VDesc, AMark.Name);
      ExtractType(VDesc);
      ExtractSym(VDesc);
      if (VCmt = '') and (VDesc <> '') then
      begin
        VCmt := VDesc;
        VDesc := '';
      end;
      if VDesc <> '' then
        VCurrentNode.ChildNodes['Notes'].Text := XMLText(VDesc);

      VCurrentNode := VCurrentNode.AddChild('Lap');
      VCurrentNode.Attributes['StartTime'] := XMLDateTime(VDT);
      if VDesc <> '' then
        VCurrentNode.ChildNodes['Notes'].Text := XMLText(VDesc);

      VLength := FGeoCalc.CalcLineLength(ALonLatLine); // distance in meters
      VStartTime := IncSecond(FNow, -Round(VLength / DummySpeedMS));

      // Minimum mandatory
      VCurrentNode.AddChild('TotalTimeSeconds').Text := XMLText(IntToStr(SecondsBetween(FNow, VStartTime)));
      VCurrentNode.AddChild('DistanceMeters').Text := XMLText(StringReplace(FloatToStr(VLength), DecimalSeparator, '.', []));
      VCurrentNode.AddChild('Calories').Text := '0';

      VDelta := (FNow - VStartTime) / ALonLatLine.Count;
      VPoints := ALonLatLine.GetEnum;

      VTrack := VCurrentNode.AddChild('Track');

      while VPoints.Next(VPoint) do begin
        VPointNode := VTrack.AddChild('Trackpoint');
        VPointNode.AddChild('Time').Text := XMLDateTime(VStartTime); // Fake time
        VPosition := VPointNode.AddChild('Position');
        VPosition.AddChild('LatitudeDegrees').Text := R2AnsiStrPoint(VPoint.Y);
        VPosition.AddChild('LongitudeDegrees').Text := R2AnsiStrPoint(VPoint.X);
        VStartTime := VStartTime + VDelta;
      end;
    end
    else begin
      if FCourses = nil then
        FCourses := FTCXNode.AddChild('Courses');

      VCurrentNode := FCourses.AddChild('Course');
      VCurrentNode.AddChild('Name').Text := TCXName(AMark.Name);
      Result := VCurrentNode;

      // Order of extraction is important
      VDesc := ExtractDesc(AMark.Desc);
      VCmt := ExtractCmt(VDesc);
      ExtractTime(VDesc, AMark.Name);
      ExtractType(VDesc);
      ExtractSym(VDesc);
      if (VCmt = '') and (VDesc <> '') then
      begin
        VCmt := VDesc;
        VDesc := '';
      end;
      VDesc := Trim(VDesc + sLineBreak + VCmt);
      if VDesc <> '' then
        VCurrentNode.ChildNodes['Notes'].Text := XMLText(VDesc);

      VTrack := VCurrentNode.AddChild('Track');

      VLength := FGeoCalc.CalcLineLength(ALonLatLine); // distance in meters
      VStartTime := IncSecond(FNow, -Round(VLength / DummySpeedMS));
      VDelta := (FNow - VStartTime) / ALonLatLine.Count;
      VPoints := ALonLatLine.GetEnum;
      VFirst := True;
      VLast.X := 0;
      VLast.Y := 0;
      while VPoints.Next(VPoint) do begin
        // <Trackpoint>
        //   <Time>2015-12-12T23:11:54Z</Time>
        //   <Position>
        //     <LatitudeDegrees>56.8571200</LatitudeDegrees>
        //     <LongitudeDegrees>35.7483100</LongitudeDegrees>
        //   </Position>
        //   <AltitudeMeters>137.9000000</AltitudeMeters>
        //   <DistanceMeters>31190.2382813</DistanceMeters>
        //   <Cadence>0</Cadence>
        //   <SensorState>Absent</SensorState>
        // </Trackpoint>

        if VFirst then begin
          VFirst := False;
          VPointNode := VCurrentNode.AddChild('CoursePoint');
          VPointNode.AddChild('Name').Text := 'Start';
          VPointNode.AddChild('Time').Text := XMLDateTime(VStartTime);
          VPosition := VPointNode.AddChild('Position');
          VPosition.AddChild('LatitudeDegrees').Text := R2AnsiStrPoint(VPoint.Y);
          VPosition.AddChild('LongitudeDegrees').Text := R2AnsiStrPoint(VPoint.X);
          VPosition.AddChild('PointType').Text := 'Generic';
        end;

        VPointNode := VTrack.AddChild('Trackpoint');
        VPointNode.AddChild('Time').Text := XMLDateTime(VStartTime); // Fake time
        VPosition := VPointNode.AddChild('Position');
        VPosition.AddChild('LatitudeDegrees').Text := R2AnsiStrPoint(VPoint.Y);
        VPosition.AddChild('LongitudeDegrees').Text := R2AnsiStrPoint(VPoint.X);
        VStartTime := VStartTime + VDelta;
        VLast := VPoint;
      end;

      if not VFirst then begin
        VPointNode := VCurrentNode.AddChild('CoursePoint');
        VPointNode.AddChild('Name').Text := 'End';
        VPointNode.AddChild('Time').Text := XMLDateTime(FNow);
        VPosition := VPointNode.AddChild('Position');
        VPosition.AddChild('LatitudeDegrees').Text := R2AnsiStrPoint(VLast.Y);
        VPosition.AddChild('LongitudeDegrees').Text := R2AnsiStrPoint(VLast.X);
        VPosition.AddChild('PointType').Text := 'Generic';
      end;
    end;
  end;

  function AddMultiLine(
    const AMark: IVectorDataItem;
    const ALonLatPath: IGeometryLonLatMultiLine;
    const AIsActivity: Boolean): TALXMLNode;
  var
    VCurrentNode: TALXMLNode;
    VLonLatPathLine: IGeometryLonLatSingleLine;
    VRootNode: TALXMLNode;
    VPoints: IEnumLonLatPoint;
    VPoint: TDoublePoint;
    VPointNode: TALXMLNode;
    VPosition: TALXMLNode;
    VLength: Double;
    VStartTime: TDateTime;
    VDelta: TDateTime;
    VCount: Integer;
    VDesc: String;
    VCmt: String;
    i: Integer;
  begin
    if ALonLatPath.Count <= 0 then begin
      Result := nil;
      Exit;
    end;
    if ALonLatPath.Count = 1 then begin
      VLonLatPathLine := ALonLatPath.Item[0];
      Result := AddLine(AMark, VLonLatPathLine, AIsActivity);
      Exit;
    end;

    if FActivities = nil then
      FActivities := FTCXNode.AddChild('Activities');

    VCurrentNode := FActivities.AddChild('Activity');
    VCurrentNode.AddChild('Id').Text := GetActivityName(AMark);
    VCurrentNode.Attributes['Sport'] := 'Other';
    Result := VCurrentNode;

    // Order of extraction is important
    VDesc := ExtractDesc(AMark.Desc);
    VCmt := ExtractCmt(VDesc);
    ExtractTime(VDesc, AMark.Name);
    ExtractType(VDesc);
    ExtractSym(VDesc);
    if (VCmt = '') and (VDesc <> '') then
    begin
      VCmt := VDesc;
      VDesc := '';
    end;
    if VDesc <> '' then
      VCurrentNode.ChildNodes['Notes'].Text := XMLText(VDesc);

    VCurrentNode := VCurrentNode.AddChild('Lap');
    if VDesc <> '' then
      VCurrentNode.ChildNodes['Notes'].Text := XMLText(VDesc);

    VLength := FGeoCalc.CalcLineLength(ALonLatPath); // distance in meters
    VStartTime := IncSecond(FNow, -Round(VLength / DummySpeedMS));

    // Minimum mandatory
    VCurrentNode.AddChild('TotalTimeSeconds').Text := XMLText(IntToStr(SecondsBetween(FNow, VStartTime)));
    VCurrentNode.AddChild('DistanceMeters').Text := XMLText(StringReplace(FloatToStr(VLength), DecimalSeparator, '.', []));
    VCurrentNode.AddChild('Calories').Text := '0';

    VCount := 0;
    for i := 0 to ALonLatPath.Count - 1 do begin
      VLonLatPathLine := ALonLatPath.Item[i];
      VCount := VCount + VLonLatPathLine.Count;
    end;
    VDelta := (FNow - VStartTime) / VCount;

    for i := 0 to ALonLatPath.Count - 1 do begin
      VLonLatPathLine := ALonLatPath.Item[i];
      if VLonLatPathLine.Count > 0 then begin

        VRootNode := VCurrentNode.AddChild('Track');
        VPoints := VLonLatPathLine.GetEnum;
        while VPoints.Next(VPoint) do begin
          VPointNode := VRootNode.AddChild('Trackpoint');
          VPointNode.AddChild('Time').Text := XMLDateTime(VStartTime); // Fake time
          VPosition := VPointNode.AddChild('Position');
          VPosition.AddChild('LatitudeDegrees').Text := R2AnsiStrPoint(VPoint.Y);
          VPosition.AddChild('LongitudeDegrees').Text := R2AnsiStrPoint(VPoint.X);
          VStartTime := VStartTime + VDelta;
        end;

      end;
    end;
  end;

var
  VLonLatPoint: IGeometryLonLatPoint;
  VLonLatSingleLine: IGeometryLonLatSingleLine;
  VLonLatMultiLine: IGeometryLonLatMultiLine;
  VActivity: Boolean;
begin
  if Supports(AMark.Geometry, IGeometryLonLatPoint, VLonLatPoint) then begin
    VActivity := IsActivity(ARoot);
    Result := AddPoint(AMark, ARoot, ARootNode, VLonLatPoint, VActivity)
  end
  else if Supports(AMark.Geometry, IGeometryLonLatSingleLine, VLonLatSingleLine) then begin
    VActivity := IsActivity(AMark);
    AddFolders(ACategory, AMark.Name, VActivity);
    Result := AddLine(AMark, VLonLatSingleLine, VActivity);
  end
  else if Supports(AMark.Geometry, IGeometryLonLatMultiLine, VLonLatMultiLine) then begin
    VActivity := IsActivity(AMark);
    AddFolders(ACategory, AMark.Name, VActivity);
    Result := AddMultiLine(AMark, VLonLatMultiLine, VActivity);
  end
  else
    Result := nil;
end;

function TExportMarks2TCX.XMLDateTime(const ADateTime: TDateTime; const ADetailed: Boolean): AnsiString;

  function LocalDateTimeToDateTime(const ADateTime: TDateTime): TDateTime;
  const
    CMinutesPerDay = 60 * 24;
  var
    VTimeZoneInfo: TTimeZoneInformation;
  begin
    FillChar(VTimeZoneInfo, SizeOf(VTimeZoneInfo), #0);
    case GetTimeZoneInformation(VTimeZoneInfo) of
      TIME_ZONE_ID_STANDARD, TIME_ZONE_ID_UNKNOWN:
        Result := ADateTime + (VTimeZoneInfo.Bias + VTimeZoneInfo.StandardBias) / CMinutesPerDay;
      TIME_ZONE_ID_DAYLIGHT:
        Result := ADateTime + (VTimeZoneInfo.Bias + VTimeZoneInfo.DaylightBias) / CMinutesPerDay;
    else
      begin
        RaiseLastOSError;
        Result := ADateTime;
      end;
    end;
  end;

var
  Format: AnsiString;
  FormatSettings: TALFormatSettings;
begin
  FormatSettings.DateSeparator := '-';
  FormatSettings.TimeSeparator := ':';
  if ADetailed then
    Format := 'yyyy"-"mm"-"dd"T"hh":"nn":"ss"."z"Z"'
  else
    Format := 'yyyy"-"mm"-"dd"T"hh":"nn":"ss"Z"';
  Result := ALFormatDateTime(Format, LocalDateTimeToDateTime(ADateTime), FormatSettings); // '2015-07-19T07:53:32Z';
end;

function TExportMarks2TCX.XMLText(const AStr: String): AnsiString;
var
  VStr: String;
begin
  VStr := AdjustLineBreaks(AStr);

  // The following is performed by ALXmlDoc:
  //VStr := StringReplace(VStr, '&',  '&amp;',  [rfReplaceAll]);
  //VStr := StringReplace(VStr, '"',  '&quot;', [rfReplaceAll]);
  //VStr := StringReplace(VStr, '''', '&apos;', [rfReplaceAll]);
  //VStr := StringReplace(VStr, '<',  '&lt;',   [rfReplaceAll]);
  //VStr := StringReplace(VStr, '>',  '&gt;',   [rfReplaceAll]);

  Result := UTF8Encode(VStr);
end;

function TExportMarks2TCX.TCXName(const AName: String): AnsiString;
{
  <xsd:simpleType name="RestrictedToken_t">
    <xsd:restriction base="Token_t">
      <xsd:minLength value="1"/>
      <xsd:maxLength value="15"/>
    </xsd:restriction>
  </xsd:simpleType>
}
begin
  Result := XMLText(Trim(Copy(Trim(AName), 1, 15)));
end;

function TExportMarks2TCX.FindSymByName(const AName: String): AnsiString;
const
  GarminSymNames: array[0..15] of String = (
    'Generic',
    'Summit',
    'Valley',
    'Water',
    'Food',
    'Danger',
    'Left',
    'Right',
    'Straight',
    'First Aid',
    '4th Category',
    '3rd Category',
    '2nd Category',
    '1st Category',
    'Hors Category',
    'Sprint');

type
  TWords = array of String;
  TIndexRec = record Similarity: Double; Sym: Integer; end;
  TIndex = array of TIndexRec;

  procedure SplitIntoWords(const AImageName: String; out AWords: TWords);
  var
    ImageName: String;
    StartInd: Integer;
    X: Integer;
    IsCapital: Boolean;
    IsLetter: Boolean;
    NextIsSmall: Boolean;
    IsDigit: Boolean;
    IsNewWord: Boolean;
    CopyDigit: Boolean;
  begin
    AWords := nil;
    ImageName := Trim(AImageName);
    StartInd := 1;
    CopyDigit := CharInSet(ImageName[1], ['0'..'9']);
    for X := 1 to Length(ImageName) do begin
      IsCapital := CharInSet(ImageName[X], ['A'..'Z']);
      IsLetter := IsCapital or CharInSet(ImageName[X], ['a'..'z']);
      IsDigit := CharInSet(ImageName[X], ['0'..'9']);
      NextIsSmall := (X < Length(ImageName)) and CharInSet(ImageName[X + 1], ['a'..'z']);
      if CopyDigit then
        IsNewWord := not IsDigit
      else
        IsNewWord := (not IsLetter) or
                     (
                       IsCapital and
                       NextIsSmall
                     );

      if IsNewWord then begin
        if X - 1 > StartInd then
        begin
          SetLength(AWords, Length(AWords) + 1);
          AWords[High(AWords)] := AnsiLowerCase(Copy(ImageName, StartInd, X - StartInd));
          CopyDigit := IsDigit;
        end;
        if CopyDigit or IsLetter then // 'word80' or 'wordWord'
          StartInd := X
        else
          StartInd := X + 1;          // 'word 80' or 'word word'
      end;
    end;

    if StartInd < Length(ImageName) then
    begin
      SetLength(AWords, Length(AWords) + 1);
      AWords[High(AWords)] := AnsiLowerCase(Copy(ImageName, StartInd, MaxInt));
    end;
  end;

  procedure BuildIndex(const AWords: TWords; out AIndex: TIndex);

    function FindSimilarity(const AGarminName: String; const AWords: TWords): Double;
    var
      GarminNames: TWords;
      X: Integer;
      Y: Integer;
    begin
      Result := 0;

      SplitIntoWords(AGarminName, GarminNames);

      for X := 0 to High(GarminNames) do
        for Y := 0 to High(AWords) do
          if AWords[Y] = GarminNames[X] then
            Result := Result + 1 + (1/100 * Length(AWords[Y])) // +1.0 for every matched word, +0.x for length (e.g. prefer longer words)
          else
            Result := Result - 0.05; // penalty for not matched words; prefer fully matched
    end;

  var
    X: Integer;
  begin
    SetLength(AIndex, Length(GarminSymNames));

    for X := 0 to High(AIndex) do
    begin
      AIndex[X].Sym := X;
      AIndex[X].Similarity := FindSimilarity(GarminSymNames[X], AWords);
    end;
  end;

  procedure SortIndex(var AIndex: TIndex; L, R: Integer);
  var
    I, J: Integer;
    P, T: TIndexRec;
  begin
    repeat
      I := L;
      J := R;
      P := AIndex[(L + R) shr 1];
      repeat
        while AIndex[I].Similarity > P.Similarity do
          Inc(I);
        while AIndex[J].Similarity < P.Similarity do
          Dec(J);
        if I <= J then
        begin
          if I <> J then
          begin
            T := AIndex[I];
            AIndex[I] := AIndex[J];
            AIndex[J] := T;
          end;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then
        SortIndex(AIndex, L, J);
      L := I;
    until I >= R;
  end;

var
  VName: String;
  VWords: TWords;
  VIndex: TIndex;
begin
  VName := AnsiLowerCase(AName);

  // Temporal usability hack for Russians
  VName := StringReplace(VName, 'лево',        ' left ',     [rfReplaceAll]);
  VName := StringReplace(VName, 'право',       ' right ',    [rfReplaceAll]);
  VName := StringReplace(VName, 'прямо',       ' straight ', [rfReplaceAll]);
  VName := StringReplace(VName, 'продолжайте', ' straight ', [rfReplaceAll]);

  SplitIntoWords(VName, VWords);
  BuildIndex(VWords, VIndex);
  SortIndex(VIndex, 0, High(VIndex));

  if VIndex[0].Similarity > 0 then
    Result := XMLText(GarminSymNames[VIndex[0].Sym])
  else
    Result := '';
  if Result = '' then
    Result := 'Generic';
end;

function TExportMarks2TCX.FindSymByMark(const AMark: IVectorDataItem): AnsiString;
var
  VAppearanceIcon: IAppearancePointIcon;
begin
  if Supports(AMark.Appearance, IAppearancePointIcon, VAppearanceIcon) and
     (VAppearanceIcon <> nil) and
     (VAppearanceIcon.Pic <> nil) then begin
    Result := FindSymByName(ChangeFileExt(ExtractFileName(VAppearanceIcon.Pic.GetName), ''));
    if Result = 'Generic' then
      Result := FindSymByName(AMark.Name);
  end
  else
    Result := FindSymByName(AMark.Name);
end;

function TExportMarks2TCX.GetActivityName(const AMark: IVectorDataItem): AnsiString;
var
  VDesc: String;
  VDT: TDateTime;
begin
  VDesc := ExtractDesc(AMark.Desc);
  ExtractCmt(VDesc);
  VDT := ExtractTime(VDesc, AMark.Name);
  if VDT = 0 then
    VDT := FNow;
  Result := XMLDateTime(VDT);
end;

// Same as IsHistory; False means "Course"
function TExportMarks2TCX.IsActivity(const AMark: IVectorDataItem): Boolean;

  function HasDateTime(const AMark: IVectorDataItem): Boolean;
  var
    VDesc: String;
  begin
    VDesc := ExtractDesc(AMark.Desc);
    ExtractCmt(VDesc);
    Result := (ExtractTime(VDesc, AMark.Name) > 0);
  end;

var
  VLonLatSingleLine: IGeometryLonLatSingleLine;
  VLonLatMultiLine: IGeometryLonLatMultiLine;
  VLCDescr: String;
begin
  if AMark = nil then begin
    Result := True;
    Exit;
  end;

  if Supports(AMark.Geometry, IGeometryLonLatSingleLine, VLonLatSingleLine) then begin
    VLCDescr := LowerCase(AMark.Desc);
    Result := (
                (VLonLatSingleLine.Count >= 500) or
                (Pos('track: true', VLCDescr) > 0)
              ) and
              (Pos('track: false', VLCDescr) <= 0) and
              HasDateTime(AMark);
  end
  else begin
    if Supports(AMark.Geometry, IGeometryLonLatMultiLine, VLonLatMultiLine) then begin
      if VLonLatMultiLine.Count = 0 then
        Result := False
      else
      if VLonLatMultiLine.Count = 1 then begin
        VLCDescr := LowerCase(AMark.Desc);
        Result := (
                    (VLonLatMultiLine.Item[0].Count >= 500) or
                    (Pos('track: true', VLCDescr) > 0)
                  ) and
                  (Pos('track: false', VLCDescr) <= 0) and
                  HasDateTime(AMark);
      end
      else
        Result := True;
    end
    else
      Result := True;
    Exit;
  end;
end;

function TExportMarks2TCX.ExtractDesc(const ADesc: String): String;

  procedure RemoveField(var AStr: String; const AFieldName: String);
  var
    X: Integer;
    Prefix: String;
    Pre: String;
  begin
    Prefix := AFieldName + ': ';
    X := Pos(Prefix, AStr);
    if X > 0 then
    begin
      Pre := Trim(Copy(AStr, 1, X - 1));
      AStr := Trim(Copy(AStr, X + Length(Prefix), MaxInt));
      X := Pos(#10, AStr);
      if X > 0 then
        AStr := Trim(Copy(AStr, X + 1, MaxInt));
      AStr := Trim(Pre + sLineBreak + AStr);
    end;
  end;

begin
  Result := Trim(AdjustLineBreaks(ADesc));

  // Remove BR-s
  Result := StringReplace(Result, '<br>' + sLineBreak,  sLineBreak,  [rfReplaceAll]);
  Result := StringReplace(Result, '<br />' + sLineBreak,  sLineBreak,  [rfReplaceAll]);
  Result := StringReplace(Result, '<br/>' + sLineBreak,  sLineBreak,  [rfReplaceAll]);
  Result := StringReplace(Result, '<br>',  '',  [rfReplaceAll]);
  Result := StringReplace(Result, '<br />',  '',  [rfReplaceAll]);
  Result := StringReplace(Result, '<br/>',  '',  [rfReplaceAll]);

  RemoveField(Result, 'number');
  RemoveField(Result, 'type');
  RemoveField(Result, 'kind');
  RemoveField(Result, 'GPS Coordinates');
end;

function TExportMarks2TCX.ExtractCmt(var ADesc: String): String;
var
  X: Integer;
  Pre: String;
begin
  // Extract "cmt:" field
  X := Pos('cmt: ', ADesc);
  if X > 0 then
  begin
    Pre := Trim(Copy(ADesc, 1, X - 1));
    ADesc := Trim(Copy(ADesc, X + Length('cmt: '), MaxInt));
    X := Pos(#10, ADesc);
    if X > 0 then
    begin
      Result := Trim(Copy(ADesc, 1, X - 1));
      ADesc := Trim(Copy(ADesc, X + 1, MaxInt));
      ADesc := Trim(Pre + sLineBreak + ADesc);
    end
    else
    begin
      Result := ADesc;
      ADesc := Pre;
    end;
  end
  else
    Result := '';
end;

function TExportMarks2TCX.ExtractTime(var ADesc: String; const AName: String): TDateTime;
var
  X: Integer;
  VPre: String;
  VDesc: String;
begin
  Result := 0;
  if TryStrToDateTime(ADesc, Result) then
  begin
    ADesc := '';
    Exit;
  end;
  VDesc := LowerCase(ADesc);

  // Extract "time:" field
  X := Pos('time: ', VDesc);
  if X > 0 then
  begin
    VPre := Trim(Copy(ADesc, 1, X - 1));
    ADesc := Trim(Copy(ADesc, X + Length('time: '), MaxInt));
    X := Pos(#10, ADesc);
    if X > 0 then
    begin
      if not TryStrToDateTime(Trim(Copy(ADesc, 1, X - 1)), Result) then Result := 0;
      ADesc := Trim(Copy(ADesc, X + 1, MaxInt));
      ADesc := Trim(VPre + sLineBreak + ADesc);
    end
    else
    begin
      if not TryStrToDateTime(ADesc, Result) then Result := 0;
      ADesc := VPre;
    end;
  end;

  if Result <> 0 then
    Exit;

  // Extract "DateTime:" field
  X := Pos('datetime: ', VDesc);
  if X > 0 then
  begin
    VPre := Trim(Copy(ADesc, 1, X - 1));
    ADesc := Trim(Copy(ADesc, X + Length('time: '), MaxInt));
    X := Pos(#10, ADesc);
    if X > 0 then
    begin
      if not TryStrToDateTime(Trim(Copy(ADesc, 1, X - 1)), Result) then Result := 0;
      ADesc := Trim(Copy(ADesc, X + 1, MaxInt));
      ADesc := Trim(VPre + sLineBreak + ADesc);
    end
    else
    begin
      if not TryStrToDateTime(ADesc, Result) then Result := 0;
      ADesc := VPre;
    end;
  end;

  if Result <> 0 then
    Exit;

  // Extract "Date:" field
  X := Pos('date: ', VDesc);
  if X > 0 then
  begin
    VPre := Trim(Copy(ADesc, 1, X - 1));
    ADesc := Trim(Copy(ADesc, X + Length('Date: '), MaxInt));
    X := Pos(#10, ADesc);
    if X > 0 then
    begin
      if not TryStrToDateTime(Trim(Copy(ADesc, 1, X - 1)), Result) then Result := 0;
      ADesc := Trim(Copy(ADesc, X + 1, MaxInt));
      ADesc := Trim(VPre + sLineBreak + ADesc);
    end
    else
    begin
      if not TryStrToDateTime(ADesc, Result) then Result := 0;
      ADesc := VPre;
    end;
  end;

  if Result = 0 then
    Result := StrToDateTimeDef(AName, 0);
end;

function TExportMarks2TCX.ExtractType(var ADesc: String): String;
var
  X: Integer;
  Pre: String;
begin
  // Extract "type:" field
  X := Pos('type: ', ADesc);
  if X > 0 then
  begin
    Pre := Trim(Copy(ADesc, 1, X - 1));
    ADesc := Trim(Copy(ADesc, X + Length('type: '), MaxInt));
    X := Pos(#10, ADesc);
    if X > 0 then
    begin
      Result := Trim(Copy(ADesc, 1, X - 1));
      ADesc := Trim(Copy(ADesc, X + 1, MaxInt));
      ADesc := Trim(Pre + sLineBreak + ADesc);
    end
    else
    begin
      Result := ADesc;
      ADesc := Pre;
    end;
  end
  else
    Result := '';
end;

function TExportMarks2TCX.ExtractSym(var ADesc: String): String;
var
  X: Integer;
  Pre: String;
begin
  // Extract "sym:" field
  X := Pos('sym: ', ADesc);
  if X > 0 then
  begin
    Pre := Trim(Copy(ADesc, 1, X - 1));
    ADesc := Trim(Copy(ADesc, X + Length('sym: '), MaxInt));
    X := Pos(#10, ADesc);
    if X > 0 then
    begin
      Result := Trim(Copy(ADesc, 1, X - 1));
      ADesc := Trim(Copy(ADesc, X + 1, MaxInt));
      ADesc := Trim(Pre + sLineBreak + ADesc);
    end
    else
    begin
      Result := ADesc;
      ADesc := Pre;
    end;
  end
  else
    Result := '';
end;


end.

