{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_ExportMarks2TCX;

// https://www8.garmin.com/xmlschemas/TrainingCenterDatabasev2.xsd

interface

uses
  Windows,
  SysUtils,
  Classes,
  ActiveX,
  Generics.Collections,
  t_GeoTypes,
  i_GeoCalc,
  i_BuildInfo,
  i_GeometryLonLat,
  i_AppearanceOfVectorItem,
  i_VectorDataItemSimple,
  i_VectorItemSubset,
  i_VectorItemTree,
  u_GpxMarkProperties,
  u_GpxFakeTimeGenerator,
  u_XmlStreamingWriter;

type
  TExportMarks2TCX = class
  private type
    TFolderInfoNode = record
      SubFolders: TStringList;
      CourseNameRefs: TStringList;
      ActivityNameRefs: TStringList;
    end;
    PFolderInfoNode = ^TFolderInfoNode;

    TCoursePointInfo = record
      CourseItem: IVectorDataItem;
      PointItems: array of IVectorDataItem;
    end;
    PCoursePointInfo = ^TCoursePointInfo;
  private
    FGeoCalc: IGeoCalc;
    FBuildInfo: IBuildInfo;
    FNow: TDateTime;
    FTcxWriter: TXmlStreamingWriter;

    FFolderCourses: TFolderInfoNode;
    FFolderHistory: TFolderInfoNode;

    FCourseItems: TList<IVectorDataItem>;
    FActivityItems: TList<IVectorDataItem>;
    FCoursePointItems: TList<PCoursePointInfo>;

    // Data Collection methods
    procedure CollectDataFromTree(const ATree: IVectorItemTree);
    procedure CollectMarksFromTree(const ACategory: string; const ATree: IVectorItemTree);
    procedure CollectMarks(const ACategory: string; const AMarksSubset: IVectorItemSubset);
    procedure UpdateFolderStructure(const ACategory, AName: string; const AIsActivity: Boolean; const AID: string);
    procedure CleanupCollectedData;
    procedure InitFolderNode(var ANode: TFolderInfoNode);
    procedure FreeFolderNode(var ANode: TFolderInfoNode);

    // XML Writing methods
    procedure WriteDocument;
    procedure WriteFolders;
    procedure WriteFolderNode(const ANode: TFolderInfoNode; const AIsHistory: Boolean);
    procedure WriteTrack(const ALonLatSingleLine: IGeometryLonLatSingleLine; const ATimeGenerator: IGpxFakeTimeGenerator);
    procedure WriteCourses;
    procedure WriteActivities;
    procedure WriteAuthor;

    // Helper functions
    function ToXmlDateTime(const ADateTime: TDateTime; const ADetailed: Boolean = False): string;
    function ToXmlText(const AStr: string): string;
    function ToTcxName(const AName: string): string;

    function FindSymByName(const AName: string): string;
    function FindSymByMark(const AMark: IVectorDataItem): string;

    function IsActivity(const AMark: IVectorDataItem; const AProp: TGpxMarkProperties): Boolean;
    function GetActivityName(const AProp: TGpxMarkProperties): string;
  public
    procedure ExportTreeToTCX(
      const AGeoCalc: IGeoCalc;
      const ABuildInfo: IBuildInfo;
      const ATree: IVectorItemTree;
      const AFileName: string
    );
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  DateUtils,
  i_EnumDoublePoint,
  u_Encodings,
  u_GeoToStrFunc;

{ TExportMarks2TCX }

constructor TExportMarks2TCX.Create;
begin
  inherited Create;

  FCourseItems := TList<IVectorDataItem>.Create;
  FActivityItems := TList<IVectorDataItem>.Create;
  FCoursePointItems := TList<PCoursePointInfo>.Create;

  InitFolderNode(FFolderCourses);
  InitFolderNode(FFolderHistory);
end;

destructor TExportMarks2TCX.Destroy;
begin
  CleanupCollectedData;

  FreeAndNil(FCourseItems);
  FreeAndNil(FActivityItems);
  FreeAndNil(FCoursePointItems);

  FreeFolderNode(FFolderCourses);
  FreeFolderNode(FFolderHistory);

  inherited Destroy;
end;

procedure TExportMarks2TCX.ExportTreeToTCX(
  const AGeoCalc: IGeoCalc;
  const ABuildInfo: IBuildInfo;
  const ATree: IVectorItemTree;
  const AFileName: string
);
var
  VFileStream: TStream;
  VEncoding: TEncoding;
begin
  FGeoCalc := AGeoCalc;
  FBuildInfo := ABuildInfo;
  FNow := Now;
  try
    // Collect data references
    CollectDataFromTree(ATree);

    // Write data to XML
    VFileStream := TFileStream.Create(AFileName, fmCreate);
    try
      VEncoding := TUTF8Encoding.Create(False);
      try
        FTcxWriter := TXmlStreamingWriter.Create(VFileStream, VEncoding);
        try
          WriteDocument;
        finally
          FreeAndNil(FTcxWriter);
        end;
      finally
        VEncoding.Free;
      end;
    finally
      VFileStream.Free;
    end;
  finally
    // Cleanup for next run
    CleanupCollectedData;
    FGeoCalc := nil;
    FBuildInfo := nil;
  end;
end;

procedure TExportMarks2TCX.InitFolderNode(var ANode: TFolderInfoNode);
begin
  ANode.SubFolders := TStringList.Create;
  ANode.SubFolders.CaseSensitive := True;
  ANode.SubFolders.Duplicates := dupIgnore;
  ANode.CourseNameRefs := TStringList.Create;
  ANode.ActivityNameRefs := TStringList.Create;
end;

procedure TExportMarks2TCX.FreeFolderNode(var ANode: TFolderInfoNode);
var
  I: Integer;
  VSubNode: PFolderInfoNode;
begin
  if ANode.SubFolders <> nil then begin
    for I := 0 to ANode.SubFolders.Count - 1 do begin
      VSubNode := PFolderInfoNode(ANode.SubFolders.Objects[I]);
      FreeFolderNode(VSubNode^);
      Dispose(VSubNode);
    end;
    FreeAndNil(ANode.SubFolders);
  end;
  FreeAndNil(ANode.CourseNameRefs);
  FreeAndNil(ANode.ActivityNameRefs);
end;

procedure TExportMarks2TCX.CleanupCollectedData;
var
  I: Integer;
begin
  FCourseItems.Clear;
  FActivityItems.Clear;

  for I := 0 to FCoursePointItems.Count - 1 do begin
    Dispose(FCoursePointItems[I]);
  end;
  FCoursePointItems.Clear;

  FreeFolderNode(FFolderCourses);
  FreeFolderNode(FFolderHistory);
  InitFolderNode(FFolderCourses);
  InitFolderNode(FFolderHistory);
end;

procedure TExportMarks2TCX.CollectDataFromTree(const ATree: IVectorItemTree);
begin
  if Assigned(ATree) then
    CollectMarksFromTree('', ATree);
end;

procedure TExportMarks2TCX.CollectMarksFromTree(const ACategory: string; const ATree: IVectorItemTree);
var
  I: Integer;
  VSubTree: IVectorItemTree;
begin
  if not Assigned(ATree) then
    Exit;

  for I := 0 to ATree.SubTreeItemCount - 1 do begin
    VSubTree := ATree.GetSubTreeItem(I);
    CollectMarksFromTree(ACategory + '\' + VSubTree.Name, VSubTree);
  end;

  CollectMarks(ACategory, ATree.Items);
end;

procedure TExportMarks2TCX.CollectMarks(const ACategory: string; const AMarksSubset: IVectorItemSubset);
var
  VMark: IVectorDataItem;
  VProp: TGpxMarkProperties;
  VEnumMarks: IEnumUnknown;
  VRootCourseItem: IVectorDataItem;
  VPointsCount, VPointIndex: integer;
  VCoursePointInfo: PCoursePointInfo;
  VIsCourceFound: Boolean;
begin
  if not Assigned(AMarksSubset) then
    Exit;

  VPointsCount := 0;
  VIsCourceFound := False;
  VRootCourseItem := nil;

  // Collect lines/multilines, find a potential root course
  VEnumMarks := AMarksSubset.GetEnum;
  while VEnumMarks.Next(1, VMark, nil) = S_OK do begin
    if Supports(VMark.Geometry, IGeometryLonLatPoint) then begin
      Inc(VPointsCount);
      VMark := nil;
      Continue;
    end;

    VProp := TGpxMarkProperties.Read(VMark);

    if IsActivity(VMark, VProp) then begin
      FActivityItems.Add(VMark);
      UpdateFolderStructure(ACategory, VMark.Name, True, GetActivityName(VProp));
    end else begin
      // It's a Course
      FCourseItems.Add(VMark);
      UpdateFolderStructure(ACategory, VMark.Name, False, ToTcxName(VMark.Name));

      if not VIsCourceFound then begin
        // First course becomes the root
        VIsCourceFound := True;
        VRootCourseItem := VMark;
      end else begin
        // More than one course, points cannot be associated
        VRootCourseItem := nil;
      end;
    end;
  end;

  if VRootCourseItem = nil then
    Exit; // No single root course found, so no points to associate

  if VPointsCount = 0 then
    Exit;

  // Collect points
  New(VCoursePointInfo);
  VCoursePointInfo.CourseItem := VRootCourseItem;
  SetLength(VCoursePointInfo.PointItems, VPointsCount);
  FCoursePointItems.Add(VCoursePointInfo);

  VPointIndex := 0;
  VEnumMarks.Reset;
  while VEnumMarks.Next(1, VMark, nil) = S_OK do begin
    if Supports(VMark.Geometry, IGeometryLonLatPoint) then begin
      VCoursePointInfo.PointItems[VPointIndex] := VMark;
      Inc(VPointIndex);
    end;
  end;
end;

procedure TExportMarks2TCX.UpdateFolderStructure(const ACategory, AName: string; const AIsActivity: Boolean; const AID: string);
var
  X: Integer;
  VCategories, VCategory, VCatXML: string;
  VCurrentNode: PFolderInfoNode;
  VSubNode: PFolderInfoNode;
  VIndex: Integer;
begin
  VCategories := Trim(ACategory);
  if (VCategories <> '') and (VCategories[1] = '\') then
    Delete(VCategories, 1, 1);
  VCategories := Trim(VCategories);

  if VCategories = '' then
    Exit;

  if AIsActivity then
    VCurrentNode := @FFolderHistory
  else
    VCurrentNode := @FFolderCourses;

  repeat
    X := Pos('\', VCategories);
    if X > 0 then begin
      VCategory := Trim(Copy(VCategories, 1, X - 1));
      VCategories := Trim(Copy(VCategories, X + 1, MaxInt));
    end else begin
      VCategory := VCategories;
      VCategories := '';
    end;

    if VCategory <> '' then begin
      VCatXML := ToXmlText(VCategory);
      VIndex := VCurrentNode.SubFolders.IndexOf(VCatXML);
      if VIndex = -1 then begin
        New(VSubNode);
        InitFolderNode(VSubNode^);
        VIndex := VCurrentNode.SubFolders.AddObject(VCatXML, TObject(VSubNode));
      end;
      VCurrentNode := PFolderInfoNode(VCurrentNode.SubFolders.Objects[VIndex]);
    end;
  until (VCategory = '') or (VCurrentNode = nil);

  if (VCategories = '') and (VCurrentNode <> nil) then begin
    if AIsActivity then
      VCurrentNode.ActivityNameRefs.Add(AID)
    else
      VCurrentNode.CourseNameRefs.Add(AID);
  end;
end;

procedure TExportMarks2TCX.WriteDocument;
begin
  FTcxWriter.Version     := '1.0';
  FTcxWriter.Encoding    := 'UTF-8';
  FTcxWriter.StandAlone  := 'no';
  FTcxWriter.IndentChars := '  ';
  FTcxWriter.LineBreak   := #13#10;

  FTcxWriter.StartDocument;
  begin
    FTcxWriter.StartElement('TrainingCenterDatabase');
    FTcxWriter.WriteAttribute('xmlns', 'http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2');
    FTcxWriter.WriteAttribute('xmlns:xsi', 'http://www.w3.org/2001/XMLSchema-instance');
    FTcxWriter.WriteAttribute('xsi:schemaLocation', 'http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2 http://www.garmin.com/xmlschemas/TrainingCenterDatabasev2.xsd');

    WriteFolders;
    WriteCourses;
    WriteActivities;
    WriteAuthor;

    FTcxWriter.EndElement; // TrainingCenterDatabase
  end;
  FTcxWriter.EndDocument;
end;

procedure TExportMarks2TCX.WriteFolders;
begin
  if (FFolderCourses.SubFolders.Count = 0) and (FFolderHistory.SubFolders.Count = 0) then
    Exit;

  FTcxWriter.StartElement('Folders');
  begin
    if FFolderCourses.SubFolders.Count > 0 then begin
      FTcxWriter.StartElement('Courses');
      FTcxWriter.WriteAttribute('Name', 'Courses');
      begin
        WriteFolderNode(FFolderCourses, False);
      end;
      FTcxWriter.EndElement; // Courses
    end;

    if FFolderHistory.SubFolders.Count > 0 then begin
      FTcxWriter.StartElement('History');
      FTcxWriter.WriteAttribute('Name', 'History');
      begin
        FTcxWriter.StartElement('Other');
        FTcxWriter.WriteAttribute('Name', 'Other');
        begin
          WriteFolderNode(FFolderHistory, True);
        end;
        FTcxWriter.EndElement; // Other
      end;
      FTcxWriter.EndElement; // History
    end;
  end;
  FTcxWriter.EndElement; // Folders
end;

procedure TExportMarks2TCX.WriteFolderNode(const ANode: TFolderInfoNode; const AIsHistory: Boolean);
var
  I: Integer;
  VSubNode: PFolderInfoNode;
  VFolderName: string;
  VElementName: string;
  VList: TStringList;
begin
  for I := 0 to ANode.SubFolders.Count - 1 do begin
    VFolderName := ANode.SubFolders.Names[I];
    VSubNode := PFolderInfoNode(ANode.SubFolders.Objects[I]);
    FTcxWriter.StartElement('Folder');
    FTcxWriter.WriteAttribute('Name', VFolderName);
    begin
      WriteFolderNode(VSubNode^, AIsHistory);
    end;
    FTcxWriter.EndElement; // Folder
  end;

  if AIsHistory then begin
    VElementName := 'ActivityRef';
    VList := ANode.ActivityNameRefs;
  end else begin
    VElementName := 'CourseNameRef';
    VList := ANode.CourseNameRefs;
  end;

  for I := 0 to VList.Count - 1 do begin
    FTcxWriter.StartElement(VElementName);
    begin
      FTcxWriter.WriteElementString('Id', VList[I]);
    end;
    FTcxWriter.EndElement;
  end;
end;

procedure TExportMarks2TCX.WriteTrack(const ALonLatSingleLine: IGeometryLonLatSingleLine; const ATimeGenerator: IGpxFakeTimeGenerator);
var
  VIndex: Integer;
  VPoint: TDoublePoint;
  VPoints: IEnumLonLatPoint;
begin
  FTcxWriter.StartElement('Track');
  begin
    VIndex := 0;
    VPoints := ALonLatSingleLine.GetEnum;
    while VPoints.Next(VPoint) do begin
      FTcxWriter.StartElement('Trackpoint');
      begin
        FTcxWriter.WriteElementString('Time', ToXmlDateTime(ATimeGenerator.TimeStamp[VIndex]));
        FTcxWriter.StartElement('Position');
        begin
          FTcxWriter.WriteElementString('LatitudeDegrees', R2StrPoint(VPoint.Y));
          FTcxWriter.WriteElementString('LongitudeDegrees', R2StrPoint(VPoint.X));
        end;
        FTcxWriter.EndElement; // Position
      end;
      FTcxWriter.EndElement; // Trackpoint
      Inc(VIndex);
    end;
  end;
  FTcxWriter.EndElement; // Track
end;

procedure TExportMarks2TCX.WriteCourses;

  procedure WriteCoursePoint(const AName, ANotes, AType: string; const ATime: TDateTime; const APoint: TDoublePoint);
  begin
    FTcxWriter.StartElement('CoursePoint');
    begin
      FTcxWriter.WriteElementString('Name', ToTcxName(AName));
      FTcxWriter.WriteElementString('Time', ToXmlDateTime(ATime));

      FTcxWriter.StartElement('Position');
      begin
        FTcxWriter.WriteElementString('LatitudeDegrees', R2StrPoint(APoint.Y));
        FTcxWriter.WriteElementString('LongitudeDegrees', R2StrPoint(APoint.X));
      end;
      FTcxWriter.EndElement; // Position

      if AType <> '' then begin
        FTcxWriter.WriteElementString('PointType', AType);
      end;

      if ANotes <> '' then begin
        FTcxWriter.WriteElementString('Notes', ToXmlText(ANotes));
      end;
    end;
    FTcxWriter.EndElement; // CoursePoint
  end;

var
  I, J: Integer;
  VCourseItem, VPointItem: IVectorDataItem;
  VLonLatLine: IGeometryLonLatSingleLine;
  VLonLatPoint: IGeometryLonLatPoint;
  VPointInfo: PCoursePointInfo;
  VProp: TGpxMarkProperties;
  VTimeGenerator: IGpxFakeTimeGenerator;
begin
  if FCourseItems.Count = 0 then
    Exit;

  FTcxWriter.StartElement('Courses');
  begin
    for I := 0 to FCourseItems.Count - 1 do begin
      VCourseItem := IVectorDataItem(FCourseItems[I]);

      if not Supports(VCourseItem.Geometry, IGeometryLonLatSingleLine, VLonLatLine) then begin
        Continue;
      end;

      VProp := TGpxMarkProperties.Read(VCourseItem);
      VTimeGenerator := TGpxFakeTimeGenerator.Create(FNow, FGeoCalc, VLonLatLine);

      FTcxWriter.StartElement('Course');
      begin
        FTcxWriter.WriteElementString('Name', ToTcxName(VCourseItem.Name));

        if VProp.Notes <> '' then begin
          FTcxWriter.WriteElementString('Notes', ToXmlText(VProp.Notes));
        end;

        // Track
        WriteTrack(VLonLatLine, VTimeGenerator);

        // Start and End Course Points
        if VLonLatLine.Count > 1 then begin
          WriteCoursePoint('Start', '', 'Generic', VTimeGenerator.StartTimeStamp, VLonLatLine.Points[0]);
          WriteCoursePoint('End', '', 'Generic', VTimeGenerator.EndTimeStamp, VLonLatLine.Points[VLonLatLine.Count-1]);
        end;

        // External Course Points
        VPointInfo := nil;
        for J := 0 to FCoursePointItems.Count - 1 do begin
          if FCoursePointItems[J].CourseItem = VCourseItem then begin
            VPointInfo := FCoursePointItems[J];
            Break;
          end;
        end;

        if VPointInfo <> nil then begin
          for J := 0 to High(VPointInfo.PointItems) do begin
            VPointItem := VPointInfo.PointItems[J];

            if not Supports(VPointItem.Geometry, IGeometryLonLatPoint, VLonLatPoint) then begin
              Continue;
            end;

            VProp := TGpxMarkProperties.Read(VPointItem);

            WriteCoursePoint(
              VPointItem.Name,
              VProp.Notes,
              FindSymByMark(VPointItem),
              VProp.Time,
              VLonLatPoint.Point
            );
          end;
        end;
      end;
      FTcxWriter.EndElement; // Course
    end;
  end;
  FTcxWriter.EndElement; // Courses
end;

procedure TExportMarks2TCX.WriteActivities;
var
  I, J: Integer;
  VActivityItem: IVectorDataItem;
  VLonLatSingleLine: IGeometryLonLatSingleLine;
  VLonLatMultiLine: IGeometryLonLatMultiLine;
  VStartTime: TDateTime;
  VTotalTime: Int64;
  VDistance: Double;
  VProp: TGpxMarkProperties;
  VTimeGenerator: IGpxFakeTimeGenerator;
begin
  if FActivityItems.Count = 0 then
    Exit;

  FTcxWriter.StartElement('Activities');
  begin
    for I := 0 to FActivityItems.Count - 1 do begin
      VActivityItem := IVectorDataItem(FActivityItems[I]);

      if Supports(VActivityItem.Geometry, IGeometryLonLatSingleLine, VLonLatSingleLine) then begin
        VDistance := FGeoCalc.CalcLineLength(VLonLatSingleLine);
        VTimeGenerator := TGpxFakeTimeGenerator.Create(FNow, FGeoCalc, VLonLatSingleLine);
      end else
      if Supports(VActivityItem.Geometry, IGeometryLonLatMultiLine, VLonLatMultiLine) then begin
        VDistance := FGeoCalc.CalcLineLength(VLonLatMultiLine);
        VTimeGenerator := TGpxFakeTimeGenerator.Create(FNow, FGeoCalc, VLonLatMultiLine);
      end else begin
        Continue;
      end;

      VProp := TGpxMarkProperties.Read(VActivityItem);

      VStartTime := VProp.Time;
      if VStartTime = 0 then VStartTime := VTimeGenerator.StartTimeStamp;

      VTotalTime := SecondsBetween(VTimeGenerator.EndTimeStamp, VStartTime);

      FTcxWriter.StartElement('Activity');
      FTcxWriter.WriteAttribute('Sport', 'Other');
      begin
        FTcxWriter.WriteElementString('Id', GetActivityName(VProp));

        if VProp.Notes <> '' then begin
          FTcxWriter.WriteElementString('Notes', ToXmlText(VProp.Notes));
        end;

        FTcxWriter.StartElement('Lap');
        FTcxWriter.WriteAttribute('StartTime', ToXmlDateTime(VStartTime));
        begin
          if VProp.Notes <> '' then begin
            FTcxWriter.WriteElementString('Notes', ToXmlText(VProp.Notes));
          end;

          // Minimum mandatory
          FTcxWriter.WriteElementString('TotalTimeSeconds', IntToStr(VTotalTime));
          FTcxWriter.WriteElementString('DistanceMeters', R2StrPoint(VDistance));
          FTcxWriter.WriteElementString('Calories', '0');

          // Tracks
          if Supports(VActivityItem.Geometry, IGeometryLonLatSingleLine, VLonLatSingleLine) then begin
            WriteTrack(VLonLatSingleLine, VTimeGenerator);
          end else
          if Supports(VActivityItem.Geometry, IGeometryLonLatMultiLine, VLonLatMultiLine) then begin
            for J := 0 to VLonLatMultiLine.Count - 1 do begin
              WriteTrack(VLonLatMultiLine.Item[J], VTimeGenerator);
            end;
          end;
        end;
        FTcxWriter.EndElement; // Lap
      end;
      FTcxWriter.EndElement; // Activity
    end;
  end;
  FTcxWriter.EndElement; // Activities
end;

procedure TExportMarks2TCX.WriteAuthor;
var
  FS: TFormatSettings;
  VBT: string;
  VDT: TDateTime;
  VVer, VVerMajor, VVerMinor, VVerBuild: string;
  VSrcRev: Integer;
  VDummy: string;
begin
  FTcxWriter.StartElement('Author');
  begin
    FTcxWriter.WriteAttribute('xsi:type', 'Application_t');
    FTcxWriter.WriteElementString('Name', 'SAS.Planet');

    FTcxWriter.StartElement('Build');
    begin
      FTcxWriter.StartElement('Version');
      begin
        FBuildInfo.GetBuildSrcInfo(VSrcRev, VDummy);
        VDT := FBuildInfo.GetBuildDate;
        if VDT = 0 then VDT := FNow;
        VVer := FBuildInfo.GetVersion;

        VVerMajor := Trim(Copy(VVer, 1, 2)); if VVerMajor = '' then VVerMajor := IntToStr(YearOf(VDT) mod 100);
        VVerMinor := Trim(Copy(VVer, 3, 2)); if VVerMinor = '' then VVerMinor := IntToStr(MonthOf(VDT));
        VVerBuild := Trim(Copy(VVer, 5, 2)); if VVerBuild = '' then VVerBuild := IntToStr(DayOf(VDT));

        FTcxWriter.WriteElementString('VersionMajor', VVerMajor);
        FTcxWriter.WriteElementString('VersionMinor', VVerMinor);
        FTcxWriter.WriteElementString('BuildMajor', VVerBuild);
        FTcxWriter.WriteElementString('BuildMinor', IntToStr(VSrcRev));
      end;
      FTcxWriter.EndElement; // Version

      VBT := LowerCase(FBuildInfo.GetBuildType);
      if VBT = 'nightly' then
        FTcxWriter.WriteElementString('Type', 'Beta')
      else if VBT = 'stable' then
        FTcxWriter.WriteElementString('Type', 'Release')
      else
        FTcxWriter.WriteElementString('Type', 'Internal');

      FS := TFormatSettings.Create($0409);
      FTcxWriter.WriteElementString('Time', FormatDateTime('mmm d yyyy, hh:nn:ss', VDT, FS));
      if (VBT = 'nightly') or (VBT = 'stable') then begin
        FTcxWriter.WriteElementString('Builder', 'SAS.Team');
      end;
    end;
    FTcxWriter.EndElement; // Build

    FTcxWriter.WriteElementString('LangID', 'EN');
    FTcxWriter.WriteElementString('PartNumber', '006-A0119-00');
  end;
  FTcxWriter.EndElement; // Author
end;

function TExportMarks2TCX.ToXmlDateTime(const ADateTime: TDateTime; const ADetailed: Boolean): string;

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
      RaiseLastOSError;
      Result := ADateTime; // make compiler happy
    end;
  end;

var
  VFormat: string;
  VFormatSettings: TFormatSettings;
begin
  VFormatSettings.DateSeparator := '-';
  VFormatSettings.TimeSeparator := ':';
  if ADetailed then
    VFormat := 'yyyy"-"mm"-"dd"T"hh":"nn":"ss"."z"Z"'
  else
    VFormat := 'yyyy"-"mm"-"dd"T"hh":"nn":"ss"Z"';
  Result := FormatDateTime(VFormat, LocalDateTimeToDateTime(ADateTime), VFormatSettings);
end;

function TExportMarks2TCX.ToXmlText(const AStr: string): string;
begin
  Result := AdjustLineBreaks(AStr);
end;

function TExportMarks2TCX.ToTcxName(const AName: string): string;
begin
  Result := ToXmlText(Trim(Copy(Trim(AName), 1, 15)));
end;

function TExportMarks2TCX.FindSymByName(const AName: string): string;
const
  GarminSymNames: array[0..15] of string = (
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
  TWords = array of string;
  TIndexRec = record Similarity: Double; Sym: Integer; end;
  TIndex = array of TIndexRec;

  procedure SplitIntoWords(const AImageName: string; out AWords: TWords);
  var
    ImageName: string;
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

    function FindSimilarity(const AGarminName: string; const AWords: TWords): Double;
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
  VName: string;
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
    Result := ToXmlText(GarminSymNames[VIndex[0].Sym])
  else
    Result := '';
  if Result = '' then
    Result := 'Generic';
end;

function TExportMarks2TCX.FindSymByMark(const AMark: IVectorDataItem): string;
var
  VAppearanceIcon: IAppearancePointIcon;
begin
  if Supports(AMark.Appearance, IAppearancePointIcon, VAppearanceIcon) and
     (VAppearanceIcon <> nil) and
     (VAppearanceIcon.Pic <> nil) then begin
    Result := FindSymByName(ChangeFileExt(ExtractFileName(VAppearanceIcon.Pic.GetName), ''));
    if Result = 'Generic' then
      Result := FindSymByName(AMark.Name);
  end else
    Result := FindSymByName(AMark.Name);
end;

function TExportMarks2TCX.GetActivityName(const AProp: TGpxMarkProperties): string;
var
  VDT: TDateTime;
begin
  VDT := AProp.Time;
  if VDT = 0 then
    VDT := FNow;
  Result := ToXmlDateTime(VDT);
end;

// Same as IsHistory; False means "Course"
function TExportMarks2TCX.IsActivity(const AMark: IVectorDataItem; const AProp: TGpxMarkProperties): Boolean;

  function HasDateTime: Boolean;
  begin
    Result := (AProp.Time > 0);
  end;

  function IsTrack(const ALine: IGeometryLonLatSingleLine; const ADesc: string): Boolean;
  begin
    Result := ( (ALine.Count >= 500) or (AProp.Track = 'true') ) and (AProp.Track <> 'false');
  end;

var
  VLonLatSingleLine: IGeometryLonLatSingleLine;
  VLonLatMultiLine: IGeometryLonLatMultiLine;
begin
  if AMark = nil then begin
    Result := True;
    Exit;
  end;

  if Supports(AMark.Geometry, IGeometryLonLatSingleLine, VLonLatSingleLine) then begin
    Result := IsTrack(VLonLatSingleLine, LowerCase(AMark.Desc)) and HasDateTime;
  end else
  if Supports(AMark.Geometry, IGeometryLonLatMultiLine, VLonLatMultiLine) then begin
    if VLonLatMultiLine.Count = 0 then begin
      Result := False;
    end else
    if VLonLatMultiLine.Count = 1 then begin
      Result := IsTrack(VLonLatMultiLine.Item[0], LowerCase(AMark.Desc)) and HasDateTime;
    end else begin
      Result := True;
    end;
  end else begin
    Result := True;
  end;
end;

end.

