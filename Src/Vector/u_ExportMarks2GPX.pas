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

unit u_ExportMarks2GPX;

interface

uses
  Windows,
  SysUtils,
  Classes,
  ActiveX,
  Math,
  t_Bitmap32,
  i_GeoCalc,
  i_BuildInfo,
  i_GeometryLonLat,
  i_AppearanceOfVectorItem,
  i_VectorDataItemSimple,
  i_VectorItemSubset,
  i_VectorItemTree,
  u_XmlStreamingWriter;

type
  TGpxGeometryType = (
    // do not change items order
    ggtWayPoint,  // IGeometryLonLatPoint
    ggtRoute,     // IGeometryLonLatSingleLine
    ggtTrack      // IGeometryLonLatSingleLine, IGeometryLonLatMultiLine
  );

  TExportMarks2GPX = class
  private
    FLineIsAlwaysTrack: Boolean;

    FGpxWriter: TXmlStreamingWriter;
    FGeoCalc: IGeoCalc;
    FBuildInfo: IBuildInfo;
    FFileName: string;
    FTrackNumber: Integer;
    FRouteNumber: Integer;
    FNowUtc: TDateTime;

    procedure AddTree(
      const ACategory: string;
      const ATree: IVectorItemTree;
      const AGeometryType: TGpxGeometryType
    );
    procedure AddMarks(
      const ACategory: string;
      const AMarksSubset: IVectorItemSubset;
      const AGeometryType: TGpxGeometryType
    );
    procedure AddMark(
      const ACategory: string;
      const AMark: IVectorDataItem;
      const AGeometryType: TGpxGeometryType
    );
    function SaveMarkIcon(const AAppearanceIcon: IAppearancePointIcon): string;

    procedure WriteMetadata(const ATree: IVectorItemTree);

    class function ToGpxColor(const AColor32: TColor32): string; static;
    class function ToUtc(const ADateTime: TDateTime): TDateTime; static;
    class function ToXmlDateTime(const ADateTime: TDateTime; const ADetailed: Boolean = False): string; static;
    class function ToXmlText(const AStr: string): string; static; inline;

    class function FindSymByName(const AName: string): string; static;
    class function FindSymByMark(const AMark: IVectorDataItem): string; static;
  public
    procedure ExportTreeToGPX(
      const AGeoCalc: IGeoCalc;
      const ABuildInfo: IBuildInfo;
      const ATree: IVectorItemTree;
      const AFileName: string
    );
  public
    constructor Create(
      const ALineIsAlwaysTrack: Boolean
    );
  end;

implementation

uses
  DateUtils,
  t_GeoTypes,
  i_BinaryData,
  i_LonLatRect,
  i_EnumDoublePoint,
  u_Encodings,
  u_GeoToStrFunc,
  u_GeoFunc,
  u_GpxMarkProperties,
  u_GpxFakeTimeGenerator,
  u_StreamReadOnlyByBinaryData;

{ TExportMarks2GPX }

constructor TExportMarks2GPX.Create(const ALineIsAlwaysTrack: Boolean);
begin
  inherited Create;
  FLineIsAlwaysTrack := ALineIsAlwaysTrack;
end;

procedure TExportMarks2GPX.WriteMetadata(const ATree: IVectorItemTree);

  function GetUserName: string;
  var
    VSize: DWord;
  begin
    VSize := 4096;
    SetLength(Result, VSize);
    if Windows.GetUserName(PChar(Result), VSize) then begin
      if VSize > 0 then
        Dec(VSize);
      SetLength(Result, VSize);
    end else
      Result := '';
  end;

  function GetUserFullName: string;

    function RegKeyRead(const ARoot: HKey; const AKey, AName: string; const ADefault: string = ''): string;
    var
      VReg: HKey;
      VSize: Integer;
      VKey: string;
      VDataType: Integer;
    begin
      Result := ADefault;
      VKey := AKey;
      if (VKey <> '') and (VKey[1] = '\') then
        Delete(VKey, 1, 1);

      if RegOpenKeyEx(ARoot, PChar(VKey), 0, KEY_READ, VReg) = ERROR_SUCCESS then
      try
        VSize := 0;
        if RegQueryValueEx(VReg, PChar(AName), nil, @VDataType, nil, @VSize) = ERROR_SUCCESS then begin
          SetLength(Result, VSize div SizeOf(Char));
          if Result <> '' then begin
            RegQueryValueEx(VReg, PChar(AName), nil, @VDataType, PByte(PChar(Result)), @VSize);

            // Cut the last #0 char...
            if Result[Length(Result)] = #0 then
              SetLength(Result, Length(Result) - 1);
          end;
        end;
      finally
        RegCloseKey(VReg);
      end;
    end;

  begin
    Result := RegKeyRead(HKEY_LOCAL_MACHINE, '\SOFTWARE\MICROSOFT\WINDOWS NT\CURRENTVERSION', 'RegisteredOwner');
    if Result = '' then
      Result := RegKeyRead(HKEY_LOCAL_MACHINE, '\SOFTWARE\MICROSOFT\WINDOWS\CURRENTVERSION', 'RegisteredOwner');
    if Result = '' then
      Result := RegKeyRead(HKEY_LOCAL_MACHINE, '\SOFTWARE\MICROSOFT\MS SETUP (ACME)\USER INFO', 'DefName');
    if Result = 'Microsoft' then
      Result := '';
    if Result = '' then
      Result := GetUserName;
  end;

  function FindFirstMark(const ATree: IVectorItemTree; const ALineOnly: Boolean): IVectorDataItem;

    function FindMark(const AMarksSubset: IVectorItemSubset; const ALineOnly: Boolean): IVectorDataItem;
    var
      VMark: IVectorDataItem;
      VEnumMarks: IEnumUnknown;
      VDummy: IGeometryLonLatSingleLine;
    begin
      Result := nil;
      if Assigned(AMarksSubset) then begin
        VEnumMarks := AMarksSubset.GetEnum;
        while VEnumMarks.Next(1, VMark, nil) = S_OK do begin
          if Assigned(VMark) and (VMark.Name <> '') then begin
            if (not ALineOnly) or Supports(VMark.Geometry, IGeometryLonLatSingleLine, VDummy) then begin
              Result := VMark;
              Break;
            end;
          end;
        end;
      end;
    end;

  var
    VSubTree: IVectorItemTree;
    I: Integer;
  begin
    Result := nil;
    if not Assigned(ATree) then
      Exit;

    Result := FindMark(ATree.Items, ALineOnly);
    if Assigned(Result) then
      Exit;

    for I := 0 to ATree.SubTreeItemCount - 1 do begin
      VSubTree := ATree.GetSubTreeItem(I);
      Result := FindFirstMark(VSubTree, ALineOnly);
      if Assigned(Result) then
        Exit;
    end;
  end;

  procedure ScanBounds(var ABounds: TDoubleRect; const ATree: IVectorItemTree);

    procedure ScanMarksBounds(var ABounds: TDoubleRect; const AMarksSubset: IVectorItemSubset);
    var
      VMark: IVectorDataItem;
      VEnumMarks: IEnumUnknown;
      VLonLat: IGeometryLonLat;
    begin
      if Assigned(AMarksSubset) then begin
        VEnumMarks := AMarksSubset.GetEnum;
        while VEnumMarks.Next(1, VMark, nil) = S_OK do begin
          if Assigned(VMark) then begin
            if Assigned(VMark.Geometry) then begin
              if (ABounds.Left = 0) and (ABounds.Right = 0) or
                 (ABounds.Top = 0) and (ABounds.Bottom = 0) then
                ABounds := VMark.Geometry.Bounds.Rect
              else
                ABounds := UnionLonLatRects(ABounds, VMark.Geometry.Bounds.Rect);
            end
            else if Supports(VMark, IGeometryLonLat, VLonLat) then begin
              if (ABounds.Left = 0) and (ABounds.Right = 0) or
                 (ABounds.Top = 0) and (ABounds.Bottom = 0) then
                ABounds := VLonLat.Bounds.Rect
              else
                ABounds := UnionLonLatRects(ABounds, VLonLat.Bounds.Rect);
            end;
          end;
        end;
      end;
    end;

  var
    VSubTree: IVectorItemTree;
    I: Integer;
  begin
    if not Assigned(ATree) then
      Exit;

    ScanMarksBounds(ABounds, ATree.Items);

    for I := 0 to ATree.SubTreeItemCount - 1 do begin
      VSubTree := ATree.GetSubTreeItem(I);
      ScanBounds(ABounds, VSubTree);
    end;
  end;

var
  VUserEMail: string;
  VMark: IVectorDataItem;
  VBounds: TDoubleRect;
begin
  VUserEMail := GetUserFullName;
  if (Pos('@', VUserEMail) <= 1) or
     (Pos('.', VUserEMail) <= 1) then
    VUserEMail := '';

  FGpxWriter.StartElement('metadata');
  begin
    // Set name of GPX = first line mark in tree.
    // If there is no line mark - use any mark.
    // If there is no marks - use file name.
    VMark := FindFirstMark(ATree, True);
    if not Assigned(VMark) then
      VMark := FindFirstMark(ATree, False);

    if Assigned(VMark) then begin
      FGpxWriter.WriteElementString('name', ToXmlText(VMark.Name));
      if VMark.Desc <> '' then begin
        FGpxWriter.WriteElementString('desc', ToXmlText(VMark.Desc));
      end;
    end else begin
      FGpxWriter.WriteElementString('name', ToXmlText(ChangeFileExt(ExtractFileName(FFileName), '')));
    end;

    // Set user name = system user name
    FGpxWriter.StartElement('author');
    begin
      FGpxWriter.WriteElementString('name', ToXmlText(GetUserName));
      if VUserEMail <> '' then begin
        FGpxWriter.StartElement('email');
        begin
          FGpxWriter.WriteAttribute('id', ToXmlText(Copy(VUserEMail, 1, Pos('@', VUserEMail) - 1)));
          FGpxWriter.WriteAttribute('domain', ToXmlText(Copy(VUserEMail, Pos('@', VUserEMail) + 1, MaxInt)));
        end;
        FGpxWriter.EndElement; // email
      end;
    end;
    FGpxWriter.EndElement; // author

    // Link
    FGpxWriter.StartElement('link');
    begin
      FGpxWriter.WriteAttribute('href', 'https://www.sasgis.org/');
      FGpxWriter.WriteElementString('text', 'SAS.Planet');
    end;
    FGpxWriter.EndElement; // link

    // Timestamp
    FGpxWriter.WriteElementString('time', ToXmlDateTime(FNowUtc));

    // Determinate bounds of all marks
    FillChar(VBounds, SizeOf(VBounds), 0);
    ScanBounds(VBounds, ATree);
    if (VBounds.Left <> 0) or (VBounds.Right <> 0) or
       (VBounds.Top <> 0) or (VBounds.Bottom <> 0) then begin
      FGpxWriter.StartElement('bounds'); // Minimum and maximum coordinates which describe the extent of the coordinates in the file
      begin
        FGpxWriter.WriteAttribute('maxlat', R2StrPoint(VBounds.Bottom));
        FGpxWriter.WriteAttribute('maxlon', R2StrPoint(VBounds.Right));
        FGpxWriter.WriteAttribute('minlat', R2StrPoint(VBounds.Top));
        FGpxWriter.WriteAttribute('minlon', R2StrPoint(VBounds.Left));
      end;
      FGpxWriter.EndElement; // bounds
    end;
  end;
  FGpxWriter.EndElement; // metadata
end;

procedure TExportMarks2GPX.ExportTreeToGPX(
  const AGeoCalc: IGeoCalc;
  const ABuildInfo: IBuildInfo;
  const ATree: IVectorItemTree;
  const AFileName: string
);

  function GetVersion: string;
  var
    VDateTime: TDateTime;
    VVer: string;
    VVerMajor: string;
    VVerMinor: string;
    VVerBuild: string;
  begin
    VVer := FBuildInfo.GetVersion;
    VDateTime := FBuildInfo.GetBuildDate;
    if VDateTime = 0 then
      VDateTime := FNowUtc;
    VVerMajor := Trim(Copy(VVer, 1, 2)); if VVerMajor = '' then VVerMajor := IntToStr(YearOf(VDateTime) mod 100);
    VVerMinor := Trim(Copy(VVer, 3, 2)); if VVerMinor = '' then VVerMinor := IntToStr(MonthOf(VDateTime));
    VVerBuild := Trim(Copy(VVer, 5, 2)); if VVerBuild = '' then VVerBuild := IntToStr(DayOf(VDateTime));
    Result := 'v.' + VVerMajor + VVerMinor + VVerBuild;
  end;

var
  I: TGpxGeometryType;
  VFileStream: TFileStream;
  VEncoding: TEncoding;
begin
  FGeoCalc := AGeoCalc;
  FBuildInfo := ABuildInfo;
  FFileName := AFileName;
  FTrackNumber := 1;
  FRouteNumber := 1;
  FNowUtc := ToUtc(Now);

  VFileStream := TFileStream.Create(FFileName, fmCreate);
  VEncoding := TUTF8Encoding.Create(False); // UTF8 without BOM
  try
    FGpxWriter := TXmlStreamingWriter.Create(VFileStream, VEncoding);
    try
      FGpxWriter.Version := '1.0';
      FGpxWriter.Encoding := 'UTF-8';
      FGpxWriter.IndentChars := '  ';
      FGpxWriter.LineBreak := #13#10;

      FGpxWriter.StartDocument;
      begin
        FGpxWriter.StartElement('gpx');
        FGpxWriter.WriteAttribute('version', '1.1');
        FGpxWriter.WriteAttribute('creator', 'SAS.Planet ' + GetVersion);

        FGpxWriter.WriteAttribute('xmlns', 'http://www.topografix.com/GPX/1/1');
        FGpxWriter.WriteAttribute('xmlns:xsi', 'http://www.w3.org/2001/XMLSchema-instance');
        FGpxWriter.WriteAttribute('xmlns:wptx1', 'http://www.garmin.com/xmlschemas/WaypointExtension/v1');
        FGpxWriter.WriteAttribute('xmlns:gpxtrx', 'http://www.garmin.com/xmlschemas/GpxExtensions/v3');
        FGpxWriter.WriteAttribute('xmlns:gpxtpx', 'http://www.garmin.com/xmlschemas/TrackPointExtension/v1');
        FGpxWriter.WriteAttribute('xmlns:gpxx', 'http://www.garmin.com/xmlschemas/GpxExtensions/v3');

        FGpxWriter.WriteAttribute('xsi:schemaLocation',
          'http://www.topografix.com/GPX/1/1 ' +
          'http://www.topografix.com/GPX/1/1/gpx.xsd ' +
          'http://www.garmin.com/xmlschemas/WaypointExtension/v1 ' +
          'http://www8.garmin.com/xmlschemas/WaypointExtensionv1.xsd ' +
          'http://www.garmin.com/xmlschemas/TrackPointExtension/v1 ' +
          'http://www.garmin.com/xmlschemas/TrackPointExtensionv1.xsd ' +
          'http://www.garmin.com/xmlschemas/GpxExtensions/v3 ' +
          'http://www8.garmin.com/xmlschemas/GpxExtensionsv3.xsd'
        );

        WriteMetadata(ATree);

        for I := Low(I) to High(I) do begin
          AddTree('', ATree, I);
        end;

        FGpxWriter.EndElement; // gpx
      end;
      FGpxWriter.EndDocument;
    finally
      FreeAndNil(FGpxWriter);
    end;
  finally
    VEncoding.Free;
    VFileStream.Free;
    FGeoCalc := nil;
  end;
end;

procedure TExportMarks2GPX.AddMarks(
  const ACategory: string;
  const AMarksSubset: IVectorItemSubset;
  const AGeometryType: TGpxGeometryType
);
var
  VMark: IVectorDataItem;
  VEnumMarks: IEnumUnknown;
begin
  if Assigned(AMarksSubset) then begin
    VEnumMarks := AMarksSubset.GetEnum;
    while VEnumMarks.Next(1, VMark, nil) = S_OK do begin
      AddMark(ACategory, VMark, AGeometryType);
    end;
  end;
end;

procedure TExportMarks2GPX.AddTree(
  const ACategory: string;
  const ATree: IVectorItemTree;
  const AGeometryType: TGpxGeometryType
);
var
  I: Integer;
  VSubTree: IVectorItemTree;
begin
  if not Assigned(ATree) then
    Exit;

  for I := 0 to ATree.SubTreeItemCount - 1 do begin
    VSubTree := ATree.GetSubTreeItem(I);
    AddTree(ACategory + '\' + VSubTree.Name, VSubTree, AGeometryType);
  end;

  AddMarks(ACategory, ATree.Items, AGeometryType);
end;

procedure TExportMarks2GPX.AddMark(
  const ACategory: string;
  const AMark: IVectorDataItem;
  const AGeometryType: TGpxGeometryType
);

  procedure AddCategories(const APrefix: string);
  var
    X: Integer;
    VCategories: string;
    VCategory: string;
  begin
    VCategories := ACategory;
    if (VCategories <> '') and (VCategories[1] = '\') then
      Delete(VCategories, 1, 1);
    VCategories := Trim(VCategories);

    if VCategories = '' then begin
      FGpxWriter.WriteElementString(APrefix + ':Category', 'default');
      Exit;
    end;

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
        FGpxWriter.WriteElementString(APrefix + ':Category', ToXmlText(VCategory));
      end;
    until VCategories = '';
  end;

  procedure AddPoint(
    const AMark: IVectorDataItem;
    const ALonLatPoint: IGeometryLonLatPoint
  );

    function IsPhoto(const AWidth: Integer): Boolean;
    begin
      Result := AWidth > 100;
    end;

  var
    VAppearanceIcon: IAppearancePointIcon;
    VProp: TGpxMarkProperties;
    VType: string;
    VSym: string;
    VHref: string;
    VDisplayMode: string;
  begin
    if AGeometryType <> ggtWayPoint then begin
      Exit;
    end;

    VProp := TGpxMarkProperties.Read(AMark);

    FGpxWriter.StartElement('wpt');
    FGpxWriter.WriteAttribute('lat', R2StrPoint(ALonLatPoint.Point.Y));
    FGpxWriter.WriteAttribute('lon', R2StrPoint(ALonLatPoint.Point.X));
    begin
      if VProp.Time <> 0 then begin
        FGpxWriter.WriteElementString('time', ToXmlDateTime(ToUtc(VProp.Time)));
      end;

      FGpxWriter.WriteElementString('name', ToXmlText(AMark.Name));

      if VProp.Cmt <> '' then
        FGpxWriter.WriteElementString('cmt', ToXmlText(VProp.Cmt));

      if VProp.Desc <> '' then
        FGpxWriter.WriteElementString('desc', ToXmlText(VProp.Desc));

      if not Supports(AMark.Appearance, IAppearancePointIcon, VAppearanceIcon) then begin
        VAppearanceIcon := nil;
      end;

      if (VAppearanceIcon <> nil) and (VAppearanceIcon.Pic <> nil) then begin
        VHref := SaveMarkIcon(VAppearanceIcon);
        VType := VProp.TypeId;
        VSym := VProp.Sym;
        if IsPhoto(VAppearanceIcon.Pic.GetMarker.Size.X) then begin
          if VType = '' then VType := 'photo';
          if VSym = '' then VSym := 'Scenic Area';
          VDisplayMode := 'SymbolAndName';
        end else begin
          if VType = '' then VType := 'user';
          if VSym = '' then VSym := FindSymByMark(AMark);
          VDisplayMode := 'SymbolOnly';
        end
      end else begin
        VHref := '';
        VSym := '';
        VType := '';
        VDisplayMode := 'SymbolAndName';
      end;

      if VHref <> '' then begin
        FGpxWriter.StartElement('link');
        begin
          FGpxWriter.WriteAttribute('href', ToXmlText(VHref));
        end;
        FGpxWriter.EndElement; // link
      end;

      FGpxWriter.WriteElementString('sym', ToXmlText(VSym));
      FGpxWriter.WriteElementString('type', ToXmlText(VType));
      FGpxWriter.WriteElementString('fix', '2d');

      // WaypointExtension
      FGpxWriter.StartElement('extensions');
      begin
        FGpxWriter.StartElement('gpxx:WaypointExtension');
        begin
          FGpxWriter.WriteElementString('gpxx:DisplayMode', ToXmlText(VDisplayMode));
          FGpxWriter.StartElement('gpxx:Categories');
          begin
            AddCategories('gpxx');
          end;
          FGpxWriter.EndElement; // gpxx:Categories
        end;
        FGpxWriter.EndElement; // gpxx:WaypointExtension

        FGpxWriter.StartElement('wptx1:WaypointExtension');
        begin
          FGpxWriter.WriteElementString('wptx1:DisplayMode', ToXmlText(VDisplayMode));
          FGpxWriter.StartElement('wptx1:Categories');
          begin
            AddCategories('wptx1');
          end;
          FGpxWriter.EndElement; // wptx1:Categories
        end;
        FGpxWriter.EndElement; // wptx1:WaypointExtension
      end;
      FGpxWriter.EndElement; // extensions
    end;
    FGpxWriter.EndElement; // wpt
  end;

  procedure AddLine(
    const AMark: IVectorDataItem;
    const ALonLatLine: IGeometryLonLatSingleLine
  );

    function IsTrack(const AProp: TGpxMarkProperties): Boolean;
    begin
      Result := (FLineIsAlwaysTrack or (AProp.Track = 'true')) and (AProp.Track <> 'false');
    end;

  var
    VAppearanceLine: IAppearanceLine;
    VPointsEnum: IEnumLonLatPoint;
    VPoint: TDoublePoint;
    VMeta: TDoublePointsMetaItem;
    VPointNum: Integer;
    VProp: TGpxMarkProperties;
    VDateTime: TDateTime;
    VFakeTimeGenerator: IGpxFakeTimeGenerator;
  begin
    VProp := TGpxMarkProperties.Read(AMark);

    if IsTrack(VProp) then begin
      if AGeometryType <> ggtTrack then begin
        Exit;
      end;

      FGpxWriter.StartElement('trk');
      begin
        FGpxWriter.WriteElementString('name', ToXmlText(AMark.Name));

        if VProp.Cmt <> '' then
          FGpxWriter.WriteElementString('cmt', ToXmlText(VProp.Cmt));

        if VProp.Desc <> '' then
          FGpxWriter.WriteElementString('desc', ToXmlText(VProp.Desc));

        FGpxWriter.WriteElementString('number', IntToStr(FTrackNumber));
        Inc(FTrackNumber);

        // TrackExtension
        if Supports(AMark.Appearance, IAppearanceLine, VAppearanceLine) then begin
          FGpxWriter.StartElement('extensions');
          begin
            FGpxWriter.StartElement('gpxx:TrackExtension');
            begin
              FGpxWriter.WriteElementString('gpxx:DisplayColor', ToGpxColor(VAppearanceLine.LineColor));
            end;
            FGpxWriter.EndElement; // gpxx:TrackExtension
          end;
          FGpxWriter.EndElement; // extensions
        end;

        FGpxWriter.StartElement('trkseg');
        begin
          VFakeTimeGenerator := TGpxFakeTimeGenerator.Create(FNowUtc, FGeoCalc, ALonLatLine);

          VPointNum := 0;
          VPointsEnum := ALonLatLine.GetEnum;

          while VPointsEnum.Next(VPoint, VMeta) do begin
            FGpxWriter.StartElement('trkpt');
            FGpxWriter.WriteAttribute('lat', R2StrPoint(VPoint.Y));
            FGpxWriter.WriteAttribute('lon', R2StrPoint(VPoint.X));
            begin
              if VMeta.IsElevationOk then begin
                FGpxWriter.WriteElementString('ele', RoundEx(VMeta.Elevation, 2));
              end;

              if VMeta.IsTimeStampOk and (VMeta.TimeStamp <> 0) then begin
                VDateTime := VMeta.TimeStamp;
              end else begin
                // 'time' must be present, otherwise track is not visible in
                // Google Earth/Strava. We do not have time, so fake it
                VDateTime := VFakeTimeGenerator.TimeStamp[VPointNum];
              end;

              FGpxWriter.WriteElementString('time', ToXmlDateTime(VDateTime, True));
            end;
            FGpxWriter.EndElement; // trkpt
            Inc(VPointNum);
          end;
        end;
        FGpxWriter.EndElement; // trkseg
      end;
      FGpxWriter.EndElement; // trk
    end
    else begin
      if AGeometryType <> ggtRoute then begin
        Exit;
      end;

      FGpxWriter.StartElement('rte');
      begin
        FGpxWriter.WriteElementString('name', ToXmlText(AMark.Name));

        if VProp.Cmt <> '' then
          FGpxWriter.WriteElementString('cmt', ToXmlText(VProp.Cmt));

        if VProp.Desc <> '' then
          FGpxWriter.WriteElementString('desc', ToXmlText(VProp.Desc));

        FGpxWriter.WriteElementString('number', IntToStr(FRouteNumber));
        Inc(FRouteNumber);

        // RouteExtention
        FGpxWriter.StartElement('extensions');
        begin
          FGpxWriter.StartElement('gpxx:RouteExtension');
          begin
            FGpxWriter.WriteElementString('gpxx:IsAutoNamed', 'false');
            if Supports(AMark.Appearance, IAppearanceLine, VAppearanceLine) then begin
              FGpxWriter.WriteElementString('gpxx:DisplayColor', ToGpxColor(VAppearanceLine.LineColor));
            end;
          end;
          FGpxWriter.EndElement; // gpxx:RouteExtension
        end;
        FGpxWriter.EndElement; // extensions

        VPointNum := 0;
        VPointsEnum := ALonLatLine.GetEnum;

        while VPointsEnum.Next(VPoint, VMeta) do begin
          FGpxWriter.StartElement('rtept');
          FGpxWriter.WriteAttribute('lat', R2StrPoint(VPoint.Y));
          FGpxWriter.WriteAttribute('lon', R2StrPoint(VPoint.X));
          begin
            if VMeta.IsElevationOk then begin
              FGpxWriter.WriteElementString('ele', RoundEx(VMeta.Elevation, 2));
            end;

            if VMeta.IsTimeStampOk and (VMeta.TimeStamp <> 0) then begin
              FGpxWriter.WriteElementString('time', ToXmlDateTime(VMeta.TimeStamp, True));
            end;

            // 'name' must be present, otherwise route is not visible
            FGpxWriter.WriteElementString('name', ToXmlText(AMark.Name + ' ' + IntToStr(VPointNum + 1)));
            FGpxWriter.WriteElementString('sym', 'Waypoint');
          end;
          FGpxWriter.EndElement; // rtept
          Inc(VPointNum);
        end;
      end;
      FGpxWriter.EndElement; // rte
    end;
  end;

  procedure AddMultiLine(
    const AMark: IVectorDataItem;
    const ALonLatPath: IGeometryLonLatMultiLine
  );
  var
    VAppearanceLine: IAppearanceLine;
    VLonLatPathLine: IGeometryLonLatSingleLine;
    VPointsEnum: IEnumLonLatPoint;
    VPoint: TDoublePoint;
    VMeta: TDoublePointsMetaItem;
    VProp: TGpxMarkProperties;
    VDateTime: TDateTime;
    I: Integer;
    VPointNum: Integer;
    VFakeTimeGenerator: IGpxFakeTimeGenerator;
  begin
    if AGeometryType <> ggtTrack then begin
      Exit;
    end;

    if ALonLatPath.Count <= 0 then begin
      Exit;
    end;

    if ALonLatPath.Count = 1 then begin
      VLonLatPathLine := ALonLatPath.Item[0];
      AddLine(AMark, VLonLatPathLine);
      Exit;
    end;

    VProp := TGpxMarkProperties.Read(AMark);

    FGpxWriter.StartElement('trk');
    begin
      FGpxWriter.WriteElementString('name', ToXmlText(AMark.Name));

      if VProp.Cmt <> '' then
        FGpxWriter.WriteElementString('cmt', ToXmlText(VProp.Cmt));

      if VProp.Desc <> '' then
        FGpxWriter.WriteElementString('desc', ToXmlText(VProp.Desc));

      FGpxWriter.WriteElementString('number', IntToStr(FTrackNumber));
      Inc(FTrackNumber);

      // TrackExtension
      if Supports(AMark.Appearance, IAppearanceLine, VAppearanceLine) then begin
        FGpxWriter.StartElement('extensions');
        begin
          FGpxWriter.StartElement('gpxx:TrackExtension');
          begin
            FGpxWriter.WriteElementString('gpxx:DisplayColor', ToGpxColor(VAppearanceLine.LineColor));
          end;
          FGpxWriter.EndElement; // gpxx:TrackExtension
        end;
        FGpxWriter.EndElement; // extensions
      end;

      VFakeTimeGenerator := TGpxFakeTimeGenerator.Create(FNowUtc, FGeoCalc, ALonLatPath);

      VPointNum := 0;
      for I := 0 to ALonLatPath.Count - 1 do begin
        VLonLatPathLine := ALonLatPath.Item[I];
        if VLonLatPathLine.Count > 0 then begin
          FGpxWriter.StartElement('trkseg');
          begin
            VPointsEnum := VLonLatPathLine.GetEnum;
            while VPointsEnum.Next(VPoint, VMeta) do begin
              FGpxWriter.StartElement('trkpt');
              FGpxWriter.WriteAttribute('lat', R2StrPoint(VPoint.Y));
              FGpxWriter.WriteAttribute('lon', R2StrPoint(VPoint.X));
              begin
                if VMeta.IsElevationOk then begin
                  FGpxWriter.WriteElementString('ele', RoundEx(VMeta.Elevation, 2));
                end;

                if VMeta.IsTimeStampOk and (VMeta.TimeStamp <> 0) then begin
                  VDateTime := VMeta.TimeStamp;
                end else begin
                  VDateTime := VFakeTimeGenerator.TimeStamp[VPointNum];
                end;

                FGpxWriter.WriteElementString('time', ToXmlDateTime(VDateTime, True));
              end;
              FGpxWriter.EndElement; // trkpt
            end;
            Inc(VPointNum);
          end;
          FGpxWriter.EndElement; // trkseg
        end;
      end;
    end;
    FGpxWriter.EndElement; // trk
  end;

var
  VLonLatPoint: IGeometryLonLatPoint;
  VLonLatSingleLine: IGeometryLonLatSingleLine;
  VLonLatMultiLine: IGeometryLonLatMultiLine;
begin
  if Supports(AMark.Geometry, IGeometryLonLatPoint, VLonLatPoint) then
    AddPoint(AMark, VLonLatPoint)
  else if Supports(AMark.Geometry, IGeometryLonLatSingleLine, VLonLatSingleLine) then
    AddLine(AMark, VLonLatSingleLine)
  else if Supports(AMark.Geometry, IGeometryLonLatMultiLine, VLonLatMultiLine) then
    AddMultiLine(AMark, VLonLatMultiLine);
end;

function TExportMarks2GPX.SaveMarkIcon(
  const AAppearanceIcon: IAppearancePointIcon
): string;
var
  VTargetPath: string;
  VTargetFullName: string;
  VPicName: string;
  VStream: TCustomMemoryStream;
  VData: IBinaryData;
begin
  Result := '';
  if AAppearanceIcon.Pic <> nil then begin
    VData := AAppearanceIcon.Pic.Source;
    if VData <> nil then begin
      VStream := TStreamReadOnlyByBinaryData.Create(VData);
      try
        VPicName := ExtractFileName(AAppearanceIcon.Pic.GetName);
        VTargetPath := 'files' + PathDelim;
        Result := VTargetPath + VPicName;
        VTargetPath := ExtractFilePath(FFileName) + VTargetPath;
        VTargetFullName := VTargetPath + VPicName;
        CreateDir(VTargetPath);
        VStream.SaveToFile(VTargetFullName);
      finally
        VStream.Free;
      end;
    end;
  end;
end;

class function TExportMarks2GPX.ToGpxColor(const AColor32: TColor32): string;
type
  TGarminColor = (
    gcBlack,         // 0
    gcDarkRed,       // 1
    gcDarkGreen,     // 2
    gcDarkYellow,    // 3
    gcDarkBlue,      // 4
    gcDarkMagenta,   // 5
    gcDarkCyan,      // 6
    gcLightGray,     // 7
    gcDarkGray,      // 8
    gcRed,           // 9
    gcGreen,         // 10
    gcYellow,        // 11
    gcBlue,          // 12
    gcMagenta,       // 13
    gcCyan,          // 14
    gcWhite,         // 15
    gcTransparent);  // 16

const
  GarminPalette: array[TGarminColor] of string = (
    'Black',        // R: 0;   G: 0;   B: 0;     0
    'DarkRed',      // R: 128; G: 0;   B: 0;     1
    'DarkGreen',    // R: 0;   G: 128; B: 0;     2
    'DarkYellow',   // R: 128; G: 128; B: 0;     3
    'DarkBlue',     // R: 0;   G: 0;   B: 128;   4
    'DarkMagenta',  // R: 128; G: 0;   B: 128;   5
    'DarkCyan',     // R: 0;   G: 128; B: 128;   6
    'LightGray',    // R: 192; G: 192; B: 192;   7
    'DarkGray',     // R: 64;  G: 64;  B: 64;    8
    'Red',          // R: 255; G: 0;   B: 0;     9
    'Green',        // R: 0;   G: 255; B: 0;     10
    'Yellow',       // R: 255; G: 255; B: 0;     11
    'Blue',         // R: 0;   G: 0;   B: 255;   12
    'Magenta',      // R: 255; G: 0;   B: 255;   13
    'Cyan',         // R: 0;   G: 255; B: 255;   14
    'White',        // R: 255; G: 255; B: 255;   15
    'Transparent');                           // 16

  GarminColors: array[TGarminColor] of TColor32Entry = (
    (B: 0;   G: 0;   R: 0;   A: 0),    // gcBlack          0
    (B: 0;   G: 0;   R: 128; A: 0),    // gcDarkRed        1
    (B: 0;   G: 128; R: 0;   A: 0),    // gcDarkGreen      2
    (B: 0;   G: 128; R: 128; A: 0),    // gcDarkYellow     3
    (B: 128; G: 0;   R: 0;   A: 0),    // gcDarkBlue       4
    (B: 128; G: 0;   R: 128; A: 0),    // gcDarkMagenta    5
    (B: 128; G: 128; R: 0;   A: 0),    // gcDarkCyan       6
    (B: 192; G: 192; R: 192; A: 0),    // gcLightGray      7
    (B: 64;  G: 64;  R: 64;  A: 0),    // gcDarkGray       8
    (B: 0;   G: 0;   R: 255; A: 0),    // gcRed            9
    (B: 0;   G: 255; R: 0;   A: 0),    // gcGreen          10
    (B: 0;   G: 255; R: 255; A: 0),    // gcYellow         11
    (B: 255; G: 0;   R: 0;   A: 0),    // gcBlue           12
    (B: 255; G: 0;   R: 255; A: 0),    // gcMagenta        13
    (B: 255; G: 255; R: 0;   A: 0),    // gcCyan           14
    (B: 255; G: 255; R: 255; A: 0),    // gcWhite          15
    (B: 0;   G: 0;   R: 0;   A: 255)); // gcTransparent    16

  function RoundColor(const AColor: Byte): Byte;
  begin
    case AColor of
      0..63    : Result := 0;
      64..191  : Result := 128;
      192..255 : Result := 255;
    end;
  end;

var
  X: TGarminColor;
  VColor: TColor32Entry;
  VGarminColor: TGarminColor;
begin
  VGarminColor := gcTransparent;

  VColor.ARGB := AColor32;
  VColor.A := 0; // ignore transparency

  if (VColor.B = VColor.R) and (VColor.B = VColor.G) then begin
    // black - gray - white
    case VColor.B of
      0..22    : VGarminColor := gcBlack;     // -> 0
      23..127  : VGarminColor := gcDarkGray;  // -> 64
      128..222 : VGarminColor := gcLightGray; // -> 192
      223..255 : VGarminColor := gcWhite;     // -> 255
    end;
  end else begin
    // other colors
    VColor.B := RoundColor(VColor.B);
    VColor.G := RoundColor(VColor.G);
    VColor.R := RoundColor(VColor.R);

    if (VColor.B and VColor.R and VColor.G) = 128 then begin
      // lost color information
      VGarminColor := gcLightGray;
    end else begin
      for X := Low(X) to Pred(High(X)) do begin // ignore gcTransparent
        if GarminColors[X].ARGB = VColor.ARGB then begin
          VGarminColor := X;
          Break;
        end;
      end;
    end;
  end;

  if VGarminColor = gcTransparent then begin
    VGarminColor := gcBlack;
    Assert(False, 'GPX GarminColor detection failed: 0x' + IntToHex(VColor.ARGB, 8));
  end;

  Result := GarminPalette[VGarminColor];
end;

class function TExportMarks2GPX.ToUtc(const ADateTime: TDateTime): TDateTime;
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

class function TExportMarks2GPX.ToXmlDateTime(
  const ADateTime: TDateTime;
  const ADetailed: Boolean
): string;
const
  CFormat: array [Boolean] of string = (
    'yyyy"-"mm"-"dd"T"hh":"nn":"ss"Z"',
    'yyyy"-"mm"-"dd"T"hh":"nn":"ss"."zzz"Z"'
  );
var
  VDetailed: Boolean;
  VFormatSettings: TFormatSettings;
begin
  VFormatSettings.DateSeparator := '-';
  VFormatSettings.TimeSeparator := ':';
  VDetailed := ADetailed and (MilliSecondOf(ADateTime) > 0);
  Result := FormatDateTime(CFormat[VDetailed], ADateTime, VFormatSettings); // '2015-07-19T07:53:32Z'
end;

class function TExportMarks2GPX.ToXmlText(const AStr: string): string;
begin
  Result := AdjustLineBreaks(AStr);
end;

class function TExportMarks2GPX.FindSymByName(const AName: string): string;
const
  GarminSymNames: array[0..138] of string = (
    'Cache In Trash Out Event',
    'Earthcache',
    'Event Cache',
    'Geocache',
    'Geocache Course',
    'Geocache Found',
    'Letterbox Hybrid',
    'Locationless (Reverse) Cache',
    'Mega-Event Cache',
    'Multi-cache',
    'Project APE Cache',
    'Traditional Cache',
    'Unknown Cache',
    'Virtual Cache',
    'Webcam Cache',
    'Wherigo Cache',

    'Airport',
    'Amusement Park',
    'Anchor',
    'Animal Tracks',
    'ATV',
    'Ball Park',
    'Bank',
    'Bar',
    'Beach',
    'Bell',
    'Big Game',
    'Bike Trail',
    'Blind',
    'Block, Blue',
    'Block, Green',
    'Block, Red',
    'Blood Trail',
    'Boat Ramp',
    'Bowling',
    'Bridge',
    'Building',
    'Buoy, White',
    'Campground',
    'Car',
    'Car Rental',
    'Car Repair',
    'Cemetery',
    'Church',
    'City (Large)',
    'City (Medium)',
    'City (Small)',
    'Civil',
    'Controlled Area',
    'Convenience Store',
    'Cover',
    'Covey',
    'Crossing',
    'Dam',
    'Danger Area',
    'Department Store',
    'Diver Down Flag 1',
    'Diver Down Flag 2',
    'Drinking Water',
    'Fast Food',
    'Fishing Area',
    'Fishing Hot Spot Facility',
    'Fitness Center',
    'Flag, Blue',
    'Flag, Green',
    'Flag, Red',
    'Food Source',
    'Forest',
    'Furbearer',
    'Gas Station',
    'Glider Area',
    'Golf Course',
    'Horn',
    'Ice Skating',
    'Information',
    'Light',
    'Live Theater',
    'Lodge',
    'Lodging',
    'Man Overboard',
    'Medical Facility',
    'Mine',
    'Movie Theater',
    'Museum',
    'Navaid, Amber',
    'Navaid, Black',
    'Navaid, Blue',
    'Navaid, Green',
    'Navaid, Orange',
    'Navaid, Red',
    'Navaid, Violet',
    'Navaid, White',
    'Oil Field',
    'Parachute Area',
    'Park',
    'Parking Area',
    'Pharmacy',
    'Picnic Area',
    'Pin, Blue',
    'Pin, Green',
    'Pin, Red',
    'Pizza',
    'Police Station',
    'Post Office',
    'Radio Beacon',
    'Residence',
    'Restaurant',
    'Restricted Area',
    'Restroom',
    'RV Park',
    'Scales',
    'Scenic Area',
    'School',
    'Shipwreck',
    'Shopping Center',
    'Short Tower',
    'Shower',
    'Ski Resort',
    'Skiing Area',
    'Skull and Crossbones',
    'Small Game',
    'Stadium',
    'Summit',
    'Swimming Area',
    'Tall Tower',
    'Telephone',
    'Toll Booth',
    'Trail Head',
    'Tree Stand',
    'Treed Quarry',
    'Truck',
    'Truck Stop',
    'Tunnel',
    'Ultralight Area',
    'Upland Game',
    'Water Source',
    'Waterfowl',
    'Wrecker',
    'Zoo');

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
  Words: TWords;
  Index: TIndex;
begin
  SplitIntoWords(AName, Words);
  BuildIndex(Words, Index);
  SortIndex(Index, 0, High(Index));

  if Index[0].Similarity > 0 then
    Result := ToXmlText(GarminSymNames[Index[0].Sym])
  else
    Result := '';
  if Result = '' then
    Result := 'Flag, Blue';
end;

class function TExportMarks2GPX.FindSymByMark(const AMark: IVectorDataItem): string;
var
  VAppearanceIcon: IAppearancePointIcon;
begin
  if Supports(AMark.Appearance, IAppearancePointIcon, VAppearanceIcon) and
     (VAppearanceIcon <> nil) and
     (VAppearanceIcon.Pic <> nil) then
    Result := FindSymByName(ChangeFileExt(ExtractFileName(VAppearanceIcon.Pic.GetName), ''))
  else
    Result := FindSymByName('');
end;

end.
