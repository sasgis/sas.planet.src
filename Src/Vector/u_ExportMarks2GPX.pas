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

unit u_ExportMarks2GPX;

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
  TExportMarks2GPX = class
  private
    FGPXDoc: TALXMLDocument;
    FGeoCalc: IGeoCalc;
    FBuildInfo: IBuildInfo;
    FFileName: string;
    FGPXNode: TALXMLNode;
    FGPXMetaNode: TALXMLNode;
    FNameNode: TALXMLNode;
    FDescNode: TALXMLNode;
    FTrackNumber: Integer;
    FNow: TDateTime;
    function AddTree(
      const ACategory: String;
      const ATree: IVectorItemTree
    ): boolean;
    function AddMarks(
      const ACategory: String;
      const AMarksSubset: IVectorItemSubset
    ): Boolean;
    procedure AddMark(
      const ACategory: String;
      const AMark: IVectorDataItem
    );
    function SaveMarkIcon(const AAppearanceIcon: IAppearancePointIcon): string;
    function Color32toGPXColor(const AColor32: TColor32): AnsiString;
    procedure PrepareExportToFile(const AFileName: string; const ATree: IVectorItemTree);
    procedure SaveToFile;
    function XMLDateTime(const ADateTime: TDateTime; const ADetailed: Boolean = False): AnsiString;
    function XMLText(const AStr: String): AnsiString;
    function FindSymByName(const AName: String): AnsiString;
    function FindSymByMark(const AMark: IVectorDataItem): AnsiString;
  public
    procedure ExportTreeToGPX(
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

{ TExportMarks2GPX }

procedure TExportMarks2GPX.PrepareExportToFile(const AFileName: string; const ATree: IVectorItemTree);

  function GetUserName: String;
  var
    I: DWord;
  begin
    I := 4096;
    SetLength(Result, I);
    if Windows.GetUserName(PChar(Result), I) then
    begin
      if I > 0 then
        Dec(I);
      SetLength(Result, I);
    end
    else
      Result := '';
  end;

  function GetUserFullName: String;

    function RegKeyRead(const ARoot: HKey; const AKey, AName: String; const ADefault: String = ''): String;
    var
      VReg: HKey;
      VSize: Integer;
      VKey: String;
      VDataType: Integer;
    begin
      Result := ADefault; // Do Not Localize
      VKey := AKey;
      if (VKey <> '') and (VKey[1] = '\') then // Do Not Localize
        Delete(VKey, 1, 1);

      if RegOpenKeyEx(ARoot, PChar(VKey), 0, KEY_READ, VReg) = ERROR_SUCCESS then
      try
        VSize := 0;
        if RegQueryValueEx(VReg, PChar(AName), nil, @VDataType, nil, @VSize) = ERROR_SUCCESS then
        begin
          SetLength(Result, VSize div SizeOf(Char));
          if Result <> '' then
          begin
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
    Result :=
      RegKeyRead(HKEY_LOCAL_MACHINE,
        '\SOFTWARE\MICROSOFT\WINDOWS NT\CURRENTVERSION', // Do Not Localize
        'RegisteredOwner'); // Do Not Localize
    if Result = '' then
      Result := RegKeyRead(HKEY_LOCAL_MACHINE,
        '\SOFTWARE\MICROSOFT\WINDOWS\CURRENTVERSION', // Do Not Localize
        'RegisteredOwner'); // Do Not Localize
    if Result = '' then
      Result := RegKeyRead(HKEY_LOCAL_MACHINE,
        '\SOFTWARE\MICROSOFT\MS SETUP (ACME)\USER INFO', // Do Not Localize
        'DefName'); // Do Not Localize
    if Result = 'Microsoft' then // Do Not Localize
      Result := ''; // Do Not Localize
    if Result = '' then
      Result := GetUserName;
  end;

  function FindFirstMark(const ATree: IVectorItemTree; const ALineOnly: Boolean): IVectorDataItem;

    function FindMark(const AMarksSubset: IVectorItemSubset; const ALineOnly: Boolean): IVectorDataItem;
    var
      VMark: IVectorDataItem;
      VEnumMarks: IEnumUnknown;
      Dummy: IGeometryLonLatSingleLine;
      i: integer;
    begin
      Result := nil;
      if Assigned(AMarksSubset) then begin
        i := 0;
        VEnumMarks := AMarksSubset.GetEnum;
        while (VEnumMarks.Next(1, VMark, @i) = S_OK) do begin
          if Assigned(VMark) and (VMark.Name <> '') then begin
            if (not ALineOnly) or
               Supports(VMark.Geometry, IGeometryLonLatSingleLine, Dummy) then begin
              Result := VMark;
              Break;
            end;
          end;
        end;
      end;
    end;

  var
    VSubTree: IVectorItemTree;
    i: Integer;
  begin
    Result := nil;
    if not Assigned(ATree) then
      Exit;

    Result := FindMark(ATree.Items, ALineOnly);

    if Assigned(Result) then
      Exit;

    for i := 0 to ATree.SubTreeItemCount - 1 do begin
      VSubTree := ATree.GetSubTreeItem(i);
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
      i: integer;
    begin
      if Assigned(AMarksSubset) then begin
        i := 0;
        VEnumMarks := AMarksSubset.GetEnum;
        while (VEnumMarks.Next(1, VMark, @i) = S_OK) do begin
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

          end; // if Assigned(VMark) then
        end; // while (VEnumMarks.Next
      end; // if Assigned(AMarksSubset) then
    end;

  var
    VSubTree: IVectorItemTree;
    i: Integer;
  begin
    if not Assigned(ATree) then
      Exit;

    ScanMarksBounds(ABounds, ATree.Items);

    for i := 0 to ATree.SubTreeItemCount - 1 do begin
      VSubTree := ATree.GetSubTreeItem(i);
      ScanBounds(ABounds, VSubTree);
    end;
  end;

  function GetVersion: String;
  var
    VDT: TDateTime;
    VVer: String;
    VVerMajor: String;
    VVerMinor: String;
    VVerBuild: String;
    VSrcRev: Integer;
    VDummy: String;
  begin
    FBuildInfo.GetBuildSrcInfo(VSrcRev, VDummy);
    VDT := FBuildInfo.GetBuildDate;
    if VDT = 0 then
      VDT := FNow;
    VVer := FBuildInfo.GetVersion;
    VVerMajor := Trim(Copy(VVer, 1, 2)); if VVerMajor = '' then VVerMajor := IntToStr(YearOf(VDT) mod 100);
    VVerMinor := Trim(Copy(VVer, 3, 2)); if VVerMinor = '' then VVerMinor := IntToStr(MonthOf(VDT));
    VVerBuild := Trim(Copy(VVer, 5, 2)); if VVerBuild = '' then VVerBuild := IntToStr(DayOf(VDT));
    Result := Format('%s.%s.%s.%d', [VVerMajor, VVerMinor, VVerBuild, VSrcRev]);
  end;

var
  VAuthorNode: TALXMLNode;
  VLinkNode: TALXMLNode;
  VUserEMail: String;
  VEMailNode: TALXMLNode;
  VMark: IVectorDataItem;
  VBounds: TDoubleRect;
  VBoundsNode: TALXMLNode;
begin
  FNow := Now;

  // Windows 8+ - this is e-mail from Windows Live/Passport
  VUserEMail := GetUserFullName;
  if (Pos('@', VUserEMail) <= 1) or
     (Pos('.', VUserEMail) <= 1) then
    VUserEMail := '';

  FGPXDoc.Options := [doNodeAutoIndent, doNodeAutoCreate];
  FGPXDoc.Active := True;
  FGPXDoc.Version := '1.0';
  FGPXDoc.Encoding := 'UTF-8';
  FGPXNode := FGPXDoc.AddChild('gpx');
  FGPXNode.Attributes['xmlns'] := 'http://www.topografix.com/GPX/1/1';

  FGPXNode.Attributes['creator'] := XMLText('SAS.Planet ' + GetVersion);
  FGPXNode.Attributes['version'] := '1.1'; // You must include the version number in your GPX document.
  FGPXNode.Attributes['xmlns:xsi'] := 'http://www.w3.org/2001/XMLSchema-instance';
  FGPXNode.Attributes['xmlns:wptx1'] := 'http://www.garmin.com/xmlschemas/WaypointExtension/v1';
  FGPXNode.Attributes['xmlns:gpxtrx'] := 'http://www.garmin.com/xmlschemas/GpxExtensions/v3';
  FGPXNode.Attributes['xmlns:gpxtpx'] := 'http://www.garmin.com/xmlschemas/TrackPointExtension/v1';
  FGPXNode.Attributes['xmlns:gpxx'] := 'http://www.garmin.com/xmlschemas/GpxExtensions/v3';
  FGPXNode.Attributes['xsi:schemaLocation'] := 'http://www.topografix.com/GPX/1/1 ' +
                                               'http://www.topografix.com/GPX/1/1/gpx.xsd ' +
                                               'http://www.garmin.com/xmlschemas/WaypointExtension/v1 ' +
                                               'http://www8.garmin.com/xmlschemas/WaypointExtensionv1.xsd ' +
                                               'http://www.garmin.com/xmlschemas/TrackPointExtension/v1 ' +
                                               'http://www.garmin.com/xmlschemas/TrackPointExtensionv1.xsd ' +
                                               'http://www.garmin.com/xmlschemas/GpxExtensions/v3 ' +
                                               'http://www8.garmin.com/xmlschemas/GpxExtensionsv3.xsd';

  FGPXMetaNode := FGPXNode.AddChild('metadata'); // Metadata about the file.
  // FGPXNode.AddChild('extensions'); // You can add extend GPX by adding your own elements from another schema here.

  FGPXMetaNode.AddChild('time').Text := XMLDateTime(FNow);
  VLinkNode := FGPXMetaNode.AddChild('link');
  VLinkNode.Attributes['href'] := 'http://www.sasgis.org/';
  VLinkNode.AddChild('text').Text := 'SAS.Planet';

  // Set name of GPX = first line mark in tree.
  // If there is no line mark - use any mark.
  // If there is no marks - use file name.
  FNameNode := FGPXMetaNode.AddChild('name');
  VMark := FindFirstMark(ATree, True);
  if not Assigned(VMark) then
    VMark := FindFirstMark(ATree, False);
  if Assigned(VMark) then
  begin
    FNameNode.Text := XMLText(VMark.Name);
    if VMark.Desc <> '' then
    begin
      FDescNode := FGPXMetaNode.AddChild('desc');
      FDescNode.Text := XMLText(VMark.Desc);
    end;
  end
  else
    FNameNode.Text := XMLText(ChangeFileExt(ExtractFileName(AFileName), ''));

  // Set user name = system user name
  VAuthorNode := FGPXMetaNode.AddChild('author');
  VAuthorNode.AddChild('name').Text := XMLText(GetUserName);
  if VUserEMail <> '' then
  begin
    VEMailNode := VAuthorNode.AddChild('email');
    VEMailNode.Attributes['id'] := XMLText(Copy(VUserEMail, 1, Pos('@', VUserEMail) - 1));
    VEMailNode.Attributes['domain'] := XMLText(Copy(VUserEMail, Pos('@', VUserEMail) + 1, MaxInt));
  end;

  // Determinate bounds of all marks
  FillChar(VBounds, SizeOf(VBounds), 0);
  ScanBounds(VBounds, ATree);
  if (VBounds.Left <> 0) or (VBounds.Right <> 0) or
     (VBounds.Top <> 0) or (VBounds.Bottom <> 0) then begin
    VBoundsNode := FGPXMetaNode.AddChild('bounds'); // Minimum and maximum coordinates which describe the extent of the coordinates in the file. <bounds minlat="56.717302" minlon="35.900255" maxlat="56.787633" maxlon="35.980514" />
    VBoundsNode.Attributes['maxlat'] := R2AnsiStrPoint(VBounds.Bottom);
    VBoundsNode.Attributes['maxlon'] := R2AnsiStrPoint(VBounds.Right);
    VBoundsNode.Attributes['minlat'] := R2AnsiStrPoint(VBounds.Top);
    VBoundsNode.Attributes['minlon'] := R2AnsiStrPoint(VBounds.Left);
  end;

  // FGPXMetaNode.AddChild('copyright'); // Copyright and license information governing use of the file.
  // FGPXMetaNode.AddChild('keywords'); // Keywords associated with the file. Search engines or databases can use this information to classify the data.
  // FGPXMetaNode.AddChild('extensions'); // You can add extend GPX by adding your own elements from another schema here.

  FFileName := AFileName;
end;

procedure TExportMarks2GPX.SaveToFile;
var
  VFileStream: TFileStream;
begin
  VFileStream := TFileStream.Create(FFileName, fmCreate);
  try
    FGPXDoc.SaveToStream(VFileStream);
  finally
    VFileStream.Free;
  end;
end;

procedure TExportMarks2GPX.ExportTreeToGPX(
  const AGeoCalc: IGeoCalc;
  const ABuildInfo: IBuildInfo;
  const ATree: IVectorItemTree;
  const AFileName: string
);
begin
  FGeoCalc := AGeoCalc;
  FBuildInfo := ABuildInfo;
  FGPXDoc := TALXMLDocument.Create;
  try
    FTrackNumber := 1;

    PrepareExportToFile(AFileName, ATree);
    AddTree('', ATree);
    SaveToFile;
  finally
    FreeAndNil(FGPXDoc);
    FGeoCalc := nil;
  end;
end;

function TExportMarks2GPX.AddMarks(
  const ACategory: String;
  const AMarksSubset: IVectorItemSubset
): Boolean;
var
  VMark: IVectorDataItem;
  VEnumMarks: IEnumUnknown;
  i: integer;
begin
  Result := False;
  if Assigned(AMarksSubset) then begin
    VEnumMarks := AMarksSubset.GetEnum;
    while (VEnumMarks.Next(1, VMark, @i) = S_OK) do begin
      AddMark(ACategory, VMark);
      Result := True;
    end;
  end;
end;

function TExportMarks2GPX.AddTree(
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

procedure TExportMarks2GPX.AddMark(
  const ACategory: String;
  const AMark: IVectorDataItem
);
const
  DummySpeedKMH = 5; // 5 Km / Hour - dummy speed for export TRK/track
  DummySpeedMS  = DummySpeedKMH * 1000 / (60 * 60); // dummy speed in meters per second

  function ExtractDesc(const ADesc: String): String;

    procedure RemoveField(var AStr: String; const AFieldName: String);
    var
      X: Integer;
      VPrefix: String;
      VPre: String;
    begin
      VPrefix := AFieldName + ': ';
      X := Pos(VPrefix, AStr);
      if X > 0 then
      begin
        VPre := Trim(Copy(AStr, 1, X - 1));
        AStr := Trim(Copy(AStr, X + Length(VPrefix), MaxInt));
        X := Pos(#10, AStr);
        if X > 0 then
          AStr := Trim(Copy(AStr, X + 1, MaxInt));
        AStr := Trim(VPre + sLineBreak + AStr);
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
    RemoveField(Result, 'track');
    RemoveField(Result, 'kind');
    RemoveField(Result, 'GPS Coordinates');
  end;

  function ExtractCmt(var ADesc: String): String;
  var
    X: Integer;
    VPre: String;
  begin
    // Extract "cmt:" field
    X := Pos('cmt: ', ADesc);
    if X > 0 then
    begin
      VPre := Trim(Copy(ADesc, 1, X - 1));
      ADesc := Trim(Copy(ADesc, X + Length('cmt: '), MaxInt));
      X := Pos(#10, ADesc);
      if X > 0 then
      begin
        Result := Trim(Copy(ADesc, 1, X - 1));
        ADesc := Trim(Copy(ADesc, X + 1, MaxInt));
        ADesc := Trim(VPre + sLineBreak + ADesc);
      end
      else
      begin
        Result := ADesc;
        ADesc := VPre;
      end;
    end
    else
      Result := '';
  end;

  function ExtractTime(var ADesc: String): TDateTime;
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
  end;

  function ExtractType(var ADesc: String): String;
  var
    X: Integer;
    VPre: String;
  begin
    // Extract "type:" field
    X := Pos('type: ', ADesc);
    if X > 0 then
    begin
      VPre := Trim(Copy(ADesc, 1, X - 1));
      ADesc := Trim(Copy(ADesc, X + Length('type: '), MaxInt));
      X := Pos(#10, ADesc);
      if X > 0 then
      begin
        Result := Trim(Copy(ADesc, 1, X - 1));
        ADesc := Trim(Copy(ADesc, X + 1, MaxInt));
        ADesc := Trim(VPre + sLineBreak + ADesc);
      end
      else
      begin
        Result := ADesc;
        ADesc := VPre;
      end;
    end
    else
      Result := '';
  end;

  function ExtractSym(var ADesc: String): String;
  var
    X: Integer;
    VPre: String;
  begin
    // Extract "sym:" field
    X := Pos('sym: ', ADesc);
    if X > 0 then
    begin
      VPre := Trim(Copy(ADesc, 1, X - 1));
      ADesc := Trim(Copy(ADesc, X + Length('sym: '), MaxInt));
      X := Pos(#10, ADesc);
      if X > 0 then
      begin
        Result := Trim(Copy(ADesc, 1, X - 1));
        ADesc := Trim(Copy(ADesc, X + 1, MaxInt));
        ADesc := Trim(VPre + sLineBreak + ADesc);
      end
      else
      begin
        Result := ADesc;
        ADesc := VPre;
      end;
    end
    else
      Result := '';
  end;

  procedure AddCategories(const AExtensionNode: TALXMLNode; const APrefix: AnsiString);
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
      AExtensionNode.AddChild(APrefix + ':Category').Text := 'default';
      Exit;
    end;

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
        AExtensionNode.AddChild(APrefix + ':Category').Text := XMLText(VCategory);
    until VCategories = '';
  end;

  procedure AddPoint(
    const AMark: IVectorDataItem;
    const ALonLatPoint: IGeometryLonLatPoint);
  var
    VWidth: Integer;

    function IsPhoto: Boolean;
    begin
      Result := (VWidth > 100);
    end;

  var
    VCurrentNode: TALXMLNode;
    VAppearanceIcon: IAppearancePointIcon;
    VExtensionsNode: TALXMLNode;
    VExtensionNode: TALXMLNode;
    VDesc: String;
    VCmt: String;
    VDT: TDateTime;
    VType: String;
    VSym: String;
  begin
    VCurrentNode := FGPXNode.AddChild('wpt');
    VCurrentNode.Attributes['lat'] := R2AnsiStrPoint(ALonLatPoint.Point.Y); // The latitude of the point. Decimal degrees, WGS84 datum.
    VCurrentNode.Attributes['lon'] := R2AnsiStrPoint(ALonLatPoint.Point.X); // The longitude of the point. Decimal degrees, WGS84 datum.
    VCurrentNode.ChildNodes['name'].Text := XMLText(AMark.Name); // The GPS name of the waypoint. This field will be transferred to and from the GPS. GPX does not place restrictions on the length of this field or the characters contained in it. It is up to the receiving application to validate the field before sending it to the GPS.
    VCurrentNode.ChildNodes['fix'].Text := '2d'; // Type of GPX fix. Must be one of: 'none' No fix; '2d' position only; '3d' position and elevation; 'dgps' DGPS; 'pps' Military signal used; To represent "fix type unknown", leave out fix entirely.

    // Order of extraction is important
    VDesc := ExtractDesc(AMark.Desc);
    VCmt := ExtractCmt(VDesc);
    VDT := ExtractTime(VDesc);
    if VDT <> 0 then
      VCurrentNode.ChildNodes['time'].Text := XMLDateTime(VDT); // Creation/modification timestamp for element. Date and time in are in Univeral Coordinated Time (UTC), not local time! Conforms to ISO 8601 specification for date/time representation. Fractional seconds are allowed for millisecond timing in tracklogs.
    VType := ExtractType(VDesc);
    VSym := ExtractSym(VDesc);
    if (VCmt = '') and (VDesc <> '') then // Google Earth ignore "desc"? And shows "cmt" only
    begin
      VCmt := VDesc;
      VDesc := '';
    end;
    if VCmt <> '' then
      VCurrentNode.ChildNodes['cmt'].Text := XMLText(VCmt); // GPS waypoint comment. Sent to GPS as comment.
    if VDesc <> '' then
      VCurrentNode.ChildNodes['desc'].Text := XMLText(VDesc); // A text description of the element. Holds additional information about the element intended for the user, not the GPS.

    // VCurrentNode.ChildNodes['ele'].Text := XMLText(''); // Elevation (in meters) of the point.
    // VCurrentNode.ChildNodes['src'].Text := XMLText(''); // Source of data. Included to give user some idea of reliability and accuracy of data. "Garmin eTrex", "USGS quad Boston North", e.g.
    // VCurrentNode.ChildNodes['sat'].Text := XMLText(''); // Number of satellites used to calculate the GPX fix.
    // VCurrentNode.ChildNodes['hdop'].Text := XMLText(''); // Horizontal dilution of precision.
    // VCurrentNode.ChildNodes['hdop'].Text := XMLText(''); // Vertical dilution of precision.
    // VCurrentNode.ChildNodes['pdop'].Text := XMLText(''); // Position dilution of precision.
    // VCurrentNode.ChildNodes['ageofdgpsdata'].Text := XMLText(''); // Number of seconds since last DGPS update.
    // VCurrentNode.ChildNodes['dgpsid'].Text := XMLText(''); // ID of DGPS station used in differential correction.
    // VCurrentNode.ChildNodes['magvar'].Text := XMLText(''); // Magnetic variation (in degrees) at the point
    // VCurrentNode.ChildNodes['geoidheight'].Text := XMLText(''); // Height (in meters) of geoid (mean sea level) above WGS84 earth ellipsoid. As defined in NMEA GGA message.

    if not Supports(AMark.Appearance, IAppearancePointIcon, VAppearanceIcon) then begin
      VAppearanceIcon := nil;
    end;
    if (VAppearanceIcon <> nil) and (VAppearanceIcon.Pic <> nil) then begin
      VWidth := VAppearanceIcon.Pic.GetMarker.Size.X;
      if IsPhoto then
      begin
        if VType = '' then
          VType := 'photo';
        if VSym = '' then
          VSym := 'Scenic Area';
        VCurrentNode.ChildNodes['type'].Text := XMLText(VType); // Type (classification) of the waypoint.
        VCurrentNode.ChildNodes['sym'].Text := XMLText(VSym); // Text of GPS symbol name. For interchange with other programs, use the exact spelling of the symbol as displayed on the GPS. If the GPS abbreviates words, spell them out.
        VCurrentNode.ChildNodes['link'].Attributes['href'] := XMLText(SaveMarkIcon(VAppearanceIcon)); // Link to additional information about the waypoint.
        VExtensionsNode := VCurrentNode.AddChild('extensions'); // You can add extend GPX by adding your own elements from another schema here.
        VExtensionNode := VExtensionsNode.AddChild('gpxx:WaypointExtension');
        VExtensionNode.AddChild('gpxx:DisplayMode').Text := 'SymbolAndName'; // Other possible values: 'SymbolOnly' 'SymbolAndDescription'
        AddCategories(VExtensionNode.AddChild('gpxx:Categories'), 'gpxx');
        VExtensionNode := VExtensionsNode.AddChild('wptx1:WaypointExtension');
        VExtensionNode.AddChild('wptx1:DisplayMode').Text := 'SymbolAndName'; // Other possible values: 'SymbolOnly' 'SymbolAndDescription'
        AddCategories(VExtensionNode.AddChild('wptx1:Categories'), 'wptx1');
      end
      else
      begin
        if VType = '' then
          VType := 'user';
        if VSym = '' then
          VSym := FindSymByMark(AMark);
        VCurrentNode.ChildNodes['type'].Text := XMLText(VType); // Type (classification) of the waypoint.
        VCurrentNode.ChildNodes['sym'].Text := XMLText(VSym); // Text of GPS symbol name. For interchange with other programs, use the exact spelling of the symbol as displayed on the GPS. If the GPS abbreviates words, spell them out.
        VCurrentNode.ChildNodes['link'].Attributes['href'] := XMLText(SaveMarkIcon(VAppearanceIcon)); // Link to additional information about the waypoint.
        VExtensionsNode := VCurrentNode.AddChild('extensions'); // You can add extend GPX by adding your own elements from another schema here.
        VExtensionNode := VExtensionsNode.AddChild('gpxx:WaypointExtension');
        VExtensionNode.AddChild('gpxx:DisplayMode').Text := 'SymbolOnly'; // Other possible values: 'SymbolAndName' 'SymbolAndDescription'
        AddCategories(VExtensionNode.AddChild('gpxx:Categories'), 'gpxx');
        VExtensionNode := VExtensionsNode.AddChild('wptx1:WaypointExtension');
        VExtensionNode.AddChild('wptx1:DisplayMode').Text := 'SymbolOnly'; // Other possible values: 'SymbolAndName' 'SymbolAndDescription'
        AddCategories(VExtensionNode.AddChild('wptx1:Categories'), 'wptx1');
      end
    end
    else begin
      VCurrentNode.ChildNodes['type'].Text := ''; // Type (classification) of the waypoint.
      VCurrentNode.ChildNodes['sym'].Text := ''; // Text of GPS symbol name. For interchange with other programs, use the exact spelling of the symbol as displayed on the GPS. If the GPS abbreviates words, spell them out.
      VExtensionsNode := VCurrentNode.AddChild('extensions'); // You can add extend GPX by adding your own elements from another schema here.
      VExtensionNode := VExtensionsNode.AddChild('gpxx:WaypointExtension');
      VExtensionNode.AddChild('gpxx:DisplayMode').Text := 'SymbolAndName'; // Other possible values: 'SymbolOnly' 'SymbolAndDescription'
      AddCategories(VExtensionNode.AddChild('gpxx:Categories'), 'gpxx');
      VExtensionNode := VExtensionsNode.AddChild('wptx1:WaypointExtension');
      VExtensionNode.AddChild('wptx1:DisplayMode').Text := 'SymbolAndName'; // Other possible values: 'SymbolOnly' 'SymbolAndDescription'
      AddCategories(VExtensionNode.AddChild('wptx1:Categories'), 'wptx1');
    end;
  end;

  procedure AddLine(
    const AMark: IVectorDataItem;
    const ALonLatLine: IGeometryLonLatSingleLine);

    function IsTrack: Boolean;
    var
      VLCDescr: String;
    begin
      VLCDescr := LowerCase(AMark.Desc);
      Result := (
                  (ALonLatLine.Count >= 500) or
                  (Pos('track: true', VLCDescr) > 0)
                ) and
                (Pos('track: false', VLCDescr) <= 0);
    end;

  var
    VCurrentNode: TALXMLNode;
    VAppearanceLine: IAppearanceLine;
    VPoints: IEnumLonLatPoint;
    VPoint: TDoublePoint;
    VPointNode: TALXMLNode;
    VExtensionsNode: TALXMLNode;
    VExtensionNode: TALXMLNode;
    VPointInd: Integer;
    VLength: Double;
    VStartTime: TDateTime;
    VDelta: TDateTime;
    VDesc: String;
    VCmt: String;
    VDT: TDateTime;
  begin
    if IsTrack then begin
      VCurrentNode := FGPXNode.AddChild('trk');
      VCurrentNode.ChildNodes['name'].Text := XMLText(AMark.Name); // GPS name of track.

      // Order of extraction is important
      VDesc := ExtractDesc(AMark.Desc);
      VCmt := ExtractCmt(VDesc);
      VDT := ExtractTime(VDesc);
      if VDT <> 0 then
        VCurrentNode.ChildNodes['time'].Text := XMLDateTime(VDT); // Creation/modification timestamp for element. Date and time in are in Univeral Coordinated Time (UTC), not local time! Conforms to ISO 8601 specification for date/time representation. Fractional seconds are allowed for millisecond timing in tracklogs.
      if (VCmt = '') and (VDesc <> '') then // Google Earth ignore "desc"? And shows "cmt" only
      begin
        VCmt := VDesc;
        VDesc := '';
      end;
      if VCmt <> '' then
        VCurrentNode.ChildNodes['cmt'].Text := XMLText(VCmt); // GPS waypoint comment. Sent to GPS as comment.
      if VDesc <> '' then
        VCurrentNode.ChildNodes['desc'].Text := XMLText(VDesc); // A text description of the element. Holds additional information about the element intended for the user, not the GPS.

      VCurrentNode.ChildNodes['number'].Text := XMLText(IntToStr(FTrackNumber)); // GPS track number.
      Inc(FTrackNumber);

      // VCurrentNode.ChildNodes['src'].Text := XMLText(''); // Source of data. Included to give user some idea of reliability and accuracy of data.
      // VCurrentNode.ChildNodes['link'].Text := XMLText(''); // Links to external information about track.
      // VCurrentNode.ChildNodes['type'].Text := XMLText(''); // Type (classification) of track.

      VExtensionsNode := VCurrentNode.AddChild('extensions'); // You can add extend GPX by adding your own elements from another schema here.
      if Supports(ALonLatLine, IAppearanceLine, VAppearanceLine) then begin
        VExtensionNode := VExtensionsNode.AddChild('gpxx:TrackExtension');
        VExtensionNode.AddChild('gpxx:DisplayColor').Text := Color32toGPXColor(VAppearanceLine.LineColor);
        AddCategories(VExtensionNode.AddChild('gpxx:Categories'), 'gpxx');
        VExtensionNode := VExtensionsNode.AddChild('gpxtrx:TrackExtension');
        VExtensionNode.AddChild('gpxtrx:DisplayColor').Text := Color32toGPXColor(VAppearanceLine.LineColor);
        AddCategories(VExtensionNode.AddChild('gpxtrx:Categories'), 'gpxtrx');
      end
      else
      begin
        VExtensionNode := VExtensionsNode.AddChild('gpxx:TrackExtension');
        AddCategories(VExtensionNode.AddChild('gpxx:Categories'), 'gpxx');
        VExtensionNode := VExtensionsNode.AddChild('gpxtrx:TrackExtension');
        AddCategories(VExtensionNode.AddChild('gpxtrx:Categories'), 'gpxtrx');
      end;

      VCurrentNode := VCurrentNode.AddChild('trkseg'); // A Track Segment holds a list of Track Points which are logically connected in order. To represent a single GPS track where GPS reception was lost, or the GPS receiver was turned off, start a new Track Segment for each continuous span of track data.

      VLength := FGeoCalc.CalcLineLength(ALonLatLine); // distance in meters
      VStartTime := IncSecond(FNow, -Round(VLength / DummySpeedMS));
      VDelta := (FNow - VStartTime) / ALonLatLine.Count;
      VPoints := ALonLatLine.GetEnum;
      while VPoints.Next(VPoint) do begin
        VPointNode := VCurrentNode.AddChild('trkpt');
        VPointNode.Attributes['lat'] := R2AnsiStrPoint(VPoint.Y);
        VPointNode.Attributes['lon'] := R2AnsiStrPoint(VPoint.X);
        VPointNode.AddChild('time').Text := XMLDateTime(VStartTime, True); // <- must be present, otherwise track is not visible in Google Earth/Strava. We do not have time, so fake it
        VStartTime := VStartTime + VDelta;
      end;
    end
    else begin
      VCurrentNode := FGPXNode.AddChild('rte');
      VCurrentNode.ChildNodes['name'].Text := XMLText(AMark.Name); // GPS name of route.

      // Order of extraction is important
      VDesc := ExtractDesc(AMark.Desc);
      VCmt := ExtractCmt(VDesc);
      if (VCmt = '') and (VDesc <> '') then // Google Earth ignore "desc"? And shows "cmt" only
      begin
        VCmt := VDesc;
        VDesc := '';
      end;
      if VCmt <> '' then
        VCurrentNode.ChildNodes['cmt'].Text := XMLText(VCmt); // GPS waypoint comment. Sent to GPS as comment.
      if VDesc <> '' then
        VCurrentNode.ChildNodes['desc'].Text := XMLText(VDesc); // A text description of the element. Holds additional information about the element intended for the user, not the GPS.

      VCurrentNode.ChildNodes['number'].Text := XMLText(IntToStr(FTrackNumber)); // GPS route number.
      Inc(FTrackNumber);

      // VCurrentNode.ChildNodes['src'].Text := XMLText(''); // Source of data. Included to give user some idea of reliability and accuracy of data.
      // VCurrentNode.ChildNodes['link'].Text := XMLText(''); // Links to external information about route.
      // VCurrentNode.ChildNodes['type'].Text := XMLText(''); // Type (classification) of route.

      VExtensionsNode := VCurrentNode.AddChild('extensions'); // You can add extend GPX by adding your own elements from another schema here.
      if Supports(ALonLatLine, IAppearanceLine, VAppearanceLine) then begin
        VExtensionNode := VExtensionsNode.AddChild('gpxx:RouteExtension');
        VExtensionNode.AddChild('gpxx:DisplayColor').Text := Color32toGPXColor(VAppearanceLine.LineColor);
        AddCategories(VExtensionNode.AddChild('gpxx:Categories'), 'gpxx');
        VExtensionNode := VExtensionsNode.AddChild('gpxtrx:TrackExtension');
        VExtensionNode.AddChild('gpxrte:DisplayColor').Text := Color32toGPXColor(VAppearanceLine.LineColor);
        AddCategories(VExtensionNode.AddChild('gpxtrx:Categories'), 'gpxtrx');
      end
      else begin
        VExtensionNode := VExtensionsNode.AddChild('gpxx:RouteExtension');
        AddCategories(VExtensionNode.AddChild('gpxx:Categories'), 'gpxx');
        VExtensionNode := VExtensionsNode.AddChild('gpxtrx:TrackExtension');
        AddCategories(VExtensionNode.AddChild('gpxtrx:Categories'), 'gpxtrx');
      end;

      VPointInd := 1;
      VPoints := ALonLatLine.GetEnum;
      while VPoints.Next(VPoint) do begin
        VPointNode := VCurrentNode.AddChild('rtept');
        VPointNode.Attributes['lat'] := R2AnsiStrPoint(VPoint.Y);
        VPointNode.Attributes['lon'] := R2AnsiStrPoint(VPoint.X);
        VPointNode.AddChild('name').Text := XMLText(AMark.Name + ' ' + IntToStr(VPointInd)); // <- must be present, otherwise route is not visible
        VPointNode.AddChild('sym').Text := 'waypoint';
        Inc(VPointInd);
      end;
    end;
  end;

  procedure AddMultiLine(
    const AMark: IVectorDataItem;
    const ALonLatPath: IGeometryLonLatMultiLine);
  var
    VCurrentNode: TALXMLNode;
    VAppearanceLine: IAppearanceLine;
    VLonLatPathLine: IGeometryLonLatSingleLine;
    VRootNode: TALXMLNode;
    VPoints: IEnumLonLatPoint;
    VPoint: TDoublePoint;
    VPointNode: TALXMLNode;
    VExtensionsNode: TALXMLNode;
    VExtensionNode: TALXMLNode;
    VLength: Double;
    VStartTime: TDateTime;
    VDelta: TDateTime;
    VCount: Integer;
    VDesc: String;
    VCmt: String;
    VDT: TDateTime;
    i: Integer;
  begin
    if ALonLatPath.Count <= 0 then
      Exit;
    if ALonLatPath.Count = 1 then begin
      VLonLatPathLine := ALonLatPath.Item[0];
      AddLine(AMark, VLonLatPathLine);
      Exit;
    end;

    VCurrentNode := FGPXNode.AddChild('trk');
    VCurrentNode.ChildNodes['name'].Text := XMLText(AMark.Name); // GPS name of track.

    VDesc := AMark.Desc;
    VCmt := ExtractCmt(VDesc);
    if VCmt <> '' then
      VCurrentNode.ChildNodes['cmt'].Text := XMLText(VCmt); // GPS waypoint comment. Sent to GPS as comment.
    VDT := ExtractTime(VDesc);
    if VDT <> 0 then
      VCurrentNode.ChildNodes['time'].Text := XMLDateTime(VDT); // Creation/modification timestamp for element. Date and time in are in Univeral Coordinated Time (UTC), not local time! Conforms to ISO 8601 specification for date/time representation. Fractional seconds are allowed for millisecond timing in tracklogs.
    if VDesc <> '' then
      VCurrentNode.ChildNodes['desc'].Text := XMLText(VDesc); // A text description of the element. Holds additional information about the element intended for the user, not the GPS.

    VCurrentNode.ChildNodes['number'].Text := XMLText(IntToStr(FTrackNumber)); // GPS track number.
    Inc(FTrackNumber);

    // VCurrentNode.ChildNodes['src'].Text := XMLText(''); // Source of data. Included to give user some idea of reliability and accuracy of data.
    // VCurrentNode.ChildNodes['link'].Text := XMLText(''); // Links to external information about track.
    // VCurrentNode.ChildNodes['type'].Text := XMLText(''); // Type (classification) of track.

    VExtensionsNode := VCurrentNode.AddChild('extensions'); // You can add extend GPX by adding your own elements from another schema here.
    if Supports(ALonLatPath, IAppearanceLine, VAppearanceLine) then begin
      VExtensionNode := VExtensionsNode.AddChild('gpxx:TrackExtension');
      VExtensionNode.AddChild('gpxx:DisplayColor').Text := Color32toGPXColor(VAppearanceLine.LineColor);
      AddCategories(VExtensionNode.AddChild('gpxx:Categories'), 'gpxx');
      VExtensionNode := VExtensionsNode.AddChild('gpxtrx:TrackExtension');
      VExtensionNode.AddChild('gpxtrx:DisplayColor').Text := Color32toGPXColor(VAppearanceLine.LineColor);
      AddCategories(VExtensionNode.AddChild('gpxtrx:Categories'), 'gpxtrx');
    end
    else
    begin
      VExtensionNode := VExtensionsNode.AddChild('gpxx:TrackExtension');
      AddCategories(VExtensionNode.AddChild('gpxx:Categories'), 'gpxx');
      VExtensionNode := VExtensionsNode.AddChild('gpxtrx:TrackExtension');
      AddCategories(VExtensionNode.AddChild('gpxtrx:Categories'), 'gpxtrx');
    end;

    VCount := 0;
    for i := 0 to ALonLatPath.Count - 1 do begin
      VLonLatPathLine := ALonLatPath.Item[i];
      VCount := VCount + VLonLatPathLine.Count;
    end;
    VLength := FGeoCalc.CalcLineLength(ALonLatPath); // distance in meters
    VStartTime := IncSecond(FNow, -Round(VLength / DummySpeedMS));
    VDelta := (FNow - VStartTime) / VCount;

    for i := 0 to ALonLatPath.Count - 1 do begin
      VLonLatPathLine := ALonLatPath.Item[i];
      if VLonLatPathLine.Count > 0 then begin
        VRootNode := VCurrentNode.AddChild('trkseg');
        VPoints := VLonLatPathLine.GetEnum;
        while VPoints.Next(VPoint) do begin
          VPointNode := VRootNode.AddChild('trkpt');
          VPointNode.Attributes['lat'] := R2AnsiStrPoint(VPoint.Y);
          VPointNode.Attributes['lon'] := R2AnsiStrPoint(VPoint.X);
          VPointNode.AddChild('time').Text := XMLDateTime(VStartTime, True); // <- must be present, otherwise track is not visible in Google Earth. We do not have time, so fake it
          VStartTime := VStartTime + VDelta;
        end;
      end;
    end;
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

function TExportMarks2GPX.Color32toGPXColor(const AColor32: TColor32): AnsiString;
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
  GarminPalette: array[TGarminColor] of AnsiString = (
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

var
  VColor: TColor32Entry;
  VGarminColor: TGarminColor;
  VMin: Integer;
  VDiff: Integer;
  X: TGarminColor;
begin
  // TODO: implement better matching alg? E.g. better human-friendly "visual" match
  VColor.ARGB := AColor32;
  if VColor.A >= 128 then
    Result := GarminPalette[gcTransparent]
  else begin
    VMin := MaxInt;
    VGarminColor := gcBlack;
    for X := gcBlack to Pred(High(X)) do begin // ignore gcTransparent
      VDiff := Sqr(GarminColors[VGarminColor].R - VColor.R) +
               Sqr(GarminColors[VGarminColor].G - VColor.G) +
               Sqr(GarminColors[VGarminColor].B - VColor.B);
      if VDiff < VMin then begin
        VMin := VDiff;
        VGarminColor := X;
      end;
    end;
    Result := GarminPalette[VGarminColor];
  end;
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

function TExportMarks2GPX.XMLDateTime(const ADateTime: TDateTime; const ADetailed: Boolean): AnsiString;

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

function TExportMarks2GPX.XMLText(const AStr: String): AnsiString;
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

function TExportMarks2GPX.FindSymByName(const AName: String): AnsiString;
const
  GarminSymNames: array[0..138] of String = (
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
  Words: TWords;
  Index: TIndex;
begin
  SplitIntoWords(AName, Words);
  BuildIndex(Words, Index);
  SortIndex(Index, 0, High(Index));

  if Index[0].Similarity > 0 then
    Result := XMLText(GarminSymNames[Index[0].Sym])
  else
    Result := '';
  if Result = '' then
    Result := 'Flag, Blue';
end;

function TExportMarks2GPX.FindSymByMark(const AMark: IVectorDataItem): AnsiString;
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
