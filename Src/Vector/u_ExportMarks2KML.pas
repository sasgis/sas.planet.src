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

unit u_ExportMarks2KML;

interface

uses
  Classes,
  SysUtils,
  Math,
  t_Bitmap32,
  t_GeoTypes,
  i_ArchiveReadWrite,
  i_ArchiveReadWriteFactory,
  i_ExportMarks2KMLConfig,
  i_EnumDoublePoint,
  i_GeometryLonLat,
  i_Appearance,
  i_AppearanceOfVectorItem,
  i_VectorDataItemSimple,
  i_VectorItemSubset,
  i_VectorItemTree,
  u_XmlStreamingWriter;

type
  TExportMarks2KML = class
  private
    FFileName: string;
    FArchiveReadWriteFactory: IArchiveReadWriteFactory;
    FZip: IArchiveWriterSequential;
    FConfig: IExportMarks2KMLConfigStatic;
    FMarkIconList: TStringList;
    FKmlWriter: TXmlStreamingWriter;

    function AddTree(const ATree: IVectorItemTree): Boolean;
    function AddMarks(const AMarksSubset: IVectorItemSubset): Boolean;
    procedure AddMark(const AMark: IVectorDataItem);
    procedure AddPointAppearence(const AAppearence: IAppearance);
    procedure AddLineAppearence(const AAppearance: IAppearance);
    procedure AddPolygonAppearence(const AAppearance: IAppearance);
    procedure AddPoint(const AGeometry: IGeometryLonLatPoint);
    procedure AddSingleLine(const AGeometry: IGeometryLonLatSingleLine);
    procedure AddMultiLine(const AGeometry: IGeometryLonLatMultiLine);
    procedure AddLine(const AGeometry: IGeometryLonLatLine);
    procedure AddContour(const AOuter: Boolean; const AGeometry: IGeometryLonLatContour);
    procedure AddSinglePolygon(const AGeometry: IGeometryLonLatSinglePolygon);
    procedure AddMultiPolygon(const AGeometry: IGeometryLonLatMultiPolygon);
    procedure AddPolygon(const AGeometry: IGeometryLonLatPolygon);
    function SaveMarkIcon(const AAppearanceIcon: IAppearancePointIcon): string;
    procedure WriteCoordinates(const ACoordinates: string); overload;
    procedure WriteCoordinates(const APointEnum: IEnumLonLatPoint); overload;
    function Color32toKMLColor(const Color32: TColor32): string; inline;
    function PointToKml(const APoint: TDoublePoint; const AElevation: Double = 0): string;
  public
    procedure ExportTreeToKML(
      const ATree: IVectorItemTree;
      const AFileName: string
    );
  public
    constructor Create(
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
      const AExportMarks2KMLConfig: IExportMarks2KMLConfig
    );
    destructor Destroy; override;
  end;

  EExportMarks2Kml = class(Exception);

implementation

uses
  ExplorerSort,
  i_BinaryData,
  i_ArchiveReadWriteConfig,
  u_ArchiveReadWriteConfig,
  u_Encodings,
  u_GeoToStrFunc,
  u_MarkPictureAnchorFunc,
  u_StreamReadOnlyByBinaryData;

{ TExportMarks2KML }

constructor TExportMarks2KML.Create(
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
  const AExportMarks2KMLConfig: IExportMarks2KMLConfig
);
begin
  Assert(AExportMarks2KMLConfig <> nil);

  inherited Create;

  FArchiveReadWriteFactory := AArchiveReadWriteFactory;
  FConfig := AExportMarks2KMLConfig.GetStatic;

  FMarkIconList := TStringList.Create;
  FMarkIconList.Sorted := True;
  FMarkIconList.CaseSensitive := False;
end;

destructor TExportMarks2KML.Destroy;
begin
  FreeAndNil(FMarkIconList);
  inherited Destroy;
end;

procedure TExportMarks2KML.ExportTreeToKML(const ATree: IVectorItemTree; const AFileName: string);
var
  VEncoding: TEncoding;
  VTmpFileName: string;
  VKmlStream: TStream;
  VZipConfig: IArchiveWriteZipConfig;
begin
  FFileName := AFileName;

  if SameText(ExtractFileExt(FFileName), '.kmz') then begin
    VZipConfig := TArchiveWriteZipConfig.Create(zclNormal, zcmDeflate, 0);
    FZip := FArchiveReadWriteFactory.ZipSequential.WriterFactory.Build(FFileName, VZipConfig);
    VTmpFileName := ChangeFileExt(FFileName, '.tmp');
    VKmlStream := TFileStream.Create(VTmpFileName, fmCreate);
  end else begin
    FZip := nil;
    VKmlStream := TFileStream.Create(FFileName, fmCreate);
  end;

  VEncoding := TUTF8Encoding.Create(False); // UTF8 without BOM
  try
    FKmlWriter := TXmlStreamingWriter.Create(VKmlStream, VEncoding);
    try
      FKmlWriter.Version     := '1.0';
      FKmlWriter.Encoding    := 'UTF-8';
      FKmlWriter.StandAlone  := '';
      FKmlWriter.IndentChars := '  ';
      FKmlWriter.LineBreak   := #13#10;

      FKmlWriter.StartDocument;
      begin
        FKmlWriter.StartElement('kml');
        FKmlWriter.WriteAttribute('xmlns', 'http://www.opengis.net/kml/2.2');
        FKmlWriter.WriteAttribute('xmlns:gx', 'http://www.google.com/kml/ext/2.2');
        FKmlWriter.WriteAttribute('xmlns:kml', 'http://www.opengis.net/kml/2.2');

        FKmlWriter.StartElement('Document');

        AddTree(ATree);
      end;
      FKmlWriter.EndDocument;
    finally
      FreeAndNil(FKmlWriter);
    end;
  finally
    VEncoding.Free;
    VKmlStream.Free;
  end;

  if Assigned(FZip) then begin
    FZip.AddFile(VTmpFileName, 'doc.kml');
    FZip := nil;
    SysUtils.DeleteFile(VTmpFileName);
  end;
end;

function TExportMarks2KML.AddMarks(const AMarksSubset: IVectorItemSubset): Boolean;
var
  I: Integer;
  VList: TStringList;
  VMark: IVectorDataItem;
begin
  Result := False;
  if Assigned(AMarksSubset) and (AMarksSubset.Count > 0) then begin
    VList := TStringList.Create;
    try
      for I := 0 to AMarksSubset.Count - 1 do begin
        VMark := AMarksSubset.Items[I];
        VList.AddObject(VMark.Name, Pointer(VMark));
      end;

      case FConfig.SortingType of
        kstNone       : {do nothing} ;
        kstByNameASC  : VList.CustomSort(ExplorerSort.StringListCompare);
        kstByNameDESC : VList.CustomSort(ExplorerSort.StringListCompareDESC);
      else
        raise EExportMarks2Kml.CreateFmt('Unknown sorting type: %d', [Integer(FConfig.SortingType)]);
      end;

      for I := 0 to VList.Count - 1 do begin
        VMark := IVectorDataItem(Pointer(VList.Objects[I]));
        AddMark(VMark);
      end;
      Result := True;
    finally
      VList.Free;
    end;
  end;
end;

function TExportMarks2KML.AddTree(const ATree: IVectorItemTree): Boolean;
var
  I: Integer;
  VSubTree: IVectorItemTree;
  VList: TStringList;
  VHasContent: Boolean;
begin
  Result := False;

  if not Assigned(ATree) then begin
    Exit;
  end;

  VHasContent := (ATree.SubTreeItemCount > 0) or (Assigned(ATree.Items) and (ATree.Items.Count > 0));
  if not VHasContent then begin
    Exit;
  end;

  // Process sub-folders first
  if ATree.SubTreeItemCount > 0 then begin
    VList := TStringList.Create;
    try
      for I := 0 to ATree.SubTreeItemCount - 1 do begin
        VSubTree := ATree.GetSubTreeItem(I);
        VList.AddObject(VSubTree.Name, Pointer(VSubTree));
      end;

      case FConfig.SortingType of
        kstNone       : {do nothing} ;
        kstByNameASC  : VList.CustomSort(ExplorerSort.StringListCompare);
        kstByNameDESC : VList.CustomSort(ExplorerSort.StringListCompareDESC);
      else
        raise EExportMarks2Kml.CreateFmt('Unknown sorting type: %d', [Integer(FConfig.SortingType)]);
      end;

      for I := 0 to VList.Count - 1 do begin
        VSubTree := IVectorItemTree(Pointer(VList.Objects[I]));
        if (VSubTree.SubTreeItemCount > 0) or (Assigned(VSubTree.Items) and (VSubTree.Items.Count > 0)) then begin
          FKmlWriter.StartElement('Folder');
          begin
            FKmlWriter.WriteElementString('name', VSubTree.Name);
            FKmlWriter.WriteElementString('open', '1');

            FKmlWriter.StartElement('Style');
            begin
              FKmlWriter.StartElement('ListStyle');
              begin
                FKmlWriter.WriteElementString('listItemType', 'check');
                FKmlWriter.WriteElementString('bgColor', '00ffffff');
              end;
              FKmlWriter.EndElement; // ListStyle
            end;
            FKmlWriter.EndElement; // Style

            // Recursive call to process the contents of the subfolder
            AddTree(VSubTree);
          end;
          FKmlWriter.EndElement; // Folder
        end;
      end;
    finally
      VList.Free;
    end;
  end;

  // Process marks in the current folder
  if AddMarks(ATree.Items) then begin
    Result := True;
  end;
end;

procedure TExportMarks2KML.AddPoint(const AGeometry: IGeometryLonLatPoint);
begin
  FKmlWriter.StartElement('Point');
  FKmlWriter.WriteElementString('extrude', '1');
  WriteCoordinates(PointToKml(AGeometry.Point));
  FKmlWriter.EndElement; // Point
end;

procedure TExportMarks2KML.AddSingleLine(const AGeometry: IGeometryLonLatSingleLine);
var
  VCoordinates: string;
begin
  FKmlWriter.StartElement('LineString');
  begin
    FKmlWriter.WriteElementString('extrude', '1');
    if AGeometry.Count > 1 then begin
      WriteCoordinates(AGeometry.GetEnum);
    end else begin
      VCoordinates := PointToKml(AGeometry.Points[0]);
      WriteCoordinates(VCoordinates + VCoordinates);
    end;
  end;
  FKmlWriter.EndElement; // LineString
end;

procedure TExportMarks2KML.AddMultiLine(const AGeometry: IGeometryLonLatMultiLine);
var
  I: Integer;
begin
  if AGeometry.Count > 1 then begin
    FKmlWriter.StartElement('MultiGeometry');
    begin
      for I := 0 to AGeometry.Count - 1 do begin
        AddSingleLine(AGeometry.Item[I]);
      end;
    end;
    FKmlWriter.EndElement; // MultiGeometry
  end else if AGeometry.Count = 1 then begin
    AddSingleLine(AGeometry.Item[0]);
  end;
end;

procedure TExportMarks2KML.AddLine(const AGeometry: IGeometryLonLatLine);
var
  VMultiLine: IGeometryLonLatMultiLine;
  VSingleLine: IGeometryLonLatSingleLine;
begin
  if Supports(AGeometry, IGeometryLonLatSingleLine, VSingleLine) then begin
    AddSingleLine(VSingleLine);
  end else if Supports(AGeometry, IGeometryLonLatMultiLine, VMultiLine) then begin
    AddMultiLine(VMultiLine);
  end else begin
    Assert(False);
  end;
end;

procedure TExportMarks2KML.AddContour(const AOuter: Boolean; const AGeometry: IGeometryLonLatContour);
var
  VCoordinates: string;
  VFirst: PDoublePoint;
  VSecond: PDoublePoint;
  VBoundaryTag: string;
begin
  if AOuter then begin
    VBoundaryTag := 'outerBoundaryIs';
  end else begin
    VBoundaryTag := 'innerBoundaryIs';
  end;

  FKmlWriter.StartElement(VBoundaryTag);
  begin
    FKmlWriter.StartElement('LinearRing');
    begin
      if AGeometry.Count > 2 then begin
        WriteCoordinates(AGeometry.GetEnum);
      end else if AGeometry.Count > 1 then begin
        VFirst := Addr(AGeometry.Points[0]);
        VSecond := VFirst;
        Inc(VSecond);
        WriteCoordinates(
          PointToKml(VFirst^) +
          PointToKml(VSecond^) +
          PointToKml(VSecond^) +
          PointToKml(VFirst^)
        );
      end else begin
        VCoordinates := PointToKml(AGeometry.Points[0]);
        WriteCoordinates(VCoordinates + VCoordinates + VCoordinates + VCoordinates);
      end;
    end;
    FKmlWriter.EndElement; // LinearRing
  end;
  FKmlWriter.EndElement; // *BoundaryIs
end;

procedure TExportMarks2KML.AddSinglePolygon(const AGeometry: IGeometryLonLatSinglePolygon);
var
  I: Integer;
begin
  FKmlWriter.StartElement('Polygon');
  begin
    FKmlWriter.WriteElementString('extrude', '1');
    AddContour(True, AGeometry.OuterBorder);
    for I := 0 to AGeometry.HoleCount - 1 do begin
      AddContour(False, AGeometry.HoleBorder[I]);
    end;
  end;
  FKmlWriter.EndElement; // Polygon
end;

procedure TExportMarks2KML.AddMultiPolygon(const AGeometry: IGeometryLonLatMultiPolygon);
var
  I: Integer;
begin
  if AGeometry.Count > 1 then begin
    FKmlWriter.StartElement('MultiGeometry');
    begin
      for I := 0 to AGeometry.Count - 1 do begin
        AddSinglePolygon(AGeometry.Item[I]);
      end;
    end;
    FKmlWriter.EndElement; // MultiGeometry
  end else if AGeometry.Count = 1 then begin
    AddSinglePolygon(AGeometry.Item[0]);
  end;
end;

procedure TExportMarks2KML.AddPolygon(const AGeometry: IGeometryLonLatPolygon);
var
  VMultiPolygon: IGeometryLonLatMultiPolygon;
  VSinglePolygon: IGeometryLonLatSinglePolygon;
begin
  if Supports(AGeometry, IGeometryLonLatSinglePolygon, VSinglePolygon) then begin
    AddSinglePolygon(VSinglePolygon);
  end else if Supports(AGeometry, IGeometryLonLatMultiPolygon, VMultiPolygon) then begin
    AddMultiPolygon(VMultiPolygon);
  end else begin
    Assert(False);
  end;
end;

procedure TExportMarks2KML.AddPointAppearence(const AAppearence: IAppearance);
const
  cSASDefaultFontSize = 11;
  cSASDefaultIconSize = 32;
var
  VScale: Double;
  VAnchor: TDoublePoint;
  VFileName: string;
  VAppearanceIcon: IAppearancePointIcon;
  VAppearanceCaption: IAppearancePointCaption;
begin
  if not Supports(AAppearence, IAppearancePointIcon, VAppearanceIcon) then begin
    VAppearanceIcon := nil;
  end;
  if not Supports(AAppearence, IAppearancePointCaption, VAppearanceCaption) then begin
    VAppearanceCaption := nil;
  end;

  if (VAppearanceCaption <> nil) or (VAppearanceIcon <> nil) then begin
    FKmlWriter.StartElement('Style');
    begin
      if VAppearanceCaption <> nil then begin
        FKmlWriter.StartElement('LabelStyle');
        begin
          FKmlWriter.WriteElementString('color', Color32toKMLColor(VAppearanceCaption.TextColor));
          VScale := VAppearanceCaption.FontSize / cSASDefaultFontSize;
          if VScale < 0.4 then begin
            VScale := 0.4; // GoogleEarth hides Caption if its scale less then 0.4
          end;
          FKmlWriter.WriteElementString('scale', R2AnsiStrPoint(VScale));
        end;
        FKmlWriter.EndElement; // LabelStyle
      end;
      if VAppearanceIcon <> nil then begin
        if VAppearanceIcon.Pic <> nil then begin
          FKmlWriter.StartElement('IconStyle');
          begin
            case FConfig.IconScaleType of
              kistAbs    : VScale := VAppearanceIcon.MarkerSize / VAppearanceIcon.Pic.GetMarker.Size.X;
              kistSmall  : VScale := VAppearanceIcon.MarkerSize / 28;
              kistMedium : VScale := VAppearanceIcon.MarkerSize / cSASDefaultIconSize;
              kistLarge  : VScale := VAppearanceIcon.MarkerSize / 38;
            else
              raise EExportMarks2Kml.CreateFmt('Unknown icon scale type: %d', [Integer(FConfig.IconScaleType)]);
            end;
            if VScale < 0.2 then begin
              VScale := 0.2; // GoogleEarth hides Icon if its scale less then 0.2
            end;
            FKmlWriter.WriteElementString('scale', R2StrPoint(VScale));

            FKmlWriter.StartElement('Icon');
            begin
              if FConfig.UseAbsPathToIcon then begin
                VFileName := FConfig.AbsPathToIcon + ExtractFileName(VAppearanceIcon.Pic.GetName);
              end else begin
                VFileName := SaveMarkIcon(VAppearanceIcon);
              end;
              FKmlWriter.WriteElementString('href', VFileName);
            end;
            FKmlWriter.EndElement; // Icon

            FKmlWriter.StartElement('hotSpot');
            begin
              VAnchor :=
                AnchorAbsoluteToRelative(
                  VAppearanceIcon.Pic.GetMarker.AnchorPoint,
                  VAppearanceIcon.Pic.GetMarker.Size
                );
              // The origin of the coordinate system is in the lower left corner of the icon
              FKmlWriter.WriteAttribute('x', R2StrPoint(VAnchor.X));
              FKmlWriter.WriteAttribute('y', R2StrPoint(1.0 - VAnchor.Y));
              FKmlWriter.WriteAttribute('xunits', 'fraction');
              FKmlWriter.WriteAttribute('yunits', 'fraction');
            end;
            FKmlWriter.EndElement; // hotSpot
          end;
          FKmlWriter.EndElement; // IconStyle
        end;
      end;
    end;
    FKmlWriter.EndElement; // Style
  end;
end;

procedure TExportMarks2KML.AddLineAppearence(const AAppearance: IAppearance);
var
  VAppearanceLine: IAppearanceLine;
begin
  if Supports(AAppearance, IAppearanceLine, VAppearanceLine) then begin
    FKmlWriter.StartElement('Style');
    begin
      FKmlWriter.StartElement('LineStyle');
      begin
        FKmlWriter.WriteElementString('color', Color32toKMLColor(VAppearanceLine.LineColor));
        FKmlWriter.WriteElementString('width', R2StrPoint(VAppearanceLine.LineWidth));
      end;
      FKmlWriter.EndElement; // LineStyle
    end;
    FKmlWriter.EndElement; // Style
  end;
end;

procedure TExportMarks2KML.AddPolygonAppearence(const AAppearance: IAppearance);
var
  VAppearanceBorder: IAppearancePolygonBorder;
  VAppearanceFill: IAppearancePolygonFill;
begin
  if not Supports(AAppearance, IAppearancePolygonBorder, VAppearanceBorder) then begin
    VAppearanceBorder := nil;
  end;
  if not Supports(AAppearance, IAppearancePolygonFill, VAppearanceFill) then begin
    VAppearanceFill := nil;
  end;

  if (VAppearanceBorder <> nil) or (VAppearanceFill <> nil) then begin
    FKmlWriter.StartElement('Style');
    begin
      if VAppearanceBorder <> nil then begin
        FKmlWriter.StartElement('LineStyle');
        begin
          FKmlWriter.WriteElementString('color', Color32toKMLColor(VAppearanceBorder.LineColor));
          FKmlWriter.WriteElementString('width', R2StrPoint(VAppearanceBorder.LineWidth));
        end;
        FKmlWriter.EndElement; // LineStyle
      end;
      if VAppearanceFill <> nil then begin
        FKmlWriter.StartElement('PolyStyle');
        begin
          FKmlWriter.WriteElementString('color', Color32toKMLColor(VAppearanceFill.FillColor));
          FKmlWriter.WriteElementString('fill', '1');
        end;
        FKmlWriter.EndElement; // PolyStyle
      end;
    end;
    FKmlWriter.EndElement; // Style
  end;
end;

procedure TExportMarks2KML.AddMark(const AMark: IVectorDataItem);
var
  VLonLatPoint: IGeometryLonLatPoint;
  VLonLatPolygon: IGeometryLonLatPolygon;
  VLonLatLine: IGeometryLonLatLine;
begin
  FKmlWriter.StartElement('Placemark');
  begin
    FKmlWriter.WriteElementString('name', AMark.Name);
    FKmlWriter.WriteElementString('description', AMark.Desc);

    if Supports(AMark.Geometry, IGeometryLonLatPoint, VLonLatPoint) then begin
      AddPointAppearence(AMark.Appearance);
      AddPoint(VLonLatPoint);
    end else if Supports(AMark.Geometry, IGeometryLonLatLine, VLonLatLine) then begin
      AddLineAppearence(AMark.Appearance);
      AddLine(VLonLatLine);
    end else if Supports(AMark.Geometry, IGeometryLonLatPolygon, VLonLatPolygon) then begin
      AddPolygonAppearence(AMark.Appearance);
      AddPolygon(VLonLatPolygon);
    end;
  end;
  FKmlWriter.EndElement; // Placemark
end;

function TExportMarks2KML.Color32toKMLColor(const Color32: TColor32): string;
var
  VColor: TColor32Entry;
begin
  VColor.ARGB := Color32;
  Result :=
    IntToHex(VColor.A, 2) +
    IntToHex(VColor.B, 2) +
    IntToHex(VColor.G, 2) +
    IntToHex(VColor.R, 2);
end;

function TExportMarks2KML.SaveMarkIcon(const AAppearanceIcon: IAppearancePointIcon): string;
const
  cFilesFolderName = 'files';
var
  I: Integer;
  VTargetFullName: string;
  VPicName: string;
  VPicNameLower: string;
  VStream: TCustomMemoryStream;
  VData: IBinaryData;
begin
  Result := '';
  if AAppearanceIcon.Pic <> nil then begin
    VData := AAppearanceIcon.Pic.Source;
    if VData <> nil then begin
      VPicName := StringReplace(AAppearanceIcon.PicName, PathDelim, '/', [rfReplaceAll]);
      Result := cFilesFolderName + '/' + VPicName;

      VPicNameLower := AnsiLowerCase(VPicName);
      if FMarkIconList.Find(VPicNameLower, I) then begin
        Exit; // icon has been saved previously
      end;

      if Assigned(FZip) then begin
        FZip.Add(VData, Result, Now);
      end else begin
        VStream := TStreamReadOnlyByBinaryData.Create(VData);
        try
          VTargetFullName := ExtractFilePath(FFileName) + StringReplace(Result, '/', PathDelim, [rfReplaceAll]);
          if not ForceDirectories(ExtractFileDir(VTargetFullName)) then begin
            RaiseLastOSError;
          end;
          VStream.SaveToFile(VTargetFullName);
        finally
          VStream.Free;
        end;
      end;

      // remember saved icon name
      FMarkIconList.Add(VPicNameLower);
    end;
  end;
end;

procedure TExportMarks2KML.WriteCoordinates(const ACoordinates: string);
begin
  FKmlWriter.StartElement('coordinates');
  FKmlWriter.WriteRaw(ACoordinates);
  FKmlWriter.EndElement(False); // coordinates
end;

procedure TExportMarks2KML.WriteCoordinates(const APointEnum: IEnumLonLatPoint);
var
  VPoint: TDoublePoint;
  VMeta: TDoublePointsMetaItem;
  VElevation: Double;
begin
  FKmlWriter.StartElement('coordinates');
  while APointEnum.Next(VPoint, VMeta) do begin
    if VMeta.IsElevationOk then begin
      VElevation := VMeta.Elevation;
    end else begin
      VElevation := 0;
    end;
    FKmlWriter.WriteRaw(PointToKml(VPoint, VElevation));
  end;
  FKmlWriter.EndElement(False); // coordinates
end;

function TExportMarks2KML.PointToKml(const APoint: TDoublePoint; const AElevation: Double): string;
var
  VElevationStr: string;
begin
  if AElevation <> 0 then begin
    VElevationStr := RoundEx(AElevation, 2);
  end else begin
    VElevationStr := '0';
  end;

  if FConfig.UseCoordFormatting then begin
    Result :=
      RoundEx(APoint.X, FConfig.CoordPrecision) + ',' +
      RoundEx(APoint.Y, FConfig.CoordPrecision) + ',' +
      VElevationStr + ' ';
  end else begin
    Result :=
      R2StrPoint(APoint.X) + ',' +
      R2StrPoint(APoint.Y) + ',' +
      VElevationStr + ' ';
  end;
end;

end.
