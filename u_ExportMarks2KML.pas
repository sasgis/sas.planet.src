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

unit u_ExportMarks2KML;

interface

uses
  Classes,
  SysUtils,
  Windows,
  GR32,
  XMLIntf,
  XMLDoc,
  ActiveX,
  i_ArchiveReadWrite,
  i_ArchiveReadWriteFactory,
  i_AppearanceOfVectorItem,
  i_VectorDataItemSimple,
  i_VectorItemSubset,
  i_VectorItemTree,
  u_GeoToStr;

type
  TExportMarks2KML = class
  private
    FKmlDoc: IXMLDocument;
    FFileName: string;
    FKmlNode: iXMLNode;
    FKmlDocumentNode: iXMLNode;
    FArchiveReadWriteFactory: IArchiveReadWriteFactory;
    FZip: IArchiveWriter;
    function AddTree(
      const AParentNode: IXMLNode;
      const ATree: IVectorItemTree
    ): boolean;
    function AddMarks(
      const AMarksSubset: IVectorItemSubset;
      const inNode: iXMLNode
    ): Boolean;
    procedure AddMark(
      const AMark: IVectorDataItemSimple;
      const inNode: iXMLNode
    );
    function SaveMarkIcon(const AAppearanceIcon: IAppearancePointIcon): string;
    function Color32toKMLColor(Color32: TColor32): string;
    procedure PrepareExportToFile(const AFileName: string);
    procedure SaveToFile;
  public
    constructor Create(const AArchiveReadWriteFactory: IArchiveReadWriteFactory);
    procedure ExportTreeToKML(
      const ATree: IVectorItemTree;
      const AFileName: string
    );
  end;

implementation

uses
  t_GeoTypes,
  i_BinaryData,
  i_EnumDoublePoint,
  i_GeometryLonLat,
  u_BinaryDataByMemStream,
  u_StreamReadOnlyByBinaryData;

function XMLTextPrepare(const Src: AnsiString): AnsiString;
var
  i, l: integer;
  Buf, P: PAnsiChar;
  ch: Integer;
begin
  Result := '';
  L := Length(src);
  if L = 0 then begin
    exit;
  end;
  GetMem(Buf, L);
  try
    P := Buf;
    for i := 1 to L do begin
      ch := Ord(src[i]);
      if (ch >= 32) or (ch = $09) or (ch = $0A) or (ch = $0D) then begin
        P^ := AnsiChar(ch);
        Inc(P);
      end;
    end;
    SetString(Result, Buf, P - Buf);
  finally
    FreeMem(Buf);
  end;
end;

function GetKMLCoordinates(
  const APointEnum: IEnumLonLatPoint
): String;
var
  VPoint: TDoublePoint;
begin
  Result := '';
  while APointEnum.Next(VPoint) do begin
    Result := Result + R2StrPoint(VPoint.X) + ',' + R2StrPoint(VPoint.Y) + ',0 ';
  end;
end;

constructor TExportMarks2KML.Create(
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory
);
begin
  inherited Create;
  FArchiveReadWriteFactory := AArchiveReadWriteFactory;
end;

procedure TExportMarks2KML.PrepareExportToFile(const AFileName: string);
begin
  FKmlDoc := TXMLDocument.Create(nil);
  FKmlDoc.Options := FKmlDoc.Options + [doNodeAutoIndent];
  FKmlDoc.Active := true;
  FKmlDoc.Version := '1.0';
  FKmlDoc.Encoding := 'UTF-8';
  FKmlNode := FKmlDoc.AddChild('kml');
  FKmlNode.Attributes['xmlns'] := 'http://earth.google.com/kml/2.2';
  FKmlDocumentNode := FKmlNode.AddChild('Document');
  FFileName := AFileName;
  if ExtractFileExt(FFileName) = '.kmz' then begin
    FZip := FArchiveReadWriteFactory.CreateZipWriterByName(FFileName);
  end else begin
    FZip := nil;
  end;
end;

procedure TExportMarks2KML.SaveToFile;
var
  KMLStream: TMemoryStream;
  VData: IBinaryData;
begin
  if Assigned(FZip) then begin
    KMLStream := TMemoryStream.Create;
    try
      FKmlDoc.SaveToStream(KMLStream);
      KMLStream.Position := 0;
      VData := TBinaryDataByMemStream.CreateWithOwn(KMLStream);
      FZip.AddFile(VData, 'doc.kml', Now);
    finally
      KMLStream.Free;
    end;
  end else begin
    FKmlDoc.SaveToFile(FFileName);
  end;
end;

procedure TExportMarks2KML.ExportTreeToKML(const ATree: IVectorItemTree;
  const AFileName: string);
begin
  PrepareExportToFile(AFileName);
  AddTree(FKmlDocumentNode, ATree);
  SaveToFile;
end;

function TExportMarks2KML.AddMarks(
  const AMarksSubset: IVectorItemSubset;
  const inNode: iXMLNode
): Boolean;
var
  VMark: IVectorDataItemSimple;
  VEnumMarks: IEnumUnknown;
  i: integer;
begin
  Result := False;
  if Assigned(AMarksSubset) then begin
    VEnumMarks := AMarksSubset.GetEnum;
    while (VEnumMarks.Next(1, VMark, @i) = S_OK) do begin
      AddMark(VMark, inNode);
      Result := True;
    end;
  end;
end;

function TExportMarks2KML.AddTree(
  const AParentNode: IXMLNode;
  const ATree: IVectorItemTree
): boolean;
var
  i: Integer;
  VNode: IXMLNode;
  VSubTree: IVectorItemTree;
begin
  Result := False;
  if not Assigned(ATree) then begin
    Exit;
  end;
  for i := 0 to ATree.SubTreeItemCount - 1 do begin
    VSubTree := ATree.GetSubTreeItem(i);
    VNode := AParentNode.AddChild('Folder');
    VNode.ChildValues['name'] := VSubTree.Name;
    VNode.ChildValues['open'] := 1;
    with VNode.AddChild('Style').AddChild('ListStyle') do begin
      ChildValues['listItemType'] := 'check';
      ChildValues['bgColor'] := '00ffffff';
    end;
    if not AddTree(VNode, VSubTree) then begin
      AParentNode.ChildNodes.Remove(VNode);
    end else begin
      Result := True;
    end;
  end;
  if AddMarks(ATree.Items, AParentNode) then begin
    Result := True;
  end;
end;

procedure TExportMarks2KML.AddMark(
  const AMark: IVectorDataItemSimple;
  const inNode: iXMLNode
);
var
  i: integer;
  width: integer;
  currNode: IXMLNode;
  rootNode: IXMLNode;
  VCoordinates: string;
  VFileName: string;
  VMarkPoint: IVectorDataItemPoint;
  VAppearanceIcon: IAppearancePointIcon;
  VAppearanceCaption: IAppearancePointCaption;
  VMarkLine: IVectorDataItemLine;
  VAppearanceLine: IAppearanceLine;
  VMarkPoly: IVectorDataItemPoly;
  VAppearanceBorder: IAppearancePolygonBorder;
  VAppearanceFill: IAppearancePolygonFill;
  VLonLatPolygon: IGeometryLonLatMultiPolygon;
  VLonLatPolygonLine: IGeometryLonLatPolygon;
  VLonLatPath: IGeometryLonLatMultiLine;
  VLonLatPathLine: IGeometryLonLatLine;
begin
  currNode := inNode.AddChild('Placemark');
  currNode.ChildValues['name'] := XMLTextPrepare(AMark.Name);
  currNode.ChildValues['description'] := XMLTextPrepare(AMark.Desc);
  if Supports(AMark, IVectorDataItemPoint, VMarkPoint) then begin
    // Placemark
    if not Supports(AMark.Appearance, IAppearancePointIcon, VAppearanceIcon) then begin
      VAppearanceIcon := nil;
    end;
    if not Supports(AMark.Appearance, IAppearancePointCaption, VAppearanceCaption) then begin
      VAppearanceCaption := nil;
    end;
    if (VAppearanceCaption <> nil) or (VAppearanceIcon <> nil)  then begin
      with currNode.AddChild('Style') do begin
        if VAppearanceCaption <> nil then begin
          with AddChild('LabelStyle') do begin
            ChildValues['color'] := Color32toKMLColor(VAppearanceCaption.TextColor);
            ChildValues['scale'] := R2StrPoint(VAppearanceCaption.FontSize / 14);
          end;
        end;
        if VAppearanceIcon <> nil then begin
          if VAppearanceIcon.Pic <> nil then begin
            with AddChild('IconStyle') do begin
              VFileName := SaveMarkIcon(VAppearanceIcon);
              width := VAppearanceIcon.Pic.GetMarker.Size.X;
              ChildValues['scale'] := R2StrPoint(VAppearanceIcon.MarkerSize / width);
              with AddChild('Icon') do begin
                ChildValues['href'] := VFileName;
              end;
              with AddChild('hotSpot') do begin
                Attributes['x'] := '0.5';
                Attributes['y'] := 0;
                Attributes['xunits'] := 'fraction';
                Attributes['yunits'] := 'fraction';
              end;
            end;
          end;
        end;
      end;
    end;
    currNode := currNode.AddChild('Point');
    currNode.ChildValues['extrude'] := 1;
    with VMarkPoint.Point.Point do begin
      VCoordinates := R2StrPoint(X) + ',' + R2StrPoint(Y) + ',0 ';
    end;
    currNode.ChildValues['coordinates'] := VCoordinates;
  end else if Supports(AMark, IVectorDataItemLine, VMarkLine) then begin
    // <Placemark><MultiGeometry><LineString></LineString><LineString>...
    // <Placemark><LineString><coordinates>
    if Supports(AMark.Appearance, IAppearanceLine, VAppearanceLine) then begin
      with currNode.AddChild('Style') do begin
        with AddChild('LineStyle') do begin
          ChildValues['color'] := Color32toKMLColor(VAppearanceLine.LineColor);
          ChildValues['width'] := R2StrPoint(VAppearanceLine.LineWidth);
        end;
      end;
    end;
    VLonLatPath := VMarkLine.Line;
    if VLonLatPath.Count>1 then begin
      // MultiGeometry
      rootNode := currNode.AddChild('MultiGeometry');
      for i := 0 to VLonLatPath.Count-1 do begin
        VLonLatPathLine := VLonLatPath.Item[i];
        if (VLonLatPathLine.Count>1) then begin
          // make path
          currNode := rootNode.AddChild('LineString');
          currNode.ChildValues['extrude'] := 1;
          VCoordinates := GetKMLCoordinates(VLonLatPathLine.GetEnum);
          currNode.ChildValues['coordinates'] := VCoordinates;
        end;
      end;
    end else begin
      // simple object
      currNode := currNode.AddChild('LineString');
      currNode.ChildValues['extrude'] := 1;
      VLonLatPathLine := VLonLatPath.Item[0];
      VCoordinates := GetKMLCoordinates(VLonLatPathLine.GetEnum);
      currNode.ChildValues['coordinates'] := VCoordinates;
    end;
  end else if Supports(AMark, IVectorDataItemPoly, VMarkPoly) then begin
    // <Placemark><MultiGeometry><Polygon><outerBoundaryIs><LinearRing><coordinates>
    // <Placemark><Polygon><outerBoundaryIs><LinearRing><coordinates>
    if not Supports(AMark.Appearance, IAppearancePolygonBorder, VAppearanceBorder) then begin
      VAppearanceBorder := nil;
    end;
    if not Supports(AMark.Appearance, IAppearancePolygonFill, VAppearanceFill) then begin
      VAppearanceFill := nil;
    end;
    if (VAppearanceBorder <> nil) or (VAppearanceFill <> nil) then begin
      with currNode.AddChild('Style') do begin
        if VAppearanceBorder <> nil then begin
          with AddChild('LineStyle') do begin
            ChildValues['color'] := Color32toKMLColor(VAppearanceBorder.LineColor);
            ChildValues['width'] := R2StrPoint(VAppearanceBorder.LineWidth);
          end;
        end;
        if VAppearanceFill <> nil then begin
          with AddChild('PolyStyle') do begin
            ChildValues['color'] := Color32toKMLColor(VAppearanceFill.FillColor);
            ChildValues['fill'] := 1;
          end;
        end;
      end;
    end;
    VLonLatPolygon := VMarkPoly.Line;
    if VLonLatPolygon.Count>1 then begin
      // MultiGeometry
      rootNode := currNode.AddChild('MultiGeometry');
      for i := 0 to VLonLatPolygon.Count-1 do begin
        VLonLatPolygonLine := VLonLatPolygon.Item[i];
        if (VLonLatPolygonLine.Count>2) then begin
          // make contour
          currNode := rootNode.AddChild('Polygon').AddChild('outerBoundaryIs').AddChild('LinearRing');
          currNode.ChildValues['extrude'] := 1;
          VCoordinates := GetKMLCoordinates(VLonLatPolygonLine.GetEnum);
          currNode.ChildValues['coordinates'] := VCoordinates;
        end;
      end;
    end else begin
      // simple object
      currNode := currNode.AddChild('Polygon').AddChild('outerBoundaryIs').AddChild('LinearRing');
      currNode.ChildValues['extrude'] := 1;
      VLonLatPolygonLine := VLonLatPolygon.Item[0];
      VCoordinates := GetKMLCoordinates(VLonLatPolygonLine.GetEnum);
      currNode.ChildValues['coordinates'] := VCoordinates;
    end;
  end;
end;

function TExportMarks2KML.Color32toKMLColor(Color32: TColor32): string;
begin
  result := IntToHex(AlphaComponent(Color32), 2) +
    IntToHex(BlueComponent(Color32), 2) +
    IntToHex(GreenComponent(Color32), 2) +
    IntToHex(RedComponent(Color32), 2);
end;

function TExportMarks2KML.SaveMarkIcon(
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
        VPicName := AAppearanceIcon.Pic.GetName;
        VTargetPath := 'files' + PathDelim;
        Result := VTargetPath + VPicName;
        if Assigned(FZip)  then begin
          FZip.AddFile(VData, Result, Now);
        end else begin
          VTargetPath := ExtractFilePath(FFileName) + VTargetPath;
          VTargetFullName := VTargetPath + VPicName;
          CreateDir(VTargetPath);
          VStream.SaveToFile(VTargetFullName);
        end;
      finally
        VStream.Free;
      end;
    end;
  end;
end;

end.
