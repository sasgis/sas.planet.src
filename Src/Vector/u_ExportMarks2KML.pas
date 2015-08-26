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

unit u_ExportMarks2KML;

interface

uses
  Classes,
  SysUtils,
  Windows,
  ALXmlDoc,
  ActiveX,
  t_Bitmap32,
  i_ArchiveReadWrite,
  i_ArchiveReadWriteFactory,
  i_GeometryLonLat,
  i_Appearance,
  i_AppearanceOfVectorItem,
  i_VectorDataItemSimple,
  i_VectorItemSubset,
  i_VectorItemTree;

type
  TExportMarks2KML = class
  private
    FKmlDoc: TALXMLDocument;
    FFileName: string;
    FKmlNode: TALXMLNode;
    FKmlDocumentNode: TALXMLNode;
    FArchiveReadWriteFactory: IArchiveReadWriteFactory;
    FZip: IArchiveWriter;
    function AddTree(
      const AParentNode: TALXMLNode;
      const ATree: IVectorItemTree
    ): boolean;
    function AddMarks(
      const AMarksSubset: IVectorItemSubset;
      const inNode: TALXMLNode
    ): Boolean;
    procedure AddMark(
      const AMark: IVectorDataItem;
      const inNode: TALXMLNode
    );
    procedure AddPointAppearence(
      const AAppearence: IAppearance;
      const inNode: TALXMLNode
    );
    procedure AddLineAppearence(
      const AAppearance: IAppearance;
      const inNode: TALXMLNode
    );
    procedure AddPolygonAppearence(
      const AAppearance: IAppearance;
      const inNode: TALXMLNode
    );
    procedure AddPoint(
      const AGeometry: IGeometryLonLatPoint;
      const inNode: TALXMLNode
    );
    procedure AddSingleLine(
      const AGeometry: IGeometryLonLatSingleLine;
      const inNode: TALXMLNode
    );
    procedure AddMultiLine(
      const AGeometry: IGeometryLonLatMultiLine;
      const inNode: TALXMLNode
    );
    procedure AddLine(
      const AGeometry: IGeometryLonLatLine;
      const inNode: TALXMLNode
    );
    procedure AddContour(
      const AOuter: Boolean;
      const AGeometry: IGeometryLonLatContour;
      const inNode: TALXMLNode
    );
    procedure AddSinglePolygon(
      const AGeometry: IGeometryLonLatSinglePolygon;
      const inNode: TALXMLNode
    );
    procedure AddMultiPolygon(
      const AGeometry: IGeometryLonLatMultiPolygon;
      const inNode: TALXMLNode
    );
    procedure AddPolygon(
      const AGeometry: IGeometryLonLatPolygon;
      const inNode: TALXMLNode
    );
    function SaveMarkIcon(const AAppearanceIcon: IAppearancePointIcon): string;
    function Color32toKMLColor(Color32: TColor32): AnsiString;
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
  ALString,
  t_GeoTypes,
  i_BinaryData,
  i_EnumDoublePoint,
  u_BinaryDataByMemStream,
  u_GeoToStrFunc,
  u_StreamReadOnlyByBinaryData;

function XMLTextPrepare(const Src: string): string;
var
  i, l: integer;
  Buf, P: PChar;
  ch: Integer;
begin
  Result := '';
  L := Length(src);
  if L = 0 then begin
    exit;
  end;
  GetMem(Buf, L * SizeOf(src[1]));
  try
    P := Buf;
    for i := 1 to L do begin
      ch := Ord(src[i]);
      if (ch >= 32) or (ch = $09) or (ch = $0A) or (ch = $0D) then begin
        P^ := Char(ch);
        Inc(P);
      end;
    end;
    SetString(Result, Buf, P - Buf);
  finally
    FreeMem(Buf);
  end;
end;
function PointToKml(const APoint: TDoublePoint): AnsiString; inline;
begin
  Result := R2AnsiStrPoint(APoint.X) + ',' + R2AnsiStrPoint(APoint.Y) + ',0 ';
end;

function GetKMLCoordinates(const APointEnum: IEnumLonLatPoint): AnsiString;
var
  VPoint: TDoublePoint;
begin
  Result := '';
  while APointEnum.Next(VPoint) do begin
    Result := Result + PointToKml(VPoint);
  end;
end;

{ TExportMarks2KML }

constructor TExportMarks2KML.Create(
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory
);
begin
  inherited Create;
  FArchiveReadWriteFactory := AArchiveReadWriteFactory;
end;

procedure TExportMarks2KML.PrepareExportToFile(const AFileName: string);
begin
  FKmlDoc.Options := [doNodeAutoIndent, doNodeAutoCreate];
  FKmlDoc.Active := True;
  FKmlDoc.Version := '1.0';
  FKmlDoc.Encoding := 'UTF-8';
  FKmlNode := FKmlDoc.AddChild('kml');
  FKmlNode.Attributes['xmlns'] := 'http://earth.google.com/kml/2.2';
  FKmlDocumentNode := FKmlNode.AddChild('Document');
  FFileName := AFileName;
  if SameText(ExtractFileExt(FFileName), '.kmz') then begin
    FZip := FArchiveReadWriteFactory.Zip.WriterFactory.BuildByFileName(FFileName);
  end else begin
    FZip := nil;
  end;
end;

procedure TExportMarks2KML.SaveToFile;
var
  VFileStream: TFileStream;
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
    VFileStream := TFileStream.Create(FFileName, fmCreate);
    try
      FKmlDoc.SaveToStream(VFileStream);
    finally
      VFileStream.Free;
    end;
  end;
end;

procedure TExportMarks2KML.ExportTreeToKML(
  const ATree: IVectorItemTree;
  const AFileName: string
);
begin
  FKmlDoc := TALXMLDocument.Create;
  try
    PrepareExportToFile(AFileName);
    AddTree(FKmlDocumentNode, ATree);
    SaveToFile;
  finally
    FKmlDoc.Free;
  end;
end;

function TExportMarks2KML.AddMarks(
  const AMarksSubset: IVectorItemSubset;
  const inNode: TALXMLNode
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
      AddMark(VMark, inNode);
      Result := True;
    end;
  end;
end;

function TExportMarks2KML.AddTree(
  const AParentNode: TALXMLNode;
  const ATree: IVectorItemTree
): boolean;
var
  i: Integer;
  VNode: TALXMLNode;
  VSubTree: IVectorItemTree;
begin
  Result := False;
  if not Assigned(ATree) then begin
    Exit;
  end;
  for i := 0 to ATree.SubTreeItemCount - 1 do begin
    VSubTree := ATree.GetSubTreeItem(i);
    VNode := AParentNode.AddChild('Folder');
    VNode.ChildNodes['name'].Text := UTF8Encode(XMLTextPrepare(VSubTree.Name));
    VNode.ChildNodes['open'].Text := '1';
    with VNode.AddChild('Style').AddChild('ListStyle') do begin
      ChildNodes['listItemType'].Text := 'check';
      ChildNodes['bgColor'].Text := '00ffffff';
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

procedure TExportMarks2KML.AddPoint(
  const AGeometry: IGeometryLonLatPoint;
  const inNode: TALXMLNode
);
var
  currNode: TALXMLNode;
  VCoordinates: AnsiString;
begin
  currNode := inNode.AddChild('Point');
  currNode.ChildNodes['extrude'].Text := '1';
  VCoordinates := PointToKml(AGeometry.Point);
  currNode.ChildNodes['coordinates'].Text := VCoordinates;
end;

procedure TExportMarks2KML.AddSingleLine(
  const AGeometry: IGeometryLonLatSingleLine;
  const inNode: TALXMLNode
);
var
  currNode: TALXMLNode;
  VCoordinates: AnsiString;
begin
  currNode := inNode.AddChild('LineString');
  currNode.ChildNodes['extrude'].Text := '1';
  if AGeometry.Count > 1 then begin
    VCoordinates := GetKMLCoordinates(AGeometry.GetEnum);
  end else begin
    VCoordinates := PointToKml(AGeometry.Points[0]);
    VCoordinates := VCoordinates + VCoordinates;
  end;
  currNode.ChildNodes['coordinates'].Text := VCoordinates;
end;

procedure TExportMarks2KML.AddMultiLine(
  const AGeometry: IGeometryLonLatMultiLine;
  const inNode: TALXMLNode
);
var
  currNode: TALXMLNode;
  i: Integer;
begin
  if AGeometry.Count > 1 then begin
    currNode := inNode.AddChild('MultiGeometry');
    for i := 0 to AGeometry.Count - 1 do begin
      AddSingleLine(AGeometry.Item[i], currNode);
    end;
  end else begin
    AddSingleLine(AGeometry.Item[0], inNode);
  end;
end;

procedure TExportMarks2KML.AddLine(
  const AGeometry: IGeometryLonLatLine;
  const inNode: TALXMLNode
);
var
  VMultiLine: IGeometryLonLatMultiLine;
  VSingleLine: IGeometryLonLatSingleLine;
begin
  if Supports(AGeometry, IGeometryLonLatSingleLine, VSingleLine) then begin
    AddSingleLine(VSingleLine, inNode);
  end else if Supports(AGeometry, IGeometryLonLatMultiLine, VMultiLine) then begin
    AddMultiLine(VMultiLine, inNode);
  end else begin
    Assert(False);
  end;
end;

procedure TExportMarks2KML.AddContour(
  const AOuter: Boolean;
  const AGeometry: IGeometryLonLatContour;
  const inNode: TALXMLNode
);
var
  currNode: TALXMLNode;
  VCoordinates: AnsiString;
  VFirst: PDoublePoint;
  VSecond: PDoublePoint;
begin
  if AOuter then begin
    currNode := inNode.AddChild('outerBoundaryIs');
  end else begin
    currNode := inNode.AddChild('innerBoundaryIs');
  end;
  currNode := currNode.AddChild('LinearRing');
  if AGeometry.Count > 2 then begin
    VCoordinates := GetKMLCoordinates(AGeometry.GetEnum);
  end else if AGeometry.Count > 1 then begin
    VFirst := Addr(AGeometry.Points[0]);
    VSecond := VFirst;
    Inc(VSecond);
    VCoordinates :=
      PointToKml(VFirst^) +
      PointToKml(VSecond^) +
      PointToKml(VSecond^) +
      PointToKml(VFirst^);
  end else begin
    VCoordinates := PointToKml(AGeometry.Points[0]);
    VCoordinates := VCoordinates + VCoordinates + VCoordinates + VCoordinates;
  end;
  currNode.ChildNodes['coordinates'].Text := VCoordinates;
end;

procedure TExportMarks2KML.AddSinglePolygon(
  const AGeometry: IGeometryLonLatSinglePolygon;
  const inNode: TALXMLNode
);
var
  currNode: TALXMLNode;
  i: Integer;
begin
  currNode := inNode.AddChild('Polygon');
  currNode.ChildNodes['extrude'].Text := '1';
  AddContour(True, AGeometry.OuterBorder, currNode);
  for i := 0 to AGeometry.HoleCount - 1 do begin
    AddContour(False, AGeometry.HoleBorder[i], currNode);
  end;
end;

procedure TExportMarks2KML.AddMultiPolygon(
  const AGeometry: IGeometryLonLatMultiPolygon;
  const inNode: TALXMLNode
);
var
  currNode: TALXMLNode;
  i: Integer;
begin
  if AGeometry.Count > 1 then begin
    currNode := inNode.AddChild('MultiGeometry');
    for i := 0 to AGeometry.Count - 1 do begin
      AddSinglePolygon(AGeometry.Item[i], currNode);
    end;
  end else begin
    AddSinglePolygon(AGeometry.Item[0], inNode);
  end;
end;

procedure TExportMarks2KML.AddPolygon(
  const AGeometry: IGeometryLonLatPolygon;
  const inNode: TALXMLNode
);
var
  VMultiPolygon: IGeometryLonLatMultiPolygon;
  VSinglePolygon: IGeometryLonLatSinglePolygon;
begin
  if Supports(AGeometry, IGeometryLonLatSinglePolygon, VSinglePolygon) then begin
    AddSinglePolygon(VSinglePolygon, inNode);
  end else if Supports(AGeometry, IGeometryLonLatMultiPolygon, VMultiPolygon) then begin
    AddMultiPolygon(VMultiPolygon, inNode);
  end else begin
    Assert(False);
  end;
end;

procedure TExportMarks2KML.AddPointAppearence(
  const AAppearence: IAppearance;
  const inNode: TALXMLNode
);
var
  VAppearanceIcon: IAppearancePointIcon;
  VAppearanceCaption: IAppearancePointCaption;
  width: integer;
  VFileName: string;
begin
  if not Supports(AAppearence, IAppearancePointIcon, VAppearanceIcon) then begin
    VAppearanceIcon := nil;
  end;
  if not Supports(AAppearence, IAppearancePointCaption, VAppearanceCaption) then begin
    VAppearanceCaption := nil;
  end;
  if (VAppearanceCaption <> nil) or (VAppearanceIcon <> nil) then begin
    with inNode.AddChild('Style') do begin
      if VAppearanceCaption <> nil then begin
        with AddChild('LabelStyle') do begin
          ChildNodes['color'].Text := Color32toKMLColor(VAppearanceCaption.TextColor);
          ChildNodes['scale'].Text := R2AnsiStrPoint(VAppearanceCaption.FontSize / 14);
        end;
      end;
      if VAppearanceIcon <> nil then begin
        if VAppearanceIcon.Pic <> nil then begin
          with AddChild('IconStyle') do begin
            VFileName := SaveMarkIcon(VAppearanceIcon);
            width := VAppearanceIcon.Pic.GetMarker.Size.X;
            ChildNodes['scale'].Text := R2AnsiStrPoint(VAppearanceIcon.MarkerSize / width);
            with AddChild('Icon') do begin
              ChildNodes['href'].Text := UTF8Encode(VFileName);
            end;
            with AddChild('hotSpot') do begin
              Attributes['x'] := '0.5';
              Attributes['y'] := '0';
              Attributes['xunits'] := 'fraction';
              Attributes['yunits'] := 'fraction';
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TExportMarks2KML.AddLineAppearence(
  const AAppearance: IAppearance;
  const inNode: TALXMLNode
);
var
  VAppearanceLine: IAppearanceLine;
begin
  if Supports(AAppearance, IAppearanceLine, VAppearanceLine) then begin
    with inNode.AddChild('Style') do begin
      with AddChild('LineStyle') do begin
        ChildNodes['color'].Text := Color32toKMLColor(VAppearanceLine.LineColor);
        ChildNodes['width'].Text := R2AnsiStrPoint(VAppearanceLine.LineWidth);
      end;
    end;
  end;
end;

procedure TExportMarks2KML.AddPolygonAppearence(
  const AAppearance: IAppearance;
  const inNode: TALXMLNode
);
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
    with inNode.AddChild('Style') do begin
      if VAppearanceBorder <> nil then begin
        with AddChild('LineStyle') do begin
          ChildNodes['color'].Text := Color32toKMLColor(VAppearanceBorder.LineColor);
          ChildNodes['width'].Text := R2AnsiStrPoint(VAppearanceBorder.LineWidth);
        end;
      end;
      if VAppearanceFill <> nil then begin
        with AddChild('PolyStyle') do begin
          ChildNodes['color'].Text := Color32toKMLColor(VAppearanceFill.FillColor);
          ChildNodes['fill'].Text := '1';
        end;
      end;
    end;
  end;
end;

procedure TExportMarks2KML.AddMark(
  const AMark: IVectorDataItem;
  const inNode: TALXMLNode
);
var
  currNode: TALXMLNode;
  VLonLatPoint: IGeometryLonLatPoint;
  VLonLatPolygon: IGeometryLonLatPolygon;
  VLonLatLine: IGeometryLonLatLine;
begin
  currNode := inNode.AddChild('Placemark');
  currNode.ChildNodes['name'].Text := UTF8Encode(XMLTextPrepare(AMark.Name));
  currNode.ChildNodes['description'].Text := UTF8Encode(XMLTextPrepare(AMark.Desc));
  if Supports(AMark.Geometry, IGeometryLonLatPoint, VLonLatPoint) then begin
    // Placemark
    AddPointAppearence(AMark.Appearance, currNode);
    AddPoint(VLonLatPoint, currNode);
  end else if Supports(AMark.Geometry, IGeometryLonLatLine, VLonLatLine) then begin
    AddLineAppearence(AMark.Appearance, currNode);
    AddLine(VLonLatLine, currNode);
  end else if Supports(AMark.Geometry, IGeometryLonLatPolygon, VLonLatPolygon) then begin
    AddPolygonAppearence(AMark.Appearance, currNode);
    AddPolygon(VLonLatPolygon, currNode);
  end;
end;

function TExportMarks2KML.Color32toKMLColor(Color32: TColor32): AnsiString;
var
  VColor: TColor32Entry;
begin
  VColor.ARGB := Color32;
  Result :=
    AlIntToHex(VColor.A, 2) +
    AlIntToHex(VColor.B, 2) +
    AlIntToHex(VColor.G, 2) +
    AlIntToHex(VColor.R, 2);
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
        VPicName := ExtractFileName(AAppearanceIcon.Pic.GetName);
        VTargetPath := 'files' + PathDelim;
        Result := VTargetPath + VPicName;
        if Assigned(FZip) then begin
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
