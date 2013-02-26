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
  Forms,
  Classes,
  SysUtils,
  Windows,
  GR32,
  XMLIntf,
  XMLDoc,
  ActiveX,
  i_ArchiveReadWrite,
  i_ArchiveReadWriteFactory,
  i_Category,
  i_VectorDataItemSimple,
  i_VectorItemSubset,
  u_GeoToStr;

type
  TExportMarks2KML = class
  private
    kmldoc: TXMLDocument;
    FFileName: string;
    inKMZ: boolean;
    doc: iXMLNode;
    FArchiveReadWriteFactory: IArchiveReadWriteFactory;
    FZip: IArchiveWriter;
    procedure AddFolders(
      const AMarksSet: IVectorItemSubset;
      const ACategoryList: IInterfaceList
    );
    function AddFolder(
      const AParentNode: IXMLNode;
      const ACategoryNamePostfix: string;
      const AMarksSubset: IVectorItemSubset
    ): boolean;
    function AddMarks(
      const AMarksSubset: IVectorItemSubset;
      const inNode: iXMLNode
    ): Boolean;
    procedure AddMark(
      const Mark: IVectorDataItemSimple;
      const inNode: iXMLNode
    );
    function SaveMarkIcon(const Mark: IVectorDataItemPointWithIconParams): string;
    function Color32toKMLColor(Color32: TColor32): string;
  public
    constructor Create(const AArchiveReadWriteFactory: IArchiveReadWriteFactory);
    destructor Destroy; override;
    procedure ExportToKML(
      const ACategoryList: IInterfaceList;
      const AMarksSubset: IVectorItemSubset;
      const AFileName: string
    );
    procedure ExportCategoryToKML(
      const ACategory: ICategory;
      const AMarksSubset: IVectorItemSubset;
      const AFileName: string
    );
    procedure ExportMarkToKML(
      const Mark: IVectorDataItemSimple;
      const AFileName: string
    );
  end;

implementation

uses
  t_GeoTypes,
  i_BinaryData,
  i_EnumDoublePoint,
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

constructor TExportMarks2KML.Create(
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory
);
var
  child: iXMLNode;
begin
  inherited Create;
  kmldoc := TXMLDocument.Create(Application);
  kmldoc.Options := kmldoc.Options + [doNodeAutoIndent];
  kmldoc.Active := true;
  kmldoc.Version := '1.0';
  kmldoc.Encoding := 'UTF-8';
  child := kmldoc.AddChild('kml');
  child.Attributes['xmlns'] := 'http://earth.google.com/kml/2.2';
  doc := child.AddChild('Document');
  FArchiveReadWriteFactory := AArchiveReadWriteFactory;
  FZip := nil;
end;

destructor TExportMarks2KML.Destroy;
begin
  kmldoc.Free;
  inherited;
end;

procedure TExportMarks2KML.ExportToKML(
  const ACategoryList: IInterfaceList;
  const AMarksSubset: IVectorItemSubset;
  const AFileName: string
);
var
  KMLStream: TMemoryStream;
  VData: IBinaryData;
begin
  FFileName := AFileName;
  inKMZ := ExtractFileExt(FFileName) = '.kmz';
  if inKMZ then begin
    FZip := FArchiveReadWriteFactory.CreateZipWriterByName(FFileName);
    AddFolders(AMarksSubset, ACategoryList);
    KMLStream := TMemoryStream.Create;
    try
      kmldoc.SaveToStream(KMLStream);
      KMLStream.Position := 0;
      VData := TBinaryDataByMemStream.CreateFromStream(KMLStream);
      FZip.AddFile(VData, 'doc.kml', Now);
    finally
      KMLStream.Free;
    end;
  end else begin
    AddFolders(AMarksSubset, ACategoryList);
    kmldoc.SaveToFile(FFileName);
  end;
end;

procedure TExportMarks2KML.ExportCategoryToKML(
  const ACategory: ICategory;
  const AMarksSubset: IVectorItemSubset;
  const AFileName: string
);
var
  KMLStream: TMemoryStream;
  VData: IBinaryData;
begin
  FFileName := AFileName;
  inKMZ := ExtractFileExt(FFileName) = '.kmz';
  if inKMZ then begin
    FZip := FArchiveReadWriteFactory.CreateZipWriterByName(FFileName);
    AddFolder(doc, ACategory.Name, AMarksSubset);
    KMLStream := TMemoryStream.Create;
    try
      kmldoc.SaveToStream(KMLStream);
      KMLStream.Position := 0;
      VData := TBinaryDataByMemStream.CreateFromStream(KMLStream);
      FZip.AddFile(VData, 'doc.kml', Now);
    finally
      KMLStream.Free;
    end;
  end else begin
    AddFolder(doc, ACategory.Name, AMarksSubset);
    kmldoc.SaveToFile(FFileName);
  end;
end;

procedure TExportMarks2KML.ExportMarkToKML(
  const Mark: IVectorDataItemSimple;
  const AFileName: string
);
var
  KMLStream: TMemoryStream;
  VData: IBinaryData;
begin
  FFileName := AFileName;
  inKMZ := ExtractFileExt(FFileName) = '.kmz';
  if inKMZ then begin
    FZip := FArchiveReadWriteFactory.CreateZipWriterByName(FFileName);
    AddMark(Mark, doc);
    KMLStream := TMemoryStream.Create;
    try
      kmldoc.SaveToStream(KMLStream);
      KMLStream.Position := 0;
      VData := TBinaryDataByMemStream.CreateFromStream(KMLStream);
      FZip.AddFile(VData, 'doc.kml', Now);
    finally
      KMLStream.Free;
    end;
  end else begin
    AddMark(Mark, doc);
    kmldoc.SaveToFile(FFileName);
  end;
end;

procedure TExportMarks2KML.AddFolders(
  const AMarksSet: IVectorItemSubset;
  const ACategoryList: IInterfaceList
);
var
  K: Integer;
  VCategory: ICategory;
  VMarksSubset: IVectorItemSubset;
begin
  for K := 0 to ACategoryList.Count - 1 do begin
    VCategory := ICategory(Pointer(ACategoryList.Items[K]));
    VMarksSubset := AMarksSet.GetSubsetByCategory(VCategory);
    AddFolder(doc, VCategory.Name, VMarksSubset);
  end;
end;

function TExportMarks2KML.AddFolder(
  const AParentNode: IXMLNode;
  const ACategoryNamePostfix: string;
  const AMarksSubset: IVectorItemSubset
): boolean;
  function FindNodeWithText(
    AParent: iXMLNode;
  const ACategoryNameElement: string
  ): IXMLNode;
  var
    i: Integer;
    tmpNode: IXMLNode;
  begin
    Result := nil;
    if AParent.HasChildNodes then begin
      for i := 0 to AParent.ChildNodes.Count - 1 do begin
        tmpNode := AParent.ChildNodes.Get(i);
        if (tmpNode.NodeName = 'Folder') and (tmpNode.ChildValues['name'] = ACategoryNameElement) then begin
          Result := tmpNode;
          break;
        end;
      end;
    end;
  end;

var
  VCatgoryNamePrefix: string;
  VCatgoryNamePostfix: string;
  VDelimiterPos: Integer;
  VNode: IXMLNode;
  VCreatedNode: Boolean;
begin
  if ACategoryNamePostfix = '' then begin
    Result := AddMarks(AMarksSubset, AParentNode);
  end else begin
    VDelimiterPos := Pos('\', ACategoryNamePostfix);
    if VDelimiterPos > 0 then begin
      VCatgoryNamePrefix := Copy(ACategoryNamePostfix, 1, VDelimiterPos - 1);
      VCatgoryNamePostfix := Copy(ACategoryNamePostfix, VDelimiterPos + 1, Length(ACategoryNamePostfix));
    end else begin
      VCatgoryNamePrefix := ACategoryNamePostfix;
      VCatgoryNamePostfix := '';
    end;
    VCreatedNode := False;
    if VCatgoryNamePrefix = '' then begin
      VNode := AParentNode;
    end else begin
      VNode := FindNodeWithText(AParentNode, VCatgoryNamePrefix);
      if (VNode = nil) then begin
        VNode := AParentNode.AddChild('Folder');
        VNode.ChildValues['name'] := VCatgoryNamePrefix;
        VNode.ChildValues['open'] := 1;
        with VNode.AddChild('Style').AddChild('ListStyle') do begin
          ChildValues['listItemType'] := 'check';
          ChildValues['bgColor'] := '00ffffff';
        end;
        VCreatedNode := True;
      end;
    end;
    Result := AddFolder(VNode, VCatgoryNamePostfix, AMarksSubset);
    if (not Result) and (VCreatedNode) then begin
      AParentNode.ChildNodes.Remove(VNode);
    end;
  end;
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
  VEnumMarks := AMarksSubset.GetEnum;
  while (VEnumMarks.Next(1, VMark, @i) = S_OK) do begin
    AddMark(VMark, inNode);
    Result := True;
  end;
end;

procedure TExportMarks2KML.AddMark(
  const Mark: IVectorDataItemSimple;
  const inNode: iXMLNode
);
var
  width: integer;
  currNode: IXMLNode;
  coordinates: string;
  VFileName: string;
  VMarkPoint: IVectorDataItemPoint;
  VMarkPointIconParams: IVectorDataItemPointWithIconParams;
  VMarkPointCaptionParams: IVectorDataItemPointWithCaptionParams;
  VMarkLine: IVectorDataItemLine;
  VMarkLineLineParams: IVectorDataItemWithLineParams;
  VMarkPoly: IVectorDataItemPoly;
  VMarkPolyFillParams: IVectorDataItemPolyWithFillParams;
  VEnum: IEnumLonLatPoint;
  VLonLat: TDoublePoint;
begin
  currNode := inNode.AddChild('Placemark');
  currNode.ChildValues['name'] := XMLTextPrepare(Mark.Name);
  currNode.ChildValues['description'] := XMLTextPrepare(Mark.Desc);
  if Supports(Mark, IVectorDataItemPoint, VMarkPoint) then begin
    if not Supports(Mark, IVectorDataItemPointWithIconParams, VMarkPointIconParams) then begin
      VMarkPointIconParams := nil;
    end;
    if not Supports(Mark, IVectorDataItemPointWithCaptionParams, VMarkPointCaptionParams) then begin
      VMarkPointCaptionParams := nil;
    end;
    if (VMarkPointCaptionParams <> nil) or (VMarkPointIconParams <> nil)  then begin
      with currNode.AddChild('Style') do begin
        if VMarkPointCaptionParams <> nil then begin
          with AddChild('LabelStyle') do begin
            ChildValues['color'] := Color32toKMLColor(VMarkPointCaptionParams.TextColor);
            ChildValues['scale'] := R2StrPoint(VMarkPointCaptionParams.FontSize / 14);
          end;
        end;
        if VMarkPointIconParams <> nil then begin
          if VMarkPointIconParams.Pic <> nil then begin
            with AddChild('IconStyle') do begin
              VFileName := SaveMarkIcon(VMarkPointIconParams);
              width := VMarkPointIconParams.Pic.GetMarker.Size.X;
              ChildValues['scale'] := R2StrPoint(VMarkPointIconParams.MarkerSize / width);
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
    coordinates := coordinates + R2StrPoint(VMarkPoint.Point.X) + ',' + R2StrPoint(VMarkPoint.Point.Y) + ',0 ';
    currNode.ChildValues['coordinates'] := coordinates;
  end else if Supports(Mark, IVectorDataItemLine, VMarkLine) then begin
    if Supports(Mark, IVectorDataItemWithLineParams, VMarkLineLineParams) then begin
      with currNode.AddChild('Style') do begin
        with AddChild('LineStyle') do begin
          ChildValues['color'] := Color32toKMLColor(VMarkLineLineParams.LineColor);
          ChildValues['width'] := R2StrPoint(VMarkLineLineParams.LineWidth);
        end;
      end;
    end;
    currNode := currNode.AddChild('LineString');
    currNode.ChildValues['extrude'] := 1;
    coordinates := '';
    VEnum := VMarkLine.Line.GetEnum;
    while VEnum.Next(VLonLat) do begin
      coordinates := coordinates + R2StrPoint(VLonLat.X) + ',' + R2StrPoint(VLonLat.Y) + ',0 ';
    end;
    currNode.ChildValues['coordinates'] := coordinates;
  end else if Supports(Mark, IVectorDataItemPoly, VMarkPoly) then begin
    if not Supports(Mark, IVectorDataItemWithLineParams, VMarkLineLineParams) then begin
      VMarkLineLineParams := nil;
    end;
    if not Supports(Mark, IVectorDataItemPolyWithFillParams, VMarkPolyFillParams) then begin
      VMarkPolyFillParams := nil;
    end;
    if (VMarkLineLineParams <> nil) or (VMarkPolyFillParams <> nil) then begin
      with currNode.AddChild('Style') do begin
        if VMarkLineLineParams <> nil then begin
          with AddChild('LineStyle') do begin
            ChildValues['color'] := Color32toKMLColor(VMarkLineLineParams.LineColor);
            ChildValues['width'] := R2StrPoint(VMarkLineLineParams.LineWidth);
          end;
        end;
        if VMarkPolyFillParams <> nil then begin
          with AddChild('PolyStyle') do begin
            ChildValues['color'] := Color32toKMLColor(VMarkPolyFillParams.FillColor);
            ChildValues['fill'] := 1;
          end;
        end;
      end;
    end;
    currNode := currNode.AddChild('Polygon').AddChild('outerBoundaryIs').AddChild('LinearRing');
    currNode.ChildValues['extrude'] := 1;
    coordinates := '';
    VEnum := VMarkPoly.Line.GetEnum;
    while VEnum.Next(VLonLat) do begin
      coordinates := coordinates + R2StrPoint(VLonLat.X) + ',' + R2StrPoint(VLonLat.Y) + ',0 ';
    end;
    currNode.ChildValues['coordinates'] := coordinates;
  end;
end;

function TExportMarks2KML.Color32toKMLColor(Color32: TColor32): string;
begin
  result := IntToHex(AlphaComponent(Color32), 2) +
    IntToHex(BlueComponent(Color32), 2) +
    IntToHex(GreenComponent(Color32), 2) +
    IntToHex(RedComponent(Color32), 2);
end;

function TExportMarks2KML.SaveMarkIcon(const Mark: IVectorDataItemPointWithIconParams): string;
var
  VTargetPath: string;
  VTargetFullName: string;
  VPicName: string;
  VStream: TCustomMemoryStream;
  VData: IBinaryData;
begin
  Result := '';
  if Mark.Pic <> nil then begin
    VData := Mark.Pic.Source;
    if VData <> nil then begin
      VStream := TStreamReadOnlyByBinaryData.Create(VData);
      try
        VPicName := Mark.Pic.GetName;
        VTargetPath := 'files' + PathDelim;
        Result := VTargetPath + VPicName;
        if inKMZ then begin
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
