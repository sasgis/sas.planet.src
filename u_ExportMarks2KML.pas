unit u_ExportMarks2KML;

interface

uses
  Forms,
  Classes,
  SysUtils,
  Windows,
  GR32,
  XMLIntf,
  msxmldom,
  XMLDoc,
  KaZip,
  ActiveX,
  u_MarksSimple,
  i_MarksSimple,
  u_GlobalState,
  u_GeoToStr,
  Ugeofun;

type
  TExportMarks2KML = class
  private
    kmldoc:TXMLDocument;
    filename:string;
    inKMZ:boolean;
    doc:iXMLNode;
    Zip: TKaZip;
    OnlyVisible:boolean;
    procedure AddMark(Mark:iMarkFull;inNode:iXMLNode);
    procedure SaveMarkIcon(Mark:IMarkFull);
    procedure AddFolders(ACategory:TList);
    procedure AddMarks(CategoryIDList:TList; inNode:iXMLNode);
    function Color32toKMLColor(Color32:TColor32):string;
  public
    constructor Create(AOnlyVisible:boolean);
    destructor Destroy; override;

    procedure ExportToKML(AFileName:string);
    procedure ExportCategoryToKML(CategoryID:TCategoryID;AFileName:string);
    procedure ExportMarkToKML(Mark:iMarkFull;AFileName:string);
  end;

implementation

constructor TExportMarks2KML.Create(AOnlyVisible:boolean);
var child:iXMLNode;
begin
  OnlyVisible:=AOnlyVisible;
  kmldoc:=TXMLDocument.Create(Application);
  kmldoc.Options:=kmldoc.Options + [doNodeAutoIndent];
  kmldoc.Active:=true;
  kmldoc.Version:='1.0';
  kmldoc.Encoding:='UTF-8';
  child:=kmldoc.AddChild('kml');
  child.Attributes['xmlns']:='http://earth.google.com/kml/2.2';
  doc:=child.AddChild('Document');
  Zip := TKaZip.Create(nil);
end;

procedure TExportMarks2KML.ExportToKML(AFileName:string);
var Category:TList;
    KMLStream:TMemoryStream;
begin
  filename:=Afilename;
  inKMZ:=ExtractFileExt(filename)='.kmz';
  Category:=GState.MarksDb.CategoryDB.GetCategoriesList;
  try
    if inKMZ then begin
      Zip.FileName := filename;
      Zip.CreateZip(filename);
      Zip.CompressionType := ctFast;
      Zip.Active := true;
      AddFolders(Category);
      KMLStream:=TMemoryStream.Create;
      try
        kmldoc.SaveToStream(KMLStream);
        KMLStream.Position:=0;
        Zip.AddStream('doc.kml',KMLStream);
      finally
        KMLStream.Free;
      end;
    end else begin
      AddFolders(Category);
      kmldoc.SaveToFile(FileName);
    end;
  finally
    Category.Free;
  end;
end;

procedure TExportMarks2KML.ExportCategoryToKML(CategoryID:TCategoryID;AFileName:string);
var Category:TList;
    KMLStream:TMemoryStream;
begin
  filename:=Afilename;
  inKMZ:=ExtractFileExt(filename)='.kmz';
  Category:=TList.create;
  try
    Category.Add(CategoryID);
    if inKMZ then begin
      Zip.FileName := filename;
      Zip.CreateZip(filename);
      Zip.CompressionType := ctFast;
      Zip.Active := true;
      AddFolders(Category);
      KMLStream:=TMemoryStream.Create;
      try
        kmldoc.SaveToStream(KMLStream);
        KMLStream.Position:=0;
        Zip.AddStream('doc.kml',KMLStream);
      finally
        KMLStream.Free;
      end;
    end else begin
      AddFolders(Category);
      kmldoc.SaveToFile(FileName);
    end;
  finally
    Category.Free;
  end;
end;

procedure TExportMarks2KML.ExportMarkToKML(Mark:iMarkFull;AFileName:string);
var KMLStream:TMemoryStream;
begin
  filename:=Afilename;
  inKMZ:=ExtractFileExt(filename)='.kmz';
  if inKMZ then begin
    Zip.FileName := filename;
    Zip.CreateZip(filename);
    Zip.CompressionType := ctFast;
    Zip.Active := true;
    AddMark(Mark,doc);
    KMLStream:=TMemoryStream.Create;
    try
      kmldoc.SaveToStream(KMLStream);
      KMLStream.Position:=0;
      Zip.AddStream('doc.kml',KMLStream);
    finally
      KMLStream.Free;
    end;
  end else begin
    AddMark(Mark,doc);
    kmldoc.SaveToFile(FileName);
  end;
end;

procedure TExportMarks2KML.AddFolders(ACategory:TList);

  function AddItem(Lev: Integer; ParentNode: IXMLNode; S: string; Data:TObject):boolean;
    function FindNodeWithText(AParent: iXMLNode; const S: string): IXMLNode;
    var
      i: Integer;
      tmpNode: IXMLNode;
    begin
      if AParent.HasChildNodes then begin
        for i:=0 to AParent.ChildNodes.Count-1 do begin
          tmpNode :=AParent.ChildNodes.Get(i);
          if (tmpNode.NodeName='Folder')and(tmpNode.ChildValues['name'] = S) then begin
            Result := tmpNode;
            break;
          end;
        end;
      end;
    end;

  var
    prefix: string;
    ID: Integer;
    aNode: IXMLNode;
    CatIdList:TList;
  begin
    Result := False;
    if S='' then begin
      Exit;
    end;
    ID:=Pos('\', S);
    prefix:='';
    if ID > 0 then begin
      prefix:=Copy(S, 1, ID - 1)
    end else begin
      prefix:=S;
      S := '';
    end;
    aNode := FindNodeWithText(ParentNode, prefix);

    if (TXMLNode(aNode) = nil) then begin
    aNode := ParentNode.AddChild('Folder');
    if ((TCategoryId(Data).visible)or(not OnlyVisible)) then begin
      aNode.ChildValues['name']:=prefix;
      aNode.ChildValues['open']:=1;
      with aNode.AddChild('Style').AddChild('ListStyle') do begin
        ChildValues['listItemType']:='check';
        ChildValues['bgColor']:='00ffffff';
      end;
      if ID=0 then begin
        CatIdList:=TList.Create;
        CatIdList.Add(Pointer(TCategoryId(Data).id));
        AddMarks(CatIdList,aNode);
        CatIdList.Free;
      end;
      result:=true;
    end else begin
      Result:=false;
    end;
    end;
    if (not AddItem(Lev + 1, aNode, Copy(S, ID + 1, Length(S)),Data))and(not Result) then begin
      ParentNode.ChildNodes.Remove(aNode);
    end;
  end;

var
  K: Integer;
begin
  for K := 0 to ACategory.Count - 1 do begin
    AddItem(0, doc, TCategoryId(ACategory.Items[K]).name, ACategory.Items[K]);
  end;
end;

procedure TExportMarks2KML.SaveMarkIcon(Mark:iMarkFull);
var
  VSourceFileName: string;
  VTargetPath: string;
  VTargetFullName: string;
begin
  VSourceFileName := GState.ProgramPath + 'marksicons' + PathDelim + Mark.PicName;
  VTargetPath := 'files' + PathDelim;
  if inKMZ then begin
    VTargetFullName := VTargetPath + Mark.PicName;
    Zip.AddFile(VSourceFileName, VTargetFullName);
  end else begin
    VTargetPath := ExtractFilePath(filename) + VTargetPath;
    VTargetFullName := VTargetPath + Mark.PicName;
    CreateDir(VTargetPath);
    CopyFile(PChar(VSourceFileName),PChar(VTargetFullName),false);
  end;
end;

procedure TExportMarks2KML.AddMarks(CategoryIDList:TList; inNode:iXMLNode);
var MarksList:IMarksSubset;
    Mark:iMarkFull;
    VEnumMarks:IEnumUnknown;
    i:integer;
begin
  MarksList:=GState.MarksDb.MarksDb.GetMarksSubset(DoubleRect(-180,90,180,-90),CategoryIDList, (not OnlyVisible));
  VEnumMarks := MarksList.GetEnum;
  while (VEnumMarks.Next(1, Mark, @i) = S_OK) do begin
    AddMark(Mark,inNode);
  end;
end;

procedure TExportMarks2KML.AddMark(Mark:iMarkFull;inNode:iXMLNode);
var
  j,width:integer;
  currNode:IXMLNode;
  coordinates:string;
begin
      currNode:=inNode.AddChild('Placemark');
      currNode.ChildValues['name']:=Mark.name;
      currNode.ChildValues['description']:=Mark.Desc;
      if Mark.IsLine then begin
        with currNode.AddChild('Style') do begin
          with AddChild('LineStyle') do begin
            ChildValues['color']:=Color32toKMLColor(Mark.Color1);
            ChildValues['width']:=R2StrPoint(Mark.Scale1);
          end;
        end;
        currNode:=currNode.AddChild('LineString');
      end;

      if Mark.IsPoint then begin
        with currNode.AddChild('Style') do begin
          with AddChild('LabelStyle') do begin
            ChildValues['color']:=Color32toKMLColor(Mark.Color1);
            ChildValues['scale']:=R2StrPoint(Mark.Scale1/14);
          end;
          if Mark.Pic <> nil then begin
            with AddChild('IconStyle') do begin
              SaveMarkIcon(Mark);
              width:=Mark.Pic.GetBitmapSize.X;
              ChildValues['scale']:=R2StrPoint(Mark.Scale2/width);
              with AddChild('Icon') do begin
                ChildValues['href']:='files\'+Mark.PicName;
              end;
              with AddChild('hotSpot') do begin
                Attributes['x']:='0.5';
                Attributes['y']:=0;
                Attributes['xunits']:='fraction';
                Attributes['yunits']:='fraction';
              end;
            end;
          end;
        end;
        currNode:=currNode.AddChild('Point');
      end;

      if Mark.isPoly then begin
        with currNode.AddChild('Style') do begin
          with AddChild('LineStyle') do begin
            ChildValues['color']:=Color32toKMLColor(Mark.Color1);
            ChildValues['width']:=R2StrPoint(Mark.Scale1);
          end;
          with AddChild('PolyStyle') do begin
            ChildValues['color']:=Color32toKMLColor(Mark.Color2);
            ChildValues['fill']:=1;
          end;
        end;
        currNode:=currNode.AddChild('Polygon').AddChild('outerBoundaryIs').AddChild('LinearRing');
      end;

      currNode.ChildValues['extrude']:=1;
      coordinates:='';
      for j := 0 to length(Mark.Points) - 1 do begin
        coordinates:=coordinates+R2StrPoint(Mark.Points[j].X)+','+R2StrPoint(Mark.Points[j].Y)+',0 ';
      end;
      currNode.ChildValues['coordinates']:=coordinates;
end;

function TExportMarks2KML.Color32toKMLColor(Color32:TColor32):string;
begin
  result:=IntToHex(AlphaComponent(Color32),2)+
          IntToHex(BlueComponent(Color32),2)+
          IntToHex(GreenComponent(Color32),2)+
          IntToHex(RedComponent(Color32),2);
end;

destructor TExportMarks2KML.Destroy;
begin
  Zip.Active := false;
  Zip.Close;
  Zip.Free;
  kmldoc.Free;
  inherited;
end;

end.

