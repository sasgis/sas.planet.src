unit u_ExportMarks2KML;

interface

uses
  Forms,
  Classes,
  SysUtils,
  Windows,
  GR32,
  xmldom,
  XMLIntf,
  msxmldom,
  XMLDoc,
  KaZip,
  u_MarksSimple,
  u_GlobalState,
  u_GeoToStr,
  u_BitmapTileVampyreSaver,
  i_BitmapTileSaveLoad,
  i_IBitmapTypeExtManager,
  u_BitmapTypeExtManagerSimple;

type
  TExportMarks2KML = class
    kmldoc:TXMLDocument;
    filename:string;
    inKMZ:boolean;
    doc:iXMLNode;
    Zip: TKaZip;
    OnlyVisible:boolean;
    function ExportToKML(AFileName:string):boolean;
    function ExportMarkToKML(Mark:TMarkFull;AFileName:string):boolean;
    function ExportCategoryToKML(CategoryID:TCategoryID;AFileName:string):boolean;
    procedure AddMark(Mark:TMarkFull;inNode:iXMLNode);
    procedure SaveMarkIcon(Mark:TMarkFull);
    procedure AddFolders(ACategory:TStrings);
    procedure AddMarks(CategoryID:TCategoryId; inNode:iXMLNode);
    constructor Create(AOnlyVisible:boolean);
    function Color32toKMLColor(Color32:TColor32):string;
    destructor Destroy; override;
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

function TExportMarks2KML.ExportToKML(AFileName:string):boolean;
var Category:TStringList;
    KMLStream:TMemoryStream;
begin
  filename:=Afilename;
  inKMZ:=ExtractFileExt(filename)='.kmz';
  Category:=TStringList.create;
  GState.MarksDb.Kategory2StringsWithObjects(Category);

  if inKMZ then begin
    Zip.FileName := filename;
    Zip.CreateZip(filename);
    Zip.CompressionType := ctFast;
    Zip.Active := true;
    AddFolders(Category);
    KMLStream:=TMemoryStream.Create;
    kmldoc.SaveToStream(KMLStream);
    KMLStream.Position:=0;
    Zip.AddStream('doc.kml',KMLStream);
    KMLStream.Free;
  end else begin
    AddFolders(Category);
    kmldoc.SaveToFile(FileName);
  end;
  Category.Free;
end;

function TExportMarks2KML.ExportCategoryToKML(CategoryID:TCategoryID;AFileName:string):boolean;
var Category:TStringList;
    KMLStream:TMemoryStream;
begin
  filename:=Afilename;
  inKMZ:=ExtractFileExt(filename)='.kmz';
  Category:=TStringList.create;
  Category.AddObject(CategoryID.name,CategoryID);

  if inKMZ then begin
    Zip.FileName := filename;
    Zip.CreateZip(filename);
    Zip.CompressionType := ctFast;
    Zip.Active := true;
    AddFolders(Category);
    KMLStream:=TMemoryStream.Create;
    kmldoc.SaveToStream(KMLStream);
    KMLStream.Position:=0;
    Zip.AddStream('doc.kml',KMLStream);
    KMLStream.Free;
  end else begin
    AddFolders(Category);
    kmldoc.SaveToFile(FileName);
  end;
  Category.Free;
end;

function TExportMarks2KML.ExportMarkToKML(Mark:TMarkFull;AFileName:string):boolean;
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
    kmldoc.SaveToStream(KMLStream);
    KMLStream.Position:=0;
    Zip.AddStream('doc.kml',KMLStream);
    KMLStream.Free;
  end else begin
    AddMark(Mark,doc);
    kmldoc.SaveToFile(FileName);
  end;
end;

procedure TExportMarks2KML.AddFolders(ACategory:TStrings);

  procedure AddItem(Lev: Integer; ParentNode: IXMLNode; S: string; Data:TObject);
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
  begin
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
    if (TXMLNode(aNode) = nil)and((TCategoryId(Data).visible)or(not OnlyVisible)) then begin
      aNode := ParentNode.AddChild('Folder');
      aNode.ChildValues['name']:=prefix;
      aNode.ChildValues['open']:=1;
      with aNode.AddChild('Style').AddChild('ListStyle') do begin
        ChildValues['listItemType']:='check';
        ChildValues['bgColor']:='00ffffff';
      end;
      if ID=0 then begin
        AddMarks(TCategoryId(Data),aNode);
      end;
    end;
    AddItem(Lev + 1, aNode, Copy(S, ID + 1, Length(S)),Data);
  end;

var
  K: Integer;
begin
  for K := 0 to ACategory.Count - 1 do begin
    AddItem(0, doc, ACategory[K], ACategory.Objects[K]);
  end;
end;

procedure TExportMarks2KML.SaveMarkIcon(Mark:TMarkFull);
begin
  if inKMZ then begin
    Zip.AddFile(GState.ProgramPath+'marksicons'+PathDelim+Mark.PicName,'files\'+Mark.PicName);
  end else begin
    CreateDir(ExtractFilePath(filename)+'files\');
    CopyFile(PChar(GState.ProgramPath+'marksicons'+PathDelim+Mark.PicName),PChar(ExtractFilePath(filename)+'files\'+Mark.PicName),false);
  end;
end;

procedure TExportMarks2KML.AddMarks(CategoryID:TCategoryId; inNode:iXMLNode);
var MarksList:TStringList;
    Mark:TMarkFull;
    i,j:integer;
    currNode:IXMLNode;
    coordinates:string;
begin
  MarksList:=TStringList.Create;
  GState.MarksDb.Marsk2StringsWithMarkId(CategoryID, MarksList);
  for I := 0 to MarksList.Count - 1 do begin
    Mark:=GState.MarksDb.GetMarkByID(TMarkId(MarksList.Objects[i]).id);
    if (Mark.visible)or(not OnlyVisible) then begin
      AddMark(Mark,inNode);
    end;
    FreeAndNil(Mark);
  end;
end;

procedure TExportMarks2KML.AddMark(Mark:TMarkFull;inNode:iXMLNode);
var MarksList:TStringList;
    i,j:integer;
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
          j:=GState.MarkIcons.IndexOf(Mark.PicName);
          if j>=0 then begin
            with AddChild('IconStyle') do begin
              SaveMarkIcon(Mark);
              ChildValues['scale']:=R2StrPoint(Mark.Scale2/TCustomBitmap32(GState.MarkIcons.Objects[j]).Width);
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
  result:=IntToHex(255-AlphaComponent(Color32),2)+
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
end;

end.
