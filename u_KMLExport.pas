unit u_KMLExport;

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
  TKMLExport = class
    kmldoc:TXMLDocument;
    filename:string;
    inKMZ:boolean;
    doc:iXMLNode;
    Zip: TKaZip;
    function ExportToKML(AFileName:string):boolean;
    procedure SaveMarkIcon(Mark:TMarkFull);
    procedure AddFolders(ACategory:TStrings);
    procedure AddMarks(CategoryID:TCategoryId; inNode:iXMLNode);
    constructor Create;
    destructor Destroy; override;
  end;

implementation

constructor TKMLExport.Create;
var child:iXMLNode;
begin
  kmldoc:=TXMLDocument.Create(Application);
  kmldoc.Options:=kmldoc.Options + [doNodeAutoIndent];
  kmldoc.Active:=true;
  kmldoc.Version:='1.0';
  kmldoc.Encoding:='UTF-8';
  child:=kmldoc.AddChild('kml');
  child.Attributes['xmlns']:='http://earth.google.com/kml/2.2';
  doc:=child.AddChild('Document');
end;

function TKMLExport.ExportToKML(AFileName:string):boolean;
var Category:TStringList;
    KMLStream:TMemoryStream;
begin
  filename:=Afilename;
  inKMZ:=ExtractFileExt(filename)='.kmz';
  Category:=TStringList.create;
  GState.MarksDb.Kategory2StringsWithObjects(Category);
  AddFolders(Category);
  KMLStream:=TMemoryStream.Create;
  kmldoc.SaveToStream(KMLStream);

  if inKMZ then begin
    Zip := TKaZip.Create(nil);
    Zip.FileName := filename;
    Zip.CreateZip(filename);
    Zip.CompressionType := ctFast;
    Zip.Active := true;
    Zip.AddStream('doc.kml',KMLStream);
    Zip.Active := false;
    Zip.Close;
    Zip.Free;
  end else begin
    kmldoc.SaveToFile(FileName);
  end;
end;

procedure TKMLExport.AddFolders(ACategory:TStrings);

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
    if TXMLNode(aNode) = nil then begin
      aNode := ParentNode.AddChild('Folder');
      aNode.ChildValues['name']:=prefix;
      aNode.ChildValues['open']:=1;
      with aNode.AddChild('Style').AddChild('ListStyle') do begin
        ChildValues['listItemType']:='check';
        ChildValues['bgColor']:='00ffffff';
      end;
      AddMarks(TCategoryId(Data),aNode);
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

procedure TKMLExport.SaveMarkIcon(Mark:TMarkFull);
begin
  if inKMZ then begin
    Zip.CreateFolder('files',now);
    Zip.AddFile(GState.ProgramPath+'marksicons'+PathDelim+Mark.PicName,'files\'+Mark.PicName);
  end else begin
    CreateDir(ExtractFilePath(filename)+'files\');
    CopyFile(PChar(GState.ProgramPath+'marksicons'+PathDelim+Mark.PicName),PChar(ExtractFilePath(filename)+'files\'+Mark.PicName),false);
  end;
end;

procedure TKMLExport.AddMarks(CategoryID:TCategoryId; inNode:iXMLNode);
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
    currNode:=inNode.AddChild('Placemark');
    currNode.ChildValues['name']:=Mark.name;
    currNode.ChildValues['description']:=Mark.Desc;
    if Mark.IsLine then begin
      with currNode.AddChild('Style') do begin
        with AddChild('LineStyle') do begin
          ChildValues['color']:=IntToHex(255-AlphaComponent(Mark.Color1),2)+
                                IntToHex(BlueComponent(Mark.Color1),2)+
                                IntToHex(GreenComponent(Mark.Color1),2)+
                                IntToHex(RedComponent(Mark.Color1),2);
          ChildValues['width']:=Mark.Scale1;
        end;
      end;
      currNode:=currNode.AddChild('LineString');
    end;

    if Mark.IsPoint then begin
      with currNode.AddChild('Style') do begin
        with AddChild('LabelStyle') do begin
          ChildValues['color']:=IntToHex(255-AlphaComponent(Mark.Color1),2)+
                                IntToHex(BlueComponent(Mark.Color1),2)+
                                IntToHex(GreenComponent(Mark.Color1),2)+
                                IntToHex(RedComponent(Mark.Color1),2);
          ChildValues['scale']:=Mark.Scale1/14;
        end;
        j:=GState.MarkIcons.IndexOf(Mark.PicName);
        if j>=0 then begin
          with AddChild('IconStyle') do begin
            SaveMarkIcon(Mark);
            ChildValues['scale']:=Mark.Scale2/TCustomBitmap32(GState.MarkIcons.Objects[j]).Width;
            with AddChild('Icon') do begin
              ChildValues['href']:='files\'+Mark.PicName;
            end;
            with AddChild('hotSpot') do begin
              Attributes['x']:=0.5;
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
          ChildValues['color']:=IntToHex(255-AlphaComponent(Mark.Color1),2)+
                                IntToHex(BlueComponent(Mark.Color1),2)+
                                IntToHex(GreenComponent(Mark.Color1),2)+
                                IntToHex(RedComponent(Mark.Color1),2);
          ChildValues['width']:=Mark.Scale1;
        end;
        with AddChild('PolyStyle') do begin
          ChildValues['color']:=IntToHex(255-AlphaComponent(Mark.Color2),2)+
                                IntToHex(BlueComponent(Mark.Color2),2)+
                                IntToHex(GreenComponent(Mark.Color2),2)+
                                IntToHex(RedComponent(Mark.Color2),2);
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

    FreeAndNil(Mark);
  end;
end;

destructor TKMLExport.Destroy;
begin
  kmldoc.Free;
end;

end.
