unit Unit2;
interface
uses
 Windows,SysUtils,Forms,Dialogs,Classes,Ugeofun, StdCtrls, Controls, rxToolEdit, rxCurrEdit,
 DB, UResStrings,DBGrids, Mask, Grids, DBCtrls;

type
  THDBGrid = class(TCustomGrid)
  public
    property RowHeights;
    property DefaultRowHeight;
  end;


  TForm2 = class(TForm)
    RB1: TRadioButton;
    GroupBox2: TGroupBox;
    RB3: TRadioButton;
    Button3: TButton;
    Button2: TButton;
    BDel: TButton;
    Label9: TLabel;
    BGo: TButton;
    GroupBox3: TGroupBox;
    RB2: TRadioButton;
    EditGF: TEdit;
    GroupBox1: TGroupBox;
    Label21: TLabel;
    Label22: TLabel;
    lat_ns: TComboBox;
    Lon_we: TComboBox;
    lat2: TCurrencyEdit;
    lat3: TCurrencyEdit;
    lon1: TCurrencyEdit;
    lon2: TCurrencyEdit;
    lon3: TCurrencyEdit;
    Lat1: TCurrencyEdit;
    CBzoom: TComboBox;
    RB4: TRadioButton;
    DBGrid2: TDBGrid;
    ComboBox1: TComboBox;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BDelClick(Sender: TObject);
    procedure BGoClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure lat_nsClick(Sender: TObject);
    procedure EditGFClick(Sender: TObject);
    procedure Lat1Click(Sender: TObject);
    procedure DBGrid2DrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure FormShow(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox1Enter(Sender: TObject);
    procedure DBGrid2Enter(Sender: TObject);
  private
  public
//   function InsertIntoMarks(LonLatArr: TStream; name,descr,picname:string; category_id,color1,color2,scale1,scale2:integer; LatT,LatB,LonL,LonR:extended; visible:boolean): Int64;
//   procedure ReadFromMarks(var id:int64; var LonLatArr: TStream;var name,descr,picname:string;var category_id,color1,color2,scale1,scale2:integer;var LatT,LatB,LonL,LonR:extended;var visible:boolean);
  end;


var
  Form2: TForm2;
  j,ld:integer;
  lon_k,lat_k:real;
  procedure MouseOnMyReg(var PWL:TResObj;xy:TPoint);

implementation
uses unit1, UaddPoint, UaddLine, UaddPoly;
{$R *.dfm}

{procedure TForm2.ReadFromMarks(var id:int64; var LonLatArr: TStream;var name,descr,picname:string;var category_id,color1,color2,scale1,scale2:integer;var LatT,LatB,LonL,LonR:extended;var visible:boolean);
var
  Stmt: TDISQLite3Statement;
begin
  Stmt := MarksDB.Prepare('SELECT category_id, name, descr, picname, color1, color2, latB, latT, lonL, LonR, scale1, scale2,lonlatarr, visible FROM Marks WHERE RowID = ?');
  try
    Stmt.Bind_Int64(1, id);
    category_id:=Stmt.Column_Int(0);
    name:=Stmt.Column_Str(1);
    descr:=Stmt.Column_Str(2);
    picname:=Stmt.Column_Str(3);
    color1:=Stmt.Column_Int(4);
    color2:=Stmt.Column_Int(5);
    latB:=Stmt.Column_Double(6);
    latT:=Stmt.Column_Double(7);
    lonL:=Stmt.Column_Double(8);
    LonR:=Stmt.Column_Double(9);
    scale1:=Stmt.Column_Int(10);
    scale2:=Stmt.Column_Int(11);
    if Stmt.Step = SQLITE_ROW then
      LonLatArr.Write(Stmt.column_blob(12)^, Stmt.Column_Bytes(12));
    visible:=(Stmt.Column_Int(13)>=0);
  finally
    Stmt.Free;
  end;
end; }

{function TForm2.InsertIntoMarks(LonLatArr: TStream; name,descr,picname:string; category_id,color1,color2,scale1,scale2:integer; LatT,LatB,LonL,LonR:extended; visible:boolean): Int64;
var l: Integer;
    p: Pointer;
    Stmt: TDISQLite3Statement;
begin
  Stmt:=MarksDB.Prepare('INSERT INTO Marks (category_id, name, descr, picname, color1, color2, latB, latT, lonL, LonR, scale1, scale2,lonlatarr, visible) VALUES (?,"'+
                         inttostr(category_id)+'","'+name+'","'+descr+'","'+picname+'","'+inttostr(color1)+'","'+inttostr(color2)+'","'+floattostr(latB)+'","'+floattostr(latT)+'","'+floattostr(lonL)+'","'+floattostr(LonR)+'","'+
                         floattostr(scale1)+'","'+floattostr(scale2)+'","'+'?'+'","'+booltostr(visible)+'")');
  try
    if LonLatArr is TCustomMemoryStream
     then with LonLatArr as TCustomMemoryStream do
           Stmt.Bind_Blob(1, Memory, Size, SQLITE_STATIC)
     else begin
           l:=LonLatArr.Size;
           GetMem(p, l);
           LonLatArr.Seek(0, soFromBeginning);
           LonLatArr.Read(p^, l);
           Stmt.Bind_Blob(1, p, l, sqlite3_Destroy_Mem);
          end;
    Stmt.Step;
    Result := MarksDB.LastInsertRowID;
  finally
    Stmt.Free;
  end;
end;  }

function CursorOnLinie(X, Y, x1, y1, x2, y2, d: Integer): Boolean;
var sine,cosinus: Double;
    dx,dy,len: Integer;
begin
  asm
   fild(y2)
   fisub(y1) // Y-Difference
   fild(x2)
   fisub(x1) // X-Difference
   fpatan    // Angle of the line in st(0)
   fsincos   // Cosinus in st(0), Sinus in st(1)
   fstp cosinus
   fstp sine
  end;
  dx:=Round(cosinus*(x-x1)+sine*(y-y1));
  dy:=Round(cosinus*(y-y1)-sine*(x-x1));
  len:=Round(cosinus*(x2-x1)+sine*(y2-y1)); // length of line
  Result:=((dy>-d)and(dy<d)and(dx>-d)and(dx<len+d));
end;

procedure MouseOnMyReg(var PWL:TResObj;xy:TPoint);
var i:integer;
    ll1,ll2:TPoint;
    ms:TMemoryStream;
    arrLL:PArrLL;
    arLL:array of TPoint;
    poly:array of TExtendedPoint;
begin
 if show_point=3 then exit;
// Fmain.CDSmarks.Filter:='( LonR>'+floattostr(Fmain.X2Lon(xy.x+2))+')and(LonL<'+floattostr(Fmain.X2Lon(xy.x-2))+
//                  ')and(LatB<'+floattostr(Fmain.Y2Lat(xy.y+2))+')and(LatT>'+floattostr(Fmain.Y2Lat(xy.y-2))+')';
 //Fmain.CDSmarks.Filter:='';
 Fmain.CDSmarks.Filtered:=true;
 Fmain.CDSmarks.First;
 while (not(Fmain.CDSmarks.Eof))and((Fmain.CDSmarksvisible.AsBoolean)or(show_point=1)) do
 begin
  ll1:=Point(FMain.Lon2X(Fmain.CDSmarkslonL.AsFloat),FMain.Lat2y(Fmain.CDSmarkslatT.AsFloat));
  ll2:=Point(FMain.Lon2X(Fmain.CDSmarkslonR.AsFloat),FMain.Lat2y(Fmain.CDSmarkslatB.AsFloat));
  if (xy.x+8>ll1.x)and(xy.x-8<ll2.x)and(xy.y+16>ll1.y)and(xy.y-16<ll2.y) then
  begin
    ms:=TMemoryStream.Create;
    TBlobField(Fmain.CDSmarks.FieldByName('LonLatArr')).SaveToStream(ms);
    ms.Position:=0;
    GetMem(arrLL,ms.size);
    ms.ReadBuffer(arrLL^,ms.size);
    SetLength(arLL,ms.size div 24);
    setlength(poly,ms.size div 24);
    for i:=0 to length(arLL)-1 do begin
                                   arLL[i].x:=Fmain.Lon2X(arrLL^[i].x);
                                   arLL[i].y:=Fmain.Lat2y(arrLL^[i].y);
                                   poly[i]:=arrLL^[i];
                                  end;
    if length(arLL)=1 then
     begin
      PWL.name:=Fmain.CDSmarksname.AsString;
      PWL.descr:=Fmain.CDSmarksdescr.AsString;
      PWL.numid:=Fmain.CDSmarksid.AsString;
      PWL.find:=true;
      Setlength(arLL,0);
      freeMem(arrLL);
      ms.Free;
      exit;
     end;
    j:=1;
    if (arrLL^[0].x<>arrLL^[length(arLL)-1].x)or
       (arrLL^[0].y<>arrLL^[length(arLL)-1].y)then
      while (j<length(arLL)) do
       begin
        if CursorOnLinie(xy.x,xy.Y,arLL[j-1].x,arLL[j-1].y,arLL[j].x,arLL[j].y,(Fmain.CDSmarksscale1.AsInteger div 2)+1)
           then begin
                 PWL.name:=Fmain.CDSmarksname.AsString;
                 PWL.descr:=Fmain.CDSmarksdescr.AsString;
                 PWL.numid:=Fmain.CDSmarksid.AsString;
                 PWL.find:=true;
                 Setlength(arLL,0);
                 freeMem(arrLL);
                 ms.Free;
                 exit;
                end;
        inc(j);
       end
     else
     if (PtInRgn(arLL,xy))and(not((PolygonSquare(arLL)>PWL.S)and(PWL.S<>0))) then
      begin
       PWL.S:=PolygonSquare(arLL);
       PWL.name:=Fmain.CDSmarksname.AsString;
       PWL.descr:=Fmain.CDSmarksdescr.AsString;
       PWL.numid:=Fmain.CDSmarksid.AsString;
       PWL.descr:=PWL.descr+'<BR>'+SAS_STR_S+': '+RoundEx(CalcS(poly,sat_map_both),2)+' '+SAS_UNITS_km2; //Fmain.R2ShortStr(CalcS(poly,sat_map_both),4,' '+SAS_UNITS_km+'.',' '+SAS_UNITS_m);
       PWL.find:=true;
      end;
   Setlength(arLL,0);
   freeMem(arrLL);
   ms.Free;
  end;
  Fmain.CDSmarks.Next;
 end;
end;

procedure TForm2.FormActivate(Sender: TObject);
begin
 Fmain.CDSmarks.Filtered:=false;
 DBGrid2.DataSource:=Fmain.DataSource1;
// Fmain.CDSmarks.First;
 if not(sender is TForm) then exit;
 CBzoom.ItemIndex:=zoom_size-1;
end;

procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 DBGrid2.DataSource:=nil;
// Fmain.CDSmarks.Filtered:=false;
 Fmain.Enabled:=true;
 //Fmain.CDSmarks.Filtered:=true;
// Fmain.generate_im('');
end;

procedure TForm2.BDelClick(Sender: TObject);
begin
 if MessageBox(handle,pchar(SAS_MSG_youasure),pchar(SAS_MSG_coution),36)=IDNO
  then exit;
 Fmain.CDSmarks.Delete;
 Fmain.CDSmarks.ApplyRange;
 Fmain.CDSmarks.MergeChangeLog;
 Fmain.CDSmarks.SaveToFile(extractfilepath(paramstr(0))+'marks.xml');
// DBGrid1.Refresh;
 Fmain.generate_im(nilLastLoad,'');
end;

procedure TForm2.BGoClick(Sender: TObject);
var accept:boolean;
    textsrch:String;
    arrLL:TExtendedPoint;
    ms:TMemoryStream;
begin
 if RB3.Checked then
  begin
   ms:=TMemoryStream.Create;
   TBlobField(Fmain.CDSmarks.FieldByName('LonLatArr')).SaveToStream(ms);
   ms.Position:=0;
   ms.ReadBuffer(arrLL,24);
   form2.Close;
   Fmain.toPos(arrLL.y,arrLL.x,CBzoom.ItemIndex+1,true);
   ms.Free;
  end;
 if RB1.Checked then
  begin
   form2.Close;
   Fmain.toPos(DMS2G(lat1.Value,lat2.Value,lat3.Value,Lat_ns.ItemIndex=1),DMS2G(lon1.Value,lon2.Value,lon3.Value,Lon_we.ItemIndex=1),CBzoom.ItemIndex+1,true);
  end;
 if RB2.Checked then
  begin
   ld:=1;
   form2.Enabled:=false;
   Fmain.EmbeddedWB1_.Navigate('http://maps.google.ru/maps?f=q&hl=ru&geocode=&q='+EditGF.Text);
  end;
 if RB4.Checked then
  begin
   textsrch:=EditGF.Text;
   form2.Close;
   Fmain.TBEditItem1AcceptText(Fmain,textsrch,accept);
  end;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
 FAddPoint.show_(self,ExtPoint(0,0),true);
end;

procedure TForm2.Button3Click(Sender: TObject);
var arrLL:PArrLL;
    arLL:array of TExtendedPoint;
    ms:TMemoryStream;
    i:integer;
begin
 if not(Fmain.CDSmarks.Eof) then
  begin
   ms:=TMemoryStream.Create;
   TBlobField(Fmain.CDSmarks.FieldByName('LonLatArr')).SaveToStream(ms);
   GetMem(arrLL,ms.size);
   SetLength(arLL,ms.size div 24);
   ms.Position:=0;
   ms.ReadBuffer(arrLL^,ms.size);
   for i:=0 to length(arLL)-1 do
    arLL[i]:=arrLL^[i];
   if ms.Size=24 then FaddPoint.show_(self,arLL[0],false);
   if (ms.Size>24) then
    if compare2EP(arLL[0],arLL[length(arLL)-1]) then FaddPoly.show_(self,arLL,false)
                                                else FaddLine.show_(self,arLL,false);
   freeMem(arrLL);
   SetLength(arLL,0);
   ms.Free;
  end;
end;

procedure TForm2.lat_nsClick(Sender: TObject);
begin
 RB1.Checked:=true;
end;

procedure TForm2.EditGFClick(Sender: TObject);
begin
 if (not(RB2.Checked))and(not(RB4.Checked)) then RB2.Checked:=true;
end;

procedure TForm2.Lat1Click(Sender: TObject);
begin
 if (not(RB1.Checked)) then RB1.Checked:=true;
end;

procedure TForm2.DBGrid2DrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin
 if Column.Field.DataType=ftMemo then
  TDBGrid(Sender).Canvas.TextRect(Rect,Rect.Left+1,Rect.Top+2,Column.Field.AsString)
end;

procedure TForm2.FormShow(Sender: TObject);
begin
 ComboBox1.Clear;
 Fmain.CDSmarks.First;
 while not(Fmain.CDSmarks.Eof) do
  begin
   ComboBox1.AddItem(Fmain.CDSmarks.FieldByName('name').AsString,nil);
   Fmain.CDSmarks.Next;
  end;
 Fmain.CDSmarks.First;
 ComboBox1.ItemIndex:=0;
 THDBGrid(DBGrid2).DefaultRowHeight:=17;
 THDBGrid(DBGrid2).UpdateControlState;
end;

procedure TForm2.ComboBox1Change(Sender: TObject);
begin
 Fmain.CDSmarks.Locate('name',ComboBox1.Text,[loCaseInsensitive,loPartialKey])
end;

procedure TForm2.ComboBox1Enter(Sender: TObject);
begin
 if (not(RB3.Checked)) then RB3.Checked:=true;
end;

procedure TForm2.DBGrid2Enter(Sender: TObject);
begin
 if (not(RB3.Checked)) then RB3.Checked:=true;
end;

end.
