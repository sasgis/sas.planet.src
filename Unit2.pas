unit Unit2;

interface

uses
  Windows,
  SysUtils,
  Forms,
  Dialogs,
  Classes,
  DB,
  Mask,
  StdCtrls,
  Controls,
  rxToolEdit,
  rxCurrEdit,
  Ugeofun,
  UResStrings,
  UMarksExplorer;

type

  TFGoTo = class(TForm)
    RB1: TRadioButton;
    GroupBox2: TGroupBox;
    RB3: TRadioButton;
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
    ComboBox1: TComboBox;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BGoClick(Sender: TObject);
    procedure lat_nsClick(Sender: TObject);
    procedure EditGFClick(Sender: TObject);
    procedure Lat1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ComboBox1Enter(Sender: TObject);
  private
  public
  end;


var
  FGoTo: TFGoTo;
  procedure MouseOnMyReg(var PWL:TResObj;xy:TPoint);

implementation
uses
  u_GlobalState,
  t_CommonTypes, 
  t_GeoTypes,
  unit1,
  UaddPoint,
  UaddLine,
  UMapType,
  UaddPoly;

{$R *.dfm}

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
var
  j:integer;
  i:integer;
  ll1,ll2:TPoint;
  ms:TMemoryStream;
  arrLL:PArrLL;
  arLL: TPointArray;
  poly:TExtendedPointArray;
begin
 if GState.show_point = mshNone then exit;
 Fmain.CDSKategory.Filtered:=true;
 if Fmain.CDSKategory.Eof then exit;
 Fmain.CDSmarks.Filtered:=true;
 Fmain.CDSmarks.First;
 while (not(Fmain.CDSmarks.Eof))and((Fmain.CDSmarksvisible.AsBoolean)or(GState.show_point=mshAll)) do
 begin
  LL1:=GState.sat_map_both.GeoConvert.LonLat2PixelPos(ExtPoint(Fmain.CDSmarkslonL.AsFloat,Fmain.CDSmarkslatT.AsFloat),GState.zoom_size-1);
  LL1 := Fmain.MapPixel2VisiblePixel(ll1);
  LL2:=GState.sat_map_both.GeoConvert.LonLat2PixelPos(ExtPoint(Fmain.CDSmarkslonR.AsFloat,Fmain.CDSmarkslatB.AsFloat),GState.zoom_size-1);
  LL2 := Fmain.MapPixel2VisiblePixel(ll2);
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
      arLL[i]:=GState.sat_map_both.GeoConvert.LonLat2PixelPos(arrLL^[i],GState.zoom_size-1);
      arLL[i] := Fmain.MapPixel2VisiblePixel(arLL[i]);
      poly[i]:=arrLL^[i];
    end;
    if length(arLL)=1 then
     begin
      PWL.name:=Fmain.CDSmarksname.AsString;
      PWL.descr:=Fmain.CDSmarksdescr.AsString;
      PWL.numid:=Fmain.CDSmarksid.AsString;
      PWL.find:=true;
      PWL.type_:=ROTpoint;
      Setlength(arLL,0);
      freeMem(arrLL);
      ms.Free;
      Fmain.CDSmarks.Filtered:=false;
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
                 PWL.type_:=ROTline;
                 Setlength(arLL,0);
                 freeMem(arrLL);
                 ms.Free;
                 Fmain.CDSmarks.Filtered:=false;
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
       PWL.find:=true;
       PWL.type_:=ROTPoly;
      end;
   Setlength(arLL,0);
   freeMem(arrLL);
   ms.Free;
  end;
  Fmain.CDSmarks.Next;
 end;
 Fmain.CDSmarks.Filtered:=false;
end;

procedure TFGoTo.FormActivate(Sender: TObject);
begin
 if not(sender is TForm) then exit;
 CBzoom.ItemIndex:=GState.zoom_size-1;
end;

procedure TFGoTo.FormClose(Sender: TObject; var Action: TCloseAction);
var i:integer;
begin
 for i:=1 to ComboBox1.items.Count do ComboBox1.Items.Objects[i-1].Free;
 ComboBox1.Clear;
 Fmain.Enabled:=true;
end;

procedure TFGoTo.BGoClick(Sender: TObject);
var accept:boolean;
    textsrch:String;
begin
 if RB3.Checked then
  begin
   if ComboBox1.ItemIndex>-1 then
    begin
     close;
     GoToMark(TMarkId(ComboBox1.Items.Objects[ComboBox1.ItemIndex]).id,CBzoom.ItemIndex+1);
    end;
  end;
 if RB1.Checked then
  begin
   Close;   
   Fmain.toPos(ExtPoint(DMS2G(lon1.Value,lon2.Value,lon3.Value,Lon_we.ItemIndex=1),DMS2G(lat1.Value,lat2.Value,lat3.Value,Lat_ns.ItemIndex=1)),CBzoom.ItemIndex+1,true);
  end;
 if RB2.Checked then
  begin
   textsrch:=EditGF.Text;
   Close;
   Fmain.EditGoogleSrchAcceptText(Fmain,textsrch,accept);
  end;
 if RB4.Checked then
  begin
   textsrch:=EditGF.Text;
   Close;
   Fmain.TBEditItem1AcceptText(Fmain,textsrch,accept);
  end;
end;

procedure TFGoTo.lat_nsClick(Sender: TObject);
begin
 RB1.Checked:=true;
end;

procedure TFGoTo.EditGFClick(Sender: TObject);
begin
 if (not(RB2.Checked))and(not(RB4.Checked)) then RB2.Checked:=true;
end;

procedure TFGoTo.Lat1Click(Sender: TObject);
begin
 if (not(RB1.Checked)) then RB1.Checked:=true;
end;

procedure TFGoTo.FormShow(Sender: TObject);
var Mark:TMarkId;
begin
 ComboBox1.Clear;
 Fmain.CDSmarks.Filtered:=false;
 Fmain.CDSmarks.First;
 while not(Fmain.CDSmarks.Eof) do
  begin
   Mark:=TMarkId.Create;
   Mark.id:=Fmain.CDSmarks.FieldByName('id').AsInteger;
   ComboBox1.AddItem(Fmain.CDSmarks.FieldByName('name').AsString,Mark);
   Fmain.CDSmarks.Next;
  end;
 Fmain.CDSmarks.First;
end;

procedure TFGoTo.ComboBox1Enter(Sender: TObject);
begin
 if (not(RB3.Checked)) then RB3.Checked:=true;
end;

end.
