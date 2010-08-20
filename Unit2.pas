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

implementation

uses
  u_GlobalState,
  u_MarksSimple,
  unit1;

{$R *.dfm}

procedure TFGoTo.FormActivate(Sender: TObject);
begin
 if not(sender is TForm) then exit;
 CBzoom.ItemIndex:=GState.ViewState.GetCurrentZoom;
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
  VId: Integer;
  VMark: TMarkFull;
begin
 if RB3.Checked then
  begin
   if ComboBox1.ItemIndex>-1 then
    begin
      VId := TMarkId(ComboBox1.Items.Objects[ComboBox1.ItemIndex]).id;
      close;
      VMark := GetMarkByID(VId);
      try
        Fmain.topos(VMark.GetGoToLonLat, CBzoom.ItemIndex, True);
      finally
        VMark.Free
      end;
    end;
  end;
 if RB1.Checked then
  begin
   Close;   
   Fmain.toPos(ExtPoint(DMS2G(lon1.Value,lon2.Value,lon3.Value,Lon_we.ItemIndex=1),DMS2G(lat1.Value,lat2.Value,lat3.Value,Lat_ns.ItemIndex=1)),CBzoom.ItemIndex,true);
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
begin
  AllMarsk2StringsWhitMarkId(ComboBox1.Items);
end;

procedure TFGoTo.ComboBox1Enter(Sender: TObject);
begin
 if (not(RB3.Checked)) then RB3.Checked:=true;
end;

end.
