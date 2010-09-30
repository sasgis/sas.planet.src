unit Unit2;

interface

uses
  Windows,
  SysUtils,
  Forms,
  Classes,
  Mask,
  StdCtrls,
  Controls,
  rxToolEdit,
  rxCurrEdit,
  t_GeoTypes,
  i_GeoCoder,
  u_CommonFormAndFrameParents,
  Ugeofun;

type

  TFGoTo = class(TCommonFormParent)
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
    FResult: IGeoCodeResult;
    Fzoom: Byte;
    function GeocodeResultFromLonLat(ASearch: WideString; ALonLat: TExtendedPoint; AMessage: WideString): IGeoCodeResult;
  public
    function ShowGeocodeModal(var AResult: IGeoCodeResult; var AZoom: Byte): Boolean;
  end;


var
  FGoTo: TFGoTo;

implementation

uses
  u_GeoToStr,
  u_GlobalState,
  u_GeoCodeResult,
  u_GeoCodePalcemark,
  u_MarksSimple,
  u_MarksReadWriteSimple,
  unit1;

{$R *.dfm}

procedure TFGoTo.FormActivate(Sender: TObject);
begin
 if not(sender is TForm) then exit;
 CBzoom.ItemIndex:=GState.ViewState.GetCurrentZoom;
end;

procedure TFGoTo.FormShow(Sender: TObject);
begin
  AllMarsk2StringsWhitMarkId(ComboBox1.Items);
end;

function TFGoTo.GeocodeResultFromLonLat(ASearch: WideString;
  ALonLat: TExtendedPoint; AMessage: WideString): IGeoCodeResult;
var
  VPlace: IGeoCodePalcemark;
  VList: IInterfaceList;
begin
  VPlace := TGeoCodePalcemark.Create(ALonLat, AMessage, 4);
  VList := TInterfaceList.Create;
  VList.Add(VPlace);
  Result := TGeoCodeResult.Create(ASearch, 203, '', VList);
end;

procedure TFGoTo.FormClose(Sender: TObject; var Action: TCloseAction);
var i:integer;
begin
 for i:=1 to ComboBox1.items.Count do ComboBox1.Items.Objects[i-1].Free;
 ComboBox1.Clear;
end;

procedure TFGoTo.BGoClick(Sender: TObject);
var
  textsrch:String;
  VId: Integer;
  VMark: TMarkFull;
  VLonLat: TExtendedPoint;
begin
  FZoom := CBzoom.ItemIndex;
  if RB3.Checked then begin
    if ComboBox1.ItemIndex>-1 then begin
      VId := TMarkId(ComboBox1.Items.Objects[ComboBox1.ItemIndex]).id;
      VMark := GetMarkByID(VId);
      try
        VLonLat := VMark.GetGoToLonLat;
        FResult := GeocodeResultFromLonLat(ComboBox1.Text, VLonLat, VMark.name);
      finally
        VMark.Free
      end;
      ModalResult := mrOk;
    end else begin
      ModalResult := mrCancel;
    end;
  end else if RB1.Checked then begin
    VLonLat.X := DMS2G(lon1.Value,lon2.Value,lon3.Value,Lon_we.ItemIndex=1);
    VLonLat.Y := DMS2G(lat1.Value,lat2.Value,lat3.Value,Lat_ns.ItemIndex=1);
    textsrch := lon2str(VLonLat.X, GState.llStrType) + ' ' + lat2str(VLonLat.Y, GState.llStrType);
    FResult := GeocodeResultFromLonLat(textsrch, VLonLat, textsrch);
    ModalResult := mrOk;
  end else if RB2.Checked then begin
    textsrch:= Trim(EditGF.Text);
    FResult := Fmain.FGoogleGeoCoder.GetLocations(textsrch, GState.ViewState.GetCenterLonLat);
    ModalResult := mrOk;
  end else if RB4.Checked then begin
    textsrch:= Trim(EditGF.Text);
    FResult := Fmain.FYandexGeoCoder.GetLocations(textsrch, GState.ViewState.GetCenterLonLat);
    ModalResult := mrOk;
  end;
end;

procedure TFGoTo.lat_nsClick(Sender: TObject);
begin
  RB1.Checked:=true;
end;

function TFGoTo.ShowGeocodeModal(var AResult: IGeoCodeResult; var AZoom: Byte): Boolean;
begin
  if ShowModal = mrOk then begin
    Result := true;
    AResult := FResult;
    AZoom := FZoom;
  end else begin
    Result := False;
    AResult := nil;
    AZoom := 0;
  end;
end;

procedure TFGoTo.EditGFClick(Sender: TObject);
begin
 if (not(RB2.Checked))and(not(RB4.Checked)) then RB2.Checked:=true;
end;

procedure TFGoTo.Lat1Click(Sender: TObject);
begin
 if (not(RB1.Checked)) then RB1.Checked:=true;
end;

procedure TFGoTo.ComboBox1Enter(Sender: TObject);
begin
 if (not(RB3.Checked)) then RB3.Checked:=true;
end;

end.
