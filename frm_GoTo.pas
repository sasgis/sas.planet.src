unit frm_GoTo;

interface

uses
  Windows,
  SysUtils,
  Forms,
  Classes,
  StdCtrls,
  ExtCtrls,
  Controls,
  t_GeoTypes,
  i_GeoCoder,
  u_CommonFormAndFrameParents,
  fr_LonLat;

type

  TfrmGoTo = class(TCommonFormParent)
    RB1: TRadioButton;
    GroupBox2: TGroupBox;
    RB3: TRadioButton;
    Label9: TLabel;
    BGo: TButton;
    GroupBox3: TGroupBox;
    RB2: TRadioButton;
    EditGF: TEdit;
    GroupBox1: TGroupBox;
    CBzoom: TComboBox;
    RB4: TRadioButton;
    ComboBox1: TComboBox;
    btnCancel: TButton;
    pnlBottomButtons: TPanel;
    pnlLonLat: TPanel;
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
    frLonLatPoint: TfrLonLat;
    function GeocodeResultFromLonLat(ASearch: WideString; ALonLat: TExtendedPoint; AMessage: WideString): IGeoCodeResult;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ShowGeocodeModal(var AResult: IGeoCodeResult; var AZoom: Byte): Boolean;
    procedure RefreshTranslation; override;
  end;


var
  frmGoTo: TfrmGoTo;

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

procedure TfrmGoTo.FormActivate(Sender: TObject);
begin
  if not(sender is TForm) then exit;
  CBzoom.ItemIndex:=GState.ViewState.GetCurrentZoom;
  frLonLatPoint.LonLat := GState.ViewState.GetCenterLonLat;
end;

procedure TfrmGoTo.FormShow(Sender: TObject);
begin
  AllMarsk2StringsWhitMarkId(ComboBox1.Items);
end;

function TfrmGoTo.GeocodeResultFromLonLat(ASearch: WideString;
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

procedure TfrmGoTo.FormClose(Sender: TObject; var Action: TCloseAction);
var i:integer;
begin
  for i:=1 to ComboBox1.items.Count do ComboBox1.Items.Objects[i-1].Free;
  ComboBox1.Clear;
end;

procedure TfrmGoTo.BGoClick(Sender: TObject);
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
    VLonLat := frLonLatPoint.LonLat;
    textsrch := lat2str(VLonLat.Y, GState.llStrType) + ' ' + lon2str(VLonLat.X, GState.llStrType);
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

procedure TfrmGoTo.lat_nsClick(Sender: TObject);
begin
  RB1.Checked:=true;
end;

procedure TfrmGoTo.RefreshTranslation;
begin
  inherited;
  frLonLatPoint.RefreshTranslation;
end;

function TfrmGoTo.ShowGeocodeModal(var AResult: IGeoCodeResult; var AZoom: Byte): Boolean;
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

procedure TfrmGoTo.EditGFClick(Sender: TObject);
begin
 if (not(RB2.Checked))and(not(RB4.Checked)) then RB2.Checked:=true;
end;

procedure TfrmGoTo.Lat1Click(Sender: TObject);
begin
 if (not(RB1.Checked)) then RB1.Checked:=true;
end;

procedure TfrmGoTo.ComboBox1Enter(Sender: TObject);
begin
 if (not(RB3.Checked)) then RB3.Checked:=true;
end;

constructor TfrmGoTo.Create(AOwner: TComponent);
begin
  inherited;
  frLonLatPoint := TfrLonLat.Create(nil);
  frLonLatPoint.Parent := pnlLonLat;
end;

destructor TfrmGoTo.Destroy;
begin
  FreeAndNil(frLonLatPoint);
  inherited;
end;

end.
