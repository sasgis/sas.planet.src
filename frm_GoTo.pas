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
  u_MarksDbGUIHelper,
  fr_LonLat;

type

  TfrmGoTo = class(TCommonFormParent)
    grpMarks: TGroupBox;
    lblZoom: TLabel;
    btnGoTo: TButton;
    grpGeoCode: TGroupBox;
    edtGeoCode: TEdit;
    grpLonLat: TGroupBox;
    cbbZoom: TComboBox;
    cbbAllMarks: TComboBox;
    btnCancel: TButton;
    pnlBottomButtons: TPanel;
    RB3: TRadioButton;
    RB2: TRadioButton;
    RB4: TRadioButton;
    RB1: TRadioButton;
    procedure btnGoToClick(Sender: TObject);
    procedure lat_nsClick(Sender: TObject);
    procedure edtGeoCodeClick(Sender: TObject);
    procedure Lat1Click(Sender: TObject);
    procedure cbbAllMarksEnter(Sender: TObject);
    procedure cbbAllMarksDropDown(Sender: TObject);
    procedure grpLonLatEnter(Sender: TObject);
  private
    FLonLat: TDoublePoint;
    FResult: IGeoCodeResult;
    frLonLatPoint: TfrLonLat;
    FMarkDBGUI: TMarksDbGUIHelper;
    FMarksList: IInterfaceList;
    function GeocodeResultFromLonLat(ASearch: WideString; ALonLat: TDoublePoint; AMessage: WideString): IGeoCodeResult;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ShowGeocodeModal(
      ALonLat: TDoublePoint;
      var AResult: IGeoCodeResult;
      var AZoom: Byte;
      AMarkDBGUI: TMarksDbGUIHelper
    ): Boolean;
    procedure RefreshTranslation; override;
  end;

var
  frmGoTo: TfrmGoTo;

implementation

uses
  c_GeoCoderGUIDSimple,
  i_GeoCoderList,
  i_MarksSimple,
  u_GlobalState,
  u_GeoCodeResult,
  u_GeoCodePlacemark;

{$R *.dfm}

function TfrmGoTo.GeocodeResultFromLonLat(ASearch: WideString;
  ALonLat: TDoublePoint; AMessage: WideString): IGeoCodeResult;
var
  VPlace: IGeoCodePlacemark;
  VList: IInterfaceList;
begin
  VPlace := TGeoCodePlacemark.Create(ALonLat, AMessage, 4);
  VList := TInterfaceList.Create;
  VList.Add(VPlace);
  Result := TGeoCodeResult.Create(ASearch, 203, '', VList);
end;

procedure TfrmGoTo.grpLonLatEnter(Sender: TObject);
begin
 if (not(RB1.Checked)) then RB1.Checked:=true;
end;

procedure TfrmGoTo.btnGoToClick(Sender: TObject);
var
  textsrch:String;
  VIndex: Integer;
  VMarkId: IMarkID;
  VMark: IMarkFull;
  VLonLat: TDoublePoint;
  VGeoCoderItem: IGeoCoderListEntity;
begin
  if RB3.Checked then begin
    VIndex := cbbAllMarks.ItemIndex;
    if VIndex >= 0 then begin
      VMarkId := IMarkId(Pointer(cbbAllMarks.Items.Objects[VIndex]));
      VMark := GState.MarksDb.MarksDb.GetMarkByID(VMarkId);
        VLonLat := VMark.GetGoToLonLat;
        FResult := GeocodeResultFromLonLat(cbbAllMarks.Text, VLonLat, VMark.name);
      ModalResult := mrOk;
    end else begin
      ModalResult := mrCancel;
    end;
  end else if RB1.Checked then begin
    VLonLat := frLonLatPoint.LonLat;
    textsrch := GState.ValueToStringConverterConfig.GetStaticConverter.LonLatConvert(VLonLat);
    FResult := GeocodeResultFromLonLat(textsrch, VLonLat, textsrch);
    ModalResult := mrOk;
  end else if RB2.Checked then begin
    textsrch:= Trim(edtGeoCode.Text);
    VGeoCoderItem := GState.MainFormConfig.MainGeoCoderConfig.GetList.Get(CGeoCoderGoogleGUID);
    if VGeoCoderItem <> nil then begin
      FResult := VGeoCoderItem.GetGeoCoder.GetLocations(textsrch, FLonLat);
      ModalResult := mrOk;
    end else begin
      ModalResult := mrCancel;
    end;
  end else if RB4.Checked then begin
    textsrch:= Trim(edtGeoCode.Text);
    VGeoCoderItem := GState.MainFormConfig.MainGeoCoderConfig.GetList.Get(CGeoCoderYandexGUID);
    if VGeoCoderItem <> nil then begin
      FResult := VGeoCoderItem.GetGeoCoder.GetLocations(textsrch, FLonLat);
      ModalResult := mrOk;
    end else begin
      ModalResult := mrCancel;
    end;
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

function TfrmGoTo.ShowGeocodeModal(
  ALonLat: TDoublePoint;
  var AResult: IGeoCodeResult;
  var AZoom: Byte;
  AMarkDBGUI: TMarksDbGUIHelper
): Boolean;
begin
  FLonLat := ALonLat;
  FMarkDBGUI := AMarkDBGUI;
  frLonLatPoint.Parent := grpLonLat;// pnlLonLat;
  cbbZoom.ItemIndex := Azoom;
  frLonLatPoint.LonLat := FLonLat;
  if ShowModal = mrOk then begin
    Result := true;
    AResult := FResult;
    AZoom := cbbZoom.ItemIndex;
  end else begin
    Result := False;
    AResult := nil;
    AZoom := 0;
  end;
  cbbAllMarks.Clear;
  FMarksList:=nil;
end;

procedure TfrmGoTo.edtGeoCodeClick(Sender: TObject);
begin
 if (not(RB2.Checked))and(not(RB4.Checked)) then RB2.Checked:=true;
end;

procedure TfrmGoTo.Lat1Click(Sender: TObject);
begin
 if (not(RB1.Checked)) then RB1.Checked:=true;
end;

procedure TfrmGoTo.cbbAllMarksDropDown(Sender: TObject);
begin
  if cbbAllMarks.Items.Count=0 then begin
    FMarksList := FMarkDBGUI.MarksDB.MarksDb.GetAllMarskIdList;
    FMarkDBGUI.MarksListToStrings(FMarksList, cbbAllMarks.Items);
  end;
end;

procedure TfrmGoTo.cbbAllMarksEnter(Sender: TObject);
begin
 if (not(RB3.Checked)) then RB3.Checked:=true;
end;

constructor TfrmGoTo.Create(AOwner: TComponent);
begin
  inherited;
  frLonLatPoint := TfrLonLat.Create(nil);
  frLonLatPoint.Width:= grpLonLat.Width;
  frLonLatPoint.Height:= grpLonLat.Height;
end;

destructor TfrmGoTo.Destroy;
begin
  FreeAndNil(frLonLatPoint);
  inherited;
end;

end.
