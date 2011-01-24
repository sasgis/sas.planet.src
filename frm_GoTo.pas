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
    RB1: TRadioButton;
    grpMarks: TGroupBox;
    RB3: TRadioButton;
    lblZoom: TLabel;
    btnGoTo: TButton;
    grpGeoCode: TGroupBox;
    RB2: TRadioButton;
    edtGeoCode: TEdit;
    grpLonLat: TGroupBox;
    cbbZoom: TComboBox;
    RB4: TRadioButton;
    cbbAllMarks: TComboBox;
    btnCancel: TButton;
    pnlBottomButtons: TPanel;
    pnlLonLat: TPanel;
    procedure btnGoToClick(Sender: TObject);
    procedure lat_nsClick(Sender: TObject);
    procedure edtGeoCodeClick(Sender: TObject);
    procedure Lat1Click(Sender: TObject);
    procedure cbbAllMarksEnter(Sender: TObject);
  private
    FResult: IGeoCodeResult;
    frLonLatPoint: TfrLonLat;
    FMarkDBGUI: TMarksDbGUIHelper;
    function GeocodeResultFromLonLat(ASearch: WideString; ALonLat: TDoublePoint; AMessage: WideString): IGeoCodeResult;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ShowGeocodeModal(var AResult: IGeoCodeResult; var AZoom: Byte; AMarkDBGUI: TMarksDbGUIHelper): Boolean;
    procedure RefreshTranslation; override;
  end;

var
  frmGoTo: TfrmGoTo;

implementation

uses
  c_GeoCoderGUIDSimple,
  i_IGeoCoderList,
  i_MarksSimple,
  u_GlobalState,
  u_GeoCodeResult,
  u_GeoCodePalcemark;

{$R *.dfm}

function TfrmGoTo.GeocodeResultFromLonLat(ASearch: WideString;
  ALonLat: TDoublePoint; AMessage: WideString): IGeoCodeResult;
var
  VPlace: IGeoCodePalcemark;
  VList: IInterfaceList;
begin
  VPlace := TGeoCodePalcemark.Create(ALonLat, AMessage, 4);
  VList := TInterfaceList.Create;
  VList.Add(VPlace);
  Result := TGeoCodeResult.Create(ASearch, 203, '', VList);
end;

procedure TfrmGoTo.btnGoToClick(Sender: TObject);
var
  textsrch:String;
  VId: Integer;
  VMark: IMarkFull;
  VLonLat: TDoublePoint;
  VGeoCoderItem: IGeoCoderListEntity;
begin
  if RB3.Checked then begin
    if cbbAllMarks.ItemIndex>-1 then begin
      VId := IMarkId(Pointer(cbbAllMarks.Items.Objects[cbbAllMarks.ItemIndex])).id;
      VMark := GState.MarksDb.MarksDb.GetMarkByID(VId);
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
      FResult := VGeoCoderItem.GetGeoCoder.GetLocations(textsrch, GState.ViewState.GetCenterLonLat);
      ModalResult := mrOk;
    end else begin
      ModalResult := mrCancel;
    end;
  end else if RB4.Checked then begin
    textsrch:= Trim(edtGeoCode.Text);
    VGeoCoderItem := GState.MainFormConfig.MainGeoCoderConfig.GetList.Get(CGeoCoderYandexGUID);
    if VGeoCoderItem <> nil then begin
      FResult := VGeoCoderItem.GetGeoCoder.GetLocations(textsrch, GState.ViewState.GetCenterLonLat);
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
  var AResult: IGeoCodeResult;
  var AZoom: Byte;
  AMarkDBGUI: TMarksDbGUIHelper
): Boolean;
var
  VMarksList: IInterfaceList;
begin
  FMarkDBGUI := AMarkDBGUI;
  frLonLatPoint.Parent := pnlLonLat;
  cbbZoom.ItemIndex := Azoom;
  frLonLatPoint.LonLat := GState.ViewState.GetCenterLonLat;
  VMarksList := FMarkDBGUI.MarksDB.MarksDb.GetAllMarskIdList;
  try
    FMarkDBGUI.MarksListToStrings(VMarksList, cbbAllMarks.Items);
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
  finally
    FreeAndNil(VMarksList);
  end;
end;

procedure TfrmGoTo.edtGeoCodeClick(Sender: TObject);
begin
 if (not(RB2.Checked))and(not(RB4.Checked)) then RB2.Checked:=true;
end;

procedure TfrmGoTo.Lat1Click(Sender: TObject);
begin
 if (not(RB1.Checked)) then RB1.Checked:=true;
end;

procedure TfrmGoTo.cbbAllMarksEnter(Sender: TObject);
begin
 if (not(RB3.Checked)) then RB3.Checked:=true;
end;

constructor TfrmGoTo.Create(AOwner: TComponent);
begin
  inherited;
  frLonLatPoint := TfrLonLat.Create(nil);
end;

destructor TfrmGoTo.Destroy;
begin
  FreeAndNil(frLonLatPoint);
  inherited;
end;

end.
