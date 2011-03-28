unit fr_ExportIPhone;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  CheckLst,
  Spin,
  ExtCtrls,
  u_CommonFormAndFrameParents;

type
  TfrExportIPhone = class(TFrame)
    pnlMaps: TPanel;
    lblMaps: TLabel;
    lblSat: TLabel;
    lblMap: TLabel;
    lblHybr: TLabel;
    lblCompress: TLabel;
    lblSatCompress: TLabel;
    lblMapCompress: TLabel;
    lblHybrCompress: TLabel;
    cbbSat: TComboBox;
    cbbMap: TComboBox;
    cbbHybr: TComboBox;
    rbSat: TRadioButton;
    rbMap: TRadioButton;
    rbHybr: TRadioButton;
    seSatCompress: TSpinEdit;
    seMapCompress: TSpinEdit;
    chkAppendTilse: TCheckBox;
    seHybrCompress: TSpinEdit;
    pnlTop: TPanel;
    btnSelectTargetPath: TButton;
    edtTargetPath: TEdit;
    lblTargetPath: TLabel;
    pnlBottom: TPanel;
    pnlRight: TPanel;
    lblZooms: TLabel;
    chklstZooms: TCheckListBox;
    chkAllZooms: TCheckBox;
    grdpnlMaps: TGridPanel;
    procedure chkAllZoomsClick(Sender: TObject);
    procedure btnSelectTargetPathClick(Sender: TObject);
  private
  public
    procedure Init;
  end;

implementation

uses
  FileCtrl,
  u_GlobalState,
  u_ResStrings,
  u_MapType;

{$R *.dfm}

procedure TfrExportIPhone.btnSelectTargetPathClick(Sender: TObject);
var
  TempPath: string;
begin
  if SelectDirectory('', '', TempPath) then begin
    edtTargetPath.Text := IncludeTrailingPathDelimiter(TempPath);
  end;
end;

procedure TfrExportIPhone.chkAllZoomsClick(Sender: TObject);
var
  i: byte;
begin
  for i:=0 to chklstZooms.Count-1 do begin
    chklstZooms.Checked[i] := TCheckBox(sender).Checked;
  end;
end;

procedure TfrExportIPhone.Init;
var
  i: integer;
  VMapType: TMapType;
  VActiveMapGUID: TGUID;
  VAddedIndex: Integer;
begin
  chklstZooms.Items.Clear;
  for i:=1 to 24 do begin
    chklstZooms.Items.Add(inttostr(i));
  end;

  VActiveMapGUID := GState.MainFormConfig.MainMapsConfig.GetActiveMap.GetSelectedGUID;

  cbbSat.items.Clear;
  cbbMap.items.Clear;
  cbbHybr.items.Clear;
  cbbSat.Items.AddObject(SAS_STR_No,nil);
  cbbMap.Items.AddObject(SAS_STR_No,nil);
  cbbHybr.Items.AddObject(SAS_STR_No,nil);

  For i:=0 to GState.MapType.Count-1 do begin
    VMapType := GState.MapType[i];
    if (VMapType.IsBitmapTiles)and(VMapType.Enabled) then begin
      if (not(VMapType.asLayer)) then begin
        VAddedIndex := cbbSat.Items.AddObject(VMapType.name,VMapType);
        if IsEqualGUID(VMapType.GUID, VActiveMapGUID) then begin
          cbbSat.ItemIndex:=VAddedIndex;
        end;
        VAddedIndex := cbbMap.Items.AddObject(VMapType.name,VMapType);
        if IsEqualGUID(VMapType.GUID, VActiveMapGUID) then begin
          cbbMap.ItemIndex:=VAddedIndex;
        end;
      end else if(VMapType.IsHybridLayer) then begin
        VAddedIndex := cbbHybr.Items.AddObject(VMapType.name,VMapType);
        if (cbbHybr.ItemIndex=-1) then begin
          if GState.MainFormConfig.MainMapsConfig.GetLayers.IsGUIDSelected(VMapType.GUID) then begin
            cbbHybr.ItemIndex:=VAddedIndex;
          end;
        end;
      end;
    end;
  end;
  if cbbSat.ItemIndex=-1 then cbbSat.ItemIndex:=1;
  if cbbMap.ItemIndex=-1 then cbbMap.ItemIndex:=0;
  if cbbHybr.ItemIndex=-1 then cbbHybr.ItemIndex:=0;
end;

end.
