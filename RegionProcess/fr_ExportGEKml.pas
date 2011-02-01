unit fr_ExportGEKml;

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
  ExtCtrls,
  u_CommonFormAndFrameParents;

type
  TfrExportGEKml = class(TFrame)
    pnlCenter: TPanel;
    lblZooms: TLabel;
    chkAllZooms: TCheckBox;
    chklstZooms: TCheckListBox;
    pnlTop: TPanel;
    lblTargetFile: TLabel;
    edtTargetFile: TEdit;
    btnSelectTargetFile: TButton;
    pnlRight: TPanel;
    pnlMain: TPanel;
    chkNotSaveNotExists: TCheckBox;
    chkUseRelativePath: TCheckBox;
    cbbMap: TComboBox;
    lblMap: TLabel;
    dlgSaveKML: TSaveDialog;
    procedure btnSelectTargetFileClick(Sender: TObject);
    procedure chkAllZoomsClick(Sender: TObject);
  private
  public
    procedure Init;
  end;

implementation

uses
  u_GlobalState,
  UMapType;

{$R *.dfm}

procedure TfrExportGEKml.btnSelectTargetFileClick(Sender: TObject);
begin
 if dlgSaveKML.Execute then
  edtTargetFile.Text:=dlgSaveKML.FileName;
end;

procedure TfrExportGEKml.chkAllZoomsClick(Sender: TObject);
var
  i: byte;
begin
  for i:=0 to chklstZooms.Count-1 do begin
    chklstZooms.Checked[i] := TCheckBox(sender).Checked;
  end;
end;

procedure TfrExportGEKml.Init;
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
  cbbMap.items.Clear;
  For i:=0 to GState.MapType.Count-1 do begin
    VMapType := GState.MapType[i];
    if VMapType.IsBitmapTiles and VMapType.TileStorage.GetIsStoreFileCache then begin
      VAddedIndex := cbbMap.Items.AddObject(VMapType.name,VMapType);
      if IsEqualGUID(VMapType.GUID, VActiveMapGUID) then begin
        cbbMap.ItemIndex:=VAddedIndex;
      end;
    end;
  end;
  if (cbbMap.Items.Count > 0) and (cbbMap.ItemIndex < 0) then begin
    cbbMap.ItemIndex := 0;
  end;
end;

end.
