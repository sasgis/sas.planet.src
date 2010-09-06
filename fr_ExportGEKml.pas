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
  ExtCtrls;

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
  UResStrings,
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
  VActiveMap: TMapType;
  VAddedIndex: Integer;
begin
  VActiveMap := GState.ViewState.GetCurrentMap;

  chklstZooms.Items.Clear;
  for i:=1 to 24 do begin
    chklstZooms.Items.Add(inttostr(i));
  end;
  cbbMap.items.Clear;

  For i:=0 to length(GState.MapType)-1 do begin
    VMapType := GState.MapType[i];
    if VMapType.IsBitmapTiles then begin
      VAddedIndex := cbbMap.Items.AddObject(VMapType.name,VMapType);
      if VMapType = VActiveMap then begin
        cbbMap.ItemIndex:=VAddedIndex;
      end;
    end;
  end;
end;

end.
