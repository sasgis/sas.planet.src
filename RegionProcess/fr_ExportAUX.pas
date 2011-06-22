unit fr_ExportAUX;

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
  ExtCtrls,
  u_CommonFormAndFrameParents;

type
  TfrExportAUX = class(TFrame)
    pnlCenter: TPanel;
    pnlMain: TPanel;
    lblMap: TLabel;
    cbbMap: TComboBox;
    pnlTop: TPanel;
    lblTargetFile: TLabel;
    edtTargetFile: TEdit;
    btnSelectTargetFile: TButton;
    dlgTargetFileSelect: TSaveDialog;
    pnlRight: TPanel;
    lblZoom: TLabel;
    cbbZoom: TComboBox;
    procedure btnSelectTargetFileClick(Sender: TObject);
  private
  public
    procedure Init(AZoom: Byte);
  end;

implementation

uses
  u_GlobalState,
  u_MapType;

{$R *.dfm}

{ TFrame3 }

procedure TfrExportAUX.btnSelectTargetFileClick(Sender: TObject);
begin
  if dlgTargetFileSelect.Execute then begin
    edtTargetFile.Text := dlgTargetFileSelect.FileName;
  end;
end;

procedure TfrExportAUX.Init(AZoom: Byte);
var
  i: integer;
  VMapType: TMapType;
  VActiveMapGUID: TGUID;
  VAddedIndex: Integer;
begin
  cbbZoom.Items.Clear;
  for i:=1 to 24 do begin
    cbbZoom.Items.Add(inttostr(i));
  end;
  cbbMap.items.Clear;
  cbbZoom.ItemIndex := AZoom;

  VActiveMapGUID := GState.MainFormConfig.MainMapsConfig.GetActiveMap.GetSelectedGUID;
  For i:=0 to GState.MapType.Count-1 do begin
    VMapType := GState.MapType[i];
    if (VMapType.IsBitmapTiles)and(VMapType.Enabled) then begin
      if VMapType.TileStorage.GetIsStoreFileCache then begin
        VAddedIndex := cbbMap.Items.AddObject(VMapType.name,VMapType);
        if IsEqualGUID(VMapType.Zmp.GUID, VActiveMapGUID) then begin
          cbbMap.ItemIndex:=VAddedIndex;
        end;
      end;
    end;
  end;
  if (cbbMap.Items.Count > 0) and (cbbMap.ItemIndex < 0) then begin
    cbbMap.ItemIndex := 0;
  end;
end;

end.
