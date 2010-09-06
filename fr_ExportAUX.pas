unit fr_ExportAUX;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfrExportAUX = class(TFrame)
    pnlCenter: TPanel;
    pnlRight: TPanel;
    lblZoom: TLabel;
    pnlMain: TPanel;
    lblMap: TLabel;
    cbbMap: TComboBox;
    pnlTop: TPanel;
    lblTargetFile: TLabel;
    edtTargetFile: TEdit;
    btnSelectTargetFile: TButton;
    dlgTargetFileSelect: TSaveDialog;
    cbbZoom: TComboBox;
    procedure btnSelectTargetFileClick(Sender: TObject);
  private
  public
    procedure Init(AZoom: Byte);
  end;

implementation

uses
  u_GlobalState,
  UResStrings,
  UMapType;

{$R *.dfm}

{ TFrame3 }

procedure TfrExportAUX.btnSelectTargetFileClick(Sender: TObject);
begin
 if dlgTargetFileSelect.Execute then
  edtTargetFile.Text:=dlgTargetFileSelect.FileName;
end;

procedure TfrExportAUX.Init(AZoom: Byte);
var
  i: integer;
  VMapType: TMapType;
  VActiveMap: TMapType;
  VAddedIndex: Integer;
begin
  VActiveMap := GState.ViewState.GetCurrentMap;

  cbbZoom.Items.Clear;
  for i:=1 to 24 do begin
    cbbZoom.Items.Add(inttostr(i));
  end;
  cbbMap.items.Clear;
  cbbZoom.ItemIndex := AZoom;

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
