unit fr_TilesGenPrev;

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
  TfrTilesGenPrev = class(TFrame)
    pnlBottom: TPanel;
    pnlRight: TPanel;
    pnlCenter: TPanel;
    lblMap: TLabel;
    lblStat: TLabel;
    cbbMap: TComboBox;
    pnlTop: TPanel;
    cbbFromZoom: TComboBox;
    lblFromZoom: TLabel;
    chkAllZooms: TCheckBox;
    lblZooms: TLabel;
    chklstZooms: TCheckListBox;
    cbbResampler: TComboBox;
    lblResampler: TLabel;
    chkReplace: TCheckBox;
    chkSaveFullOnly: TCheckBox;
    chkFromPrevZoom: TCheckBox;
    procedure cbbFromZoomChange(Sender: TObject);
    procedure chkAllZoomsClick(Sender: TObject);
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

procedure TfrTilesGenPrev.cbbFromZoomChange(Sender: TObject);
var
  i: integer;
begin
  chklstZooms.Items.Clear;
  for i:= cbbFromZoom.ItemIndex+1 downto 1 do begin
    chklstZooms.Items.Add(inttostr(i));
  end;
  for i:= 8 to cbbFromZoom.ItemIndex do begin
    chklstZooms.ItemEnabled[i]:=false;
  end;
end;

procedure TfrTilesGenPrev.chkAllZoomsClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to chklstZooms.Count - 1 do begin
    if chklstZooms.ItemEnabled[i] then begin
      chklstZooms.Checked[i] := chkAllZooms.Checked;
    end;
  end;
end;

procedure TfrTilesGenPrev.Init(AZoom: Byte);
var
  i: integer;
  VMapType: TMapType;
  VActiveMap: TMapType;
  VAddedIndex: Integer;
begin
  cbbFromZoom.Items.Clear;
  for i:=2 to 24 do begin
    cbbFromZoom.Items.Add(inttostr(i));
  end;
  cbbFromZoom.ItemIndex := AZoom;
  cbbFromZoomChange(cbbFromZoom);

  VActiveMap := GState.ViewState.GetCurrentMap;
  cbbMap.items.Clear;
  For i:=0 to length(GState.MapType)-1 do begin
    VMapType := GState.MapType[i];
    if VMapType.IsBitmapTiles then begin
      if VMapType.UseGenPrevious then begin
        VAddedIndex := cbbMap.Items.AddObject(VMapType.name, VMapType);
        if VMapType = VActiveMap then begin
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
