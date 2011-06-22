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
  i_ImageResamplerFactory,
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
    Bevel1: TBevel;
    procedure cbbFromZoomChange(Sender: TObject);
    procedure chkAllZoomsClick(Sender: TObject);
    procedure chkFromPrevZoomClick(Sender: TObject);
    procedure chklstZoomsClickCheck(Sender: TObject);
  private
    procedure InitResamplersList(AList: IImageResamplerFactoryList; ABox: TComboBox);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Init(AZoom: Byte);
  end;

implementation

uses
  gnugettext,
  u_GlobalState,
  u_MapType;

{$R *.dfm}

const
  CZommDeltaMax = 8;

procedure TfrTilesGenPrev.cbbFromZoomChange(Sender: TObject);
var
  i: integer;
begin
  chklstZooms.Items.Clear;
  for i := cbbFromZoom.ItemIndex+1 downto 1 do begin
    chklstZooms.Items.Add(inttostr(i));
  end;
  chklstZoomsClickCheck(nil);
  chklstZooms.Repaint;
end;

procedure TfrTilesGenPrev.chkAllZoomsClick(Sender: TObject);
var
  i: integer;
begin
  if chkAllZooms.State <> cbGrayed then begin
    for i := 0 to chklstZooms.Count - 1 do begin
      if chklstZooms.ItemEnabled[i] then begin
        chklstZooms.Checked[i] := chkAllZooms.Checked;
      end;
    end;
  end;
end;

procedure TfrTilesGenPrev.chkFromPrevZoomClick(Sender: TObject);
begin
  chklstZoomsClickCheck(nil);
end;

procedure TfrTilesGenPrev.chklstZoomsClickCheck(Sender: TObject);
var
  i: Integer;
  VLastCheckedZoom: Integer;
  VZoom: Integer;
  VSourceZoom: Integer;
  VAllChecked: Boolean;
  VAllUnChecked: Boolean;
begin
  if chkFromPrevZoom.Checked then begin
    VSourceZoom := cbbFromZoom.ItemIndex + 1;
    VLastCheckedZoom := VSourceZoom;
    i := 0;
    while i < chklstZooms.Items.Count do begin
      VZoom := VSourceZoom - i - 1;
      if VLastCheckedZoom - VZoom > CZommDeltaMax then begin
        Break;
      end else begin
        chklstZooms.ItemEnabled[i] := True;
        if chklstZooms.Checked[i] then begin
          VLastCheckedZoom := VZoom;
        end;
      end;
      Inc(i);
    end;
    while i < chklstZooms.Items.Count do begin
      chklstZooms.ItemEnabled[i] := False;
      Inc(i);
    end;
  end else begin
    for i := CZommDeltaMax to chklstZooms.Items.Count - 1 do begin
      chklstZooms.ItemEnabled[i] := false;
    end;
  end;
  VAllChecked := True;
  VAllUnChecked := True;
  for i := 0 to chklstZooms.Items.Count - 1 do begin
    if chklstZooms.ItemEnabled[i] then begin
      if chklstZooms.Checked[i] then begin
        VAllUnChecked := False;
      end else begin
        VAllChecked := False;
      end;
    end;
  end;
  if VAllChecked then begin
    chkAllZooms.State := cbChecked;
  end else if VAllUnChecked then begin
    chkAllZooms.State := cbUnchecked;
  end else begin
    chkAllZooms.State := cbGrayed;
  end;
end;

constructor TfrTilesGenPrev.Create(AOwner: TComponent);
begin
  TP_Ignore(Self, 'cbbResampler.Items');
  TP_Ignore(Self, 'cbbResampler.Text');
  inherited;
end;

procedure TfrTilesGenPrev.Init(AZoom: Byte);
var
  i: integer;
  VMapType: TMapType;
  VActiveMapGUID: TGUID;
  VAddedIndex: Integer;
begin
  cbbFromZoom.Items.Clear;
  for i:=2 to 24 do begin
    cbbFromZoom.Items.Add(inttostr(i));
  end;
  cbbFromZoom.ItemIndex := AZoom;
  cbbFromZoomChange(cbbFromZoom);

  VActiveMapGUID := GState.MainFormConfig.MainMapsConfig.GetActiveMap.GetSelectedGUID;
  cbbMap.items.Clear;
  For i:=0 to GState.MapType.Count-1 do begin
    VMapType := GState.MapType[i];
    if VMapType.IsBitmapTiles then begin
      if (VMapType.UseGenPrevious)and(VMapType.Enabled) then begin
        VAddedIndex := cbbMap.Items.AddObject(VMapType.name, VMapType);
        if IsEqualGUID(VMapType.Zmp.GUID, VActiveMapGUID) then begin
          cbbMap.ItemIndex:=VAddedIndex;
        end;
      end;
    end;
  end;
  if (cbbMap.Items.Count > 0) and (cbbMap.ItemIndex < 0) then begin
    cbbMap.ItemIndex := 0;
  end;
  InitResamplersList(GState.ImageResamplerConfig.GetList, cbbResampler);
  cbbResampler.ItemIndex := GState.ImageResamplerConfig.ActiveIndex;
end;

procedure TfrTilesGenPrev.InitResamplersList(AList: IImageResamplerFactoryList;
  ABox: TComboBox);
var
  i: Integer;
begin
  ABox.Items.Clear;
  for i := 0 to AList.Count - 1 do begin
    ABox.Items.Add(AList.Captions[i]);
  end;
end;

end.
