unit fr_TilesDelete;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin;

type
  TfrTilesDelete = class(TFrame)
    cbbMap: TComboBox;
    seDelSize: TSpinEdit;
    chkDelBySize: TCheckBox;
    lblMap: TLabel;
    pnlTop: TPanel;
    pnlBottom: TPanel;
    pnlRight: TPanel;
    lblZoom: TLabel;
    cbbZoom: TComboBox;
    pnlCenter: TPanel;
    lblStat: TLabel;
    pnlDelBySize: TPanel;
    pnlDelSize: TPanel;
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

procedure TfrTilesDelete.Init(AZoom: Byte);
var
  i: integer;
  VMapType: TMapType;
  VActiveMap: TMapType;
  VAddedIndex: Integer;
begin
  cbbZoom.Items.Clear;
  for i:=1 to 24 do begin
    cbbZoom.Items.Add(inttostr(i));
  end;
  cbbZoom.ItemIndex := AZoom;

  VActiveMap := GState.ViewState.GetCurrentMap;
  cbbMap.items.Clear;
  For i:=0 to length(GState.MapType)-1 do begin
    VMapType := GState.MapType[i];
    if VMapType.TileStorage.GetUseDel then begin
      VAddedIndex := cbbMap.Items.AddObject(VMapType.name,VMapType);
      if VMapType = VActiveMap then begin
        cbbMap.ItemIndex:=VAddedIndex;
      end;
    end;
  end;
  if (cbbMap.Items.Count > 0) and (cbbMap.ItemIndex < 0) then begin
    cbbMap.ItemIndex := 0;
  end;
end;

end.
