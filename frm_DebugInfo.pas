unit frm_DebugInfo;

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
  Grids,
  u_WindowLayerBasicList;

type
  TfrmDebugInfo = class(TForm)
    sgrdDebugInfo: TStringGrid;
    pnlBottom: TPanel;
    btnRefresh: TButton;
    procedure btnRefreshClick(Sender: TObject);
  private
    FLayersList: TWindowLayerBasicList;
  public
    procedure ShowStatistic(ALayersList: TWindowLayerBasicList);
  end;

var
  frmDebugInfo: TfrmDebugInfo;

implementation

uses
  u_WindowLayerBasic;

{$R *.dfm}

procedure TfrmDebugInfo.btnRefreshClick(Sender: TObject);
var
  i: Integer;
  VCount: Cardinal;
  VTime: TDateTime;
  VLayer: TWindowLayerAbstract;
  VLayerCount: Integer;
  VRow: Integer;
begin
  if FLayersList <> nil then begin
    VLayerCount := FLayersList.Count;
    sgrdDebugInfo.RowCount := VLayerCount + sgrdDebugInfo.FixedRows;
    sgrdDebugInfo.Cells[0, 0] := 'Class';
    sgrdDebugInfo.Cells[1, 0] := 'Redraw cnt';
    sgrdDebugInfo.Cells[2, 0] := 'Redraw time avg';
    sgrdDebugInfo.Cells[3, 0] := 'Redraw time total';

    for i := 0 to VLayerCount - 1 do begin
      VLayer := FLayersList[i];
      VCount := VLayer.RedrawCounter;
      VTime := VLayer.RedrawTime;
      VRow := i + sgrdDebugInfo.FixedRows;
      sgrdDebugInfo.Cells[0, VRow] := VLayer.ClassName;
      sgrdDebugInfo.Cells[1, VRow] := IntToStr(VCount);
      if VCount > 0 then begin
        sgrdDebugInfo.Cells[2, VRow] := FormatDateTime('ss.zzz', VTime/VCount);
      end else begin
        sgrdDebugInfo.Cells[2, VRow] := '';
      end;
      sgrdDebugInfo.Cells[3, VRow] := FormatDateTime('nn:ss.zzz', VTime);
    end;
  end;
end;

procedure TfrmDebugInfo.ShowStatistic(ALayersList: TWindowLayerBasicList);
begin
  FLayersList := ALayersList;
  btnRefreshClick(nil);
  Show;
end;

end.
