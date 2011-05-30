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
  i_IDList,
  i_InternalPerformanceCounter,
  u_WindowLayerBasicList;

type
  TfrmDebugInfo = class(TForm)
    sgrdDebugInfo: TStringGrid;
    pnlBottom: TPanel;
    btnRefresh: TButton;
    btnReset: TButton;
    procedure btnRefreshClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
  private
    FPerfCounterList: IInternalPerformanceCounterList;
    FPrevStateList: IIDInterfaceList;
    procedure AddRowFromCounter(AName: string; ARow: Integer; ACounter: IInternalPerformanceCounter);
    function AddRowsFromList(AParentName: string; AStartRaw: Integer; AList: IInternalPerformanceCounterList): Integer;
    procedure PrepareGridHeader;
    procedure RefreshData;
  public
    constructor Create(AOwner: TComponent; APerfCounterList: IInternalPerformanceCounterList); reintroduce;
  end;

var
  frmDebugInfo: TfrmDebugInfo;

implementation

uses
  ActiveX,
  u_WindowLayerBasic;

{$R *.dfm}

procedure TfrmDebugInfo.AddRowFromCounter(AName: string; ARow: Integer;
  ACounter: IInternalPerformanceCounter);
var
  VCount: Cardinal;
  VTime: TDateTime;
  VId: Integer;
  VPrevData: IInternalPerformanceCounterStaticData;
begin
  VId := ACounter.Id;
  VCount := ACounter.Counter;
  VTime := ACounter.TotalTime;

  VPrevData := nil;
  if FPrevStateList <> nil then begin
    VPrevData := IInternalPerformanceCounterStaticData(FPrevStateList.GetByID(VId));
  end;
  if VPrevData <> nil then begin
    VCount := VCount - VPrevData.Counter;
    VTime := VTime - VPrevData.TotalTime;
  end;

  if sgrdDebugInfo.RowCount <= ARow then begin
    sgrdDebugInfo.RowCount := ARow + 1;
  end;

  sgrdDebugInfo.Cells[0, ARow] := AName;
  if VCount > 0 then begin
    sgrdDebugInfo.Cells[1, ARow] := IntToStr(VCount);
    sgrdDebugInfo.Cells[2, ARow] := FormatDateTime('ss.zzz', VTime/VCount);
    sgrdDebugInfo.Cells[3, ARow] := FormatDateTime('nn:ss.zzz', VTime);
  end else begin
    sgrdDebugInfo.Cells[1, ARow] := '';
    sgrdDebugInfo.Cells[2, ARow] := '';
    sgrdDebugInfo.Cells[3, ARow] := '';
  end;
end;

function TfrmDebugInfo.AddRowsFromList(AParentName: string; AStartRaw: Integer;
  AList: IInternalPerformanceCounterList): Integer;
var
  VEnum: IEnumUnknown;
  VUnknown: IUnknown;
  VCounter: IInternalPerformanceCounter;
  VList: IInternalPerformanceCounterList;
  Vcnt: Integer;
  VName: string;
begin
  Result := AStartRaw;
  VEnum := AList.GetEunm;
  VName := AParentName + '/';
  while VEnum.Next(1, VUnknown, Addr(Vcnt)) = S_OK do begin
    if Supports(VUnknown, IInternalPerformanceCounter, VCounter) then begin
      AddRowFromCounter(VName + VCounter.Name, Result, VCounter);
      Inc(Result);
    end else if Supports(VUnknown, IInternalPerformanceCounterList, VList) then begin
      Result := AddRowsFromList(VName + VList.Name, Result, VList);
    end;
  end;
end;

procedure TfrmDebugInfo.btnRefreshClick(Sender: TObject);
begin
  RefreshData;
end;

procedure TfrmDebugInfo.btnResetClick(Sender: TObject);
begin
  FPrevStateList := FPerfCounterList.GetStaticDataList;
  RefreshData;
end;

constructor TfrmDebugInfo.Create(AOwner: TComponent;
  APerfCounterList: IInternalPerformanceCounterList);
begin
  inherited Create(AOwner);
  FPerfCounterList := APerfCounterList;
end;

procedure TfrmDebugInfo.FormCreate(Sender: TObject);
begin
  sgrdDebugInfo.ColWidths[0] := 150;
  sgrdDebugInfo.RowCount := 2;
  sgrdDebugInfo.FixedRows := 1;
end;

procedure TfrmDebugInfo.FormShow(Sender: TObject);
begin
  RefreshData;;
end;

procedure TfrmDebugInfo.PrepareGridHeader;
begin
  sgrdDebugInfo.Cells[0, 0] := 'Class';
  sgrdDebugInfo.Cells[1, 0] := 'Redraw cnt';
  sgrdDebugInfo.Cells[2, 0] := 'Redraw time avg';
  sgrdDebugInfo.Cells[3, 0] := 'Redraw time total';
end;

procedure TfrmDebugInfo.RefreshData;
var
  VLastRow: Integer;
  i, j: Integer;
begin
  PrepareGridHeader;
  VLastRow := sgrdDebugInfo.FixedRows;
  if FPerfCounterList <> nil then begin
    VLastRow := AddRowsFromList('', VLastRow, FPerfCounterList);
  end;
  for i := VLastRow to sgrdDebugInfo.RowCount - 1 do begin
    for j := 0 to sgrdDebugInfo.ColCount - 1 do begin
      sgrdDebugInfo.Cells[j, i] := '';
    end;
  end;
end;

end.
