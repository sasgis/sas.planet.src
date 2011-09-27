{******************************************************************************}
{* SAS.Планета (SAS.Planet)                                                   *}
{* Copyright (C) 2007-2011, авторы программы SAS.Планета (SAS.Planet).        *}
{* Это программа является свободным программным обеспечением. Вы можете       *}
{* распространять и/или модифицировать её согласно условиям Стандартной       *}
{* Общественной Лицензии GNU, опубликованной Фондом Свободного Программного   *}
{* Обеспечения, версии 3. Эта программа распространяется в надежде, что она   *}
{* будет полезной, но БЕЗ ВСЯКИХ ГАРАНТИЙ, в том числе подразумеваемых        *}
{* гарантий ТОВАРНОГО СОСТОЯНИЯ ПРИ ПРОДАЖЕ и ГОДНОСТИ ДЛЯ ОПРЕДЕЛЁННОГО      *}
{* ПРИМЕНЕНИЯ. Смотрите Стандартную Общественную Лицензию GNU версии 3, для   *}
{* получения дополнительной информации. Вы должны были получить копию         *}
{* Стандартной Общественной Лицензии GNU вместе с программой. В случае её     *}
{* отсутствия, посмотрите http://www.gnu.org/licenses/.                       *}
{*                                                                            *}
{* http://sasgis.ru/sasplanet                                                 *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

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
  i_InternalPerformanceCounter;

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
  ActiveX;

{$R *.dfm}

procedure TfrmDebugInfo.AddRowFromCounter(AName: string; ARow: Integer;
  ACounter: IInternalPerformanceCounter);
var
  VCount: Cardinal;
  VTime: TDateTime;
  VId: Integer;
  VPrevData: IInternalPerformanceCounterStaticData;
  VAvgTime: Extended;
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
    VAvgTime := VTime/VCount*24*60*60;
    sgrdDebugInfo.Cells[2, ARow] := FloatToStrF(VAvgTime, ffFixed, 20, 8);
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
  sgrdDebugInfo.Cells[2, 0] := 'Redraw time avg, s';
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
