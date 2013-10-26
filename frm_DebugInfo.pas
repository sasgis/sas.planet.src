{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit frm_DebugInfo;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  ExtCtrls,
  Grids,
  Menus,
  i_IDList,
  i_InterfaceListStatic,
  i_InterfaceListSimple,
  i_InternalPerformanceCounter,
  i_DebugInfoSubSystem;

type
  TfrmDebugInfo = class(TForm)
    sgrdDebugInfo: TStringGrid;
    pnlBottom: TPanel;
    btnRefresh: TButton;
    btnReset: TButton;
    btnSaveToFile: TButton;
    btnCopyToClipboard: TButton;
    chkHideEmtyRows: TCheckBox;
    chkAutoRefresh: TCheckBox;
    tmrRefresh: TTimer;
    chkAlphaBlend: TCheckBox;
    pmFiltering: TPopupMenu;
    pmiCountIsGreaterOrEqual: TMenuItem;
    pmiCountReset: TMenuItem;
    pmiSep1: TMenuItem;
    pmiTotalIsGreaterOrEqual: TMenuItem;
    pmiTotalReset: TMenuItem;
    lblFiltering: TLabel;
    pmiSortBy: TMenuItem;
    pmiSortByTotalTime: TMenuItem;
    pmiSortByTotalAvg: TMenuItem;
    pmiSortByTotalCount: TMenuItem;
    pmiSortByUiTime: TMenuItem;
    pmiSortByUICount: TMenuItem;
    pmiSortByUiAvg: TMenuItem;
    pmiSortByName: TMenuItem;
    pmiSep2: TMenuItem;
    procedure btnRefreshClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnSaveToFileClick(Sender: TObject);
    procedure btnCopyToClipboardClick(Sender: TObject);
    procedure tmrRefreshTimer(Sender: TObject);
    procedure chkAutoRefreshClick(Sender: TObject);
    procedure chkAlphaBlendClick(Sender: TObject);
    procedure pmFilteringPopup(Sender: TObject);
    procedure pmiCountResetClick(Sender: TObject);
    procedure pmiTotalResetClick(Sender: TObject);
    procedure pmiCountIsGreaterOrEqualClick(Sender: TObject);
    procedure pmiTotalIsGreaterOrEqualClick(Sender: TObject);
    procedure SortByClick(Sender: TObject);
  private
    FDebugInfoSubSystem: IDebugInfoSubSystem;
    FPrevStateList: IIDInterfaceList;
    FCurrStateList: IInterfaceListSimple;
  private
    FSortIndex: Integer;
    FMenuFiltering_MinimumCount: Integer;
    FMenuFiltering_EnabledCount: Boolean;
    FMenuFiltering_MinimumTotal: Double;
    FMenuFiltering_EnabledTotal: Boolean;
  private
    procedure UpdateMenuFiltering;
    procedure UpdateGrid;
    function UpdateGridRow(
      const ARow: Integer;
      const AName: string;
      const APrevData: IInternalPerformanceCounterStaticData;
      const ACurrData: IInternalPerformanceCounterStaticData
    ): Boolean;
    function GetPopupRow: Integer;
    procedure SortDataForGrid;

    procedure PrepareGridHeader;
    function GetGridLinesText(const ATop, ABottom: Integer): String;
  public
    constructor Create(AOwner: TComponent; const ADebugInfoSubSystem: IDebugInfoSubSystem); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  Dialogs,
  u_Clipboard,
  u_IDInterfaceList,
  u_InterfaceListSimple,
  u_SortFunc,
  u_GeoToStr;

function _DoubleToStr(const AValue: Double): String;
begin
  Result := FloatToStrF(AValue, ffFixed, 20, 8);
end;

function _TimeToStr(const ATime: TDateTime): String;
begin
  Result := FormatDateTime('nn:ss.zzz', ATime);
end;

{$R *.dfm}

constructor TfrmDebugInfo.Create(
  AOwner: TComponent;
  const ADebugInfoSubSystem: IDebugInfoSubSystem
);
begin
  inherited Create(AOwner);
  FDebugInfoSubSystem := ADebugInfoSubSystem;
  FMenuFiltering_EnabledCount := False;
  FMenuFiltering_EnabledTotal := False;
  FPrevStateList := TIDInterfaceList.Create(False);
  FCurrStateList := TInterfaceListSimple.Create;
  FSortIndex := 0;
end;

destructor TfrmDebugInfo.Destroy;
begin
  inherited;
end;

procedure TfrmDebugInfo.btnCopyToClipboardClick(Sender: TObject);
var
  VText: String;
begin
  VText := GetGridLinesText(sgrdDebugInfo.Selection.Top, sgrdDebugInfo.Selection.Bottom);
  CopyStringToClipboard(Handle, VText);
end;

procedure TfrmDebugInfo.btnRefreshClick(Sender: TObject);
begin
  UpdateGrid;
end;

procedure TfrmDebugInfo.btnResetClick(Sender: TObject);
var
  VList: IInterfaceListStatic;
  i: Integer;
  VItem: IInternalPerformanceCounterStaticData;
begin
  sgrdDebugInfo.RowCount := 2;
  VList := FDebugInfoSubSystem.GetStaticDataList;
  FPrevStateList.Clear;
  if Assigned(VList) then begin
    for i := 0 to VList.Count - 1 do begin
      VItem := IInternalPerformanceCounterStaticData(VList[i]);
      FPrevStateList.Add(VItem.Id, VItem);
    end;
  end;
  UpdateGrid;
end;

procedure TfrmDebugInfo.btnSaveToFileClick(Sender: TObject);
var
  VText, VFileName: String;
  VSL: TStringList;
begin
  VFileName := '';
  VText := GetGridLinesText(0, sgrdDebugInfo.RowCount-1);

  with TSaveDialog.Create(Self) do
  try
    if Execute(Handle) then
      VFileName := FileName;
  finally
    Free;
  end;

  if (0<Length(VFileName)) then begin
    VSL:=TStringList.Create;
    try
      VSL.Text := VText;
      VSL.SaveToFile(VFileName);
    finally
      VSL.Free;
    end;
  end;
end;

procedure TfrmDebugInfo.chkAlphaBlendClick(Sender: TObject);
begin
  Self.AlphaBlend := chkAlphaBlend.Checked;
end;

procedure TfrmDebugInfo.chkAutoRefreshClick(Sender: TObject);
begin
  tmrRefresh.Enabled := chkAutoRefresh.Checked;
end;

procedure TfrmDebugInfo.FormCreate(Sender: TObject);
begin
  sgrdDebugInfo.ColWidths[0] := 360;
  sgrdDebugInfo.RowCount := 2;
  sgrdDebugInfo.FixedRows := 1;
end;

procedure TfrmDebugInfo.FormShow(Sender: TObject);
begin
  UpdateMenuFiltering;
  UpdateGrid;
end;

function TfrmDebugInfo.GetGridLinesText(const ATop, ABottom: Integer): String;

  procedure _AddLine(const AIndex: Integer);
  var S: String;
  begin
    if (0<Length(Result)) then
      Result := Result + #13#10;

    S := sgrdDebugInfo.Rows[AIndex].Text;
    S := StringReplace(S, #13, Chr(VK_TAB), [rfReplaceAll]);
    S := StringReplace(S, #10, Chr(VK_TAB), [rfReplaceAll]);
    Result := Result + S;
  end;

var
  i: Integer;
  VAddCurrent: Boolean;

begin
  VAddCurrent := TRUE;
  Result := '';

  for i := ATop to ABottom do begin
    if (i=sgrdDebugInfo.Row) then
      VAddCurrent:=FALSE;

    _AddLine(i);
  end;

  if VAddCurrent then
    _AddLine(sgrdDebugInfo.Row);
end;

function TfrmDebugInfo.GetPopupRow: Integer;
var
  VPoint: TPoint;
  VCol: Integer;
begin
  // get line
  VPoint := pmFiltering.PopupPoint;
  VPoint := sgrdDebugInfo.ScreenToClient(VPoint);
  sgrdDebugInfo.MouseToCell(VPoint.X, VPoint.Y, VCol, Result);
end;

procedure TfrmDebugInfo.pmFilteringPopup(Sender: TObject);
begin
  pmiCountReset.Visible := FMenuFiltering_EnabledCount;
  if FMenuFiltering_EnabledCount then begin
    pmiCountReset.Caption := pmiCountReset.Hint + ' (>=' + IntToStr(FMenuFiltering_MinimumCount) + ')';
  end;

  pmiTotalReset.Visible := FMenuFiltering_EnabledTotal;
  if FMenuFiltering_EnabledTotal then begin
    pmiTotalReset.Caption := pmiTotalReset.Hint + ' (>=' + _TimeToStr(FMenuFiltering_MinimumTotal) + ')';
  end;
end;

procedure TfrmDebugInfo.pmiCountIsGreaterOrEqualClick(Sender: TObject);
var
  VRow: Integer;
  VText: String;
begin
  VRow := GetPopupRow;
  if (VRow>=0) then
  try
    // get Count value
    VText := sgrdDebugInfo.Cells[1, VRow];
    if TryStrToInt(VText, VRow) then begin
      FMenuFiltering_MinimumCount := VRow;
      FMenuFiltering_EnabledCount := True;
      UpdateMenuFiltering;
    end;
  except
  end;

  if FMenuFiltering_EnabledCount then begin
    UpdateGrid;
  end;
end;

procedure TfrmDebugInfo.pmiCountResetClick(Sender: TObject);
begin
  FMenuFiltering_EnabledCount := False;
  UpdateMenuFiltering;
  UpdateGrid;
end;

procedure TfrmDebugInfo.pmiTotalIsGreaterOrEqualClick(Sender: TObject);
var
  VRow, VInt: Integer;
  VText: String;
  VValue: Double;
begin
  VRow := GetPopupRow;
  if (VRow>=0) then
  try
    // get Total value ('nn:ss.zzz')
    VText := sgrdDebugInfo.Cells[5, VRow];
    // get before ':'
    VRow := Pos(':', VText);
    if (VRow > 0) then begin
      VInt := StrToInt(Copy(VText, 1, VRow-1));
      Delete(VText, 1, VRow);
    end else begin
      VInt := 0;
    end;

    if TryStrPointToFloat(VText, VValue) then begin
      FMenuFiltering_MinimumTotal := (VValue + VInt) / (24*60*60);
      FMenuFiltering_EnabledTotal := True;
      UpdateMenuFiltering;
    end;
  except
  end;

  if FMenuFiltering_EnabledTotal then begin
    UpdateGrid;
  end;
end;

procedure TfrmDebugInfo.pmiTotalResetClick(Sender: TObject);
begin
  FMenuFiltering_EnabledTotal := False;
  UpdateMenuFiltering;
  UpdateGrid;
end;

procedure TfrmDebugInfo.PrepareGridHeader;
begin
  sgrdDebugInfo.Cells[0, 0] := 'Class';
  sgrdDebugInfo.Cells[1, 0] := 'Count';
  sgrdDebugInfo.Cells[2, 0] := 'Time avg, s';
  sgrdDebugInfo.Cells[3, 0] := 'Time total';
  sgrdDebugInfo.Cells[4, 0] := 'UI Count';
  sgrdDebugInfo.Cells[5, 0] := 'UI Time avg, s';
  sgrdDebugInfo.Cells[6, 0] := 'UI Time total';
  sgrdDebugInfo.Cells[7, 0] := 'Time max, s';
  sgrdDebugInfo.Cells[8, 0] := 'Time min, s';
end;

procedure TfrmDebugInfo.tmrRefreshTimer(Sender: TObject);
begin
  UpdateGrid;
end;

procedure TfrmDebugInfo.SortByClick(Sender: TObject);
var
  VSortIndex: Integer;
begin
  VSortIndex := FSortIndex;
  if Assigned(Sender) and (Sender is TComponent) then begin
    VSortIndex := TComponent(Sender).Tag;
    if (VSortIndex < 0) or (VSortIndex > 6) then begin
      VSortIndex := FSortIndex;
    end;
  end;
  if FSortIndex <> VSortIndex then begin
    FSortIndex := VSortIndex;
    UpdateGrid;
  end;
end;

function CompareDataNames(const Item1, Item2: IInterface): Integer;
begin
  Result :=
    CompareStr(
      IInternalPerformanceCounterStaticData(Item1).Name,
      IInternalPerformanceCounterStaticData(Item2).Name
    );
end;

procedure TfrmDebugInfo.SortDataForGrid;
var
  VSortMeasureInteger: array of Integer;
  VSortMeasureDouble: array of Double;
  i: Integer;
  VPrevData, VCurrData: IInternalPerformanceCounterStaticData;
  VId: Integer;
  VValueInteger: Integer;
  VValueDouble: Double;
begin
  if FSortIndex = 0 then begin
    SortInterfaceListByCompareFunction(
      FCurrStateList,
      CompareDataNames
    );
  end else begin
    if FSortIndex in [1, 4] then begin
      SetLength(VSortMeasureInteger, FCurrStateList.Count);
    end else begin
      SetLength(VSortMeasureDouble, FCurrStateList.Count);
    end;
    for i := 0 to FCurrStateList.Count - 1 do begin
      VCurrData := IInternalPerformanceCounterStaticData(FCurrStateList.Items[i]);
      VId := VCurrData.Id;
      VPrevData := IInternalPerformanceCounterStaticData(FPrevStateList.GetByID(VId));
      if FSortIndex in [1, 2, 3] then begin
        VValueInteger := VCurrData.Counter;
        VValueDouble := VCurrData.TotalTime;
      end else begin
        VValueInteger := VCurrData.CounterInMain;
        VValueDouble := VCurrData.TotalTimeInMain;
      end;
      if Assigned(VPrevData) then begin
        if FSortIndex in [2, 3] then begin
          VValueInteger := VValueInteger - Integer(VPrevData.Counter);
          VValueDouble := VValueDouble - VPrevData.TotalTime;
        end else begin
          VValueInteger := VValueInteger - Integer(VPrevData.CounterInMain);
          VValueDouble := VValueDouble - VPrevData.TotalTimeInMain;
        end;
      end;
      if FSortIndex in [1, 4] then begin
        VSortMeasureInteger[i] := -VValueInteger;
      end else if FSortIndex in [2, 5] then begin
        if VValueInteger <> 0 then begin
          VSortMeasureDouble[i] := - VValueDouble / VValueInteger;
        end else begin
          VSortMeasureDouble[i] := 0;
        end;
      end else begin
        VSortMeasureDouble[i] := -VValueDouble;
      end;
    end;
    if FSortIndex in [1, 4] then begin
      SortInterfaceListByIntegerMeasure(FCurrStateList, VSortMeasureInteger);
    end else begin
      SortInterfaceListByDoubleMeasure(FCurrStateList, VSortMeasureDouble);
    end;
  end;
end;

procedure TfrmDebugInfo.UpdateGrid;
var
  VCurrStaticData: IInterfaceListStatic;
  i: Integer;
  VLastRow: Integer;
  VName: string;
  VPrevData, VCurrData: IInternalPerformanceCounterStaticData;
  VId: Integer;
begin
  PrepareGridHeader;
  if FDebugInfoSubSystem = nil then begin
    Exit;
  end;

  VCurrStaticData := FDebugInfoSubSystem.GetStaticDataList;
  FCurrStateList.Clear;
  FCurrStateList.AddListStatic(VCurrStaticData);
  SortDataForGrid;

  VLastRow := sgrdDebugInfo.FixedRows;
  for i := 0 to FCurrStateList.Count - 1 do begin
    VCurrData := IInternalPerformanceCounterStaticData(FCurrStateList.Items[i]);
    VName := VCurrData.Name;
    VId := VCurrData.Id;
    VPrevData := IInternalPerformanceCounterStaticData(FPrevStateList.GetByID(VId));
    if UpdateGridRow(VLastRow, VName, VPrevData, VCurrData) then begin
      Inc(VLastRow);
    end;
  end;
  if VLastRow < sgrdDebugInfo.RowCount then begin
    sgrdDebugInfo.RowCount := VLastRow;
  end;
end;

function TfrmDebugInfo.UpdateGridRow(
  const ARow: Integer;
  const AName: string;
  const APrevData: IInternalPerformanceCounterStaticData;
  const ACurrData: IInternalPerformanceCounterStaticData
): Boolean;
var
  VCount: Cardinal;
  VTime: TDateTime;
  VCountInMain: Cardinal;
  VTimeInMain: TDateTime;
  VMax: TDateTime;
  VMin: TDateTime;
  VAvgTime: Extended;
begin
  Result := False;
  if ACurrData <> nil then begin
    VCount := ACurrData.Counter;
    VTime := ACurrData.TotalTime;
    VCountInMain := ACurrData.CounterInMain;
    VTimeInMain := ACurrData.TotalTimeInMain;
    VMax := ACurrData.MaxTime;
    VMin := ACurrData.MinTime;
  end else begin
    VCount := 0;
    VTime := 0;
    VCountInMain := 0;
    VTimeInMain := 0;
    VMax := 0;
    VMin := 0;
  end;

  if APrevData <> nil then begin
    VCount := VCount - APrevData.Counter;
    VTime := VTime - APrevData.TotalTime;
    VCountInMain := VCountInMain - APrevData.CounterInMain;
    VTimeInMain := VTimeInMain - APrevData.TotalTimeInMain;
  end;

  if not chkHideEmtyRows.Checked or (VCount > 0) then
  if (not FMenuFiltering_EnabledCount) or (Integer(VCount) >= FMenuFiltering_MinimumCount) then
  if (not FMenuFiltering_EnabledTotal) or (VTime >= FMenuFiltering_MinimumTotal) then begin
    if sgrdDebugInfo.RowCount <= ARow then begin
      sgrdDebugInfo.RowCount := ARow + 1;
    end;
    sgrdDebugInfo.Cells[0, ARow] := AName;
    if VCount > 0 then begin
      sgrdDebugInfo.Cells[1, ARow] := IntToStr(VCount);
      VAvgTime := VTime/VCount*24*60*60;
      sgrdDebugInfo.Cells[2, ARow] := _DoubleToStr(VAvgTime);
      sgrdDebugInfo.Cells[3, ARow] := _TimeToStr(VTime);
      if VCountInMain > 0 then begin
        sgrdDebugInfo.Cells[4, ARow] := IntToStr(VCountInMain);
        VAvgTime := VTimeInMain/VCountInMain*24*60*60;
        sgrdDebugInfo.Cells[5, ARow] := _DoubleToStr(VAvgTime);
        sgrdDebugInfo.Cells[6, ARow] := _TimeToStr(VTimeInMain);
      end else begin
        sgrdDebugInfo.Cells[4, ARow] := '';
        sgrdDebugInfo.Cells[5, ARow] := '';
        sgrdDebugInfo.Cells[6, ARow] := '';
      end;
      sgrdDebugInfo.Cells[7, ARow] := _DoubleToStr(VMax*24*60*60);
      sgrdDebugInfo.Cells[8, ARow] := _DoubleToStr(VMin*24*60*60);
    end else begin
      sgrdDebugInfo.Cells[1, ARow] := '';
      sgrdDebugInfo.Cells[2, ARow] := '';
      sgrdDebugInfo.Cells[3, ARow] := '';
      sgrdDebugInfo.Cells[4, ARow] := '';
      sgrdDebugInfo.Cells[5, ARow] := '';
      sgrdDebugInfo.Cells[6, ARow] := '';
      sgrdDebugInfo.Cells[7, ARow] := '';
      sgrdDebugInfo.Cells[8, ARow] := '';
    end;
    Result := True;
  end;
end;

procedure TfrmDebugInfo.UpdateMenuFiltering;
begin
  if FMenuFiltering_EnabledCount or FMenuFiltering_EnabledTotal then
    lblFiltering.Caption := lblFiltering.Hint
  else
    lblFiltering.Caption := '';
end;

end.
