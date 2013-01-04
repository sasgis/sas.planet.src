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
  Messages,
  SysUtils,
  Classes,
  Controls,
  Forms,
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
    btnSaveToFile: TButton;
    btnCopyToClipboard: TButton;
    chkHideEmtyRows: TCheckBox;
    chkAutoRefresh: TCheckBox;
    tmrRefresh: TTimer;
    chkAlphaBlend: TCheckBox;
    procedure btnRefreshClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnSaveToFileClick(Sender: TObject);
    procedure btnCopyToClipboardClick(Sender: TObject);
    procedure tmrRefreshTimer(Sender: TObject);
    procedure chkAutoRefreshClick(Sender: TObject);
    procedure chkAlphaBlendClick(Sender: TObject);
  private
    FCounterNamesCache: TStringList;
    FPerfCounterList: IInternalPerformanceCounterList;
    FPrevStateList: IIDInterfaceList;
    procedure UpdateNamesCache;
    procedure UpdateNamesFromList(const AParentName: string; const AList: IInternalPerformanceCounterList);
    procedure UpdateNamesFromCounter(const AName: string; const ACounter: IInternalPerformanceCounter);
    procedure UpdateGrid;
    function UpdateGridRow(
      ARow: Integer;
      const AName: string;
      const APrevData: IInternalPerformanceCounterStaticData;
      const ACurrData: IInternalPerformanceCounterStaticData
    ): Boolean;

    procedure PrepareGridHeader;
    function GetGridLinesText(const ATop, ABottom: Integer): String;
  public
    constructor Create(AOwner: TComponent; const APerfCounterList: IInternalPerformanceCounterList); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  Dialogs,
  u_Clipboard,
  ActiveX;

{$R *.dfm}

constructor TfrmDebugInfo.Create(
  AOwner: TComponent;
  const APerfCounterList: IInternalPerformanceCounterList
);
begin
  inherited Create(AOwner);
  FPerfCounterList := APerfCounterList;
  FCounterNamesCache := TStringList.Create;
end;

destructor TfrmDebugInfo.Destroy;
begin
  FreeAndNil(FCounterNamesCache);
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
begin
  sgrdDebugInfo.RowCount := 2;
  FPrevStateList := FPerfCounterList.GetStaticDataList;
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

procedure TfrmDebugInfo.PrepareGridHeader;
begin
  sgrdDebugInfo.Cells[0, 0] := 'Class';
  sgrdDebugInfo.Cells[1, 0] := 'Count';
  sgrdDebugInfo.Cells[2, 0] := 'Time max, s';
  sgrdDebugInfo.Cells[3, 0] := 'Time avg, s';
  sgrdDebugInfo.Cells[4, 0] := 'Time min, s';
  sgrdDebugInfo.Cells[5, 0] := 'Time total';
end;

procedure TfrmDebugInfo.tmrRefreshTimer(Sender: TObject);
begin
  UpdateGrid;
end;

procedure TfrmDebugInfo.UpdateGrid;
var
  VCurrStaticData: IIDInterfaceList;
  i: Integer;
  VLastRow: Integer;
  VName: string;
  VPrevData, VCurrData: IInternalPerformanceCounterStaticData;
  VId: Integer;
  VUsedDataCount: Integer;
begin
  PrepareGridHeader;
  if FPerfCounterList = nil then begin
    Exit;
  end;

  VCurrStaticData := FPerfCounterList.GetStaticDataList;
  if VCurrStaticData.Count > FCounterNamesCache.Count then begin
    UpdateNamesCache;
  end;
  VUsedDataCount := 0;
  VLastRow := sgrdDebugInfo.FixedRows;
  for i := 0 to FCounterNamesCache.Count - 1 do begin
    VName := FCounterNamesCache.Strings[i];
    VId := Integer(FCounterNamesCache.Objects[i]);
    if FPrevStateList <> nil then begin
      VPrevData := IInternalPerformanceCounterStaticData(FPrevStateList.GetByID(VId));
    end else begin
      VPrevData := nil;
    end;
    VCurrData := IInternalPerformanceCounterStaticData(VCurrStaticData.GetByID(VId));
    if VCurrData <> nil then begin
      inc(VUsedDataCount);
    end;
    if UpdateGridRow(VLastRow, VName, VPrevData, VCurrData) then begin
      Inc(VLastRow);
    end;
  end;
  if VLastRow < sgrdDebugInfo.RowCount then begin
    sgrdDebugInfo.RowCount := VLastRow;
  end;
  if VUsedDataCount < VCurrStaticData.Count then begin
    UpdateNamesCache;
  end;
end;

function TfrmDebugInfo.UpdateGridRow(
  ARow: Integer;
  const AName: string;
  const APrevData, ACurrData: IInternalPerformanceCounterStaticData
): Boolean;
var
  VCount: Cardinal;
  VTime: TDateTime;
  VMax: TDateTime;
  VMin: TDateTime;
  VAvgTime: Extended;
begin
  Result := False;
  if ACurrData <> nil then begin
    VCount := ACurrData.Counter;
    VTime := ACurrData.TotalTime;
    VMax := ACurrData.MaxTime;
    VMin := ACurrData.MinTime;
  end else begin
    VCount := 0;
    VTime := 0;
    VMax := 0;
    VMin := 0;
  end;

  if APrevData <> nil then begin
    VCount := VCount - APrevData.Counter;
    VTime := VTime - APrevData.TotalTime;
  end;

  if not chkHideEmtyRows.Checked or (VCount > 0) then begin
    if sgrdDebugInfo.RowCount <= ARow then begin
      sgrdDebugInfo.RowCount := ARow + 1;
    end;
    sgrdDebugInfo.Cells[0, ARow] := AName;
    if VCount > 0 then begin
      sgrdDebugInfo.Cells[1, ARow] := IntToStr(VCount);
      VAvgTime := VTime/VCount*24*60*60;
      sgrdDebugInfo.Cells[2, ARow] := FloatToStrF(VMax*24*60*60, ffFixed, 20, 8);
      sgrdDebugInfo.Cells[3, ARow] := FloatToStrF(VAvgTime, ffFixed, 20, 8);
      sgrdDebugInfo.Cells[4, ARow] := FloatToStrF(VMin*24*60*60, ffFixed, 20, 8);
      sgrdDebugInfo.Cells[5, ARow] := FormatDateTime('nn:ss.zzz', VTime);
    end else begin
      sgrdDebugInfo.Cells[1, ARow] := '';
      sgrdDebugInfo.Cells[2, ARow] := '';
      sgrdDebugInfo.Cells[3, ARow] := '';
      sgrdDebugInfo.Cells[4, ARow] := '';
      sgrdDebugInfo.Cells[5, ARow] := '';
    end;
    Result := True;
  end;
end;

procedure TfrmDebugInfo.UpdateNamesCache;
begin
  FCounterNamesCache.Clear;
  FCounterNamesCache.Duplicates := dupAccept;
  UpdateNamesFromList('', FPerfCounterList);
  FCounterNamesCache.Sort;
end;

procedure TfrmDebugInfo.UpdateNamesFromCounter(
  const AName: string;
  const ACounter: IInternalPerformanceCounter
);
begin
  FCounterNamesCache.AddObject(AName, TObject(ACounter.Id));
end;

procedure TfrmDebugInfo.UpdateNamesFromList(
  const AParentName: string;
  const AList: IInternalPerformanceCounterList
);
var
  VEnum: IEnumUnknown;
  VUnknown: IUnknown;
  VCounter: IInternalPerformanceCounter;
  VList: IInternalPerformanceCounterList;
  Vcnt: Integer;
  VName: string;
begin
  VEnum := AList.GetEunm;
  if VEnum <> nil then begin
    VName := AParentName + '/';
    while VEnum.Next(1, VUnknown, Addr(Vcnt)) = S_OK do begin
      if Supports(VUnknown, IInternalPerformanceCounter, VCounter) then begin
        UpdateNamesFromCounter(VName + VCounter.Name, VCounter);
      end else if Supports(VUnknown, IInternalPerformanceCounterList, VList) then begin
        UpdateNamesFromList(VName + VList.Name, VList);
      end;
    end;
  end;
end;

end.
