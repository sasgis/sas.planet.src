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
    procedure btnRefreshClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnSaveToFileClick(Sender: TObject);
    procedure btnCopyToClipboardClick(Sender: TObject);
  private
    FPerfCounterList: IInternalPerformanceCounterList;
    FPrevStateList: IIDInterfaceList;
    procedure AddRowFromCounter(AName: string; ARow: Integer; ACounter: IInternalPerformanceCounter);
    function AddRowsFromList(AParentName: string; AStartRaw: Integer; AList: IInternalPerformanceCounterList): Integer;
    procedure PrepareGridHeader;
    procedure RefreshData;
    function GetGridLinesText(const ATop, ABottom: Integer): String;
  public
    constructor Create(AOwner: TComponent; APerfCounterList: IInternalPerformanceCounterList); reintroduce;
  end;

implementation

uses
  Dialogs,
  u_Clipboard,
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

procedure TfrmDebugInfo.btnCopyToClipboardClick(Sender: TObject);
var
  VText: String;
begin
  VText := GetGridLinesText(sgrdDebugInfo.Selection.Top, sgrdDebugInfo.Selection.Bottom);
  CopyStringToClipboard(Handle, VText);
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
