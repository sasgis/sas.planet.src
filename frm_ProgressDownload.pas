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

unit frm_ProgressDownload;

interface

uses
  Forms,
  windows,
  messages,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  Controls,
  Classes,
  DateUtils,
  RarProgress,
  u_CommonFormAndFrameParents,
  i_LogForTaskThread,
  i_LanguageManager,
  i_ValueToStringConverter,
  u_ResStrings,
  u_ThreadDownloadTiles;

type
  TfrmProgressDownload = class(TFormWitghLanguageManager)
    Panel1: TPanel;
    Memo1: TMemo;
    LabelValue0: TLabel;
    LabelValue1: TLabel;
    LabelValue2: TLabel;
    LabelValue3: TLabel;
    LabelName0: TLabel;
    LabelName1: TLabel;
    LabelName2: TLabel;
    LabelName3: TLabel;
    LabelName4: TLabel;
    LabelValue4: TLabel;
    SaveSessionDialog: TSaveDialog;
    UpdateTimer: TTimer;
    Button3: TButton;
    ButtonSave: TButton;
    Button1: TButton;
    Button2: TButton;
    pnlBottom: TPanel;
    pnlProgress: TPanel;
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure UpdateTimerTimer(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Panel1Resize(Sender: TObject);
  private
    FValueToStringConverterConfig: IValueToStringConverterConfig;
    FDownloadThread: TThreadDownloadTiles;
    FLog: ILogForTaskThread;
    FLastLogID: Cardinal;
    FStoped: boolean;
    FFinished: Boolean;
    FRarProgress: TRarProgress;
    procedure InitProgressForm;
    procedure UpdateProgressForm;
    procedure UpdateMemoProgressForm;
    function GetTimeEnd(loadAll,load:integer; AElapsedTime: TDateTime):String;
    function GetLenEnd(loadAll,obrab,loaded:integer;len:real):string;
    procedure StopThread;
  public
    constructor Create(
      ALanguageManager: ILanguageManager;
      AValueToStringConverterConfig: IValueToStringConverterConfig;
      ADownloadThread: TThreadDownloadTiles;
      ALog: ILogForTaskThread
    ); reintroduce; virtual;
    destructor Destroy; override;

    property DownloadThread: TThreadDownloadTiles read FDownloadThread;
  public
  end;

implementation

uses
  SysUtils,
  Graphics,
  IniFiles,
  i_ConfigDataWriteProvider,
  u_ConfigDataWriteProviderByIniFile;

{$R *.dfm}

procedure TfrmProgressDownload.Button2Click(Sender: TObject);
begin
  FDownloadThread.Terminate;
  UpdateTimer.Enabled := false;
  close;
end;

procedure TfrmProgressDownload.Button3Click(Sender: TObject);
begin
  Perform(wm_SysCommand, SC_MINIMIZE, 0)
end;

procedure TfrmProgressDownload.Button1Click(Sender: TObject);
begin
  if FStoped then begin
    FDownloadThread.DownloadResume;
    FStoped := false;
    button1.Caption := SAS_STR_Pause;
  end else begin
    FDownloadThread.DownloadPause;
    FStoped := true;
    button1.Caption := SAS_STR_Continue;
  end
end;

procedure TfrmProgressDownload.FormCreate(Sender: TObject);
begin
  FStoped := false;
  FFinished := False;
end;

constructor TfrmProgressDownload.Create(
  ALanguageManager: ILanguageManager;
  AValueToStringConverterConfig: IValueToStringConverterConfig;
  ADownloadThread: TThreadDownloadTiles;
  ALog: ILogForTaskThread
);
begin
  inherited Create(ALanguageManager);
  FValueToStringConverterConfig := AValueToStringConverterConfig;
  FDownloadThread := ADownloadThread;
  FLog := ALog;
  FRarProgress := TRarProgress.Create(Self);
  with FRarProgress do begin
    Height := 17;
    Min := 0;
    Max := 100;
    Progress1 := 0;
    Progress2 := 0;
    Double := True;
    LightColor1 := 16770764;
    DarkColor1 := 13395456;
    LightColor2 := 16768959;
    FrameColor1 := 16758122;
    FrameColor2 := 16747546;
    FillColor1 := 16757606;
    FillColor2 := 16749867;
    BackFrameColor1 := 16633762;
    BackFrameColor2 := 16634540;
    BackFillColor := 16635571;
    ShadowColor := clGray;
  end;
  FRarProgress.Parent := pnlProgress;
  FRarProgress.Align := alClient;
  InitProgressForm;
end;

destructor TfrmProgressDownload.Destroy;
begin
  StopThread;
  FreeAndNil(FDownloadThread);
  FLog := nil;
  FreeAndNil(FRarProgress);
  inherited;
end;
procedure TfrmProgressDownload.InitProgressForm;
begin
  FRarProgress.Max := FDownloadThread.TotalInRegion;
  FRarProgress.Progress1 := FDownloadThread.Downloaded;
  FRarProgress.Progress2 := FDownloadThread.Processed;
  Visible:=true;
end;

procedure TfrmProgressDownload.Panel1Resize(Sender: TObject);
begin
  FRarProgress.Top:=TPanel(sender).Height-48;
  FRarProgress.Width:=TPanel(sender).Width-14;
end;

procedure TfrmProgressDownload.UpdateProgressForm;
var
  VComplete: string;
  VValueConverter: IValueToStringConverter;
begin
  if FDownloadThread.TotalInRegion > 0 then begin
    VComplete := inttostr(round(FDownloadThread.Processed/FDownloadThread.TotalInRegion*100))+'%';
  end else begin
    VComplete := '~%';
  end;
  VValueConverter := FValueToStringConverterConfig.GetStatic;
  if FDownloadThread.Finished then begin
    if not FFinished then begin
      FFinished := True;
      UpdateTimer.Enabled := false;
      UpdateMemoProgressForm;
      Caption := SAS_MSG_LoadComplete+' ('+VComplete+')';
      LabelValue0.Caption := inttostr(FDownloadThread.TotalInRegion)+' '+SAS_STR_files+' (z'+inttostr(FDownloadThread.Zoom + 1)+')';
      LabelValue1.Caption := inttostr(FDownloadThread.Processed)+' '+SAS_STR_files;
      LabelValue2.Caption := inttostr(FDownloadThread.Downloaded)+' ('+ VValueConverter.DataSizeConvert(FDownloadThread.DownloadSize)+') '+SAS_STR_Files;
      LabelValue3.Caption := GetTimeEnd(FDownloadThread.TotalInRegion, FDownloadThread.Processed, FDownloadThread.ElapsedTime);
      LabelValue4.Caption := GetLenEnd(FDownloadThread.TotalInRegion, FDownloadThread.Processed, FDownloadThread.Downloaded, FDownloadThread.DownloadSize);
      FRarProgress.Max := FDownloadThread.TotalInRegion;
      FRarProgress.Progress1 := FDownloadThread.Processed;
      FRarProgress.Progress2 := FDownloadThread.Downloaded;
      Repaint;
    end;
  end else begin
    UpdateMemoProgressForm;
    if (FStoped) then begin
      Caption:=SAS_STR_Stop1+'... ('+VComplete+')';
    end else begin
      Caption:=SAS_STR_LoadProcess+'... ('+VComplete+')';
      Application.ProcessMessages;
      LabelValue0.Caption := inttostr(FDownloadThread.TotalInRegion)+' '+SAS_STR_files+' (z'+inttostr(FDownloadThread.Zoom + 1)+')';
      LabelValue1.Caption:=inttostr(FDownloadThread.Processed)+' '+SAS_STR_files;
      LabelValue2.Caption:=inttostr(FDownloadThread.Downloaded)+' ('+VValueConverter.DataSizeConvert(FDownloadThread.DownloadSize)+') '+SAS_STR_Files;
      LabelValue3.Caption := GetTimeEnd(FDownloadThread.TotalInRegion, FDownloadThread.Processed, FDownloadThread.ElapsedTime);
      LabelValue4.Caption:=GetLenEnd(FDownloadThread.TotalInRegion, FDownloadThread.Processed, FDownloadThread.Downloaded, FDownloadThread.DownloadSize);
      UpdateMemoProgressForm;
      FRarProgress.Max := FDownloadThread.TotalInRegion;
      FRarProgress.Progress1 := FDownloadThread.Processed;
      FRarProgress.Progress2 := FDownloadThread.Downloaded;
    end;
  end;
end;

procedure TfrmProgressDownload.UpdateMemoProgressForm;
var
  i: Cardinal;
  VAddToMemo: String;
begin
  VAddToMemo := FLog.GetLastMessages(100, FLastLogID, i);
  if i > 0 then begin
    if Memo1.Lines.Count>5000 then begin
      Memo1.Lines.Clear;
    end;
   Memo1.Lines.Add(VAddToMemo);
  end;
end;

function TfrmProgressDownload.GetLenEnd(loadAll,obrab,loaded:integer;len:real):string;
var
  VValueConverter: IValueToStringConverter;
begin
  if loaded=0 then begin
    result:='~ Кб';
  end else begin
    VValueConverter := FValueToStringConverterConfig.GetStatic;
    Result:= VValueConverter.DataSizeConvert((len/loaded)*(loadAll-obrab));
  end;
end;

function TfrmProgressDownload.GetTimeEnd(loadAll,load:integer; AElapsedTime: TDateTime):String;
var
  dd:integer;
  VExpectedTime: TDateTime;
begin
  if load=0 then begin
    result:='~';
  end else begin
    VExpectedTime := AElapsedTime * (loadAll / load);
    dd := DaysBetween(AElapsedTime, VExpectedTime);
    Result:='';
    if dd > 0 then Result := inttostr(dd)+' дней, ';
    Result := Result+FormatDateTime('hh:nn:ss',VExpectedTime - AElapsedTime);
  end;
end;

procedure TfrmProgressDownload.UpdateTimerTimer(Sender: TObject);
begin
  UpdateProgressForm
end;

procedure TfrmProgressDownload.ButtonSaveClick(Sender: TObject);
var
  VFileName: string;
  VIni: TMemIniFile;
  VSLSData: IConfigDataWriteProvider;
  VSessionSection: IConfigDataWriteProvider;
begin
  if SaveSessionDialog.Execute then begin
    VFileName := SaveSessionDialog.FileName;
    if VFileName <> '' then begin
      VIni := TMemIniFile.Create(VFileName);
      VSLSData := TConfigDataWriteProviderByIniFile.Create(VIni);
      VSessionSection := VSLSData.GetOrCreateSubItem('Session');
      FDownloadThread.SaveToFile(VSessionSection);
      VIni.UpdateFile;
    end;
  end;
end;

procedure TfrmProgressDownload.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FDownloadThread.Terminate;
  UpdateTimer.Enabled := false;
  Action := caFree;
end;

procedure TfrmProgressDownload.StopThread;
begin
  FDownloadThread.Terminate;
  Application.ProcessMessages;
end;

end.
