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
  Buttons,
  ComCtrls,
  DateUtils,
  RarProgress,
  u_CommonFormAndFrameParents,
  i_LogSimpleProvider,
  i_LanguageManager,
  i_ValueToStringConverter,
  u_ThreadDownloadTiles;

type
  TfrmProgressDownload = class(TFormWitghLanguageManager)
    Panel1: TPanel;
    mmoLog: TMemo;
    lblToProcessValue: TLabel;
    lblProcessedValue: TLabel;
    lblDownloadedValue: TLabel;
    lblTimeToFinishValue: TLabel;
    lblToProcess: TLabel;
    lblProcessed: TLabel;
    lblDownloaded: TLabel;
    lblTimeToFinish: TLabel;
    lblSizeToFinish: TLabel;
    lblSizeToFinishValue: TLabel;
    SaveSessionDialog: TSaveDialog;
    UpdateTimer: TTimer;
    btnMinimize: TButton;
    btnSave: TButton;
    btnPause: TButton;
    btnClose: TButton;
    pnlBottom: TPanel;
    pnlProgress: TPanel;
    chkAutoCloseWhenFinish: TCheckBox;
    pnlToProcess: TPanel;
    pnlProcessed: TPanel;
    pnlDownloaded: TPanel;
    pnlSizeToFinish: TPanel;
    pnlTimeToFinish: TPanel;
    procedure btnCloseClick(Sender: TObject);
    procedure btnMinimizeClick(Sender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure UpdateTimerTimer(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Panel1Resize(Sender: TObject);
  private
    FValueToStringConverterConfig: IValueToStringConverterConfig;
    FDownloadThread: TThreadDownloadTiles;
    FLog: ILogSimpleProvider;
    FLastLogID: Cardinal;
    FStoped: boolean;
    FFinished: Boolean;
    FProgress: TRarProgress;
    procedure UpdateProgressForm;
    procedure UpdateMemoProgressForm;
    function GetTimeEnd(loadAll,load:integer; AElapsedTime: TDateTime):String;
    function GetLenEnd(loadAll,obrab,loaded:integer;len:real):string;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AValueToStringConverterConfig: IValueToStringConverterConfig;
      ADownloadThread: TThreadDownloadTiles;
      const ALog: ILogSimpleProvider
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  Graphics,
  IniFiles,
  i_ConfigDataWriteProvider,
  u_ResStrings,
  u_ConfigDataWriteProviderByIniFile;

{$R *.dfm}

constructor TfrmProgressDownload.Create(
  const ALanguageManager: ILanguageManager;
  const AValueToStringConverterConfig: IValueToStringConverterConfig;
  ADownloadThread: TThreadDownloadTiles;
  const ALog: ILogSimpleProvider
);
begin
  inherited Create(ALanguageManager);
  FValueToStringConverterConfig := AValueToStringConverterConfig;
  FDownloadThread := ADownloadThread;
  FLog := ALog;
  FProgress := TRarProgress.Create(Self);
  with FProgress do begin
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
  FProgress.Parent := pnlProgress;
  FProgress.Align := alClient;
  FProgress.Max := FDownloadThread.TotalInRegion;
  FProgress.Progress1 := FDownloadThread.Downloaded;
  FProgress.Progress2 := FDownloadThread.Processed;
  if FDownloadThread.PausedByUser then begin
    FStoped := true;
    btnPause.Caption := SAS_STR_Continue;
  end else begin
    FStoped := false;
    btnPause.Caption := SAS_STR_Pause;
  end;
  FFinished := False;
end;

destructor TfrmProgressDownload.Destroy;
begin
  FDownloadThread.Terminate;
  Application.ProcessMessages;
  FreeAndNil(FDownloadThread);
  FLog := nil;
  FreeAndNil(FProgress);
  inherited;
end;

procedure TfrmProgressDownload.btnCloseClick(Sender: TObject);
begin
  FDownloadThread.Terminate;
  UpdateTimer.Enabled := false;
  Close;
end;

procedure TfrmProgressDownload.btnMinimizeClick(Sender: TObject);
begin
  Perform(wm_SysCommand, SC_MINIMIZE, 0)
end;

procedure TfrmProgressDownload.btnPauseClick(Sender: TObject);
begin
  if FStoped then begin
    FDownloadThread.DownloadResume;
    FStoped := false;
    btnPause.Caption := SAS_STR_Pause;
  end else begin
    FDownloadThread.DownloadPause;
    FStoped := true;
    btnPause.Caption := SAS_STR_Continue;
  end
end;

procedure TfrmProgressDownload.Panel1Resize(Sender: TObject);
begin
  FProgress.Top:=TPanel(sender).Height-48;
  FProgress.Width:=TPanel(sender).Width-14;
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
      lblToProcessValue.Caption := inttostr(FDownloadThread.TotalInRegion)+' '+SAS_STR_files+' (z'+inttostr(FDownloadThread.Zoom + 1)+')';
      lblProcessedValue.Caption := inttostr(FDownloadThread.Processed)+' '+SAS_STR_files;
      lblDownloadedValue.Caption := inttostr(FDownloadThread.Downloaded)+' ('+ VValueConverter.DataSizeConvert(FDownloadThread.DownloadSize)+') '+SAS_STR_Files;
      lblTimeToFinishValue.Caption := GetTimeEnd(FDownloadThread.TotalInRegion, FDownloadThread.Processed, FDownloadThread.ElapsedTime);
      lblSizeToFinishValue.Caption := GetLenEnd(FDownloadThread.TotalInRegion, FDownloadThread.Processed, FDownloadThread.Downloaded, FDownloadThread.DownloadSize);
      FProgress.Max := FDownloadThread.TotalInRegion;
      FProgress.Progress1 := FDownloadThread.Processed;
      FProgress.Progress2 := FDownloadThread.Downloaded;
      Repaint;
      if chkAutoCloseWhenFinish.Checked then begin
        btnCloseClick(nil);
      end;
    end;
  end else begin
    UpdateMemoProgressForm;
    if (FStoped) then begin
      Caption:=SAS_STR_Stop1+'... ('+VComplete+')';
    end else begin
      Caption:=SAS_STR_LoadProcess+'... ('+VComplete+')';
      Application.ProcessMessages;
      lblToProcessValue.Caption := inttostr(FDownloadThread.TotalInRegion)+' '+SAS_STR_files+' (z'+inttostr(FDownloadThread.Zoom + 1)+')';
      lblProcessedValue.Caption:=inttostr(FDownloadThread.Processed)+' '+SAS_STR_files;
      lblDownloadedValue.Caption:=inttostr(FDownloadThread.Downloaded)+' ('+VValueConverter.DataSizeConvert(FDownloadThread.DownloadSize)+') '+SAS_STR_Files;
      lblTimeToFinishValue.Caption := GetTimeEnd(FDownloadThread.TotalInRegion, FDownloadThread.Processed, FDownloadThread.ElapsedTime);
      lblSizeToFinishValue.Caption:=GetLenEnd(FDownloadThread.TotalInRegion, FDownloadThread.Processed, FDownloadThread.Downloaded, FDownloadThread.DownloadSize);
      UpdateMemoProgressForm;
      FProgress.Max := FDownloadThread.TotalInRegion;
      FProgress.Progress1 := FDownloadThread.Processed;
      FProgress.Progress2 := FDownloadThread.Downloaded;
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
    if mmoLog.Lines.Count>5000 then begin
      mmoLog.Lines.Clear;
    end;
   mmoLog.Lines.Add(VAddToMemo);
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

procedure TfrmProgressDownload.btnSaveClick(Sender: TObject);
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

end.
