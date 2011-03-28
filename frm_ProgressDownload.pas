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
  u_MapType,
  u_ResStrings,
  u_ThreadDownloadTiles;

type
  TfrmProgressDownload = class(TCommonFormParent)
    Panel1: TPanel;
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    RProgr: TRarProgress;
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
    ButtonSave: TButton;
    UpdateTimer: TTimer;
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure UpdateTimerTimer(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FDownloadThread: TThreadDownloadTiles;
    FLog: ILogForTaskThread;
    FLastLogID: Cardinal;
    FStoped: boolean;
    FFinished: Boolean;
    FMapUpdateEvent: TMapUpdateEvent;
    procedure InitProgressForm;
    procedure UpdateProgressForm;
    procedure UpdateMemoProgressForm;
    function GetTimeEnd(loadAll,load:integer; AElapsedTime: TDateTime):String;
    function GetLenEnd(loadAll,obrab,loaded:integer;len:real):string;
    procedure ThreadFinish;
    procedure StopThread;
  public
    constructor Create(
      AOwner: TComponent;
      ADownloadThread: TThreadDownloadTiles;
      ALog: ILogForTaskThread;
      AMapUpdateEvent: TMapUpdateEvent
    ); reintroduce; virtual;
    destructor Destroy; override;
    procedure RefreshTranslation; override;

    property DownloadThread: TThreadDownloadTiles read FDownloadThread;
  public
  end;

implementation

uses
  SysUtils,
  i_ValueToStringConverter,
  u_GlobalState;

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
    button1.Caption := SAS_STR_Stop;
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
  AOwner: TComponent;
  ADownloadThread: TThreadDownloadTiles;
  ALog: ILogForTaskThread;
  AMapUpdateEvent: TMapUpdateEvent
);
begin
  inherited Create(AOwner);
  FMapUpdateEvent := AMapUpdateEvent;
  FDownloadThread := ADownloadThread;
  FLog := ALog;
  InitProgressForm;
end;

destructor TfrmProgressDownload.Destroy;
begin
  StopThread;
  FreeAndNil(FDownloadThread);
  FLog := nil;
  inherited;
end;
procedure TfrmProgressDownload.InitProgressForm;
begin
  RProgr.Max := FDownloadThread.TotalInRegion;
  RProgr.Progress1 := FDownloadThread.Downloaded;
  RProgr.Progress2 := FDownloadThread.Processed;
  LabelName0.Caption := SAS_STR_ProcessedNoMore+':';
  LabelName1.Caption := SAS_STR_AllProcessed;
  LabelName2.Caption := SAS_STR_AllLoad;
  LabelName3.Caption := SAS_STR_TimeRemained;
  LabelName4.Caption := SAS_STR_LoadRemained;
  Visible:=true;
end;

procedure TfrmProgressDownload.RefreshTranslation;
begin
  inherited;
  LabelName0.Caption := SAS_STR_ProcessedNoMore+':';
  LabelName1.Caption := SAS_STR_AllProcessed;
  LabelName2.Caption := SAS_STR_AllLoad;
  LabelName3.Caption := SAS_STR_TimeRemained;
  LabelName4.Caption := SAS_STR_LoadRemained;
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
  VValueConverter := GState.ValueToStringConverterConfig.GetStaticConverter;
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
      RProgr.Max := FDownloadThread.TotalInRegion;
      RProgr.Progress1 := FDownloadThread.Processed;
      RProgr.Progress2 := FDownloadThread.Downloaded;
      Repaint;
      ThreadFinish;
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
      RProgr.Max := FDownloadThread.TotalInRegion;
      RProgr.Progress1 := FDownloadThread.Processed;
      RProgr.Progress2 := FDownloadThread.Downloaded;
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
    result:='~  б';
  end else begin
    VValueConverter := GState.ValueToStringConverterConfig.GetStaticConverter;
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
begin
  if (SaveSessionDialog.Execute)and(SaveSessionDialog.FileName<>'') then begin
    FDownloadThread.SaveToFile(SaveSessionDialog.FileName);
  end;
end;

procedure TfrmProgressDownload.ThreadFinish;
begin
  if Addr(FMapUpdateEvent) <> nil then begin
    FMapUpdateEvent(FDownloadThread.MapType);
  end;
end;

procedure TfrmProgressDownload.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FDownloadThread.Terminate;
  UpdateTimer.Enabled := false;
  Action := caFree;
end;

procedure TfrmProgressDownload.StopThread;
var
  VWaitResult: DWORD;
begin
  FDownloadThread.Terminate;
  Application.ProcessMessages;
  VWaitResult := WaitForSingleObject(FDownloadThread.Handle, 1000);
  if VWaitResult = WAIT_TIMEOUT then begin
    TerminateThread(FDownloadThread.Handle, 0);
  end;
end;

end.
