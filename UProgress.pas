unit UProgress;

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
  i_ILogForTaskThread,
  UResStrings,
  u_ThreadDownloadTiles;

type
  TFProgress = class(TCommonFormParent)
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
    procedure SetProgressForm;
    procedure UpdateProgressForm;
    procedure UpdateMemoProgressForm;
    function GetTimeEnd(loadAll,load:integer):String;
    function GetLenEnd(loadAll,obrab,loaded:integer;len:real):string;
    procedure ThreadFinish;
    procedure StopThread;
  public
    constructor Create(AOwner: TComponent; ADownloadThread: TThreadDownloadTiles; ALog: ILogForTaskThread); reintroduce; virtual;
    destructor Destroy; override;

    property DownloadThread: TThreadDownloadTiles read FDownloadThread;
  public
  end;

implementation

uses
  SysUtils,
  Unit1,
  u_GlobalState,
  u_GeoToStr;

{$R *.dfm}


procedure TFProgress.Button2Click(Sender: TObject);
begin
  StopThread;
  UpdateTimer.Enabled := false;
  close;
end;

procedure TFProgress.Button3Click(Sender: TObject);
begin
  Perform(wm_SysCommand, SC_MINIMIZE, 0)
end;

procedure TFProgress.Button1Click(Sender: TObject);
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

procedure TFProgress.FormCreate(Sender: TObject);
begin
  FStoped := false;
  FFinished := False;
end;

constructor TFProgress.Create(AOwner: TComponent;
  ADownloadThread: TThreadDownloadTiles; ALog: ILogForTaskThread);
begin
  inherited Create(AOwner);
  FDownloadThread := ADownloadThread;
  FLog := ALog;
  SetProgressForm;
end;

destructor TFProgress.Destroy;
begin
  StopThread;
  FreeAndNil(FDownloadThread);
  FLog := nil;
  inherited;
end;
procedure TFProgress.SetProgressForm;
begin
  RProgr.Max := FDownloadThread.TotalInRegion;
  RProgr.Progress1 := FDownloadThread.Downloaded;
  RProgr.Progress2 := FDownloadThread.Processed;
  LabelName0.Caption := SAS_STR_ProcessedNoMore+':';
  LabelValue0.Caption := inttostr(FDownloadThread.TotalInRegion)+' '+SAS_STR_files+' (х'+inttostr(FDownloadThread.Zoom + 1)+')';
  LabelName1.Caption := SAS_STR_AllProcessed;
  LabelName2.Caption := SAS_STR_AllLoad;
  LabelName3.Caption := SAS_STR_TimeRemained;
  LabelName4.Caption := SAS_STR_LoadRemained;
  Visible:=true;
end;

procedure TFProgress.UpdateProgressForm;
begin
  if FDownloadThread.Finished then begin
    if not FFinished then begin
      FFinished := True;
      UpdateTimer.Enabled := false;
      UpdateMemoProgressForm;
      Caption := SAS_MSG_LoadComplete+' ('+inttostr(round(FDownloadThread.Processed/FDownloadThread.TotalInRegion*100))+'%)';
      LabelValue1.Caption := inttostr(FDownloadThread.Processed)+' '+SAS_STR_files;
      LabelValue2.Caption := inttostr(FDownloadThread.Downloaded)+' ('+kb2KbMbGb(FDownloadThread.DownloadSize)+') '+SAS_STR_Files;
      LabelValue3.Caption := GetTimeEnd(FDownloadThread.TotalInRegion, FDownloadThread.Processed);
      LabelValue4.Caption := GetLenEnd(FDownloadThread.TotalInRegion, FDownloadThread.Processed, FDownloadThread.Downloaded, FDownloadThread.DownloadSize);
      RProgr.Progress1 := FDownloadThread.Processed;
      RProgr.Progress2 := FDownloadThread.Downloaded;
      Repaint;
      ThreadFinish;
    end;
  end else begin
    UpdateMemoProgressForm;
    if (FStoped) then begin
      Caption:=SAS_STR_Stop1+'... ('+inttostr(round(FDownloadThread.Processed/FDownloadThread.TotalInRegion*100))+'%)';
    end else begin
      Caption:=SAS_STR_LoadProcess+'... ('+inttostr(round(FDownloadThread.Processed/FDownloadThread.TotalInRegion*100))+'%)';
      Application.ProcessMessages;
      LabelValue1.Caption:=inttostr(FDownloadThread.Processed)+' '+SAS_STR_files;
      LabelValue2.Caption:=inttostr(FDownloadThread.Downloaded)+' ('+kb2KbMbGb(FDownloadThread.DownloadSize)+') '+SAS_STR_Files;
      LabelValue3.Caption:=GetTimeEnd(FDownloadThread.TotalInRegion, FDownloadThread.Processed);
      LabelValue4.Caption:=GetLenEnd(FDownloadThread.TotalInRegion, FDownloadThread.Processed, FDownloadThread.Downloaded, FDownloadThread.DownloadSize);
      UpdateMemoProgressForm;
      RProgr.Progress1 := FDownloadThread.Processed;
      RProgr.Progress2 := FDownloadThread.Downloaded;
    end;
  end;
end;

procedure TFProgress.UpdateMemoProgressForm;
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

function TFProgress.GetLenEnd(loadAll,obrab,loaded:integer;len:real):string;
begin
  if loaded=0 then begin
    result:='~  б';
    exit;
  end;
  Result:=kb2KbMbGb((len/loaded)*(loadAll-obrab));
end;

function TFProgress.GetTimeEnd(loadAll,load:integer):String;
var dd:integer;
    VElapsedTime: TDateTime;
begin
  if load=0 then begin
    result:='~';
    exit;
  end;
  VElapsedTime := FDownloadThread.ElapsedTime;
  dd := DaysBetween(VElapsedTime,(VElapsedTime*(loadAll/load)));
  Result:='';
  if dd > 0 then Result := inttostr(dd)+' дней, ';
  Result := Result+FormatDateTime('hh:nn:ss',(VElapsedTime*(loadAll / load))-VElapsedTime);
end;

procedure TFProgress.UpdateTimerTimer(Sender: TObject);
begin
  UpdateProgressForm
end;

procedure TFProgress.ButtonSaveClick(Sender: TObject);
begin
  if (SaveSessionDialog.Execute)and(SaveSessionDialog.FileName<>'') then begin
    FDownloadThread.SaveToFile(SaveSessionDialog.FileName);
  end;
end;

procedure TFProgress.ThreadFinish;
begin
  GState.MainFileCache.Clear;
  FMain.generate_im;
end;

procedure TFProgress.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  UpdateTimer.Enabled := false;
  Action := caFree;
end;

procedure TFProgress.StopThread;
var
  VWaitResult: DWORD;
begin
  FDownloadThread.Terminate;
  Application.ProcessMessages;
  VWaitResult := WaitForSingleObject(FDownloadThread.Handle, 10000);
  if VWaitResult = WAIT_TIMEOUT then begin
    TerminateThread(FDownloadThread.Handle, 0);
  end;
end;

end.
