unit u_ThreadExportAbstract;

interface

uses
  Classes,
  Forms,
  t_GeoTypes,
  unit4,
  UResStrings;

type
  TThreadExportAbstract = class(TThread)
  private
    FProgressForm: TFprogress2;
    FMessageForShow: string;
    FShowFormCaption: string;
    FShowOnFormLine0: string;
    FShowOnFormLine1: string;
    FProgressOnForm: integer;

    procedure UpdateProgressFormBar;
    procedure UpdateProgressFormCaption;
    procedure UpdateProgressFormStr0;
    procedure UpdateProgressFormStr1;

    procedure SynShowMessage;
    procedure UpdateProgressFormClose;
    procedure CloseFProgress(Sender: TObject; var Action: TCloseAction); virtual;
  protected
    FPolygLL: TExtendedPointArray;
    FZoomArr: array [0..23] of boolean;

    FTilesToProcess: Int64;
    FTilesProcessed: Int64;
    procedure ProgressFormUpdateOnProgress; virtual;
    procedure ProgressFormUpdateCaption(ALine0, ACaption: string);
    procedure ShowMessageSync(AMessage: string);
    function IsCancel: Boolean;

    procedure ExportRegion; virtual; abstract;
    procedure Execute; override;
    procedure Terminate; reintroduce; virtual; 
  public
    constructor Create(
      APolygon: TExtendedPointArray;
      Azoomarr: array of boolean
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  Dialogs;

procedure TThreadExportAbstract.CloseFProgress(Sender: TObject;
  var Action: TCloseAction);
begin
  Terminate;
end;

constructor TThreadExportAbstract.Create(APolygon: TExtendedPointArray;
  Azoomarr: array of boolean);
var
  i: Integer;
begin
  inherited Create(false);
  Priority := tpLowest;
  FreeOnTerminate := true;
  Application.CreateForm(TFProgress2, FProgressForm);
  FProgressForm.OnClose := CloseFProgress;
  FProgressForm.ProgressBar1.Progress1 := 0;
  FProgressForm.ProgressBar1.Max := 100;
  FProgressForm.Visible := true;
  FPolygLL := Copy(APolygon);
  for i := 0 to 23 do begin
    FZoomArr[i] := Azoomarr[i];
  end;

end;

destructor TThreadExportAbstract.Destroy;
begin
  FPolygLL := nil;
  inherited;
end;

procedure TThreadExportAbstract.Execute;
begin
  inherited;
  try
    ExportRegion;
    Synchronize(UpdateProgressFormClose);
  except
    on e: Exception do begin
      ShowMessageSync(e.Message);
    end;
  end;
end;

function TThreadExportAbstract.IsCancel: Boolean;
begin
  result := Terminated;
end;

procedure TThreadExportAbstract.ProgressFormUpdateCaption(ALine0,
  ACaption: string);
begin
  FShowOnFormLine0 := ALine0;
  Synchronize(UpdateProgressFormStr0);
  FShowFormCaption := ACaption;
  Synchronize(UpdateProgressFormCaption);
end;

procedure TThreadExportAbstract.ProgressFormUpdateOnProgress;
begin
  FProgressOnForm := round((FTilesProcessed / FTilesToProcess) * 100);
  Synchronize(UpdateProgressFormBar);
  FShowOnFormLine1 := SAS_STR_Processed + ' ' + inttostr(FTilesProcessed);
  Synchronize(UpdateProgressFormStr1);
end;

procedure TThreadExportAbstract.ShowMessageSync(AMessage: string);
begin
  FMessageForShow := AMessage;
  Synchronize(SynShowMessage);
end;

procedure TThreadExportAbstract.SynShowMessage;
begin
  ShowMessage(FMessageForShow);
end;

procedure TThreadExportAbstract.Terminate;
begin
  inherited;
end;

procedure TThreadExportAbstract.UpdateProgressFormCaption;
begin
  FProgressForm.Caption := FShowFormCaption;
end;

procedure TThreadExportAbstract.UpdateProgressFormClose;
begin
  FProgressForm.Close;
end;

procedure TThreadExportAbstract.UpdateProgressFormStr0;
begin
  FProgressForm.MemoInfo.Lines[0] := FShowOnFormLine0;
end;

procedure TThreadExportAbstract.UpdateProgressFormStr1;
begin
  FProgressForm.MemoInfo.Lines[1] := FShowOnFormLine1;
end;

procedure TThreadExportAbstract.UpdateProgressFormBar;
begin
  FProgressForm.ProgressBar1.Progress1 := FProgressOnForm;
end;

end.
