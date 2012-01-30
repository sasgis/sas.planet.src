unit u_ThreadRegionProcessAbstract;

interface

uses
  Classes,
  Forms,
  i_VectorItemLonLat,
  i_OperationNotifier,
  u_OperationNotifier,
  frm_ProgressSimple;

type
  TThreadRegionProcessAbstract = class(TThread)
  private
    FCancelNotifierInternal: IOperationNotifierInternal;
    FCancelNotifier: IOperationNotifier;
    FOperationID: Integer;
    FProgressForm: TfrmProgressSimple;
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
    FPolygLL: ILonLatPolygon;

    FTilesToProcess: Int64;
    FTilesProcessed: Int64;
    procedure ProgressFormUpdateProgressAndLine1(AProgress: Integer; ALine1: string);
    procedure ProgressFormUpdateProgressLine0AndLine1(AProgress: Integer; ALine0, ALine1: string);
    procedure ProgressFormUpdateCaption(ALine0, ACaption: string);

    procedure ShowMessageSync(AMessage: string);

    procedure ProcessRegion; virtual; abstract;
    procedure Execute; override;
    procedure Terminate; reintroduce; virtual;

    property CancelNotifier: IOperationNotifier read FCancelNotifier;
    property OperationID: Integer read FOperationID;
  public
    constructor Create(
      APolygon: ILonLatPolygon
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  Dialogs;

procedure TThreadRegionProcessAbstract.CloseFProgress(Sender: TObject;
  var Action: TCloseAction);
begin
  Terminate;
end;

constructor TThreadRegionProcessAbstract.Create(APolygon: ILonLatPolygon);
var
  VOperationNotifier: TOperationNotifier;
begin
  inherited Create(false);
  Priority := tpLowest;
  FreeOnTerminate := true;
  Application.CreateForm(TfrmProgressSimple, FProgressForm);
  FProgressForm.OnClose := CloseFProgress;
  FProgressForm.ProgressBar1.Progress1 := 0;
  FProgressForm.ProgressBar1.Max := 100;
  FProgressForm.Visible := true;
  FPolygLL := APolygon;
  VOperationNotifier := TOperationNotifier.Create;
  FCancelNotifierInternal := VOperationNotifier;
  FCancelNotifier := VOperationNotifier;
  FOperationID := FCancelNotifier.CurrentOperation;
end;

destructor TThreadRegionProcessAbstract.Destroy;
begin
  FCancelNotifierInternal.NextOperation;
  FPolygLL := nil;
  inherited;
end;

procedure TThreadRegionProcessAbstract.Execute;
begin
  inherited;
  try
    try
      ProcessRegion;
    except
      on e: Exception do begin
        ShowMessageSync(e.Message);
      end;
    end;
  finally
    Synchronize(UpdateProgressFormClose);
  end;
end;

procedure TThreadRegionProcessAbstract.ProgressFormUpdateCaption(ALine0,
  ACaption: string);
begin
  FShowOnFormLine0 := ALine0;
  Synchronize(UpdateProgressFormStr0);
  FShowFormCaption := ACaption;
  Synchronize(UpdateProgressFormCaption);
end;

procedure TThreadRegionProcessAbstract.ProgressFormUpdateProgressAndLine1(
  AProgress: Integer; ALine1: string);
begin
  FProgressOnForm := AProgress;
  Synchronize(UpdateProgressFormBar);
  FShowOnFormLine1 := ALine1;
  Synchronize(UpdateProgressFormStr1);
end;

procedure TThreadRegionProcessAbstract.ProgressFormUpdateProgressLine0AndLine1(
  AProgress: Integer; ALine0, ALine1: string);
begin
  FProgressOnForm := AProgress;
  Synchronize(UpdateProgressFormBar);
  FShowOnFormLine0 := ALine0;
  Synchronize(UpdateProgressFormStr0);
  FShowOnFormLine1 := ALine1;
  Synchronize(UpdateProgressFormStr1);
end;

procedure TThreadRegionProcessAbstract.ShowMessageSync(AMessage: string);
begin
  FMessageForShow := AMessage;
  Synchronize(SynShowMessage);
end;

procedure TThreadRegionProcessAbstract.SynShowMessage;
begin
  ShowMessage(FMessageForShow);
end;

procedure TThreadRegionProcessAbstract.Terminate;
begin
  FCancelNotifierInternal.NextOperation;
  inherited;
end;

procedure TThreadRegionProcessAbstract.UpdateProgressFormCaption;
begin
  FProgressForm.Caption := FShowFormCaption;
end;

procedure TThreadRegionProcessAbstract.UpdateProgressFormClose;
begin
  FProgressForm.Close;
end;

procedure TThreadRegionProcessAbstract.UpdateProgressFormStr0;
begin
  FProgressForm.MemoInfo.Lines[0] := FShowOnFormLine0;
end;

procedure TThreadRegionProcessAbstract.UpdateProgressFormStr1;
begin
  FProgressForm.MemoInfo.Lines[1] := FShowOnFormLine1;
end;

procedure TThreadRegionProcessAbstract.UpdateProgressFormBar;
begin
  FProgressForm.ProgressBar1.Progress1 := FProgressOnForm;
end;

end.
