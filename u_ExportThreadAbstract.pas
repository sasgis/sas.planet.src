unit u_ExportThreadAbstract;

interface

uses
  Classes,
  t_GeoTypes,
  unit4,
  UResStrings;

type
  TExportThreadAbstract = class(TThread)
  protected
    FPolygLL: TExtendedPointArray;
    FZoomArr: array [0..23] of boolean;

    FProgressForm: TFprogress2;
    FShowFormCaption: string;
    FShowOnFormLine0: string;
    FShowOnFormLine1: string;
    FProgressOnForm: integer;
    FMessageForShow: string;

    FTilesToProcess:integer;
    FTilesProcessed:integer;

    procedure UpdateProgressFormBar;
    procedure UpdateProgressFormCaption;
    procedure UpdateProgressFormStr0;
    procedure UpdateProgressFormStr1;
    procedure UpdateProgressFormClose;
    procedure SynShowMessage;
    procedure ExportRegion; virtual; abstract;
    procedure Execute; override;
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
  Dialogs,
  Forms;

constructor TExportThreadAbstract.Create(APolygon: TExtendedPointArray;
  Azoomarr: array of boolean);
var
  i: Integer;
begin
  inherited Create(false);
  Priority := tpLowest;
  FreeOnTerminate := true;
  Application.CreateForm(TFProgress2, FProgressForm);
  FProgressForm.ProgressBar1.Progress1 := 0;
  FProgressForm.ProgressBar1.Max := 100;
  FProgressForm.Visible := true;
  FPolygLL := Copy(APolygon);
  for i := 0 to 23 do begin
    FZoomArr[i] := Azoomarr[i];
  end;

end;

destructor TExportThreadAbstract.Destroy;
begin
  FPolygLL := nil;
  inherited;
end;

procedure TExportThreadAbstract.Execute;
begin
  inherited;
  try
    ExportRegion;
    Synchronize(UpdateProgressFormClose);
  except
    on e: Exception do begin
      FMessageForShow := e.Message;
      Synchronize(SynShowMessage);
    end;
  end;
end;

procedure TExportThreadAbstract.SynShowMessage;
begin
  ShowMessage(FMessageForShow);
end;

procedure TExportThreadAbstract.UpdateProgressFormCaption;
begin
  FProgressForm.Caption := FShowFormCaption;
end;

procedure TExportThreadAbstract.UpdateProgressFormClose;
begin
  FProgressForm.Close;
end;

procedure TExportThreadAbstract.UpdateProgressFormStr0;
begin
  FProgressForm.MemoInfo.Lines[0] := FShowOnFormLine0;
end;

procedure TExportThreadAbstract.UpdateProgressFormStr1;
begin
  FProgressForm.MemoInfo.Lines[1] := FShowOnFormLine1;
end;

procedure TExportThreadAbstract.UpdateProgressFormBar;
begin
  FProgressForm.ProgressBar1.Progress1 := FProgressOnForm;
end;

end.
