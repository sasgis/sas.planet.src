unit u_ThreadRegionProcessAbstract;

interface

uses
  Classes,
  Forms,
  i_JclNotify,
  i_VectorItemLonLat,
  i_OperationNotifier,
  i_RegionProcessProgressInfo,
  frm_ProgressSimple;

type
  TThreadRegionProcessAbstract = class(TThread)
  private
    FOperationID: Integer;
    FCancelListener: IJclListener;
    FProgressInfo: IRegionProcessProgressInfo;
    FPolygLL: ILonLatPolygon;

    FMessageForShow: string;
    FCancelNotifier: IOperationNotifier;
    procedure OnCancel;
    procedure SynShowMessage;
    procedure ShowMessageSync(AMessage: string);
  protected
    procedure ProcessRegion; virtual; abstract;
    procedure Execute; override;

    property CancelNotifier: IOperationNotifier read FCancelNotifier;
    property OperationID: Integer read FOperationID;
    property ProgressInfo: IRegionProcessProgressInfo read FProgressInfo;
    property PolygLL: ILonLatPolygon read FPolygLL;
  public
    constructor Create(
      ACancelNotifier: IOperationNotifier;
      AOperationID: Integer;
      AProgressInfo: IRegionProcessProgressInfo;
      APolygon: ILonLatPolygon
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  Dialogs,
  u_NotifyEventListener;

constructor TThreadRegionProcessAbstract.Create(
  ACancelNotifier: IOperationNotifier;
  AOperationID: Integer;
  AProgressInfo: IRegionProcessProgressInfo;
  APolygon: ILonLatPolygon
);
begin
  inherited Create(false);
  Priority := tpLowest;
  FreeOnTerminate := true;
  FCancelNotifier := ACancelNotifier;
  FOperationID := AOperationID;
  FProgressInfo := AProgressInfo;
  FPolygLL := APolygon;
  if not FCancelNotifier.IsOperationCanceled(FOperationID) then begin
    FCancelListener := TNotifyNoMmgEventListener.Create(Self.OnCancel);
    FCancelNotifier.AddListener(FCancelListener);
  end;
  if FCancelNotifier.IsOperationCanceled(FOperationID) then begin
    Terminate;
  end;
end;

destructor TThreadRegionProcessAbstract.Destroy;
begin
  if (FCancelListener <> nil) and (FCancelNotifier <> nil) then begin
    FCancelNotifier.RemoveListener(FCancelListener);
    FCancelListener := nil;
    FCancelNotifier := nil;
  end;
  FPolygLL := nil;
  FProgressInfo.Finish;
  FProgressInfo := nil;
  inherited;
end;

procedure TThreadRegionProcessAbstract.Execute;
begin
  try
    ProcessRegion;
  except
    on e: Exception do begin
      ShowMessageSync(e.Message);
    end;
  end;
end;

procedure TThreadRegionProcessAbstract.OnCancel;
begin
  Terminate;
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

end.
