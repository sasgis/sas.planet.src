unit u_RegionProcessProgressInfoDownload;

interface

uses
  Types,
  i_LogSimple,
  i_LogSimpleProvider,
  i_ConfigDataWriteProvider,
  i_RegionProcessProgressInfo,
  i_RegionProcessProgressInfoDownload;

type
  TRegionProcessProgressInfoDownload = class(TInterfacedObject, IProgressInfoBase, IRegionProcessProgressInfoDownload, IRegionProcessProgressInfoDownloadInternal)
  private
    FProcessedRatio: Double;
    FFinished: Boolean;
    FElapsedTime: TDateTime;
    FStartTime: TDateTime;
    FTotalInRegion: Int64;
    FProcessed: Int64;
    FLastProcessedPoint: TPoint;
    FLastSuccessfulPoint: TPoint;
  private
    function GetProcessedRatio: Double;
    procedure SetProcessedRatio(const AValue: Double);
    property ProcessedRatio: Double read GetProcessedRatio write SetProcessedRatio;

    function GetFinished: Boolean;
    property Finished: Boolean read GetFinished;

    procedure Finish;
  private
    function GetTotalToProcess: Int64;
    function GetDownloaded: Int64;
    function GetProcessed: Int64;
    function GetDownloadSize: Double;
    function GetElapsedTime: TDateTime;
    function GetZoom: Byte;
    function GetLogProvider: ILogSimpleProvider;

    procedure Pause;
    procedure Resume;
    procedure SaveState(const ASLSSection: IConfigDataWriteProvider);

    procedure SetPaused;
    procedure SetStarted;
    procedure AddProcessedTile(const ATile: TPoint);
    procedure AddDownloadedTile(const ATile: TPoint);
    procedure SetTotalToProcess(AValue: Int64);
    function GetLog: ILogSimple;
  public
    constructor Create;
  end;

implementation

{ TRegionProcessProgressInfoDownload }

procedure TRegionProcessProgressInfoDownload.AddDownloadedTile(
  const ATile: TPoint
);
begin
  FLastSuccessfulPoint := ATile;
  Inc(FProcessed);
end;

procedure TRegionProcessProgressInfoDownload.AddProcessedTile(
  const ATile: TPoint);
begin

end;

constructor TRegionProcessProgressInfoDownload.Create;
begin

end;

procedure TRegionProcessProgressInfoDownload.Finish;
begin

end;

function TRegionProcessProgressInfoDownload.GetDownloaded: Int64;
begin

end;

function TRegionProcessProgressInfoDownload.GetDownloadSize: Double;
begin

end;

function TRegionProcessProgressInfoDownload.GetElapsedTime: TDateTime;
begin

end;

function TRegionProcessProgressInfoDownload.GetFinished: Boolean;
begin

end;

function TRegionProcessProgressInfoDownload.GetLog: ILogSimple;
begin

end;

function TRegionProcessProgressInfoDownload.GetLogProvider: ILogSimpleProvider;
begin

end;

function TRegionProcessProgressInfoDownload.GetProcessed: Int64;
begin

end;

function TRegionProcessProgressInfoDownload.GetProcessedRatio: Double;
begin

end;

function TRegionProcessProgressInfoDownload.GetTotalToProcess: Int64;
begin

end;

function TRegionProcessProgressInfoDownload.GetZoom: Byte;
begin

end;

procedure TRegionProcessProgressInfoDownload.Pause;
begin

end;

procedure TRegionProcessProgressInfoDownload.Resume;
begin

end;

procedure TRegionProcessProgressInfoDownload.SaveState(
  const ASLSSection: IConfigDataWriteProvider);
begin

end;

procedure TRegionProcessProgressInfoDownload.SetPaused;
begin

end;

procedure TRegionProcessProgressInfoDownload.SetProcessedRatio(
  const AValue: Double);
begin

end;

procedure TRegionProcessProgressInfoDownload.SetStarted;
begin

end;

procedure TRegionProcessProgressInfoDownload.SetTotalToProcess(AValue: Int64);
begin

end;

end.
