unit u_RegionProcessProgressInfoDownload;

interface

uses
  Types,
  SysUtils,
  i_LogSimple,
  i_LogSimpleProvider,
  i_ConfigDataWriteProvider,
  i_RegionProcessProgressInfo,
  i_RegionProcessProgressInfoDownload;

type
  TRegionProcessProgressInfoDownload = class(TInterfacedObject, IProgressInfoBase, IRegionProcessProgressInfoDownload, IRegionProcessProgressInfoDownloadInternal)
  private
    FCS: IReadWriteSync;
    FLog: ILogSimple;
    FLogProvider: ILogSimpleProvider;
    FProcessedRatio: Double;
    FFinished: Boolean;
    FElapsedTime: TDateTime;
    FStartTime: TDateTime;
    FTotalInRegion: Int64;
    FProcessed: Int64;
    FDownloadedSize: UInt64;
    FDownloadedCount: Int64;
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
    function GetDownloadSize: UInt64;
    function GetElapsedTime: TDateTime;
    function GetZoom: Byte;
    function GetLogProvider: ILogSimpleProvider;

    procedure Pause;
    procedure Resume;
    procedure SaveState(const ASLSSection: IConfigDataWriteProvider);

    procedure SetPaused;
    procedure SetStarted;
    procedure AddProcessedTile(const ATile: TPoint);
    procedure AddDownloadedTile(const ATile: TPoint; const ASize: Cardinal);
    procedure SetTotalToProcess(AValue: Int64);
    function GetLog: ILogSimple;
  public
    constructor Create;
  end;

implementation

{ TRegionProcessProgressInfoDownload }

constructor TRegionProcessProgressInfoDownload.Create;
begin
  inherited Create;
end;

procedure TRegionProcessProgressInfoDownload.AddDownloadedTile(
  const ATile: TPoint;
  const ASize: Cardinal
);
begin
  FLastSuccessfulPoint := ATile;
  Inc(FDownloadedSize, ASize);
  Inc(FDownloadedCount);
  Inc(FProcessed);
end;

procedure TRegionProcessProgressInfoDownload.AddProcessedTile(
  const ATile: TPoint);
begin
  FLastProcessedPoint := ATile;
end;

procedure TRegionProcessProgressInfoDownload.Finish;
begin

end;

function TRegionProcessProgressInfoDownload.GetDownloaded: Int64;
begin
  Result := FDownloadedCount
end;

function TRegionProcessProgressInfoDownload.GetDownloadSize: UInt64;
begin
  Result := FDownloadedSize;
end;

function TRegionProcessProgressInfoDownload.GetElapsedTime: TDateTime;
begin
  Result := FElapsedTime;
end;

function TRegionProcessProgressInfoDownload.GetFinished: Boolean;
begin
  Result := FFinished;
end;

function TRegionProcessProgressInfoDownload.GetLog: ILogSimple;
begin
  Result := FLog;
end;

function TRegionProcessProgressInfoDownload.GetLogProvider: ILogSimpleProvider;
begin
  Result := FLogProvider;
end;

function TRegionProcessProgressInfoDownload.GetProcessed: Int64;
begin
  Result := FProcessed;
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
