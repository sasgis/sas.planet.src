unit u_TileDownloaderEventElement;

interface

uses
  Windows,
  Classes,
  SysUtils,
  SyncObjs,
  i_JclNotify,
  i_TileDownloader,
  i_TileDownloaderConfig,
  i_TileError,
  i_DownloadResult,
  i_DownloadChecker,
  i_DownloadResultFactory,
  u_DownloadCheckerStuped,
  u_DownloadResultFactoryTileDownload,
  u_MapType;

type
  TEventElementStatus = class
  private
    FCancelNotifier: IJclNotifier;
    FCancelListener: IJclListener;
    FThreadSafeCS: TCriticalSection;
    FCancelled: Boolean;
    function GetIsCanselled: Boolean;
    procedure OnCancelEvent(Sender: TObject);
  public
    constructor Create(ACancelNotifier: IJclNotifier);
    destructor Destroy; override;
    property IsCanceled: Boolean read GetIsCanselled;
  end;

  TTileDownloaderEventElement = class(TInterfacedObject, ITileDownloaderEvent)
  private
    FProcessed: Boolean;
    FEventStatus: TEventElementStatus;
    FCallBackList: TList;
    FRES_TileDownloadUnexpectedError: string;

    FMapTileUpdateEvent: TMapTileUpdateEvent;
    FErrorLogger: ITileErrorLogger;
    FMapType: TMapType;
    FDownloadResult: IDownloadResult;
    FResultFactory: IDownloadResultFactory;
    FDownloadChecker: IDownloadChecker;
    FCancelNotifier: IJclNotifier;

    FUrl: string;
    FRawRequestHeader: string;
    FRawResponseHeader: string;
    FTileXY: TPoint;
    FTileZoom: Byte;
    FTileSize: Cardinal;
    FCheckTileSize: Boolean;
    FOldTileSize: Cardinal;
    FTileMIME: string;
    FTileStream: TMemoryStream;
    FErrorString: string;
    FHttpStatusCode: Cardinal;

    procedure GuiSync;
  public
    constructor Create(
      AMapTileUpdateEvent: TMapTileUpdateEvent;
      AErrorLogger: ITileErrorLogger;
      AMapType: TMapType;
      ACancelNotifier: IJclNotifier
    );
    destructor Destroy; override;

    procedure ProcessEvent;

    procedure AddToCallBackList(ACallBack: TOnDownloadCallBack);
    procedure ExecCallBackList;

    procedure OnBeforeRequest(AConfig: ITileDownloaderConfigStatic);
    procedure OnAfterResponse();

    function  GetUrl: string;
    procedure SetUrl(Value: string);
    function  GetRawRequestHeader: string;
    procedure SetRawRequestHeader(Value: string);
    function  GetRawResponseHeader: string;
    procedure SetRawResponseHeader(Value: string);
    function  GetTileXY: TPoint;
    procedure SetTileXY(Value: TPoint);
    function  GetTileZoom: Byte;
    procedure SetTileZoom(Value: Byte);
    function  GetTileSize: Cardinal;
    procedure SetTileSize(Value: Cardinal);
    function  GetCheckTileSize: Boolean;
    procedure SetCheckTileSize(Value: Boolean);
    function  GetOldTileSize: Cardinal;
    procedure SetOldTileSize(Value: Cardinal);
    function  GetTileMIME: string;
    procedure SetTileMIME(Value: string);
    function  GetTileStream: TMemoryStream;
    procedure SetTileStream(Value: TMemoryStream);
    function  GetHttpStatusCode: Cardinal;
    procedure SetHttpStatusCode(Value: Cardinal);
    function  GetDownloadResult: IDownloadResult;
    procedure SetDownloadResult(Value: IDownloadResult);
    function  GetResultFactory: IDownloadResultFactory;
    function GetCancelNotifier: IJclNotifier;

    property Url: string read GetUrl write SetUrl;
    property RawRequestHeader: string read GetRawRequestHeader write SetRawRequestHeader;
    property RawResponseHeader: string read GetRawResponseHeader write SetRawResponseHeader;
    property TileXY: TPoint read GetTileXY write SetTileXY;
    property TileZoom: Byte read GetTileZoom write SetTileZoom;
    property TileSize: Cardinal read GetTileSize write SetTileSize;
    property CheckTileSize: Boolean read GetCheckTileSize write SetCheckTileSize;
    property OldTileSize: Cardinal read GetOldTileSize write SetOldTileSize;
    property TileMIME: string read GetTileMIME write SetTileMIME;
    property TileStream: TMemoryStream read GetTileStream write SetTileStream;
    property HttpStatusCode: Cardinal read GetHttpStatusCode write SetHttpStatusCode;
    property DownloadResult: IDownloadResult read GetDownloadResult write SetDownloadResult;
    property ResultFactory: IDownloadResultFactory read GetResultFactory;
    property CancelNotifier: IJclNotifier read GetCancelNotifier;
  end;

implementation

uses
  u_GlobalState,
  u_TileErrorInfo,
  u_ResStrings,
  u_NotifyEventListener;

{ TEventElementStatus }

constructor TEventElementStatus.Create(ACancelNotifier: IJclNotifier);
begin
  inherited Create;
  FCancelled := False;
  FThreadSafeCS := TCriticalSection.Create;
  FCancelNotifier := ACancelNotifier;
  FCancelListener := TNotifyEventListener.Create(Self.OnCancelEvent);
  if FCancelNotifier <> nil then begin
    FCancelNotifier.Add(FCancelListener);
  end;
end;

destructor TEventElementStatus.Destroy;
begin
  try
    if FCancelNotifier <> nil then begin
      FCancelNotifier.Remove(FCancelListener);
    end;
    FreeAndNil(FThreadSafeCS);
  finally
    inherited;
  end;
end;

procedure TEventElementStatus.OnCancelEvent(Sender: TObject);
begin
  FThreadSafeCS.Acquire;
  try
    FCancelled := True;
  finally
    FThreadSafeCS.Release;
  end;
end;

function TEventElementStatus.GetIsCanselled: Boolean;
begin
  FThreadSafeCS.Acquire;
  try
    Result := FCancelled;
  finally
    FThreadSafeCS.Release;
  end;
end;

{ TTileDownloaderEventElement }

constructor TTileDownloaderEventElement.Create(
  AMapTileUpdateEvent: TMapTileUpdateEvent;
  AErrorLogger: ITileErrorLogger;
  AMapType: TMapType;
  ACancelNotifier: IJclNotifier
);
begin
  inherited Create;
  FCancelNotifier := ACancelNotifier;
  FEventStatus := TEventElementStatus.Create(FCancelNotifier);
  FProcessed := False;
  FMapTileUpdateEvent := AMapTileUpdateEvent;
  FErrorLogger := AErrorLogger;
  FMapType := AMapType;
  FDownloadResult := nil;
  FResultFactory := nil;
  FDownloadChecker := nil;

  FUrl := '';
  FRawRequestHeader := '';
  FRawResponseHeader := '';
  FTileXY.X := 0;
  FTileXY.Y := 0;
  FTileZoom := 0;
  FTileSize := 0;
  FCheckTileSize := False;
  FOldTileSize := 0;
  FTileMIME := '';
  FTileStream := TMemoryStream.Create;
  FErrorString := '';
  FHttpStatusCode := 0;

  FCallBackList := TList.Create;

  FRES_TileDownloadUnexpectedError := SAS_ERR_TileDownloadUnexpectedError;
end;

destructor TTileDownloaderEventElement.Destroy;
begin
  try
    try
      if Assigned(FEventStatus) and
         not FEventStatus.IsCanceled and
         not FProcessed then
      begin
        ProcessEvent;
      end;
      FCallBackList.Clear;
      FreeAndNil(FCallBackList);
    finally
      FreeAndNil(FEventStatus);
      FreeAndNil(FTileStream);
    end; 
  finally
    inherited Destroy;
  end;
end;

procedure TTileDownloaderEventElement.OnBeforeRequest(AConfig: ITileDownloaderConfigStatic);
begin
  if not FEventStatus.IsCanceled then
  begin
    FResultFactory := TDownloadResultFactoryTileDownload.Create(
      GState.DownloadResultTextProvider,
      FTileZoom,
      FTileXY,
      FMapType,
      FUrl,
      FRawRequestHeader
    );

    FDownloadChecker := TDownloadCheckerStuped.Create(
      FResultFactory,
      AConfig.IgnoreMIMEType,
      AConfig.ExpectedMIMETypes,
      AConfig.DefaultMIMEType,
      FCheckTileSize,
      FOldTileSize
    );

    FDownloadResult := FDownloadChecker.BeforeRequest(FUrl, FRawRequestHeader);
  end;
end;

procedure TTileDownloaderEventElement.OnAfterResponse();
begin
  if not FEventStatus.IsCanceled then
  begin
    FDownloadResult := FDownloadChecker.AfterResponse(FHttpStatusCode, FTileMIME, FRawResponseHeader);
    if FDownloadResult = nil then
    begin
      FDownloadResult := FDownloadChecker.AfterReciveData(FTileStream.Size, FTileStream.Memory, FHttpStatusCode, FRawResponseHeader);
      if FDownloadResult = nil then
      begin
        if FTileStream.Size = 0 then
          FDownloadResult := FResultFactory.BuildDataNotExistsZeroSize(FRawResponseHeader)
        else
          FDownloadResult := FResultFactory.BuildOk(
            FHttpStatusCode,
            FRawResponseHeader,
            FTileMIME,
            FTileStream.Size,
            FTileStream.Memory
          );
      end;
    end; 
  end;
end;

procedure TTileDownloaderEventElement.GuiSync;
begin
  if Addr(FMapTileUpdateEvent) <> nil then
    FMapTileUpdateEvent(FMapType, FTileZoom, FTileXY);
end;

procedure TTileDownloaderEventElement.ProcessEvent;
var
  VErrorString: string;
  VResultOk: IDownloadResultOk;
  VResultDownloadError: IDownloadResultError;
begin
  try
    VErrorString := '';
    try
      ExecCallBackList;
      if Supports(FDownloadResult, IDownloadResultOk, VResultOk) then begin
        if not FEventStatus.IsCanceled then
          GState.DownloadInfo.Add(1, VResultOk.Size);
      end else if Supports(FDownloadResult, IDownloadResultError, VResultDownloadError) then begin
        VErrorString := VResultDownloadError.ErrorText;
      end;
    except
      on E: Exception do
        VErrorString := E.Message;
      else
        VErrorString := FRES_TileDownloadUnexpectedError;
    end;
    if VErrorString <> '' then begin
      if (FErrorLogger <> nil) and (not FEventStatus.IsCanceled) then
        FErrorLogger.LogError( TTileErrorInfo.Create(FMapType, FTileZoom, FTileXY, VErrorString) );
    end else begin
      if not FEventStatus.IsCanceled then
        TThread.Synchronize(nil, GuiSync);
    end;
  finally
    FProcessed := True;
  end;
end;

procedure TTileDownloaderEventElement.AddToCallBackList(ACallBack: TOnDownloadCallBack);
var
  VCallBack: POnDownloadCallBack;
begin
  if Assigned(ACallBack) then begin
    if not Assigned(FCallBackList) then
      FCallBackList := TList.Create;
    New(VCallBack);
    TOnDownloadCallBack(VCallBack^) := ACallBack;
    FCallBackList.Add(VCallBack);
  end;
end;

procedure TTileDownloaderEventElement.ExecCallBackList;
var
  i: Integer;
  VCallBack: POnDownloadCallBack;
begin
  if Assigned(FCallBackList) then
  try
    for i := FCallBackList.Count - 1 downto 0 do // !!! FILO
    try
      VCallBack := FCallBackList.Items[i];
      if Assigned(VCallBack) then
      try
        if not FEventStatus.IsCanceled then
          VCallBack^(Self);
      finally
        Dispose(VCallBack);
      end;
    except
      // ignore all
    end;
  finally
    FCallBackList.Clear;
  end;
end;

procedure TTileDownloaderEventElement.SetUrl(Value: string);
begin
  FUrl := Value;
end;

function TTileDownloaderEventElement.GetUrl: string;
begin
  Result := FUrl;
end;

procedure TTileDownloaderEventElement.SetRawRequestHeader(Value: string);
begin
  FRawRequestHeader := Value;
end;

function TTileDownloaderEventElement.GetRawRequestHeader: string;
begin
  Result := FRawRequestHeader;
end;

procedure TTileDownloaderEventElement.SetRawResponseHeader(Value: string);
begin
  FRawResponseHeader := Value;
end;

function TTileDownloaderEventElement.GetRawResponseHeader: string;
begin
  Result := FRawResponseHeader;
end;

procedure TTileDownloaderEventElement.SetTileXY(Value: TPoint);
begin
  FTileXY := Value;
end;

function TTileDownloaderEventElement.GetTileXY: TPoint;
begin
  Result := FTileXY;
end;

procedure TTileDownloaderEventElement.SetTileZoom(Value: Byte);
begin
  FTileZoom := Value;
end;

function TTileDownloaderEventElement.GetTileZoom: Byte;
begin
  Result := FTileZoom;
end;

procedure TTileDownloaderEventElement.SetTileSize(Value: Cardinal);
begin
  FTileSize := Value;
end;

function TTileDownloaderEventElement.GetTileSize: Cardinal;
begin
  Result := FTileSize;
end;

procedure TTileDownloaderEventElement.SetCheckTileSize(Value: Boolean);
begin
  FCheckTileSize := Value;
end;

function TTileDownloaderEventElement.GetCheckTileSize: Boolean;
begin
  Result := FCheckTileSize;
end;

procedure TTileDownloaderEventElement.SetOldTileSize(Value: Cardinal);
begin
  FOldTileSize := Value;
end;

function TTileDownloaderEventElement.GetOldTileSize: Cardinal;
begin
  Result := FOldTileSize;
end;

procedure TTileDownloaderEventElement.SetTileMIME(Value: string);
begin
  FTileMIME := Value;
end;

function TTileDownloaderEventElement.GetTileMIME: string;
begin
  Result := FTileMIME;
end;

procedure TTileDownloaderEventElement.SetTileStream(Value: TMemoryStream);
begin
  FTileStream := Value;
end;

function TTileDownloaderEventElement.GetTileStream: TMemoryStream;
begin
  Result := FTileStream;
end;

procedure TTileDownloaderEventElement.SetHttpStatusCode(Value: Cardinal);
begin
  FHttpStatusCode := Value;
end;

function TTileDownloaderEventElement.GetHttpStatusCode: Cardinal;
begin
  Result := HttpStatusCode;
end;

procedure TTileDownloaderEventElement.SetDownloadResult(Value: IDownloadResult);
begin
  FDownloadResult := Value;
end;

function TTileDownloaderEventElement.GetDownloadResult: IDownloadResult;
begin
  Result := FDownloadResult;
end;

function TTileDownloaderEventElement.GetResultFactory: IDownloadResultFactory;
begin
  Result := FResultFactory;
end;

function TTileDownloaderEventElement.GetCancelNotifier: IJclNotifier;
begin
  Result := FCancelNotifier;
end;

end.
