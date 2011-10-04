unit u_TileDownloaderEventElement;

interface

uses
  Windows,
  Classes,
  SysUtils,
  SyncObjs,
  i_JclNotify,
  i_OperationNotifier,
  i_MapVersionInfo,
  i_LastResponseInfo,
  i_TileDownloader,
  i_TileDownloaderConfig,
  i_TileError,
  i_DownloadResult,
  i_DownloadInfoSimple,
  i_TileDownloadRequest,
  i_DownloadChecker,
  i_DownloadResultFactory,
  u_DownloadCheckerStuped,
  u_DownloadResultFactory,
  u_MapType;

type
  TEventElementStatus = class
  private
    FCancelNotifier: IOperationNotifier;
    FCancelListener: IJclListener;
    FThreadSafeCS: TCriticalSection;
    FCancelled: Boolean;
    FOperationID: Integer;
    function GetIsCanselled: Boolean;
    function GetNotifier: IOperationNotifier;
    procedure OnCancelEvent(Sender: TObject);
  public
    constructor Create(ACancelNotifier: IOperationNotifier; AOperationID: Integer);
    destructor Destroy; override;
    property IsCanceled: Boolean read GetIsCanselled;
    property Notifier: IOperationNotifier read GetNotifier;
  end;

  TTileDownloaderEventElement = class(TInterfacedObject, ITileDownloaderEvent)
  private
    FProcessed: Boolean;
    FEventStatus: TEventElementStatus;
    FCallBackList: TList;
    FRES_TileDownloadUnexpectedError: string;

    FDownloadInfo: IDownloadInfoSimple;
    FMapTileUpdateEvent: TMapTileUpdateEvent;
    FErrorLogger: ITileErrorLogger;
    FMapType: TMapType;
    FDownloadResult: IDownloadResult;
    FResultFactory: IDownloadResultFactory;
    FDownloadChecker: IDownloadChecker;

    FRequest: ITileDownloadRequest;
    FLastResponseInfo: ILastResponseInfo;
    FVersionInfo: IMapVersionInfo;
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
      ADownloadInfo: IDownloadInfoSimple;
      AMapTileUpdateEvent: TMapTileUpdateEvent;
      AErrorLogger: ITileErrorLogger;
      AMapType: TMapType;
      ACancelNotifier: IOperationNotifier;
      AOperationID: Integer
    );
    destructor Destroy; override;

    procedure ProcessEvent;

    procedure AddToCallBackList(ACallBack: TOnDownloadCallBack);
    procedure ExecCallBackList;

    procedure OnBeforeRequest(AConfig: ITileDownloaderConfigStatic);
    procedure OnAfterResponse(const ARawResponseHeader: string);

    function  GetRequest: ITileDownloadRequest;
    procedure SetRequest(AValue: ITileDownloadRequest);
    function  GetLastResponseInfo: ILastResponseInfo;
    procedure SetLastResponseInfo(AValue: ILastResponseInfo);
    function  GetVersionInfo: IMapVersionInfo;
    procedure SetVersionInfo(AValue: IMapVersionInfo);
    function  GetTileXY: TPoint;
    procedure SetTileXY(AValue: TPoint);
    function  GetTileZoom: Byte;
    procedure SetTileZoom(AValue: Byte);
    function  GetTileSize: Cardinal;
    procedure SetTileSize(AValue: Cardinal);
    function  GetCheckTileSize: Boolean;
    procedure SetCheckTileSize(AValue: Boolean);
    function  GetOldTileSize: Cardinal;
    procedure SetOldTileSize(AValue: Cardinal);
    function  GetTileMIME: string;
    procedure SetTileMIME(AValue: string);
    function  GetTileStream: TMemoryStream;
    procedure SetTileStream(AValue: TMemoryStream);
    function  GetHttpStatusCode: Cardinal;
    procedure SetHttpStatusCode(AValue: Cardinal);
    function  GetDownloadResult: IDownloadResult;
    procedure SetDownloadResult(AValue: IDownloadResult);
    function  GetResultFactory: IDownloadResultFactory;
    function  GetCancelNotifier: IOperationNotifier;

    property Request: ITileDownloadRequest read GetRequest write SetRequest;
    property LastResponseInfo: ILastResponseInfo read GetLastResponseInfo write SetLastResponseInfo;
    property VersionInfo: IMapVersionInfo read GetVersionInfo write SetVersionInfo;
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
    property CancelNotifier: IOperationNotifier read GetCancelNotifier;
  end;

implementation

uses
  u_GlobalState,
  u_TileErrorInfo,
  u_ResStrings,
  u_NotifyEventListener;

{ TEventElementStatus }

constructor TEventElementStatus.Create(
  ACancelNotifier: IOperationNotifier;
  AOperationID: Integer
);
begin
  inherited Create;
  FCancelled := False;
  FOperationID := AOperationID;
  FThreadSafeCS := TCriticalSection.Create;
  FCancelNotifier := ACancelNotifier;
  FCancelListener := TNotifyEventListener.Create(Self.OnCancelEvent);
  if FCancelNotifier <> nil then begin
    FCancelNotifier.AddListener(FCancelListener);
  end;
end;

destructor TEventElementStatus.Destroy;
begin
  try
    if FCancelNotifier <> nil then begin
      FCancelNotifier.RemoveListener(FCancelListener);
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
    Result := FCancelled or FCancelNotifier.IsOperationCanceled(FOperationID);
  finally
    FThreadSafeCS.Release;
  end;
end;

function TEventElementStatus.GetNotifier: IOperationNotifier;
begin
  FThreadSafeCS.Acquire;
  try
    Result := FCancelNotifier;
  finally
    FThreadSafeCS.Release;
  end;
end;

{ TTileDownloaderEventElement }

constructor TTileDownloaderEventElement.Create(
  ADownloadInfo: IDownloadInfoSimple;
  AMapTileUpdateEvent: TMapTileUpdateEvent;
  AErrorLogger: ITileErrorLogger;
  AMapType: TMapType;
  ACancelNotifier: IOperationNotifier;
  AOperationID: Integer
);
begin
  inherited Create;
  FEventStatus := TEventElementStatus.Create(ACancelNotifier, AOperationID);
  FProcessed := False;
  FDownloadInfo := ADownloadInfo;
  FMapTileUpdateEvent := AMapTileUpdateEvent;
  FErrorLogger := AErrorLogger;
  FMapType := AMapType;
  FDownloadResult := nil;
  FResultFactory := nil;
  FDownloadChecker := nil;
  
  FRequest := nil;
  FLastResponseInfo := nil;
  FVersionInfo := nil;
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
  if not FEventStatus.IsCanceled then begin

    FResultFactory := TDownloadResultFactory.Create(
      GState.DownloadResultTextProvider   // TODO: Избавиться от GState
    );

    FDownloadChecker := TDownloadCheckerStuped.Create(
      FResultFactory,
      AConfig.IgnoreMIMEType,
      AConfig.ExpectedMIMETypes,
      AConfig.DefaultMIMEType,
      FCheckTileSize,
      FOldTileSize
    );

    FDownloadResult := FDownloadChecker.BeforeRequest(FRequest);
  end;
end;

procedure TTileDownloaderEventElement.OnAfterResponse(const ARawResponseHeader: string);
var
  VHeader: string;
begin
  VHeader := ARawResponseHeader;
  if not FEventStatus.IsCanceled then begin
    FDownloadResult :=
      FDownloadChecker.AfterResponse(
        FRequest,
        FHttpStatusCode,
        FTileMIME,
        VHeader
      );
    if FDownloadResult = nil then begin
      FDownloadResult :=
        FDownloadChecker.AfterReciveData(
          FRequest,
          FTileStream.Size,
          FTileStream.Memory,
          FHttpStatusCode,
          VHeader
        );
      if FDownloadResult = nil then begin
        if FTileStream.Size = 0 then begin
          FDownloadResult :=
            FResultFactory.BuildDataNotExistsZeroSize(
              FRequest,
              VHeader
            );
        end else begin
          FDownloadResult :=
            FResultFactory.BuildOk(
              FRequest,
              FHttpStatusCode,
              VHeader,
              FTileMIME,
              FTileStream.Size,
              FTileStream.Memory
            );
        end;
      end;
    end; 
  end;
end;

procedure TTileDownloaderEventElement.GuiSync;
begin
  if not FEventStatus.IsCanceled and (Addr(FMapTileUpdateEvent) <> nil) then begin
    FMapTileUpdateEvent(FMapType, FTileZoom, FTileXY);
  end;
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
        if not FEventStatus.IsCanceled and (FDownloadInfo <> nil) then begin
          FDownloadInfo.Add(1, VResultOk.Size);
        end;
      end else if Supports(FDownloadResult, IDownloadResultError, VResultDownloadError) then begin
        VErrorString := VResultDownloadError.ErrorText;
      end;
    except
      on E: Exception do begin
        VErrorString := E.Message;
      end
      else begin
        VErrorString := FRES_TileDownloadUnexpectedError;
      end;
    end;
    if VErrorString <> '' then begin
      if (FErrorLogger <> nil) and (not FEventStatus.IsCanceled) then begin
        FErrorLogger.LogError(
          TTileErrorInfo.Create(
            FMapType,
            FTileZoom,
            FTileXY,
            VErrorString
          )
        );
      end;
    end else begin
      if not FEventStatus.IsCanceled then begin
        TThread.Synchronize(nil, GuiSync);
      end;
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
    if not Assigned(FCallBackList) then begin
      FCallBackList := TList.Create;
    end;
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
        if not FEventStatus.IsCanceled then begin
          VCallBack^(Self);
        end;
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

procedure TTileDownloaderEventElement.SetRequest(AValue: ITileDownloadRequest);
begin
  FRequest := AValue;
end;

function TTileDownloaderEventElement.GetRequest: ITileDownloadRequest;
begin
  Result := FRequest;
end;

procedure TTileDownloaderEventElement.SetLastResponseInfo(AValue: ILastResponseInfo);
begin
  FLastResponseInfo := AValue;
end;

function TTileDownloaderEventElement.GetLastResponseInfo: ILastResponseInfo;
begin
  Result := FLastResponseInfo;
end;

procedure TTileDownloaderEventElement.SetVersionInfo(AValue: IMapVersionInfo);
begin
  FVersionInfo := AValue;
end;

function TTileDownloaderEventElement.GetVersionInfo: IMapVersionInfo;
begin
  Result := FVersionInfo;
end;

procedure TTileDownloaderEventElement.SetTileXY(AValue: TPoint);
begin
  FTileXY := AValue;
end;

function TTileDownloaderEventElement.GetTileXY: TPoint;
begin
  Result := FTileXY;
end;

procedure TTileDownloaderEventElement.SetTileZoom(AValue: Byte);
begin
  FTileZoom := AValue;
end;

function TTileDownloaderEventElement.GetTileZoom: Byte;
begin
  Result := FTileZoom;
end;

procedure TTileDownloaderEventElement.SetTileSize(AValue: Cardinal);
begin
  FTileSize := AValue;
end;

function TTileDownloaderEventElement.GetTileSize: Cardinal;
begin
  Result := FTileSize;
end;

procedure TTileDownloaderEventElement.SetCheckTileSize(AValue: Boolean);
begin
  FCheckTileSize := AValue;
end;

function TTileDownloaderEventElement.GetCheckTileSize: Boolean;
begin
  Result := FCheckTileSize;
end;

procedure TTileDownloaderEventElement.SetOldTileSize(AValue: Cardinal);
begin
  FOldTileSize := AValue;
end;

function TTileDownloaderEventElement.GetOldTileSize: Cardinal;
begin
  Result := FOldTileSize;
end;

procedure TTileDownloaderEventElement.SetTileMIME(AValue: string);
begin
  FTileMIME := AValue;
end;

function TTileDownloaderEventElement.GetTileMIME: string;
begin
  Result := FTileMIME;
end;

procedure TTileDownloaderEventElement.SetTileStream(AValue: TMemoryStream);
begin
  FTileStream := AValue;
end;

function TTileDownloaderEventElement.GetTileStream: TMemoryStream;
begin
  Result := FTileStream;
end;

procedure TTileDownloaderEventElement.SetHttpStatusCode(AValue: Cardinal);
begin
  FHttpStatusCode := AValue;
end;

function TTileDownloaderEventElement.GetHttpStatusCode: Cardinal;
begin
  Result := HttpStatusCode;
end;

procedure TTileDownloaderEventElement.SetDownloadResult(AValue: IDownloadResult);
begin
  FDownloadResult := AValue;
end;

function TTileDownloaderEventElement.GetDownloadResult: IDownloadResult;
begin
  Result := FDownloadResult;
end;

function TTileDownloaderEventElement.GetResultFactory: IDownloadResultFactory;
begin
  Result := FResultFactory;
end;

function TTileDownloaderEventElement.GetCancelNotifier: IOperationNotifier;
begin
  Result := FEventStatus.Notifier;
end;

end.
