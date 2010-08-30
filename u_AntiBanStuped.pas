unit u_AntiBanStuped;

interface

uses
  Windows,
  IniFiles,
  SyncObjs,
  VCLZip,
  i_IConfigDataProvider,
  i_IAntiBan,
  i_ITileDownlodSession,
  Unit1;

type
  TAntiBanStuped = class(TInterfacedObject, IAntiBan)
  private
    FContent_Type: string;
    FUsePreloadPage: integer;
    FPreloadPage: string;
    FBanIfLen: integer;
    FDownloadTilesCount: Longint;
    FBanFlag: Boolean;
    FBanCS: TCriticalSection;
    procedure addDwnforban;
    procedure IncDownloadedAndCheckAntiBan();
    function CheckIsBan(
      ATile: TPoint;
      AZoom: Byte;
      AUrl: string;
      ADownloadResult: TDownloadTileResult;
      AStatusCode: Cardinal;
      AContentType: string;
      ADownloadBuffer: Pointer;
      ADownloadSize: Cardinal
    ): Boolean;
    procedure ExecOnBan(ALastUrl: String);
  public
    constructor Create(AConfig: IConfigDataProvider);
    destructor Destroy; override;
    procedure PreDownload(
      ADownloader: ITileDownlodSession;
      ATile: TPoint;
      AZoom: Byte;
      AUrl: string
    );
    function PostCheckDownload(
      ADownloader: ITileDownlodSession;
      ATile: TPoint;
      AZoom: Byte;
      AUrl: string;
      ADownloadResult: TDownloadTileResult;
      AStatusCode: Cardinal;
      AContentType: string;
      ADownloadBuffer: Pointer;
      ADownloadSize: Cardinal
    ): TDownloadTileResult;
  end;



implementation

uses
  ShellAPI,
  Classes,
  SysUtils,
  Forms,
  SHDocVw;

type
  TExecOnBan = class
  private
    FLastUrl: string;
    procedure ExecOnBan;
  public
    procedure Exec(ALastUrl: String);
  end;

{ TExecOnBan }

procedure TExecOnBan.Exec(ALastUrl: String);
begin
  FLastUrl := ALastUrl;
  TThread.Synchronize(nil, ExecOnBan);
end;

procedure TExecOnBan.ExecOnBan;
begin
  ShellExecute(Fmain.Handle, nil, PChar(FLastUrl), nil, nil, SW_RESTORE);
end;

{ TAntiBanStuped }

procedure TAntiBanStuped.addDwnforban;
begin
  if FPreloadPage='' then begin
    FMain.WebBrowser1.Navigate('http://maps.google.com/?ie=UTF8&ll='+inttostr(random(100)-50)+','+inttostr(random(300)-150)+'&spn=1,1&t=k&z=8');
  end else begin
    FMain.WebBrowser1.NavigateWait(FPreloadPage);
  end;
  while (FMain.WebBrowser1.ReadyState<>READYSTATE_COMPLETE) do begin
    Application.ProcessMessages;
  end;
end;

function TAntiBanStuped.CheckIsBan(ATile: TPoint; AZoom: Byte;
  AUrl: string; ADownloadResult: TDownloadTileResult;
  AStatusCode: Cardinal; AContentType: string; ADownloadBuffer: Pointer;
  ADownloadSize: Cardinal): Boolean;
begin
  Result := false;
  if (ADownloadResult = dtrErrorMIMEType)
    and(ADownloadSize <> 0)
    and(FBanIfLen <> 0)
    and(ADownloadSize < (FBanIfLen + 50))
    and(ADownloadSize >(FBanIfLen-50)) then
  begin
    result := true;
  end;
end;

constructor TAntiBanStuped.Create(AConfig: IConfigDataProvider);
var
  VParams: IConfigDataProvider;
begin
  FBanCS := TCriticalSection.Create;
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');
  FUsePreloadPage:=VParams.ReadInteger('UsePreloadPage',0);
  FPreloadPage:=VParams.ReadString('PreloadPage','');
  FBanIfLen:=VParams.ReadInteger('BanIfLen',0);
  FContent_Type:=VParams.ReadString('ContentType','image/jpg');
end;

destructor TAntiBanStuped.Destroy;
begin
  FreeAndNil(FBanCS);
  inherited;
end;

procedure TAntiBanStuped.ExecOnBan(ALastUrl: String);
begin
  FBanCS.Acquire;
  if FBanFlag then begin
    FBanFlag := false;
    FBanCS.Release;
    with TExecOnBan.Create do try
      Exec(ALastUrl);
    finally
      Free;
    end;
  end else begin
    FBanCS.Release;
  end;

end;

procedure TAntiBanStuped.IncDownloadedAndCheckAntiBan;
var
  cnt: Integer;
  RunAntiBan: Boolean;
begin
  cnt := InterlockedIncrement(FDownloadTilesCount);
  if (FUsePreloadPage>0) then begin
    if (FUsePreloadPage > 1) then begin
      RunAntiBan := (cnt mod FUsePreloadPage) = 0;
    end else begin
      RunAntiBan := (cnt = 1);
    end;
    if RunAntiBan then begin
      TThread.Synchronize(nil, addDwnforban);
    end;
  end;
end;

function TAntiBanStuped.PostCheckDownload(
  ADownloader: ITileDownlodSession; ATile: TPoint; AZoom: Byte;
  AUrl: string; ADownloadResult: TDownloadTileResult;
  AStatusCode: Cardinal; AContentType: string; ADownloadBuffer: Pointer;
  ADownloadSize: Cardinal): TDownloadTileResult;
begin
  Result := ADownloadResult;
  if CheckIsBan(ATile, AZoom, AUrl, ADownloadResult, AStatusCode, AContentType, ADownloadBuffer, ADownloadSize) then begin
    Result := dtrBanError;
  end;

  if Result = dtrBanError then begin
    ExecOnBan(AUrl);
  end else if Result = dtrOK then begin
    FBanCS.Acquire;
    FBanFlag := True;
    FBanCS.Release;
  end;
end;

procedure TAntiBanStuped.PreDownload(
  ADownloader: ITileDownlodSession; ATile: TPoint; AZoom: Byte;
  AUrl: string);
begin
  IncDownloadedAndCheckAntiBan;
end;

end.
