{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_AntiBanStuped;

interface

uses
  Windows,
  SyncObjs,
  i_ConfigDataProvider,
  i_AntiBan,
  i_ProxySettings,
  i_DownloadRequest,
  i_DownloadResult,
  i_DownloadResultFactory,
  i_TileDownlodSession;

type
  TAntiBanStuped = class(TInterfacedObject, IAntiBan)
  private
    FUsePreloadPage: integer;
    FPreloadPage: string;
    FDownloadTilesCount: Longint;
    FBanFlag: Boolean;
    FBanCS: TCriticalSection;
    FProxyConfig: IProxyConfig;
    procedure addDwnforban;
    procedure IncDownloadedAndCheckAntiBan();
    function CheckIsBan(
      ADownloadResult: IDownloadResult
    ): Boolean;
    procedure ExecOnBan(ALastUrl: String);
  public
    constructor Create(
      AProxyConfig: IProxyConfig;
      AConfig: IConfigDataProvider
    );
    destructor Destroy; override;
    procedure PreDownload(
      ARequest: IDownloadRequest;
      ADownloader: ITileDownlodSession
    );
    function PostCheckDownload(
      AResultFactory: IDownloadResultFactory;
      ADownloader: ITileDownlodSession;
      ADownloadResult: IDownloadResult
    ): IDownloadResult;
  end;



implementation

uses
  Classes,
  SysUtils,
  frm_InvisibleBrowser;

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
  OpenUrlInBrowser(FLastUrl);
end;

{ TAntiBanStuped }

procedure TAntiBanStuped.addDwnforban;
begin
  if FPreloadPage = '' then begin
    frmInvisibleBrowser.NavigateAndWait('http://maps.google.com/?ie=UTF8&ll=' + inttostr(random(100) - 50) + ',' + inttostr(random(300) - 150) + '&spn=1,1&t=k&z=8');
  end else begin
    frmInvisibleBrowser.NavigateAndWait(FPreloadPage);
  end;
end;

function TAntiBanStuped.CheckIsBan(
  ADownloadResult: IDownloadResult
): Boolean;
begin
  Result := false;
end;

constructor TAntiBanStuped.Create(
  AProxyConfig: IProxyConfig;
  AConfig: IConfigDataProvider
);
var
  VParams: IConfigDataProvider;
begin
  FProxyConfig := AProxyConfig;
  FBanCS := TCriticalSection.Create;
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');
  FUsePreloadPage := VParams.ReadInteger('UsePreloadPage', 0);
  FPreloadPage := VParams.ReadString('PreloadPage', '');
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
  if (FUsePreloadPage > 0) then begin
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
  AResultFactory: IDownloadResultFactory;
  ADownloader: ITileDownlodSession;
  ADownloadResult: IDownloadResult
): IDownloadResult;
begin
  Result := ADownloadResult;
  if CheckIsBan(ADownloadResult) then begin
    Result := AResultFactory.BuildBanned(ADownloadResult.Request, 'X3');
  end;

  if Supports(Result, IDownloadResultBanned) then begin
    ExecOnBan(ADownloadResult.Request.Url);
  end else if Supports(Result, IDownloadResultOk) then begin
    FBanCS.Acquire;
    FBanFlag := True;
    FBanCS.Release;
  end;
end;

procedure TAntiBanStuped.PreDownload(
  ARequest: IDownloadRequest;
  ADownloader: ITileDownlodSession
);
begin
  IncDownloadedAndCheckAntiBan;
end;

end.
