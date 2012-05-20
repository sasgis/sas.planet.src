{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
  SysUtils,
  i_BinaryData,
  i_ConfigDataProvider,
  i_AntiBan,
  i_DownloadRequest,
  i_DownloadResult,
  i_DownloadResultFactory,
  i_InvisibleBrowser;

type
  TAntiBanStuped = class(TInterfacedObject, IAntiBan)
  private
    FInvisibleBrowser: IInvisibleBrowser;
    FUsePreloadPage: integer;
    FPreloadPage: string;
    FDownloadTilesCount: Longint;
    FBanFlag: Boolean;
    FBanCS: IReadWriteSync;
    procedure addDwnforban;
    procedure IncDownloadedAndCheckAntiBan;
    procedure ExecOnBan(const ALastUrl: String);
  private
    procedure PreDownload(
      const ARequest: IDownloadRequest
    );
    function PostCheckDownload(
      const AResultFactory: IDownloadResultFactory;
      const ARequest: IDownloadRequest;
      const ARecivedBuffer: IBinaryData;
      var AStatusCode: Cardinal;
      var AResponseHead: string
    ): IDownloadResult;
  public
    constructor Create(
      const AInvisibleBrowser: IInvisibleBrowser;
      const AConfig: IConfigDataProvider
    );
    destructor Destroy; override;
  end;



implementation

uses
  Classes,
  u_Synchronizer,
  u_InetFunc;

type
  TExecOnBan = class
  private
    FLastUrl: string;
    procedure ExecOnBan;
  public
    procedure Exec(const ALastUrl: String);
  end;

{ TExecOnBan }

procedure TExecOnBan.Exec(const ALastUrl: String);
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
var
  VUrl: WideString;
begin
  if FPreloadPage = '' then begin
    VUrl := 'http://maps.google.com/?ie=UTF8&ll=' + inttostr(random(100) - 50) + ',' + inttostr(random(300) - 150) + '&spn=1,1&t=k&z=8';
  end else begin
    VUrl := FPreloadPage;
  end;
  FInvisibleBrowser.NavigateAndWait(VUrl);
end;

constructor TAntiBanStuped.Create(
  const AInvisibleBrowser: IInvisibleBrowser;
  const AConfig: IConfigDataProvider
);
var
  VParams: IConfigDataProvider;
begin
  inherited Create;
  FInvisibleBrowser := AInvisibleBrowser;
  FBanCS := MakeSyncObj(Self, TRUE);
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');
  FUsePreloadPage := VParams.ReadInteger('UsePreloadPage', 0);
  FPreloadPage := VParams.ReadString('PreloadPage', '');
end;

destructor TAntiBanStuped.Destroy;
begin
  FBanCS := nil;
  inherited;
end;

procedure TAntiBanStuped.ExecOnBan(const ALastUrl: String);
begin
  FBanCS.BeginWrite;
  if FBanFlag then begin
    FBanFlag := false;
    FBanCS.EndWrite;

    with TExecOnBan.Create do begin
      try
        Exec(ALastUrl);
      finally
        Free;
      end;
    end;
  end else begin
    FBanCS.EndWrite;
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
      addDwnforban;
    end;
  end;
end;

function TAntiBanStuped.PostCheckDownload(
  const AResultFactory: IDownloadResultFactory;
  const ARequest: IDownloadRequest;
  const ARecivedBuffer: IBinaryData;
  var AStatusCode: Cardinal;
  var AResponseHead: string
): IDownloadResult;
begin
  Result := nil;
  if false then begin // TODO: сделать хоть какую-то проверку
    Result := AResultFactory.BuildBanned(ARequest, AStatusCode, AResponseHead);
  end;

  if Supports(Result, IDownloadResultBanned) then begin
    ExecOnBan(ARequest.Url);
  end else if Supports(Result, IDownloadResultOk) then begin
    FBanCS.BeginWrite;
    try
      FBanFlag := True;
    finally
      FBanCS.EndWrite;
    end;
  end;
end;

procedure TAntiBanStuped.PreDownload(
  const ARequest: IDownloadRequest
);
begin
  IncDownloadedAndCheckAntiBan;
end;

end.
