{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_AntiBanStuped;

interface

uses
  SysUtils,
  i_BinaryData,
  i_ConfigDataProvider,
  i_AntiBan,
  i_SimpleFlag,
  i_DownloadRequest,
  i_DownloadResult,
  i_DownloadResultFactory,
  i_InvisibleBrowser,
  u_BaseInterfacedObject;

type
  TAntiBanStuped = class(TBaseInterfacedObject, IAntiBan)
  private
    FInvisibleBrowser: IInvisibleBrowser;
    FUsePreloadPage: integer;
    FPreloadPage: string;
    FDownloadTilesCounter: ICounter;
    FBanFlag: ISimpleFlag;
    procedure addDwnforban;
    procedure IncDownloadedAndCheckAntiBan;
    procedure ExecOnBan(const ALastUrl: string);
  private
    procedure PreDownload(
      const ARequest: IDownloadRequest
    );
    function PostCheckDownload(
      const AResultFactory: IDownloadResultFactory;
      const ARequest: IDownloadRequest;
      const ARecivedBuffer: IBinaryData;
      var AStatusCode: Cardinal;
      var AResponseHead: AnsiString
    ): IDownloadResult;
  public
    constructor Create(
      const AInvisibleBrowser: IInvisibleBrowser;
      const AConfig: IConfigDataProvider
    );
  end;



implementation

uses
  Classes,
  u_SimpleFlagWithInterlock,
  u_InetFunc;

type
  TExecOnBan = class
  private
    FLastUrl: string;
    procedure ExecOnBan;
  public
    procedure Exec(const ALastUrl: string);
  end;

{ TExecOnBan }

procedure TExecOnBan.Exec(const ALastUrl: string);
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
  VUrl: string;
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
  Assert(AInvisibleBrowser <> nil);
  Assert(AConfig <> nil);
  inherited Create;
  FInvisibleBrowser := AInvisibleBrowser;
  FDownloadTilesCounter := TCounterInterlock.Create;
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');
  FUsePreloadPage := VParams.ReadInteger('UsePreloadPage', 0);
  FPreloadPage := VParams.ReadString('PreloadPage', '');
  FBanFlag := TSimpleFlagWithInterlock.Create;
end;

procedure TAntiBanStuped.ExecOnBan(const ALastUrl: string);
begin
  if FBanFlag.CheckFlagAndReset then begin
    with TExecOnBan.Create do begin
      try
        Exec(ALastUrl);
      finally
        Free;
      end;
    end;
    FBanFlag.SetFlag;
  end;
end;

procedure TAntiBanStuped.IncDownloadedAndCheckAntiBan;
var
  cnt: Integer;
  RunAntiBan: Boolean;
begin
  cnt := FDownloadTilesCounter.Inc;
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
  var AResponseHead: AnsiString
): IDownloadResult;
begin
  Result := nil;
  if false then begin // TODO: сделать хоть какую-то проверку
    Result := AResultFactory.BuildBanned(ARequest, AStatusCode, AResponseHead);
  end;

  if Supports(Result, IDownloadResultBanned) then begin
    ExecOnBan(ARequest.Url);
  end else if Supports(Result, IDownloadResultOk) then begin
    FBanFlag.SetFlag;
  end;
end;

procedure TAntiBanStuped.PreDownload(
  const ARequest: IDownloadRequest
);
begin
  IncDownloadedAndCheckAntiBan;
end;

end.
