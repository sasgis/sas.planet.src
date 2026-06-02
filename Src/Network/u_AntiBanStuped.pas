{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
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
    FIsEnabled: Boolean;
    FInvisibleBrowser: IInvisibleBrowser;
    FUsePreloadPage: integer;
    FPreloadPage: string;
    FDownloadTilesCounter: ICounter;
    FBanFlag: ISimpleFlag;
    procedure ProcessAntiBan;
    procedure IncDownloadedAndCheckAntiBan;
    procedure ExecOnBan(const ALastUrl: string);
  private
    { IAntiBan }
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

  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');
  FUsePreloadPage := VParams.ReadInteger('UsePreloadPage', 0);
  FPreloadPage := VParams.ReadString('PreloadPage', '');

  if (FUsePreloadPage > 0) and (FPreloadPage <> '') then begin
    FIsEnabled := True;
    FInvisibleBrowser := AInvisibleBrowser;
    FDownloadTilesCounter := TCounterInterlock.Create;
    FBanFlag := TSimpleFlagWithInterlock.Create;
  end;
end;

procedure TAntiBanStuped.IncDownloadedAndCheckAntiBan;
var
  VCount: Integer;
  VRunAntiBan: Boolean;
begin
  VCount := FDownloadTilesCounter.Inc;
  if FUsePreloadPage > 0 then begin
    if FUsePreloadPage > 1 then begin
      VRunAntiBan := (VCount mod FUsePreloadPage) = 0;
    end else begin
      VRunAntiBan := (VCount = 1);
    end;
    if VRunAntiBan then begin
      ProcessAntiBan;
    end;
  end;
end;

procedure TAntiBanStuped.ProcessAntiBan;
begin
  FInvisibleBrowser.NavigateAndWait(FPreloadPage);
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

function TAntiBanStuped.PostCheckDownload(
  const AResultFactory: IDownloadResultFactory;
  const ARequest: IDownloadRequest;
  const ARecivedBuffer: IBinaryData;
  var AStatusCode: Cardinal;
  var AResponseHead: AnsiString
): IDownloadResult;
begin
  Result := nil;

  if not FIsEnabled then begin
    Exit;
  end;

  if False then begin // TODO: сделать хоть какую-то проверку
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
  if FIsEnabled then begin
    IncDownloadedAndCheckAntiBan;
  end;
end;

end.
