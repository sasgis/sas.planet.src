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

unit u_DownloaderHttpWithTTL;

interface

uses
  SysUtils,
  i_NotifierOperation,
  i_ListenerTime,
  i_NotifierTime,
  i_DownloadRequest,
  i_DownloadResult,
  i_DownloadResultFactory,
  i_Downloader,
  u_DownloaderHttp,
  u_BaseInterfacedObject;

type
  TDownloaderHttpWithTTL = class(TBaseInterfacedObject, IDownloader)
  private
    FResultFactory: IDownloadResultFactory;
    FGCNotifier: INotifierTime;
    FAllowUseCookie: Boolean;
    FTryDetectContentType: Boolean;
    FTTLListener: IListenerTimeWithUsedFlag;
    FCS: IReadWriteSync;
    FDownloader: IDownloader;
    procedure OnTTLTrim;
  private
    function DoRequest(
      const ARequest: IDownloadRequest;
      const ACancelNotifier: INotifierOperation;
      const AOperationID: Integer
    ): IDownloadResult;
  public
    constructor Create(
      const AGCNotifier: INotifierTime;
      const AResultFactory: IDownloadResultFactory;
      const AAllowUseCookie: Boolean = False;
      const ATryDetectContentType: Boolean = False
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_Synchronizer,
  u_ListenerTime;

{ TDownloaderHttpWithTTL }

constructor TDownloaderHttpWithTTL.Create(
  const AGCNotifier: INotifierTime;
  const AResultFactory: IDownloadResultFactory;
  const AAllowUseCookie: Boolean;
  const ATryDetectContentType: Boolean
);
const
  CHttpClientTTL = 300000; // 5 min
begin
  inherited Create;
  FAllowUseCookie := AAllowUseCookie;
  FTryDetectContentType := ATryDetectContentType;
  FResultFactory := AResultFactory;
  FGCNotifier := AGCNotifier;
  FCS := MakeSyncRW_Std(Self, FALSE);
  FTTLListener := TListenerTTLCheck.Create(Self.OnTTLTrim, CHttpClientTTL);
  FGCNotifier.Add(FTTLListener);
end;

destructor TDownloaderHttpWithTTL.Destroy;
begin
  if Assigned(FGCNotifier) and Assigned(FTTLListener) then begin
    FGCNotifier.Remove(FTTLListener);
    FTTLListener := nil;
    FGCNotifier := nil;
  end;
  FCS := nil;
  inherited;
end;

function TDownloaderHttpWithTTL.DoRequest(
  const ARequest: IDownloadRequest;
  const ACancelNotifier: INotifierOperation;
  const AOperationID: Integer
): IDownloadResult;
var
  VDownloader: IDownloader;
begin
  FCS.BeginWrite;
  try
    FTTLListener.UpdateUseTime;
    VDownloader := FDownloader;
    if VDownloader = nil then begin
      VDownloader := TDownloaderHttp.Create(FResultFactory, FAllowUseCookie, True, FTryDetectContentType);
      FDownloader := VDownloader;
    end;
    Result := VDownloader.DoRequest(ARequest, ACancelNotifier, AOperationID);
    FTTLListener.UpdateUseTime;
  finally
    FCS.EndWrite;
  end;
end;

procedure TDownloaderHttpWithTTL.OnTTLTrim;
begin
  FCS.BeginWrite;
  try
    FDownloader := nil;
  finally
    FCS.EndWrite;
  end;
end;

end.
