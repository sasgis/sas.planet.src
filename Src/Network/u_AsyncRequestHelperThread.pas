{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2019, SAS.Planet development team.                      *}
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

unit u_AsyncRequestHelperThread;

interface

uses
  Windows,
  Classes,
  SysUtils,
  i_NotifierOperation,
  i_Downloader,
  i_DownloadResult,
  i_DownloadRequest;

type
  TAsyncRequestHelperThread = class(TThread)
  private
    FDownloader: IDownloader;
    FRequest: IDownloadRequest;
    FCancelNotifier: INotifierOperation;
    FOperationID: Integer;
    FOnResultCallBack: TRequestAsyncCallBack;
  protected
    procedure Execute; override;
  public
    constructor Create(
      const ADownloader: IDownloader;
      const ARequest: IDownloadRequest;
      const ACancelNotifier: INotifierOperation;
      const AOperationID: Integer;
      const AOnResultCallBack: TRequestAsyncCallBack
    );
  end;

implementation

uses
  u_ReadableThreadNames;

{ TAsyncRequestHelperThread }

constructor TAsyncRequestHelperThread.Create(
  const ADownloader: IDownloader;
  const ARequest: IDownloadRequest;
  const ACancelNotifier: INotifierOperation;
  const AOperationID: Integer;
  const AOnResultCallBack: TRequestAsyncCallBack
);
begin
  FDownloader := ADownloader;
  FRequest := ARequest;
  FCancelNotifier := ACancelNotifier;
  FOperationID := AOperationID;
  FOnResultCallBack := AOnResultCallBack;
  FreeOnTerminate := True;
  inherited Create(False);
end;

procedure TAsyncRequestHelperThread.Execute;
var
  VResult: IDownloadResult;
begin
  SetCurrentThreadName(Self.ClassName);
  try
    VResult := FDownloader.DoRequest(FRequest, FCancelNotifier, FOperationID);
    FOnResultCallBack(VResult, FOperationID);
  except
    {$IFDEF DEBUG}
    on E: Exception do begin
      OutputDebugString(PChar(IntToStr(GetCurrentThreadId) + ' <E> ' + E.ClassName + ':' + E.Message));
    end;
    {$ENDIF}
  end;
end;

end.
