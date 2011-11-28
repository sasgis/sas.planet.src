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

unit u_TileDownloaderUIOneTile;

interface

uses
  Windows,
  SyncObjs,
  Classes,
  Types,
  i_JclNotify,
  i_TileError,
  i_DownloadInfoSimple,
  u_TileDownloaderThread,
  u_MapType;

type
  TTileDownloaderUIOneTile = class(TTileDownloaderThread)
  private
    FLoadXY: TPoint;
    FZoom: Byte;
//    FEvent: TEvent;
//    FRequestFinishedListener: IJclListener;
//    procedure OnRequestFinished(Sender: TObject);
  protected
    procedure Execute; override;
  public
    constructor Create(
      AXY: TPoint;
      AZoom: byte;
      AMapType: TMapType;
      ADownloadInfo: IDownloadInfoSimple;
      AErrorLogger: ITileErrorLogger
    ); overload;
  end;

implementation

uses
  SysUtils,
  u_TileErrorInfo;

constructor TTileDownloaderUIOneTile.Create(
  AXY: TPoint;
  AZoom: byte;
  AMapType: TMapType;
  ADownloadInfo: IDownloadInfoSimple;
  AErrorLogger: ITileErrorLogger
);
begin
  inherited Create(False, ADownloadInfo, AErrorLogger, 1);
  FLoadXY := AXY;
  FZoom := AZoom;
  FMapType := AMapType;
  Priority := tpLower;
  FreeOnTerminate := true;
  randomize;
end;

procedure TTileDownloaderUIOneTile.Execute;
var
  VOperatonID: Integer;
begin
  if FMapType.Abilities.UseDownload then
  try
    VOperatonID := FCancelNotifier.CurrentOperation;
    Download(FLoadXY, FZoom, OnTileDownload, False, FCancelNotifier, VOperatonID);
  except
    on E: Exception do begin
      FErrorLogger.LogError(
        TTileErrorInfo.Create(
          FMapType,
          FZoom,
          FLoadXY,
          E.Message
        )
      );
    end;
  end;
end;

end.
