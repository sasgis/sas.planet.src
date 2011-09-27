{******************************************************************************}
{* SAS.Планета (SAS.Planet)                                                   *}
{* Copyright (C) 2007-2011, авторы программы SAS.Планета (SAS.Planet).        *}
{* Это программа является свободным программным обеспечением. Вы можете       *}
{* распространять и/или модифицировать её согласно условиям Стандартной       *}
{* Общественной Лицензии GNU, опубликованной Фондом Свободного Программного   *}
{* Обеспечения, версии 3. Эта программа распространяется в надежде, что она   *}
{* будет полезной, но БЕЗ ВСЯКИХ ГАРАНТИЙ, в том числе подразумеваемых        *}
{* гарантий ТОВАРНОГО СОСТОЯНИЯ ПРИ ПРОДАЖЕ и ГОДНОСТИ ДЛЯ ОПРЕДЕЛЁННОГО      *}
{* ПРИМЕНЕНИЯ. Смотрите Стандартную Общественную Лицензию GNU версии 3, для   *}
{* получения дополнительной информации. Вы должны были получить копию         *}
{* Стандартной Общественной Лицензии GNU вместе с программой. В случае её     *}
{* отсутствия, посмотрите http://www.gnu.org/licenses/.                       *}
{*                                                                            *}
{* http://sasgis.ru/sasplanet                                                 *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_TileDownloaderUIOneTile;

interface

uses
  Windows,
  Classes,
  Types,
  i_TileError,
  i_DownloadInfoSimple,
  u_TileDownloaderThreadBase,
  u_MapType;

type
  TTileDownloaderUIOneTile = class(TTileDownloaderThreadBase)
  private
    FMapTileUpdateEvent: TMapTileUpdateEvent;
    FErrorLogger: ITileErrorLogger;
    FDownloadInfo: IDownloadInfoSimple;
    FLoadXY: TPoint;

    procedure AfterWriteToFile;
  protected
    procedure Execute; override;
  public
    constructor Create(
      AXY: TPoint;
      AZoom: byte;
      AMapType: TMapType;
      ADownloadInfo: IDownloadInfoSimple;
      AMapTileUpdateEvent: TMapTileUpdateEvent;
      AErrorLogger: ITileErrorLogger
    ); overload;
  end;

implementation

uses
  SysUtils,
  i_DownloadResult,
  u_TileErrorInfo,
  u_ResStrings;

constructor TTileDownloaderUIOneTile.Create(
  AXY: TPoint;
  AZoom: byte;
  AMapType: TMapType;
  ADownloadInfo: IDownloadInfoSimple;
  AMapTileUpdateEvent: TMapTileUpdateEvent;
  AErrorLogger: ITileErrorLogger
);
begin
  inherited Create(False);
  FMapTileUpdateEvent := AMapTileUpdateEvent;
  FDownloadInfo := ADownloadInfo;
  FErrorLogger := AErrorLogger;
  FLoadXY := AXY;
  FZoom := AZoom;
  FMapType := AMapType;

  Priority := tpLower;
  FreeOnTerminate := true;
  randomize;
end;

procedure TTileDownloaderUIOneTile.AfterWriteToFile;
begin
  if Addr(FMapTileUpdateEvent) <> nil then begin
    FMapTileUpdateEvent(FMapType, FZoom, FLoadXY);
  end;
end;

procedure TTileDownloaderUIOneTile.Execute;
var
  VResult: IDownloadResult;
  VErrorString: string;
  VResultOk: IDownloadResultOk;
  VResultDownloadError: IDownloadResultError;
  VOperatonID: Integer;
begin
  VOperatonID := FCancelNotifier.CurrentOperation;
  if FMapType.Abilities.UseDownload then begin
      try
        VResult := FMapType.DownloadTile(VOperatonID, FCancelNotifier, FLoadXY, FZoom, false);
        if not Terminated then begin
          VErrorString := '';
          if Supports(VResult, IDownloadResultOk, VResultOk) then begin
            FDownloadInfo.Add(1, VResultOk.Size);
          end else if Supports(VResult, IDownloadResultError, VResultDownloadError) then begin
            VErrorString := VResultDownloadError.ErrorText;
          end;
        end;
      except
        on E: Exception do begin
          VErrorString := E.Message;
        end;
      end;
  end else begin
    VErrorString := SAS_ERR_NotLoads;
  end;
  if not Terminated then begin
    if VErrorString = '' then begin
      Synchronize(AfterWriteToFile);
    end else begin
      FErrorLogger.LogError(
        TTileErrorInfo.Create(
          FMapType,
          FZoom,
          FLoadXY,
          VErrorString
        )
      );
    end;
  end;
end;

end.
