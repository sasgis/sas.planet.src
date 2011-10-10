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

unit u_TileDownloaderFrontEnd;

interface

uses
  Windows,
  SysUtils,
  i_ConfigDataProvider,
  i_CoordConverterFactory,
  i_LanguageManager,
  i_TileRequestBuilderConfig,
  i_TileDownloader,
  i_TileDownloaderConfig,
  i_ZmpInfo;

type
  TTileDownloaderFrontEnd = class
  private
    FDownloader: ITileDownloader;
    FUseDwn: Boolean;
  public
    constructor Create(
      AConfig: IConfigDataProvider;
      ATileDownloaderConfig: ITileDownloaderConfig;
      ATileRequestBuilderConfig: ITileRequestBuilderConfig;
      AZmp: IZmpInfo;
      ACoordConverterFactory: ICoordConverterFactory;
      ALangManager: ILanguageManager
    );
    destructor Destroy; override;
    procedure Download(AEvent: ITileDownloaderEvent);
    property UseDwn: Boolean read FUseDwn;
  end;

implementation

uses
  u_TileDownloaderBaseCore;

{ TTileDownloaderFrontEnd }

constructor TTileDownloaderFrontEnd.Create(
  AConfig: IConfigDataProvider;
  ATileDownloaderConfig: ITileDownloaderConfig;
  ATileRequestBuilderConfig: ITileRequestBuilderConfig;
  AZmp: IZmpInfo;
  ACoordConverterFactory: ICoordConverterFactory;
  ALangManager: ILanguageManager
);
var
  VDownloaderStr: string;
begin
  inherited Create;
  FDownloader := nil;
  FUseDwn := False;
  try
    VDownloaderStr := AConfig.ReadString('Downloader', 'sasplanet');
    if LowerCase(VDownloaderStr) = 'sasplanet' then begin
      FDownloader := TTileDownloaderBaseCore.Create(
        AConfig,
        ATileDownloaderConfig,
        ATileRequestBuilderConfig,
        AZmp,
        ACoordConverterFactory,
        ALangManager
      );
      if Assigned(FDownloader) then begin
        FUseDwn := FDownloader.Enabled;
      end;
    end;
  finally
    if FDownloader = nil then begin
      FUseDwn := False;
    end;
  end;
end;

destructor TTileDownloaderFrontEnd.Destroy;
begin
  FDownloader := nil;
  inherited Destroy;
end;

procedure TTileDownloaderFrontEnd.Download(AEvent: ITileDownloaderEvent);
begin
  if FUseDwn and Assigned(AEvent) then begin
    if Assigned(FDownloader) then begin
      FDownloader.Download(AEvent)
    end else begin
      FUseDwn := False;
    end;
  end;
end;

end.
