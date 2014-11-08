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

unit u_TileDownloadRequestBuilderLazy;

interface

uses
  SysUtils,
  i_NotifierOperation,
  i_TileRequest,
  i_Downloader,
  i_LastResponseInfo,
  i_TileDownloadRequest,
  i_TileDownloadRequestBuilder,
  i_TileDownloadRequestBuilderFactory,
  u_BaseInterfacedObject;

type
  TTileDownloadRequestBuilderLazy = class(TBaseInterfacedObject, ITileDownloadRequestBuilder)
  private
    FFactory: ITileDownloadRequestBuilderFactory;
    FDownloader: IDownloader;
    FBuilder: ITileDownloadRequestBuilder;
    FBuilderCS: IReadWriteSync;
  private
    function BuildRequest(
      const ASource: ITileRequest;
      const ALastResponseInfo: ILastResponseInfo;
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer
    ): ITileDownloadRequest;
  public
    constructor Create(
      const ADownloader: IDownloader;
      const AFactory: ITileDownloadRequestBuilderFactory
    );
  end;

implementation

uses
  u_Synchronizer;

{ TTileDownloadRequestBuilderLazy }

constructor TTileDownloadRequestBuilderLazy.Create(
  const ADownloader: IDownloader;
  const AFactory: ITileDownloadRequestBuilderFactory
);
begin
  inherited Create;
  FBuilderCS := GSync.SyncVariable.Make(Self.ClassName);
  FDownloader := ADownloader;
  FFactory := AFactory;
end;

function TTileDownloadRequestBuilderLazy.BuildRequest(
  const ASource: ITileRequest;
  const ALastResponseInfo: ILastResponseInfo;
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer
): ITileDownloadRequest;
var
  VBuilder: ITileDownloadRequestBuilder;
begin
  Result := nil;
  if (ACancelNotifier <> nil) and (not ACancelNotifier.IsOperationCanceled(AOperationID)) then begin
    if FFactory.State.GetStatic.Enabled then begin
      // allow build
      FBuilderCS.BeginWrite;
      try
        VBuilder := FBuilder;
        if VBuilder = nil then begin
          if FFactory.State.GetStatic.Enabled then begin
            VBuilder := FFactory.BuildRequestBuilder(FDownloader);
            if VBuilder <> nil then begin
              FBuilder := VBuilder;
            end;
          end;
        end;
      finally
        FBuilderCS.EndWrite;
      end;

      if VBuilder <> nil then begin
        Result := VBuilder.BuildRequest(ASource, ALastResponseInfo, ACancelNotifier, AOperationID);
      end;
    end;
  end;
end;

end.
