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

unit u_TileDownloadRequest;

interface

uses
  i_BinaryData,
  i_DownloadRequest,
  i_InetConfig,
  i_TileRequest,
  i_DownloadChecker,
  i_TileDownloadRequest,
  u_BaseInterfacedObject;

type
  TTileDownloadRequest = class(TBaseInterfacedObject, IDownloadRequest, ITileDownloadRequest, IRequestWithChecker)
  private
    FUrl: AnsiString;
    FRequestHeader: AnsiString;
    FInetConfig: IInetConfigStatic;
    FCheker: IDownloadChecker;
    FSource: ITileRequest;
  private
    function GetUrl: AnsiString;
    function GetRequestHeader: AnsiString;
    function GetInetConfig: IInetConfigStatic;
  private
    function GetSource: ITileRequest;
  private
    function GetChecker: IDownloadChecker;
  public
    constructor Create(
      const AUrl: AnsiString;
      const ARequestHeader: AnsiString;
      const AInetConfig: IInetConfigStatic;
      const ACheker: IDownloadChecker;
      const ASource: ITileRequest
    );
  end;

  TTileDownloadPostRequest = class(TTileDownloadRequest, IDownloadPostRequest)
  private
    FPostData: IBinaryData;
  private
    function GetPostData: IBinaryData;
  public
    constructor Create(
      const AUrl: AnsiString;
      const ARequestHeader: AnsiString;
      const APostData: IBinaryData;
      const AInetConfig: IInetConfigStatic;
      const ACheker: IDownloadChecker;
      const ASource: ITileRequest
    );
  end;

implementation

{ TTileDownloadRequest }

constructor TTileDownloadRequest.Create(
  const AUrl, ARequestHeader: AnsiString;
  const AInetConfig: IInetConfigStatic;
  const ACheker: IDownloadChecker;
  const ASource: ITileRequest
);
begin
  inherited Create;
  FUrl := AUrl;
  FRequestHeader := ARequestHeader;
  FInetConfig := AInetConfig;
  FSource := ASource;
  FCheker := ACheker;
end;

function TTileDownloadRequest.GetChecker: IDownloadChecker;
begin
  Result := FCheker;
end;

function TTileDownloadRequest.GetInetConfig: IInetConfigStatic;
begin
  Result := FInetConfig;
end;

function TTileDownloadRequest.GetRequestHeader: AnsiString;
begin
  Result := FRequestHeader;
end;

function TTileDownloadRequest.GetSource: ITileRequest;
begin
  Result := FSource;
end;

function TTileDownloadRequest.GetUrl: AnsiString;
begin
  Result := FUrl;
end;

{ TTileDownloadPostRequest }

constructor TTileDownloadPostRequest.Create(
  const AUrl, ARequestHeader: AnsiString;
  const APostData: IBinaryData;
  const AInetConfig: IInetConfigStatic;
  const ACheker: IDownloadChecker;
  const ASource: ITileRequest
);
begin
  inherited Create(
    AUrl,
    ARequestHeader,
    AInetConfig,
    ACheker,
    ASource
  );
  FPostData := APostData;
  Assert(FPostData <> nil);
end;

function TTileDownloadPostRequest.GetPostData: IBinaryData;
begin
  Result := FPostData;
end;

end.
