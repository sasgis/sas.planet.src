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

unit u_TileRequestResult;

interface

uses
  i_TileRequest,
  i_DownloadResult,
  i_TileDownloadRequest,
  i_TileRequestResult,
  u_BaseInterfacedObject;

type
  TTileRequestResult = class(TBaseInterfacedObject, ITileRequestResult)
  private
    FRequest: ITileRequest;
  protected
    function GetRequest: ITileRequest;
  public
    constructor Create(
      const ARequest: ITileRequest
    );
  end;

  TTileRequestResultCanceledBeforBuildDownloadRequest = class(TTileRequestResult, ITileRequestResultCanceled)
  end;

  TTileRequestResultCanceledAfterBuildDownloadRequest = class(TTileRequestResultCanceledBeforBuildDownloadRequest, ITileRequestResultWithDownloadRequest)
  private
    FDownloadRequest: ITileDownloadRequest;
  protected
    function GetDownloadRequest: ITileDownloadRequest;
  public
    constructor Create(
      const ADownloadRequest: ITileDownloadRequest
    );
  end;

  TTileRequestResultCanceledAfterDownloadRequest = class(TTileRequestResultCanceledAfterBuildDownloadRequest, ITileRequestResultWithDownloadResult)
  private
    FDownloadResult: IDownloadResult;
  protected
    function GetDownloadResult: IDownloadResult;
  public
    constructor Create(
      const ADownloadResult: IDownloadResult
    );
  end;

  TTileRequestResultOk = class(TTileRequestResult, ITileRequestResultOk, ITileRequestResultWithDownloadRequest, ITileRequestResultWithDownloadResult)
  private
    FDownloadRequest: ITileDownloadRequest;
    FDownloadResult: IDownloadResult;
  protected
    function GetDownloadRequest: ITileDownloadRequest;
  protected
    function GetDownloadResult: IDownloadResult;
  public
    constructor Create(
      const ADownloadResult: IDownloadResult
    );
  end;

  TTileRequestResultErrorBeforBuildDownloadRequest = class(TTileRequestResult, ITileRequestResultError)
  private
    FErrorText: string;
  protected
    function GetErrorText: string;
  public
    constructor Create(
      const ARequest: ITileRequest;
      const AErrorText: string
    );
  end;

  TTileRequestResultErrorAfterBuildDownloadRequest = class(TTileRequestResultErrorBeforBuildDownloadRequest, ITileRequestResultWithDownloadRequest)
  private
    FDownloadRequest: ITileDownloadRequest;
  protected
    function GetDownloadRequest: ITileDownloadRequest;
  public
    constructor Create(
      const ADownloadRequest: ITileDownloadRequest;
      const AErrorText: string
    );
  end;

  TTileRequestResultDownloadError = class(TTileRequestResultErrorAfterBuildDownloadRequest, ITileRequestResultWithDownloadResult)
  private
    FDownloadResultError: IDownloadResultError;
  protected
    function GetDownloadResult: IDownloadResult;
  public
    constructor Create(
      const ADownloadResultError: IDownloadResultError
    );
  end;

  TTileRequestResultErrorAfterDownloadRequest = class(TTileRequestResultErrorAfterBuildDownloadRequest, ITileRequestResultWithDownloadResult)
  private
    FDownloadResult: IDownloadResult;
  protected
    function GetDownloadResult: IDownloadResult;
  public
    constructor Create(
      const ADownloadResult: IDownloadResult;
      const AErrorText: string
    );
  end;

implementation

{ TTileRequestResult }

constructor TTileRequestResult.Create(const ARequest: ITileRequest);
begin
  inherited Create;
  FRequest := ARequest;
end;

function TTileRequestResult.GetRequest: ITileRequest;
begin
  Result := FRequest;
end;

{ TTileRequestResultCanceledAfterBuildDownloadRequest }

constructor TTileRequestResultCanceledAfterBuildDownloadRequest.Create(
  const ADownloadRequest: ITileDownloadRequest
);
begin
  FDownloadRequest := ADownloadRequest;
  inherited Create(FDownloadRequest.Source);
end;

function TTileRequestResultCanceledAfterBuildDownloadRequest.GetDownloadRequest: ITileDownloadRequest;
begin
  Result := FDownloadRequest;
end;

{ TTileRequestResultCanceledAfterDownloadRequest }

constructor TTileRequestResultCanceledAfterDownloadRequest.Create(
  const ADownloadResult: IDownloadResult
);
var
  VRequest: ITileDownloadRequest;
begin
  FDownloadResult := ADownloadResult;
  VRequest := FDownloadResult.Request as ITileDownloadRequest;
  inherited Create(VRequest);
end;

function TTileRequestResultCanceledAfterDownloadRequest.GetDownloadResult: IDownloadResult;
begin
  Result := FDownloadResult;
end;

{ TTileRequestResultOk }

constructor TTileRequestResultOk.Create(const ADownloadResult: IDownloadResult);
begin
  FDownloadResult := ADownloadResult;
  FDownloadRequest := FDownloadResult.Request as ITileDownloadRequest;
  inherited Create(FDownloadRequest.Source);
end;

function TTileRequestResultOk.GetDownloadRequest: ITileDownloadRequest;
begin
  Result := FDownloadRequest;
end;

function TTileRequestResultOk.GetDownloadResult: IDownloadResult;
begin
  Result := FDownloadResult;
end;

{ TTileRequestResultErrorAfterBuildDownloadRequest }

constructor TTileRequestResultErrorAfterBuildDownloadRequest.Create(
  const ADownloadRequest: ITileDownloadRequest;
  const AErrorText: string
);
begin
  FDownloadRequest := ADownloadRequest;
  inherited Create(FDownloadRequest.Source, AErrorText);
end;

function TTileRequestResultErrorAfterBuildDownloadRequest.GetDownloadRequest: ITileDownloadRequest;
begin
  Result := FDownloadRequest;
end;

{ TTileRequestResultErrorAfterDownloadRequest }

constructor TTileRequestResultErrorAfterDownloadRequest.Create(
  const ADownloadResult: IDownloadResult;
  const AErrorText: string
);
var
  VRequest: ITileDownloadRequest;
begin
  FDownloadResult := ADownloadResult;
  VRequest := FDownloadResult.Request as ITileDownloadRequest;
  inherited Create(VRequest, AErrorText);
end;

function TTileRequestResultErrorAfterDownloadRequest.GetDownloadResult: IDownloadResult;
begin
  Result := FDownloadResult;
end;

{ TTileRequestResultErrorBeforBuildDownloadRequest }

constructor TTileRequestResultErrorBeforBuildDownloadRequest.Create(
  const ARequest: ITileRequest;
  const AErrorText: string
);
begin
  inherited Create(ARequest);
  FErrorText := AErrorText;
end;

function TTileRequestResultErrorBeforBuildDownloadRequest.GetErrorText: string;
begin
  Result := FErrorText;
end;

{ TTileRequestResultDownloadError }

constructor TTileRequestResultDownloadError.Create(
  const ADownloadResultError: IDownloadResultError);
var
  VRequest: ITileDownloadRequest;
begin
  FDownloadResultError := ADownloadResultError;
  VRequest := FDownloadResultError.Request as ITileDownloadRequest;
  inherited Create(VRequest, 'Download error: ' + FDownloadResultError.ErrorText);
end;

function TTileRequestResultDownloadError.GetDownloadResult: IDownloadResult;
begin
  Result := FDownloadResultError;
end;

end.
