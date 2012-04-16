{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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

unit u_DownloadResult;

interface

uses
  Types,
  i_BinaryData,
  i_DownloadRequest,
  i_DownloadResult;

type
  TDownloadResult = class(TInterfacedObject, IDownloadResult)
  private
    FRequest: IDownloadRequest;
  protected
    function GetRequest: IDownloadRequest;
    function GetIsServerExists: Boolean; virtual; abstract;
  public
    constructor Create(
      const ARequest: IDownloadRequest
    );
  end;

  TDownloadResultCanceled = class(TDownloadResult, IDownloadResultCanceled)
  protected
    function GetIsServerExists: Boolean; override;
  end;

  TDownloadResultOk = class(TDownloadResult, IDownloadResultOk, IDownloadResultWithServerRespond)
  private
    FStatusCode: Cardinal;
    FRawResponseHeader: string;
    FContentType: string;
    FData: IBinaryData;
  protected
    function GetIsServerExists: Boolean; override;
  protected
    function GetStatusCode: Cardinal;
    function GetRawResponseHeader: string;
    function GetContentType: string;
    function GetData: IBinaryData;
  public
    constructor Create(
      const ARequest: IDownloadRequest;
      AStatusCode: Cardinal;
      const ARawResponseHeader: string;
      const AContentType: string;
      const AData: IBinaryData
    );
  end;

  TDownloadResultError = class(TDownloadResult, IDownloadResultError)
  private
    FErrorText: string;
  protected
    function GetErrorText: string;
  public
    constructor Create(
      const ARequest: IDownloadRequest;
      const AErrorText: string
    );
  end;

  TDownloadResultProxyError = class(TDownloadResultError, IDownloadResultProxyError)
  protected
    function GetIsServerExists: Boolean; override;
  end;

  TDownloadResultNoConnetctToServer = class(TDownloadResultError, IDownloadResultNoConnetctToServer)
  protected
    function GetIsServerExists: Boolean; override;
  end;

  TDownloadResultNoConnetctToServerByErrorCode = class(TDownloadResultNoConnetctToServer)
  public
    constructor Create(
      const ARequest: IDownloadRequest;
      const AErrorText: string;
      AErrorCode: DWORD
    );
  end;

  TDownloadResultLoadError = class(TDownloadResultError)
  protected
    function GetIsServerExists: Boolean; override;
  end;

  TDownloadResultLoadErrorByStatusCode = class(TDownloadResultLoadError)
  public
    constructor Create(
      const ARequest: IDownloadRequest;
      const AErrorText: string;
      AStatusCode: DWORD
    );
  end;

  TDownloadResultLoadErrorByUnknownStatusCode = class(TDownloadResultLoadError)
  public
    constructor Create(
      const ARequest: IDownloadRequest;
      const AErrorText: string;
      AStatusCode: DWORD
    );
  end;

  TDownloadResultLoadErrorByErrorCode = class(TDownloadResultLoadError)
  public
    constructor Create(
      const ARequest: IDownloadRequest;
      const AErrorText: string;
      AErrorCode: DWORD
    );
  end;

  TDownloadResultBanned = class(TDownloadResultError, IDownloadResultBanned, IDownloadResultWithServerRespond)
  private
    FStatusCode: Cardinal;
    FRawResponseHeader: string;
  protected
    function GetIsServerExists: Boolean; override;
    function GetStatusCode: Cardinal;
    function GetRawResponseHeader: string;
  public
    constructor Create(
      const ARequest: IDownloadRequest;
      AStatusCode: Cardinal;
      const ARawResponseHeader: string;
      const AErrorText: string
    );
  end;

  TDownloadResultBadContentType = class(TDownloadResultError, IDownloadResultBadContentType, IDownloadResultWithServerRespond)
  private
    FStatusCode: Cardinal;
    FRawResponseHeader: string;
    FContentType: string;
  protected
    function GetContentType: string;
  protected
    function GetIsServerExists: Boolean; override;
    function GetStatusCode: Cardinal;
    function GetRawResponseHeader: string;
  public
    constructor Create(
      const ARequest: IDownloadRequest;
      const AContentType: string;
      AStatusCode: Cardinal;
      const ARawResponseHeader: string;
      const AErrorText: string
    );
  end;

  TDownloadResultDataNotExists = class(TDownloadResult, IDownloadResultDataNotExists, IDownloadResultWithServerRespond)
  private
    FReasonText: string;
    FStatusCode: Cardinal;
    FRawResponseHeader: string;
  protected
    function GetIsServerExists: Boolean; override;
  protected
    function GetReasonText: string;
    function GetStatusCode: Cardinal;
    function GetRawResponseHeader: string;
  public
    constructor Create(
      const ARequest: IDownloadRequest;
      const AReasonText: string;
      AStatusCode: Cardinal;
      const ARawResponseHeader: string
    );
  end;

  TDownloadResultDataNotExistsByStatusCode = class(TDownloadResultDataNotExists)
  public
    constructor Create(
      const ARequest: IDownloadRequest;
      const ARawResponseHeader: string;
      const AErrorText: string;
      AStatusCode: DWORD
    );
  end;

  TDownloadResultDataNotExistsZeroSize = class(TDownloadResultDataNotExists)
  public
    constructor Create(
      const ARequest: IDownloadRequest;
      AStatusCode: Cardinal;
      const ARawResponseHeader: string;
      const AErrorText: string
    );
  end;

  TDownloadResultNotNecessary = class(TDownloadResult, IDownloadResultNotNecessary, IDownloadResultWithServerRespond)
  private
    FReasonText: string;
    FStatusCode: Cardinal;
    FRawResponseHeader: string;
  protected
    function GetIsServerExists: Boolean; override;
  protected
    function GetReasonText: string;
    function GetStatusCode: Cardinal;
    function GetRawResponseHeader: string;
  public
    constructor Create(
      const ARequest: IDownloadRequest;
      const AReasonText: string;
      AStatusCode: Cardinal;
      const ARawResponseHeader: string
    );
  end;

implementation

uses
  SysUtils;

{ TDownloadResult }

constructor TDownloadResult.Create(
  const ARequest: IDownloadRequest
);
begin
  inherited Create;
  FRequest := ARequest;
end;

function TDownloadResult.GetRequest: IDownloadRequest;
begin
  Result := FRequest;
end;

{ TDownloadResultOk }

constructor TDownloadResultOk.Create(
  const ARequest: IDownloadRequest;
  AStatusCode: Cardinal;
  const ARawResponseHeader, AContentType: string;
  const AData: IBinaryData
);
begin
  inherited Create(ARequest);
  FStatusCode := AStatusCode;
  FRawResponseHeader := ARawResponseHeader;
  FContentType := AContentType;
  FData := AData;
end;

function TDownloadResultOk.GetContentType: string;
begin
  Result := FContentType;
end;

function TDownloadResultOk.GetData: IBinaryData;
begin
  Result := FData;
end;

function TDownloadResultOk.GetIsServerExists: Boolean;
begin
  Result := True;
end;

function TDownloadResultOk.GetRawResponseHeader: string;
begin
  Result := FRawResponseHeader;
end;

function TDownloadResultOk.GetStatusCode: Cardinal;
begin
  Result := FStatusCode;
end;

{ TDownloadResultError }

constructor TDownloadResultError.Create(
  const ARequest: IDownloadRequest;
  const AErrorText: string
);
begin
  inherited Create(ARequest);
  FErrorText := AErrorText;
end;

function TDownloadResultError.GetErrorText: string;
begin
  Result := FErrorText;
end;

{ TDownloadResultProxyError }

function TDownloadResultProxyError.GetIsServerExists: Boolean;
begin
  Result := False;
end;

{ TDownloadResultNoConnetctToServer }

function TDownloadResultNoConnetctToServer.GetIsServerExists: Boolean;
begin
  Result := False;
end;

{ TDownloadResultBanned }

constructor TDownloadResultBanned.Create(
  const ARequest: IDownloadRequest;
  AStatusCode: Cardinal;
  const ARawResponseHeader, AErrorText: string
);
begin
  inherited Create(ARequest, AErrorText);
  FStatusCode := AStatusCode;
  FRawResponseHeader := ARawResponseHeader;
end;

function TDownloadResultBanned.GetIsServerExists: Boolean;
begin
  Result := True;
end;

function TDownloadResultBanned.GetRawResponseHeader: string;
begin
  Result := FRawResponseHeader;
end;

function TDownloadResultBanned.GetStatusCode: Cardinal;
begin
  Result := FStatusCode;
end;

{ TDownloadResultBadContentType }

constructor TDownloadResultBadContentType.Create(
  const ARequest: IDownloadRequest;
  const AContentType: string;
  AStatusCode: Cardinal;
  const ARawResponseHeader, AErrorText: string
);
begin
  inherited Create(ARequest, Format(AErrorText, [AContentType]));
  FContentType := AContentType;
  FStatusCode := AStatusCode;
  FRawResponseHeader := ARawResponseHeader;
end;

function TDownloadResultBadContentType.GetContentType: string;
begin
  Result := FContentType;
end;

function TDownloadResultBadContentType.GetIsServerExists: Boolean;
begin
  Result := True;
end;

function TDownloadResultBadContentType.GetRawResponseHeader: string;
begin
  Result := FRawResponseHeader;
end;

function TDownloadResultBadContentType.GetStatusCode: Cardinal;
begin
  Result := FStatusCode;
end;

{ TDownloadResultNoConnetctToServerByErrorCode }

constructor TDownloadResultNoConnetctToServerByErrorCode.Create(
  const ARequest: IDownloadRequest;
  const AErrorText: string;
  AErrorCode: DWORD
);
begin
  inherited Create(ARequest, Format(AErrorText, [AErrorCode]));
end;

{ TDownloadResultLoadErrorByStatusCode }

constructor TDownloadResultLoadErrorByStatusCode.Create(
  const ARequest: IDownloadRequest;
  const AErrorText: string;
  AStatusCode: DWORD
);
begin
  inherited Create(ARequest, Format(AErrorText, [AStatusCode]));
end;

{ TDownloadResultLoadErrorByErrorCode }

constructor TDownloadResultLoadErrorByErrorCode.Create(
  const ARequest: IDownloadRequest;
  const AErrorText: string;
  AErrorCode: DWORD
);
begin
  inherited Create(ARequest, Format(AErrorText, [AErrorCode]));
end;

{ TDownloadResultLoadError }

function TDownloadResultLoadError.GetIsServerExists: Boolean;
begin
  Result := True;
end;

{ TIDownloadResultDataNotExists }

constructor TDownloadResultDataNotExists.Create(
  const ARequest: IDownloadRequest;
  const AReasonText: string;
  AStatusCode: Cardinal;
  const ARawResponseHeader: string
);
begin
  inherited Create(ARequest);
  FReasonText := AReasonText;
  FStatusCode := AStatusCode;
  FRawResponseHeader := ARawResponseHeader;
end;

function TDownloadResultDataNotExists.GetIsServerExists: Boolean;
begin
  Result := True;
end;

function TDownloadResultDataNotExists.GetRawResponseHeader: string;
begin
  Result := FRawResponseHeader;
end;

function TDownloadResultDataNotExists.GetReasonText: string;
begin
  Result := FReasonText;
end;

function TDownloadResultDataNotExists.GetStatusCode: Cardinal;
begin
  Result := FStatusCode;
end;

{ TDownloadResultNotNecessary }

constructor TDownloadResultNotNecessary.Create(
  const ARequest: IDownloadRequest;
  const AReasonText: string;
  AStatusCode: Cardinal;
  const ARawResponseHeader: string
);
begin
  inherited Create(ARequest);
  FReasonText := AReasonText;
  FStatusCode := AStatusCode;
  FRawResponseHeader := ARawResponseHeader;
end;

function TDownloadResultNotNecessary.GetIsServerExists: Boolean;
begin
  Result := True;
end;

function TDownloadResultNotNecessary.GetRawResponseHeader: string;
begin
  Result := FRawResponseHeader;
end;

function TDownloadResultNotNecessary.GetReasonText: string;
begin
  Result := FReasonText;
end;

function TDownloadResultNotNecessary.GetStatusCode: Cardinal;
begin
  Result := FStatusCode;
end;

{ TDownloadResultDataNotExistsByStatusCode }

constructor TDownloadResultDataNotExistsByStatusCode.Create(
  const ARequest: IDownloadRequest;
  const ARawResponseHeader, AErrorText: string;
  AStatusCode: DWORD
);
begin
  inherited Create(ARequest, Format(AErrorText, [AStatusCode]), AStatusCode, ARawResponseHeader);
end;

{ TDownloadResultDataNotExistsZeroSize }

constructor TDownloadResultDataNotExistsZeroSize.Create(
  const ARequest: IDownloadRequest;
  AStatusCode: Cardinal;
  const ARawResponseHeader, AErrorText: string
);
begin
  inherited Create(ARequest, AErrorText, AStatusCode, ARawResponseHeader);
end;

{ TDownloadResultLoadErrorByUnknownStatusCode }

constructor TDownloadResultLoadErrorByUnknownStatusCode.Create(
  const ARequest: IDownloadRequest;
  const AErrorText: string;
  AStatusCode: DWORD
);
begin
  inherited Create(ARequest, Format(AErrorText, [AStatusCode]));
end;

{ TDownloadResultCanceled }

function TDownloadResultCanceled.GetIsServerExists: Boolean;
begin
  Result := False;
end;

end.
