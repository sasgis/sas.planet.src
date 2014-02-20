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
  VarRecUtils,
  i_BinaryData,
  i_DownloadRequest,
  i_DownloadResult,
  u_BaseInterfacedObject;

type
  TDownloadResult = class(TBaseInterfacedObject, IDownloadResult)
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
    FRawResponseHeader: AnsiString;
    FContentType: AnsiString;
    FData: IBinaryData;
  protected
    function GetIsServerExists: Boolean; override;
  protected
    function GetStatusCode: Cardinal;
    function GetRawResponseHeader: AnsiString;
    function GetContentType: AnsiString;
    function GetData: IBinaryData;
  public
    constructor Create(
      const ARequest: IDownloadRequest;
      const AStatusCode: Cardinal;
      const ARawResponseHeader: AnsiString;
      const AContentType: AnsiString;
      const AData: IBinaryData
    );
  end;

  TDownloadResultError = class(TDownloadResult, IDownloadResultError)
  private
    FErrorTextFormat: string;
    FErrorTextArgs: TConstArray;
  protected
    function GetErrorText: string;
  public
    constructor Create(
      const ARequest: IDownloadRequest;
      const AErrorTextFormat: string;
      const AErrorTextArgs: array of const
    );
    destructor Destroy; override;
  end;

  TDownloadResultProxyError = class(TDownloadResultError, IDownloadResultProxyError)
  protected
    function GetIsServerExists: Boolean; override;
  end;

  TDownloadResultNoConnetctToServer = class(TDownloadResultError, IDownloadResultNoConnetctToServer)
  protected
    function GetIsServerExists: Boolean; override;
  end;

  TDownloadResultUnknownError = class(TDownloadResultError, IDownloadResultUnknownError)
  protected
    function GetIsServerExists: Boolean; override;
  end;

  TDownloadResultNoConnetctToServerByErrorCode = class(TDownloadResultNoConnetctToServer)
  public
    constructor Create(
      const ARequest: IDownloadRequest;
      const AErrorTextFormat: string;
      const AErrorTextArgs: array of const;
      const AErrorCode: DWORD
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
      const AErrorTextFormat: string;
      const AErrorTextArgs: array of const;
      const AStatusCode: DWORD
    );
  end;

  TDownloadResultLoadErrorByUnknownStatusCode = class(TDownloadResultLoadError)
  public
    constructor Create(
      const ARequest: IDownloadRequest;
      const AErrorTextFormat: string;
      const AErrorTextArgs: array of const;
      const AStatusCode: DWORD
    );
  end;

  TDownloadResultLoadErrorByErrorCode = class(TDownloadResultLoadError)
  public
    constructor Create(
      const ARequest: IDownloadRequest;
      const AErrorTextFormat: string;
      const AErrorTextArgs: array of const;
      const AErrorCode: DWORD
    );
  end;

  TDownloadResultBanned = class(TDownloadResultError, IDownloadResultBanned, IDownloadResultWithServerRespond)
  private
    FStatusCode: Cardinal;
    FRawResponseHeader: AnsiString;
  protected
    function GetIsServerExists: Boolean; override;
    function GetStatusCode: Cardinal;
    function GetRawResponseHeader: AnsiString;
  public
    constructor Create(
      const ARequest: IDownloadRequest;
      const AStatusCode: Cardinal;
      const ARawResponseHeader: AnsiString;
      const AErrorTextFormat: string;
      const AErrorTextArgs: array of const
    );
  end;

  TDownloadResultBadContentType = class(TDownloadResultError, IDownloadResultBadContentType, IDownloadResultWithServerRespond)
  private
    FStatusCode: Cardinal;
    FRawResponseHeader: AnsiString;
    FContentType: AnsiString;
  protected
    function GetContentType: AnsiString;
  protected
    function GetIsServerExists: Boolean; override;
    function GetStatusCode: Cardinal;
    function GetRawResponseHeader: AnsiString;
  public
    constructor Create(
      const ARequest: IDownloadRequest;
      const AContentType: AnsiString;
      const AStatusCode: Cardinal;
      const ARawResponseHeader: AnsiString;
      const AErrorTextFormat: string;
      const AErrorTextArgs: array of const
    );
  end;

  TDownloadResultDataNotExists = class(TDownloadResult, IDownloadResultDataNotExists, IDownloadResultWithServerRespond)
  private
    FReasonTextFormat: string;
    FReasonTextArgs: TConstArray;
    FStatusCode: Cardinal;
    FRawResponseHeader: AnsiString;
  protected
    function GetIsServerExists: Boolean; override;
  protected
    function GetReasonText: string;
    function GetStatusCode: Cardinal;
    function GetRawResponseHeader: AnsiString;
  public
    constructor Create(
      const ARequest: IDownloadRequest;
      const AReasonTextFormat: string;
      const AReasonTextArgs: array of const;
      const AStatusCode: Cardinal;
      const ARawResponseHeader: AnsiString
    );
    destructor Destroy; override;
  end;

  TDownloadResultDataNotExistsByStatusCode = class(TDownloadResultDataNotExists)
  public
    constructor Create(
      const ARequest: IDownloadRequest;
      const ARawResponseHeader: AnsiString;
      const AErrorTextFormat: string;
      const AErrorTextArgs: array of const;
      const AStatusCode: DWORD
    );
  end;

  TDownloadResultDataNotExistsZeroSize = class(TDownloadResultDataNotExists)
  public
    constructor Create(
      const ARequest: IDownloadRequest;
      const AStatusCode: Cardinal;
      const ARawResponseHeader: AnsiString;
      const AErrorTextFormat: string;
      const AErrorTextArgs: array of const
    );
  end;

  TDownloadResultNotNecessary = class(TDownloadResult, IDownloadResultNotNecessary, IDownloadResultWithServerRespond)
  private
    FReasonTextFormat: string;
    FReasonTextArgs: TConstArray;
    FStatusCode: Cardinal;
    FRawResponseHeader: AnsiString;
  protected
    function GetIsServerExists: Boolean; override;
  protected
    function GetReasonText: string;
    function GetStatusCode: Cardinal;
    function GetRawResponseHeader: AnsiString;
  public
    constructor Create(
      const ARequest: IDownloadRequest;
      const AReasonTextFormat: string;
      const AReasonTextArgs: array of const;
      const AStatusCode: Cardinal;
      const ARawResponseHeader: AnsiString
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  gnugettext;

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
  const AStatusCode: Cardinal;
  const ARawResponseHeader, AContentType: AnsiString;
  const AData: IBinaryData
);
begin
  inherited Create(ARequest);
  FStatusCode := AStatusCode;
  FRawResponseHeader := ARawResponseHeader;
  FContentType := AContentType;
  FData := AData;
end;

function TDownloadResultOk.GetContentType: AnsiString;
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

function TDownloadResultOk.GetRawResponseHeader: AnsiString;
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
  const AErrorTextFormat: string;
  const AErrorTextArgs: array of const
);
begin
  inherited Create(ARequest);
  FErrorTextFormat := AErrorTextFormat;
  FErrorTextArgs := CreateConstArray(AErrorTextArgs);
end;

destructor TDownloadResultError.Destroy;
begin
  FinalizeConstArray(FErrorTextArgs);
  inherited;
end;

function TDownloadResultError.GetErrorText: string;
begin
  try
    if Length(FErrorTextArgs)>0 then
      Result := Format(gettext(FErrorTextFormat), FErrorTextArgs)
    else
      Result := FErrorTextFormat;
  except
    Result := FErrorTextFormat;
  end;
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

{  TDownloadResultUnknownError }

function TDownloadResultUnknownError.GetIsServerExists: Boolean;
begin
  Result := False;
end;

{ TDownloadResultBanned }

constructor TDownloadResultBanned.Create(
  const ARequest: IDownloadRequest;
  const AStatusCode: Cardinal;
  const ARawResponseHeader: AnsiString;
  const AErrorTextFormat: string;
  const AErrorTextArgs: array of const
);
begin
  inherited Create(ARequest, AErrorTextFormat, AErrorTextArgs);
  FStatusCode := AStatusCode;
  FRawResponseHeader := ARawResponseHeader;
end;

function TDownloadResultBanned.GetIsServerExists: Boolean;
begin
  Result := True;
end;

function TDownloadResultBanned.GetRawResponseHeader: AnsiString;
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
  const AContentType: AnsiString;
  const AStatusCode: Cardinal;
  const ARawResponseHeader: AnsiString;
  const AErrorTextFormat: string;
  const AErrorTextArgs: array of const
);
begin
  inherited Create(ARequest, AErrorTextFormat, AErrorTextArgs);
  FContentType := AContentType;
  FStatusCode := AStatusCode;
  FRawResponseHeader := ARawResponseHeader;
end;

function TDownloadResultBadContentType.GetContentType: AnsiString;
begin
  Result := FContentType;
end;

function TDownloadResultBadContentType.GetIsServerExists: Boolean;
begin
  Result := True;
end;

function TDownloadResultBadContentType.GetRawResponseHeader: AnsiString;
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
  const AErrorTextFormat: string;
  const AErrorTextArgs: array of const;
  const AErrorCode: DWORD
);
begin
  inherited Create(ARequest, AErrorTextFormat, AErrorTextArgs);
end;

{ TDownloadResultLoadErrorByStatusCode }

constructor TDownloadResultLoadErrorByStatusCode.Create(
  const ARequest: IDownloadRequest;
  const AErrorTextFormat: string;
  const AErrorTextArgs: array of const;
  const AStatusCode: DWORD
);
begin
  inherited Create(ARequest, AErrorTextFormat, AErrorTextArgs);
end;

{ TDownloadResultLoadErrorByErrorCode }

constructor TDownloadResultLoadErrorByErrorCode.Create(
  const ARequest: IDownloadRequest;
  const AErrorTextFormat: string;
  const AErrorTextArgs: array of const;
  const AErrorCode: DWORD
);
begin
  inherited Create(ARequest, AErrorTextFormat, AErrorTextArgs);
end;

{ TDownloadResultLoadError }

function TDownloadResultLoadError.GetIsServerExists: Boolean;
begin
  Result := True;
end;

{ TIDownloadResultDataNotExists }

constructor TDownloadResultDataNotExists.Create(
  const ARequest: IDownloadRequest;
  const AReasonTextFormat: string;
  const AReasonTextArgs: array of const;
  const AStatusCode: Cardinal;
  const ARawResponseHeader: AnsiString
);
begin
  inherited Create(ARequest);
  FReasonTextFormat := AReasonTextFormat;
  FReasonTextArgs := CreateConstArray(AReasonTextArgs);
  FStatusCode := AStatusCode;
  FRawResponseHeader := ARawResponseHeader;
end;

destructor TDownloadResultDataNotExists.Destroy;
begin
  FinalizeConstArray(FReasonTextArgs);
  inherited;
end;

function TDownloadResultDataNotExists.GetIsServerExists: Boolean;
begin
  Result := True;
end;

function TDownloadResultDataNotExists.GetRawResponseHeader: AnsiString;
begin
  Result := FRawResponseHeader;
end;

function TDownloadResultDataNotExists.GetReasonText: string;
begin
  Result := Format(gettext(FReasonTextFormat), FReasonTextArgs);
end;

function TDownloadResultDataNotExists.GetStatusCode: Cardinal;
begin
  Result := FStatusCode;
end;

{ TDownloadResultNotNecessary }

constructor TDownloadResultNotNecessary.Create(
  const ARequest: IDownloadRequest;
  const AReasonTextFormat: string;
  const AReasonTextArgs: array of const;
  const AStatusCode: Cardinal;
  const ARawResponseHeader: AnsiString
);
begin
  inherited Create(ARequest);
  FReasonTextFormat := AReasonTextFormat;
  FReasonTextArgs := CreateConstArray(AReasonTextArgs);
  FStatusCode := AStatusCode;
  FRawResponseHeader := ARawResponseHeader;
end;

destructor TDownloadResultNotNecessary.Destroy;
begin
  FinalizeConstArray(FReasonTextArgs);
  inherited;
end;

function TDownloadResultNotNecessary.GetIsServerExists: Boolean;
begin
  Result := True;
end;

function TDownloadResultNotNecessary.GetRawResponseHeader: AnsiString;
begin
  Result := FRawResponseHeader;
end;

function TDownloadResultNotNecessary.GetReasonText: string;
begin
  Result := Format(gettext(FReasonTextFormat), FReasonTextArgs);
end;

function TDownloadResultNotNecessary.GetStatusCode: Cardinal;
begin
  Result := FStatusCode;
end;

{ TDownloadResultDataNotExistsByStatusCode }

constructor TDownloadResultDataNotExistsByStatusCode.Create(
  const ARequest: IDownloadRequest;
  const ARawResponseHeader: AnsiString;
  const AErrorTextFormat: string;
  const AErrorTextArgs: array of const;
  const AStatusCode: DWORD
);
begin
  inherited Create(ARequest, AErrorTextFormat, AErrorTextArgs, AStatusCode, ARawResponseHeader);
end;

{ TDownloadResultDataNotExistsZeroSize }

constructor TDownloadResultDataNotExistsZeroSize.Create(
  const ARequest: IDownloadRequest;
  const AStatusCode: Cardinal;
  const ARawResponseHeader: AnsiString;
  const AErrorTextFormat: string;
  const AErrorTextArgs: array of const
);
begin
  inherited Create(ARequest, AErrorTextFormat, AErrorTextArgs, AStatusCode, ARawResponseHeader);
end;

{ TDownloadResultLoadErrorByUnknownStatusCode }

constructor TDownloadResultLoadErrorByUnknownStatusCode.Create(
  const ARequest: IDownloadRequest;
  const AErrorTextFormat: string;
  const AErrorTextArgs: array of const;
  const AStatusCode: DWORD
);
begin
  inherited Create(ARequest, AErrorTextFormat, AErrorTextArgs);
end;

{ TDownloadResultCanceled }

function TDownloadResultCanceled.GetIsServerExists: Boolean;
begin
  Result := False;
end;

end.
