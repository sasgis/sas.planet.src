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

unit u_DownloaderHttpBase;

interface

uses
  Classes,
  i_BinaryData,
  i_Downloader,
  i_DownloadResult,
  i_DownloadRequest,
  i_DownloadResultFactory,
  i_NotifierOperation,
  u_BaseInterfacedObject;

type
  TDownloaderHttpBase = class(TBaseInterfacedObject, IDownloaderAsync)
  protected
    FResultFactory: IDownloadResultFactory;

    function OnAfterResponse(
      const ATryDecodeContent: Boolean;
      const ATryDetectContentType: Boolean;
      const ARequest: IDownloadRequest;
      const AStatusCode: Cardinal;
      const ARawHeaderText: AnsiString;
      var AResponseBody: TMemoryStream
    ): IDownloadResult;

    function InternalMakeResponse(
      const ARequest: IDownloadRequest;
      const AResponseBody: IBinaryData;
      var AStatusCode: Cardinal;
      var AContentType: AnsiString;
      var ARawHeaderText: AnsiString
    ): IDownloadResult;
  private
    { IDownloaderAsync }
    procedure DoRequestAsync(
      const ARequest: IDownloadRequest;
      const ACancelNotifier: INotifierOperation;
      const AOperationID: Integer;
      const AOnResultCallBack: TRequestAsyncCallBack
    );
  public
    constructor Create(
      const AResultFactory: IDownloadResultFactory
    );
  end;

implementation

uses
  UrlMon,
  SysUtils,
  ALString,
  i_DownloadChecker,
  u_AsyncRequestHelperThread,
  u_BinaryData,
  u_ContentDecoder,
  u_HttpStatusChecker,
  u_StrFunc;

function DetectContentType(const AData: Pointer; const ASize: Int64): AnsiString;
const
  // IE9. Returns image/png and image/jpeg instead of image/x-png and image/pjpeg
  FMFD_RETURNUPDATEDIMGMIMES = $20;
var
  VResult: HRESULT;
  VContentType: PWideChar;
begin
  Assert(AData <> nil);
  Assert(ASize > 0);

  Result := '';

  VResult := UrlMon.FindMimeFromData(nil, nil, AData, ASize, nil,
    FMFD_RETURNUPDATEDIMGMIMES, VContentType, 0);

  if VResult = S_OK then begin
    Result := AnsiString(VContentType);

    // fix detected mime types for IE versions prior IE 9
    if AlLowerCase(Result) = 'image/x-png' then begin
      Result := 'image/png';
    end else if AlLowerCase(Result) = 'image/pjpeg' then begin
      Result := 'image/jpeg';
    end;
  end;
end;

{ TDownloaderHttpBase }

constructor TDownloaderHttpBase.Create(
  const AResultFactory: IDownloadResultFactory
);
begin
  Assert(AResultFactory <> nil);
  inherited Create;
  FResultFactory := AResultFactory;
end;

procedure TDownloaderHttpBase.DoRequestAsync(
  const ARequest: IDownloadRequest;
  const ACancelNotifier: INotifierOperation;
  const AOperationID: Integer;
  const AOnResultCallBack: TRequestAsyncCallBack
);
begin
  TAsyncRequestHelperThread.Create(
    (Self as IDownloader),
    ARequest,
    ACancelNotifier,
    AOperationID,
    AOnResultCallBack
  );
end;

function TDownloaderHttpBase.OnAfterResponse(
  const ATryDecodeContent: Boolean;
  const ATryDetectContentType: Boolean;
  const ARequest: IDownloadRequest;
  const AStatusCode: Cardinal;
  const ARawHeaderText: AnsiString;
  var AResponseBody: TMemoryStream
): IDownloadResult;
var
  VStatusCode: Cardinal;
  VResponseBody: IBinaryData;
  VRawHeaderText: AnsiString;
  VContentType: AnsiString;
  VContentEncoding: AnsiString;
  VDetectedContentType: AnsiString;
begin
  Result := nil;

  if FResultFactory = nil then begin
    Exit;
  end;

  VStatusCode := AStatusCode;

  if IsOkStatus(VStatusCode) then begin
    VRawHeaderText := ARawHeaderText;

    if ATryDecodeContent then begin
      VContentEncoding := GetHeaderValue(VRawHeaderText, 'Content-Encoding');
      try
        TContentDecoder.Decode(VContentEncoding, AResponseBody);
        VRawHeaderText := DeleteHeaderEntry(VRawHeaderText, 'Content-Encoding');
      except
        on E: Exception do begin
          VResponseBody := TBinaryData.Create(AResponseBody.Size, AResponseBody.Memory);
          Result := FResultFactory.BuildBadContentEncoding(ARequest, VStatusCode,
            VRawHeaderText, VContentEncoding, VResponseBody, '%s: %s', [E.ClassName, E.Message]);
          Exit;
        end;
      end;
    end;

    VContentType := GetHeaderValue(VRawHeaderText, 'Content-Type');
    if ATryDetectContentType and (AResponseBody.Size > 0) then begin
      VDetectedContentType := DetectContentType(AResponseBody.Memory, AResponseBody.Size);
      if (VDetectedContentType <> '') and (AlLowerCase(VDetectedContentType) <> AlLowerCase(VContentType)) then begin
        VRawHeaderText := SetHeaderValue(VRawHeaderText, 'Content-Type', VDetectedContentType);
        VContentType := VDetectedContentType;
      end;
    end;

    VResponseBody :=
      TBinaryData.Create(
        AResponseBody.Size,
        AResponseBody.Memory
      );

    Result :=
      InternalMakeResponse(
        ARequest,
        VResponseBody,
        VStatusCode,
        VContentType,
        VRawHeaderText
      );
  end else if IsDownloadErrorStatus(VStatusCode) then begin
    Result :=
      FResultFactory.BuildLoadErrorByStatusCode(
        ARequest,
        VStatusCode
      );
  end else if IsContentNotExistStatus(VStatusCode) then begin
    Result :=
      FResultFactory.BuildDataNotExistsByStatusCode(
        ARequest,
        ARawHeaderText,
        VStatusCode
      );
  end else begin
    Result :=
      FResultFactory.BuildLoadErrorByUnknownStatusCode(
        ARequest,
        VStatusCode
      );
  end;
end;

function TDownloaderHttpBase.InternalMakeResponse(
  const ARequest: IDownloadRequest;
  const AResponseBody: IBinaryData;
  var AStatusCode: Cardinal;
  var AContentType: AnsiString;
  var ARawHeaderText: AnsiString
): IDownloadResult;
var
  VRequestWithChecker: IRequestWithChecker;
begin
  if Supports(ARequest, IRequestWithChecker, VRequestWithChecker) then begin
    Result :=
      VRequestWithChecker.Checker.AfterReciveData(
        FResultFactory,
        ARequest,
        AResponseBody,
        AStatusCode,
        AContentType,
        ARawHeaderText
      );
    if Result <> nil then begin
      Exit;
    end;
  end;

  if AResponseBody.Size > 0 then begin
    Result :=
      FResultFactory.BuildOk(
        ARequest,
        AStatusCode,
        ARawHeaderText,
        AContentType,
        AResponseBody
      );
  end else begin
    Result :=
      FResultFactory.BuildDataNotExistsZeroSize(
        ARequest,
        AStatusCode,
        ARawHeaderText
      );
  end;
end;

end.
