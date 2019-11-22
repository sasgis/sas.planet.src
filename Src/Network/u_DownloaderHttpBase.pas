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
      const AStatusCode: Cardinal;
      const AContentType: AnsiString;
      const ARawHeaderText: AnsiString
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
  i_DownloadChecker,
  u_AsyncRequestHelperThread,
  u_BinaryData,
  u_ContentDecoder,
  u_HttpStatusChecker,
  u_StrFunc,
  u_NetworkStrFunc;

function DetectContentType(const AData: Pointer; const ASize: Int64): RawByteString;
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
    Result := LowerCaseA(AnsiString(VContentType));

    // fix detected mime types for IE versions prior IE 9
    if Result = 'image/x-png' then begin
      Result := 'image/png';
    end else
    if Result = 'image/pjpeg' then begin
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
  VRawHeaderText: RawByteString;
  VContentType: RawByteString;
  VContentEncoding: RawByteString;
  VDetectedContentType: RawByteString;
begin
  Result := nil;

  if FResultFactory = nil then begin
    Exit;
  end;

  VStatusCode := AStatusCode;

  if IsOkStatus(VStatusCode) then begin
    VRawHeaderText := ARawHeaderText;

    if ATryDecodeContent then begin
      VContentEncoding := GetHeaderValueUp(VRawHeaderText, 'CONTENT-ENCODING');
      try
        TContentDecoder.Decode(VContentEncoding, AResponseBody);
        DeleteHeaderValueUp(VRawHeaderText, 'CONTENT-ENCODING');
      except
        on E: Exception do begin
          VResponseBody := TBinaryData.Create(AResponseBody.Size, AResponseBody.Memory);
          Result := FResultFactory.BuildBadContentEncoding(ARequest, VStatusCode,
            VRawHeaderText, VContentEncoding, VResponseBody, '%s: %s', [E.ClassName, E.Message]);
          Exit;
        end;
      end;
    end;

    VContentType := GetHeaderValueUp(VRawHeaderText, 'CONTENT-TYPE');
    if ATryDetectContentType and (AResponseBody.Size > 0) then begin
      VDetectedContentType := DetectContentType(AResponseBody.Memory, AResponseBody.Size);
      if (VDetectedContentType <> '') and (VDetectedContentType <> LowerCaseA(VContentType)) then begin
        if not ReplaceHeaderValueUp(VRawHeaderText, 'CONTENT-TYPE', VDetectedContentType) then begin
          AddHeaderValue(VRawHeaderText, 'Content-Type', VDetectedContentType);
        end;
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
  const AStatusCode: Cardinal;
  const AContentType: AnsiString;
  const ARawHeaderText: AnsiString
): IDownloadResult;
var
  VStatusCode: Cardinal;
  VContentType: AnsiString;
  VRawHeaderText: AnsiString;
  VRequestWithChecker: IRequestWithChecker;
begin
  VStatusCode := AStatusCode;
  VContentType := AContentType;
  VRawHeaderText := ARawHeaderText;

  if Supports(ARequest, IRequestWithChecker, VRequestWithChecker) then begin
    Result :=
      VRequestWithChecker.Checker.AfterReciveData(
        FResultFactory,
        ARequest,
        AResponseBody,
        VStatusCode,
        VContentType,
        VRawHeaderText
      );
    if Result <> nil then begin
      Exit;
    end;
  end;

  if AResponseBody.Size > 0 then begin
    Result :=
      FResultFactory.BuildOk(
        ARequest,
        VStatusCode,
        VRawHeaderText,
        VContentType,
        AResponseBody
      );
  end else begin
    Result :=
      FResultFactory.BuildDataNotExistsZeroSize(
        ARequest,
        VStatusCode,
        VRawHeaderText
      );
  end;
end;

end.
