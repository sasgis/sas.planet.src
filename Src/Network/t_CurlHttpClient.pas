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

unit t_CurlHttpClient;

interface

uses
  Classes;

type
  {$IFNDEF UNICODE}
  RawByteString = AnsiString;
  {$ENDIF}

  TReqMethod = (rmHead, rmGet, rmPost);

  TCurlOptions = record
    StoreCookie: Boolean;
    FollowLocation: Boolean;
    AcceptEncoding: Boolean;
    IgnoreSSLCertificateErrors: Boolean;
    TimeOutMS: Integer;
    ConnectionTimeOutMS: Integer;
    class operator Equal(const A, B: TCurlOptions): Boolean; inline;
    class operator NotEqual(const A, B: TCurlOptions): Boolean; inline;
  end;
  PCurlOptions = ^TCurlOptions;

  TCurlAuthType = (
    atAny,
    atBasic,
    atDigest,
    atNegotiate,
    atNtlm,
    atDigestIE,
    atNtlmWB,
    atBearer
  );

  TCurlProxy = record
    Address: RawByteString;
    NoProxy: RawByteString;
    UserName: AnsiString;
    UserPass: AnsiString;
    AuthType: TCurlAuthType;
    class operator Equal(const A, B: TCurlProxy): Boolean; inline;
    class operator NotEqual(const A, B: TCurlProxy): Boolean; inline;
  end;
  PCurlProxy = ^TCurlProxy;

  TCurlRequest = record
    Method: TReqMethod;
    Url: RawByteString;
    Headers: RawByteString;
    PostData: Pointer;
    PostDataSize: Integer;
    Options: PCurlOptions;
    Proxy: PCurlProxy;
  end;
  PCurlRequest = ^TCurlRequest;

  TCurlResponse = record
    Code: Integer;
    Headers: RawByteString;
    Data: TMemoryStream;
    ErrorReason: string;
  end;
  PCurlResponse = ^TCurlResponse;

  TCurlDebugCallBack = procedure(
    const ADebugMsg: RawByteString;
    const AUserData: Pointer
  );

  TCurlProgressCallBack = procedure(
    const ATotal: Integer;
    const ADownload: Integer;
    const AUserData: Pointer
  );

implementation

{ TCurlOptions }

class operator TCurlOptions.Equal(const A, B: TCurlOptions): Boolean;
begin
  Result :=
    (A.StoreCookie = B.StoreCookie) and
    (A.FollowLocation = B.FollowLocation) and
    (A.IgnoreSSLCertificateErrors = B.IgnoreSSLCertificateErrors) and
    (A.TimeOutMS = B.TimeOutMS) and
    (A.ConnectionTimeOutMS = B.ConnectionTimeOutMS);
end;

class operator TCurlOptions.NotEqual(const A, B: TCurlOptions): Boolean;
begin
  Result := not (A = B);
end;

{ TCurlProxy }

class operator TCurlProxy.Equal(const A, B: TCurlProxy): Boolean;
begin
  Result :=
    (A.Address = B.Address) and
    (A.NoProxy = B.NoProxy) and
    (A.UserName = B.UserName) and
    (A.UserPass = B.UserPass) and
    (A.AuthType = B.AuthType);
end;

class operator TCurlProxy.NotEqual(const A, B: TCurlProxy): Boolean;
begin
  Result := not (A = B);
end;

end.
